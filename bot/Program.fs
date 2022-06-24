module TorrentBot.App

open System
open TorrentBot.Common

module Parser =
    open HtmlAgilityPack
    open System.IO

    type ParserConfig =
        { nodes: string
          title: string
          link: string }

    type Item = { title: string; link: Uri }

    let parse (config: ParserConfig) (baseUrl: Uri) (data: byte []) =
        use stream = new MemoryStream(data)
        Text.Encoding.RegisterProvider(Text.CodePagesEncodingProvider.Instance)

        let doc = HtmlDocument()
        doc.Load(stream, Text.Encoding.GetEncoding(1251))

        let nodes = doc.DocumentNode.SelectNodes(config.nodes)

        if isNull nodes then
            Error(Exception.stackTrace "Not found items in search html page")
        else
            nodes
            |> Seq.map (fun node ->
                { title =
                    node.SelectSingleNode(config.title).InnerHtml
                    |> Web.HttpUtility.HtmlDecode
                  link =
                    Uri(
                        baseUrl,
                        node
                            .SelectSingleNode(config.link)
                            .GetAttributeValue("href", "")
                    ) })
            |> Array.ofSeq
            |> Ok

type AddTorrentToRssRequested =
    | AddTorrentToRssRequested of user: string * torrent: byte []
    interface Cmd

type Env =
    { salt: string
      origin: string
      cookies: Map<string, string> }

module App =
    type private SearchDownloaded =
        | SearchDownloaded of user: string * domain: string * url: Uri * response: Result<byte [], exn>
        interface Msg

    type private TorrentDownloaded =
        | TorrentDownloaded of user: string * response: Result<byte [], exn>
        interface Msg

    let private formatSearchResult (domain: string) (xs: Parser.Item seq) =
        xs
        |> Seq.truncate 10
        |> Seq.map (fun x ->
            let cmd = String.toBase64Url $"%s{domain}%s{x.link.PathAndQuery}"
            sprintf """%s <a href="https://t.me/$ME$?start=%s">[DOWNLOAD]</a>""" (String.elipsize 50 x.title) cmd)
        |> String.concat "\n"
        |> sprintf "Search result:\n%s"

    let private handleBotMessage (env: Env) user (text: string) : string * Cmd list =
        match text.Split(' ', 3) with
        | [| "/search"; domain; query |] ->
            let url = $"https://%s{domain}/forum/tracker.php?nm=%s{Uri.EscapeDataString(query)}"

            "Search…", [ Http.HttpGetCmd(url, env.cookies, (fun r -> SearchDownloaded(user, domain, Uri(url), r))) ]
        | [| "/start"; encUrl |] ->
            let url = String.fromBase64Url encUrl

            "Download scheduled",
            [ Http.HttpGetCmd($"https://%s{url}", env.cookies, (fun r -> TorrentDownloaded(user, r))) ]
        | [| "/my_rss_link" |] ->
            let url =
                KeyGenerator.encode env.salt user
                |> sprintf "%srss/%s" env.origin

            $"You RSS link: {url}", []
        | _ -> "Unknown command or wrong parameters", []

    let onMessage configs (env: Env) (msg: Msg) : Cmd list =
        match msg with
        | :? Bot.NewBotMessage as Bot.NewBotMessage (user, text) ->
            let botResp, cmds = handleBotMessage env user text
            Bot.SendBotResponse(user, botResp, false) :: cmds
        | :? SearchDownloaded as SearchDownloaded (user, domain, url, response) ->
            let parseConfig = Map.find domain configs

            response
            |> Result.bind (Parser.parse parseConfig url)
            |> function
                | Ok data -> formatSearchResult domain data
                | Error e ->
                    eprintfn $"{e}"
                    $"Error: {e.Message}"
            |> fun text -> [ Bot.SendBotResponse(user, text, true) ]
        | :? TorrentDownloaded as TorrentDownloaded (user, Ok data) -> [ AddTorrentToRssRequested(user, data) ]
        | _ -> []

module RssGenerator =
    let private item url : string =
        $"""<item>
      <title>FIXME</title>
      <link>{url}</link>
    </item>"""

    let generate (urls: string seq) =
        let items = urls |> Seq.map item |> String.concat "\n"

        $"""<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
  <channel>
   {items}
  </channel>
</rss>"""

module RssServer =
    type Id = Id of string

    type State =
        { torrents: (string * Id * byte []) list }
        static member empty = { torrents = [] }

        static member update (env: Env) (cmd: Cmd) (state: State) =
            match cmd with
            | :? AddTorrentToRssRequested as AddTorrentToRssRequested (user, data) ->
                let id = Hasher.computeHash env.salt data
                { state with torrents = (user, Id id, data) :: state.torrents }
            | _ -> state

    let onMessage (env: Env) (state: State) (msg: Msg) : Cmd list =
        match msg with
        | :? Server.RequestReceived as (Server.RequestReceived (url, _) as req) ->
            match url.AbsolutePath with
            | Regex "/rss/(.+)" [ encUser ] ->
                let torrents =
                    state.torrents
                    |> Seq.filter (fun (user, _, _) -> KeyGenerator.encode env.salt user = encUser)
                    |> Seq.map (fun (_, (Id id), _) -> $"{env.origin}t/{id}")
                    |> RssGenerator.generate
                    |> Text.Encoding.UTF8.GetBytes

                [ Server.ResponseSended(torrents, req) ]
            | Regex "/t/(.+)" [ torrentId ] ->
                state.torrents
                |> Seq.tryPick (function
                    | (_, id, data) when id = Id torrentId -> Some data
                    | _ -> None)
                |> Option.defaultValue [||]
                |> fun data -> [ Server.ResponseSended(data, req) ]
            | _ -> [ Server.ResponseSended([||], req) ]
        | _ -> []

let parseConfig textConfig =
    Edn.map2
        (Edn.paramS "origin")
        (Edn.param
            "sites"
            (Edn.parseStringMap (
                Edn.map2
                    (Edn.paramS "cookie")
                    (Edn.param
                        "parser"
                        (Edn.map3 (Edn.paramS "nodes") (Edn.paramS "title") (Edn.paramS "link") (fun n t l ->
                            { Parser.nodes = n
                              Parser.title = t
                              Parser.link = l })))
                    (fun c p -> {| cookie = c; parser = p |})
            )))
        (fun o s -> {| origin = o; sites = s |})
    |> Edn.exec textConfig

let start salt (encConfig: string) handleCmdExt handleMsgExt =
    let config =
        parseConfig (Text.Encoding.UTF8.GetString(Convert.FromBase64String encConfig))

    let env: Env =
        { salt = salt
          origin = config.origin
          cookies = config.sites |> Map.map (fun _ x -> x.cookie) }

    let state = ref RssServer.State.empty

    let handleMsg (msg: Msg) : Cmd list =
        [ yield! handleMsgExt msg
          RssServer.onMessage env state.Value msg
          App.onMessage (config.sites |> Map.map (fun _ v -> v.parser)) env msg ]
        |> List.concat

    let handleCmd (dispatch: Msg -> unit) (cmd: Cmd) : unit =
        handleCmdExt dispatch cmd
        state.Value <- RssServer.State.update env cmd state.Value

    EventBus.make handleMsg handleCmd

[<EntryPoint>]
let main _ =
    let encConfig = Environment.GetEnvironmentVariable "CONFIG"
    let bot = Bot.makeContext ()

    let handleCmd (dispatch: Msg -> unit) (cmd: Cmd) : unit =
        printfn $"CMD: %A{cmd}\n"
        Server.onCommand cmd
        Bot.onCommand bot cmd
        Http.onCommand dispatch cmd

    let handleMsg msg =
        printfn $"MSG: %A{msg}\n"
        []

    let dispatch = start (Random.Shared.Next() |> string) encConfig handleCmd handleMsg

    printfn "Server started\n"

    [ Server.start dispatch
      Bot.start bot dispatch ]
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously

    0

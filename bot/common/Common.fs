module TorrentBot.Common

open System
open System.Text.RegularExpressions

type Msg =
    interface
    end

type Cmd =
    interface
    end

[<AutoOpen>]
module Prelude =
    let (===) = LanguagePrimitives.PhysicalEquality

module Exception =
    let inline stackTrace msg =
        try
            failwith msg
        with
        | e -> e

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

module String =
    let elipsize limit (str: string) =
        if str.Length <= limit then
            str
        else
            sprintf "%s…" (str.Substring(0, limit).TrimEnd())

    let toBase64Url (s: string) =
        s
        |> System.Text.Encoding.UTF8.GetBytes
        |> Convert.ToBase64String
        |> fun x -> x.Replace('+', '-').Replace('/', '_')

    let fromBase64Url (url: string) =
        url.Replace('-', '+').Replace('_', '/')
        |> Convert.FromBase64String
        |> System.Text.Encoding.UTF8.GetString

module Debug =
    let log prefix s =
        printfn $"{prefix} {s}"
        s

module Http =
    open System.Net.Http

    type HttpGetCmd =
        | HttpGetCmd of url: string * (Result<byte [], exn> -> Msg)
        interface Cmd

    let onCommand (cookies: Map<string, string>) (dispatch: Msg -> unit) (cmd: Cmd) =
        match cmd with
        | :? HttpGetCmd as HttpGetCmd (url, callback) ->
            task {
                let client = new HttpClient()
                let req = new HttpRequestMessage(HttpMethod.Get, url)

                req.Headers.Add(
                    "User-Agent",
                    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/15.4 Safari/605.1.15"
                )

                match Map.tryFind (Uri url).Host cookies with
                | Some cookie -> req.Headers.Add("Cookie", cookie)
                | None -> ()

                req.Headers.Add("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")

                let! response = client.SendAsync(req)
                let! data = response.Content.ReadAsByteArrayAsync()

                dispatch (callback (Ok data))
            }
            |> ignore
        | _ -> ()

module Bot =
    open Telegram.Bot
    open Telegram.Bot.Types
    open Telegram.Bot.Types.Enums

    type NewBotMessage =
        | NewBotMessage of user: string * message: string
        interface Msg

    type SendBotResponse =
        | SendBotResponse of user: string * text: string * isHtml: bool
        interface Cmd

    type Context = private { client: TelegramBotClient }

    let makeContext (token: string) =
        { client = new TelegramBotClient(token) }

    let onCommand { client = client } (cmd: Cmd) =
        match cmd with
        | :? SendBotResponse as SendBotResponse (user, text, isHtml) ->
            task {
                let! text =
                    if text.Contains "$ME$" then
                        task {
                            let! me = client.GetMeAsync()
                            return text.Replace("$ME$", me.Username)
                        }
                    else
                        Threading.Tasks.Task.FromResult text

                let! _ =
                    client.SendTextMessageAsync(
                        ChatId.op_Implicit user,
                        text,
                        if isHtml then
                            Nullable ParseMode.Html
                        else
                            Nullable()
                    )

                ()
            }
            |> ignore
        | _ -> ()

    let startWebHook { client = client } url =
        async {
            do!
                client.SetWebhookAsync(url, dropPendingUpdates = true)
                |> Async.AwaitTask
        }

module Server =
    module private TelegramHandler =
        open Telegram.Bot.Types

        let handle dispatch (update: Update) : unit =
            dispatch (Bot.NewBotMessage(string update.Message.From.Id, update.Message.Text))

    module private Deserializer =
        open Newtonsoft.Json
        open System.IO

        let deserialize (bytes: byte []) : 'r =
            use stream = new MemoryStream(bytes)
            use reader = new StreamReader(stream, Text.Encoding.UTF8)

            JsonSerializer
                .Create()
                .Deserialize<'r>(new JsonTextReader(reader))

    open Suave
    open Suave.Filters
    open Suave.Operators

    type RequestReceived =
        | RequestReceived of url: Uri * (byte [] -> unit)
        interface Msg

    type ResponseSended =
        | ResponseSended of byte [] * RequestReceived
        interface Cmd

    let onCommand (cmd: Cmd) =
        match cmd with
        | :? ResponseSended as ResponseSended (data, (RequestReceived (_, write))) -> write data
        | _ -> ()

    let start sessionId (dispatch: Msg -> unit) =
        choose [ path $"/%s{sessionId}"
                 >=> request (fun req ->
                     Deserializer.deserialize req.rawForm
                     |> TelegramHandler.handle dispatch

                     Successful.no_content)
                 request (fun r ctx ->
                     async {
                         let! outData = Async.FromContinuations(fun (f, _, _) -> dispatch (RequestReceived(r.url, f)))
                         return! Successful.ok outData ctx
                     }) ]
        |> startWebServerAsync { defaultConfig with bindings = [ HttpBinding.create HTTP Net.IPAddress.Any 8080us ] }
        |> snd

module Hasher =
    open System.Security.Cryptography

    let computeHash (salt: string) (data: byte []) =
        let bsalt = Text.Encoding.UTF8.GetBytes salt
        let sha = IncrementalHash.CreateHash(HashAlgorithmName.SHA256)
        sha.AppendData(bsalt)
        sha.AppendData(data)
        let result = sha.GetHashAndReset()

        Convert
            .ToBase64String(result)
            .Replace('+', '-')
            .Replace('/', '_')
            .Replace('=', '.')

module KeyGenerator =
    open System.Security.Cryptography

    let encode (salt: string) (data: string) =
        (salt + data + salt
         |> Text.Encoding.UTF8.GetBytes
         |> SHA256.HashData
         |> Convert.ToBase64String)
            .Replace('+', '-')
            .Replace('/', '_')
            .Replace('=', '~')

module Edn =
    open FParsec

    let private pkey: Parser<_, unit> =
        pchar ':'
        >>. many1Satisfy (fun c -> isLetter c || c = '_' || c = '-' || c = '.')

    let private pstring: Parser<_, unit> =
        pchar '"' >>. many1Satisfy (fun c -> c <> '"')
        .>> pchar '"'

    let private pspace: Parser<_, unit> = manySatisfy (fun c -> c = ' ' || c = '\n')

    let private any: Parser<_, unit> = many1Satisfy (fun _ -> true)

    let private precord = pipe2 (pkey .>> pspace) pstring (fun k v -> k, v)

    type 'r t = private { p: Parser<'r, unit> }

    let private mapString: _ t =
        { p =
            pchar '{' >>. many (precord .>> pspace)
            .>> pchar '}'
            |>> Map.ofList }

    let private precord2 = pipe2 (pkey .>> pspace) mapString.p (fun k v -> k, v)

    let private precord_ name pvalue =
        (pchar ':' .>> pstringCI name .>> pspace)
        >>. pvalue
        .>> pspace

    let param name t = name, { p = (precord_ name t.p) }
    let paramS name = param name { p = pstring }
    let paramM name = param name { p = mapString.p }

    let valueParse =
        let expr, exprImpl = createParserForwardedToRef ()

        let dicP =
            pchar '{'
            >>. (many (
                     pspace
                     >>. (choice [ pkey |>> sprintf ":%s"
                                   pstring |>> sprintf "\"%s\"" ]
                          .>> pspace
                          .>>. expr)
                     .>> pspace
                 )
                 |>> (List.fold (fun a (k, v) -> $"{a} {k} {v}") ""))
            .>> pchar '}'
            |>> sprintf "{%s}"

        exprImpl.Value <-
            choice [ pstring |>> sprintf "\"%s\""
                     dicP ]

        expr

    let parseMap p1 parse : _ t =
        { p = (pchar '{' >>. p1.p .>> pchar '}') |>> parse }

    let private getParseValue =
        function
        | Success (x, _, _) -> x
        | Failure (e, _, _) -> failwith e

    let string = { p = pstring }

    let map2 (n1, p1) (n2, p2) convert : _ t =
        { p =
            pchar '{'
            >>. many (pkey .>> pspace .>>. valueParse .>> pspace)
            .>> pchar '}'
            |>> (fun xs ->
                let dic = Map.ofSeq xs
                let a = Map.find n1 dic |> run p1.p |> getParseValue
                let b = Map.find n2 dic |> run p2.p |> getParseValue
                convert a b) }

    let map3 p1 p2 p3 parse : _ t =
        { p =
            pchar '{' >>. (pipe3 p1.p p2.p p3.p parse)
            .>> pchar '}' }

    let map4 (n1, p1) (n2, p2) (n3, p3) (n4, p4) parse : _ t =
        let kvp = pipe2 (pkey .>> pspace) (choice [ pstring ] .>> pspace) (fun k v -> k, v)

        { p =
            pchar '{' >>. (pipe4 kvp kvp kvp kvp parse)
            .>> pchar '}' }

    let parseStringMap itemParser : _ t =
        { p =
            pchar '{'
            >>. (many1 (
                     tuple2 (pstring .>> pspace) itemParser.p
                     .>> pspace
                 )
                 |>> Map.ofList)
            .>> pchar '}' }

    let private pedn parse : Parser<_, unit> =
        pspace >>. pchar '{' >>. parse.p .>> pchar '}'

    let exec text parser =
        match run parser.p text with
        | Success (x, _, _) -> x
        | Failure (e, _, _) -> failwith e

module EventBus =
    let make handleMsg (handleCmd: (Msg -> _) -> Cmd -> _) =
        let mail: MailboxProcessor<Msg> =
            MailboxProcessor.Start (fun mail ->
                async {
                    while true do
                        let! msg = mail.Receive()

                        try
                            handleMsg msg |> List.iter (handleCmd mail.Post)
                        with
                        | e ->
                            printfn "ERROR: %O" e
                            exit -1
                })

        mail.Post

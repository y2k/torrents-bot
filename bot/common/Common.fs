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
        | HttpGetCmd of url: string * cookie: Map<string, string> * (Result<byte [], exn> -> Msg)
        interface Cmd

    let onCommand (dispatch: Msg -> unit) (cmd: Cmd) =
        match cmd with
        | :? HttpGetCmd as HttpGetCmd (url, cookies, callback) ->
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

    let makeContext () =
        { client = new TelegramBotClient(Environment.GetEnvironmentVariable "TELEGRAM_BOT_TOKEN") }

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

    let start { client = client } (dispatch: Msg -> unit) =
        async {
            client.StartReceiving(
                Action<_, _, _> (fun _ (update: Update) _ ->
                    dispatch (NewBotMessage(string update.Message.From.Id, update.Message.Text))),
                Action<_, _, _>(fun _ _ _ -> ())
            )
        }

module Server =
    open System.Net

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

    let start (dispatch: Msg -> unit) =
        async {
            let listener = new HttpListener()
            listener.Prefixes.Add "http://localhost:8080/"
            listener.Start()

            while true do
                let! context = listener.GetContextAsync() |> Async.AwaitTask

                dispatch (
                    RequestReceived(
                        context.Request.Url,
                        (fun data ->
                            context.Response.OutputStream.Write(data)
                            context.Response.Close())
                    )
                )
        }

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
            .Replace('=', '.')

module Edn =
    open FParsec

    let private pkey: Parser<_, unit> =
        pchar ':'
        >>. many1Satisfy (fun c -> isLetter c || c = '_' || c = '.')

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

    let paramS name = { p = (precord_ name pstring) }
    let paramM name = { p = (precord_ name mapString.p) }
    let param name t = { p = (precord_ name t.p) }

    let parseMap p1 parse : _ t =
        { p = (pchar '{' >>. p1.p .>> pchar '}') |>> parse }

    let map2 p1 p2 parse : _ t =
        { p =
            pchar '{' >>. (pipe2 (p1.p .>> pspace) p2.p parse)
            .>> pchar '}' }

    let map3 p1 p2 p3 parse : _ t =
        { p =
            pchar '{' >>. (pipe3 p1.p p2.p p3.p parse)
            .>> pchar '}' }

    let map4 p1 p2 p3 p4 parse : _ t =
        { p =
            pchar '{' >>. (pipe4 p1.p p2.p p3.p p4.p parse)
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

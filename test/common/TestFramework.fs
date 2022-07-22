module TestFramework

open TorrentBot.Common
open TorrentBot.App
open System

type SystemBehaviour =
    abstract member writeToBot: string -> unit
    abstract member readFromBot: unit -> string list
    abstract member downloadString: string -> string

open System.Text
open System.Security.Cryptography

let private encConfig =
    """{:origin "http://localhost:8080/"
 :bot-token "..."
 :salt "_SALT_"
 :sites
 {"site.com"
  {:cookie "cookie"
   :parser
   {:nodes "//tr[@data-topic_id]"
    :title "*//div[contains(@class,'t-title')]/a"
    :link "*//a[contains(@class,'tr-dl')]"}}}}"""
    |> Text.Encoding.UTF8.GetBytes
    |> Convert.ToBase64String

open Ionic.Zip

let private unzip pass (path: string) =
    if not (IO.File.Exists path) then
        failwithf "Can't find file: %s" path

    use zip = new ZipFile(path)
    let entity = zip |> Seq.head
    use buf = new IO.MemoryStream()
    entity.ExtractWithPassword(buf, pass)
    buf.ToArray()

let private handleTestCmd (dispatch: Msg -> unit) (cmd: Cmd) =
    match cmd with
    | :? Http.HttpGetCmd as Http.HttpGetCmd (url, callback) ->
        let id =
            url
            |> Encoding.UTF8.GetBytes
            |> MD5.HashData
            |> Convert.ToHexString

        let pass = Environment.GetEnvironmentVariable "TEST_PWD"

        if isNull pass then
            failwith "Environment TEST_PWD not set"

        let data = unzip pass $"../../../common/__test_data/%s{id}.zip"
        dispatch (callback (Ok data))
    | _ -> ()

let private retryAssertInner f =
    let rec loop n (delay: int) =
        async {
            if n <= 0 then
                return false
            else
                let result = f ()

                if result then
                    return true
                else
                    do! Async.Sleep delay
                    return! loop (n - 1) (2 * delay)
        }

    loop 5 100

let assertWithRetry f =
    Threading.Thread.Sleep(1000)
    f ()

let runTestApplication () =
    let mutable log: Cmd list = []
    let mutable telegramResponses: string list = []

    let config =
        parseConfig (Text.Encoding.UTF8.GetString(Convert.FromBase64String encConfig))

    let env: Env =
        { salt = config.salt
          origin = config.origin
          sites = config.sites |> Map.map (fun _ v -> v.parser) }

    let dispatch: Msg -> unit =
        start
            env
            (fun d cmd ->
                handleTestCmd d cmd

                match cmd with
                | :? Bot.SendBotResponse as Bot.SendBotResponse (_, text, _) ->
                    telegramResponses <- text :: telegramResponses
                | _ -> ()

                log <- cmd :: log)
            (fun _ -> [])

    { new SystemBehaviour with
        member _.writeToBot msg =
            telegramResponses <- []
            dispatch (Bot.NewBotMessage("alice", msg))

        member _.downloadString url =
            async {
                let req = Server.RequestReceived(Uri(url), ignore)
                dispatch req

                let mutable result: string = null

                while isNull result do
                    do! Async.Sleep 100

                    log
                    |> List.choose (function
                        | :? Server.ResponseSended as Server.ResponseSended (content, r) when r === req ->
                            Text.Encoding.UTF8.GetString content |> Some
                        | _ -> None)
                    |> List.iter (fun r -> result <- r)

                return result
            }
            |> Async.RunSynchronously

        member _.readFromBot() = telegramResponses }

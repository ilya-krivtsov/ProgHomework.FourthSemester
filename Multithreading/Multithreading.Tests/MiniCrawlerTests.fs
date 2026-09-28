// <copyright file="MiniCrawlerTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module MiniCrawler.Tests

open System
open System.Collections.Concurrent
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading.Tasks
open NUnit.Framework
open FsUnit

let private freePort () =
    let probe = new TcpListener(IPAddress.Loopback, 0)
    probe.Start()
    let port = (probe.LocalEndpoint :?> IPEndPoint).Port
    probe.Stop()
    port

let private url (port: int) (path: string) =
    sprintf "http://localhost:%d%s" port path

let private html (links: string list) =
    let anchors =
        links
        |> List.map (fun href -> sprintf "<a href=\"%s\">link</a>" href)
        |> String.concat "\n"

    sprintf "<html><body>%s</body></html>" anchors

type private TestSite(port: int, pages: Map<string, string>) =

    let listener = new HttpListener()
    let requests = ConcurrentDictionary<string, int>()

    let serve () =
        try
            while true do
                let context = listener.GetContext()
                let path = context.Request.Url.AbsolutePath
                requests.AddOrUpdate(path, 1, fun _ seen -> seen + 1) |> ignore

                let body, status =
                    match Map.tryFind path pages with
                    | Some found -> found, 200
                    | None -> "not found", 404

                let bytes = Encoding.UTF8.GetBytes(body)
                context.Response.StatusCode <- status
                context.Response.ContentType <- "text/html; charset=utf-8"
                context.Response.ContentLength64 <- int64 bytes.Length
                context.Response.OutputStream.Write(bytes, 0, bytes.Length)
                context.Response.OutputStream.Close()
        with _ ->
            ()

    do
        listener.Prefixes.Add(url port "/")
        listener.Start()

    let loop = Task.Run(Action serve)

    member _.Port = port
    member _.BaseUrl = url port "/"
    member _.Url(path: string) = url port path
    member _.TotalRequests = requests.Values |> Seq.sum

    member _.RequestCount(path: string) =
        match requests.TryGetValue path with
        | true, seen -> seen
        | _ -> 0

    interface IDisposable with
        member this.Dispose() =
            listener.Stop()
            listener.Close()

            try
                loop.Wait 5000 |> ignore
            with _ ->
                ()

let private withSite (pages: int -> Map<string, string>) (body: TestSite -> unit) =
    let port = freePort ()
    use site = new TestSite(port, pages port)
    body site

let private runCrawl (url: string) = crawl url |> Async.RunSynchronously

[<Test>]
let ``crawl returns one entry for every link on the page`` () =
    withSite
        (fun port ->
            Map.ofList
                [ "/", html [ url port "/a"; url port "/b"; url port "/c" ]
                  "/a", html []
                  "/b", html []
                  "/c", html [] ])
        (fun site ->
            let crawled = runCrawl site.BaseUrl |> List.map fst |> List.sort

            crawled |> should equal [ site.Url "/a"; site.Url "/b"; site.Url "/c" ])

[<Test>]
let ``crawl reports the content length of each linked page`` () =
    withSite
        (fun port ->
            Map.ofList
                [ "/", html [ url port "/a"; url port "/b" ]
                  "/a", html []
                  "/b", html [ url port "/deep" ]
                  "/deep", html [] ])
        (fun site ->
            let crawled = runCrawl site.BaseUrl |> List.sort

            crawled
            |> should
                equal
                [ site.Url "/a", (html []).Length
                  site.Url "/b", (html [ site.Url "/deep" ]).Length ])

[<Test>]
let ``crawl returns nothing for a page without links`` () =
    withSite (fun _ -> Map.ofList [ "/", html [] ]) (fun site -> runCrawl site.BaseUrl |> should be Empty)

[<Test>]
let ``crawl does not follow links found on the linked pages`` () =
    withSite
        (fun port ->
            Map.ofList
                [ "/", html [ url port "/a" ]
                  "/a", html [ url port "/deep" ]
                  "/deep", html [] ])
        (fun site ->
            let crawled = runCrawl site.BaseUrl |> List.map fst

            crawled |> should equal [ site.Url "/a" ]
            site.RequestCount "/deep" |> should equal 0)

[<Test>]
let ``crawl downloads each linked page exactly once`` () =
    withSite
        (fun port ->
            Map.ofList
                [ "/", html [ url port "/a"; url port "/a"; url port "/b" ]
                  "/a", html []
                  "/b", html [] ])
        (fun site ->
            runCrawl site.BaseUrl |> ignore

            site.RequestCount "/a" |> should equal 1
            site.RequestCount "/b" |> should equal 1)

[<Test>]
let ``crawl requests every linked page from the server`` () =
    withSite
        (fun port -> Map.ofList [ "/", html [ url port "/a"; url port "/b" ]; "/a", html []; "/b", html [] ])
        (fun site ->
            runCrawl site.BaseUrl |> ignore

            site.RequestCount "/" |> should equal 1
            site.RequestCount "/a" |> should equal 1
            site.RequestCount "/b" |> should equal 1)

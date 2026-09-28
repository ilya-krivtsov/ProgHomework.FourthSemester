// <copyright file="MiniCrawler.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module MiniCrawler

open System.Net.Http
open System.Text.RegularExpressions

/// <summary>
/// Downloads the page at <paramref name="url"/>, follows the <c>http://</c> links
/// found on it, and returns each linked page together with its content length.
/// </summary>
/// <param name="url">Absolute <c>http://</c> address of the page to start from.</param>
let crawl (url: string) =
    let pattern = Regex "<a href=\"(?<url>http://[^\"']+)\">"

    async {
        use client = new HttpClient()
        let! response = client.GetStringAsync url |> Async.AwaitTask
        let matches = pattern.Matches(response)

        let childUrls =
            [ 0 .. (matches.Count - 1) ]
            |> List.map (fun i -> ((matches.Item i).Groups.Item 1).Value)

        let childRequests =
            childUrls
            |> List.distinct
            |> List.map (fun url ->
                async {
                    let! response = client.GetStringAsync url |> Async.AwaitTask
                    return url, response
                })

        let! childResponeses = Control.Async.Parallel childRequests

        return
            childResponeses
            |> List.ofArray
            |> List.map (fun (url, content) -> url, content.Length)
    }

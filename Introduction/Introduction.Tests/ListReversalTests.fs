// <copyright file="ListReversalTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module ListReversal.Tests

open NUnit.Framework
open FsUnit
open FsCheck

[<Test>]
let ``reverse empty list returns empty list`` () = listReverse [] |> should equal []

[<Test>]
let ``reverse single element list returns same list`` () = listReverse [ 1 ] |> should equal [ 1 ]

[<Test>]
let ``reverse list of multiple elements`` () =
    listReverse [ 1; 2; 3; 4 ] |> should equal [ 4; 3; 2; 1 ]

[<Test>]
let ``reverse list with repeated elements`` () =
    listReverse [ 1; 2; 2; 3 ] |> should equal [ 3; 2; 2; 1 ]

[<Test>]
let ``reverse list preserves length`` () =
    let input = [ 1; 2; 3; 4; 5 ]
    let result = listReverse input
    result |> should haveLength input.Length

[<Test>]
let ``reverse of reverse is original list`` () =
    let input = [ 1; 2; 3; 4; 5 ]
    listReverse input |> listReverse |> should equal input

[<Test>]
let ``double reverse is identity`` () =
    Check.QuickThrowOnFailure(fun xs -> xs |> (listReverse >> listReverse) = id xs)

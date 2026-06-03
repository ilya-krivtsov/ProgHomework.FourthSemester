// <copyright file="ElementSearchTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module ElementSearch.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``findElement finds existing element`` () =
    findElement 3 [ 1; 2; 3; 4 ] |> should equal (Some 3)

[<Test>]
let ``findElement returns None for missing element`` () =
    findElement 5 [ 1; 2; 3 ] |> should equal None

[<Test>]
let ``findElement on empty list returns None`` () = findElement 1 [] |> should equal None

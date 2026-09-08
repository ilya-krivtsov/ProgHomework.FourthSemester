// <copyright file="ElementSearchTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module ElementSearch.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``findElement on empty list returns None`` () = findElement 1 [] |> should equal None

[<Test>]
let ``findElement on one element list returns 0`` () =
    findElement 1 [ 1 ] |> should equal (Some 0)

[<Test>]
let ``findElement returns index of existing element`` () =
    findElement 3 [ 0; 1; 2; 3 ] |> should equal (Some 3)

[<Test>]
let ``findElement returns index of first occurrence`` () =
    findElement 2 [ 0; 1; 2; 2; 2; 2 ] |> should equal (Some 2)

[<Test>]
let ``findElement returns None for missing element`` () =
    findElement 5 [ 0; 1; 2 ] |> should equal None

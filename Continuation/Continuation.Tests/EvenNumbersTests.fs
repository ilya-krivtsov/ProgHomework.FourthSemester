// <copyright file="EvenNumbersTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module EvenNumbers.Tests

open NUnit.Framework
open FsUnit
open FsCheck

[<Test>]
let ``variant a equivalent to variant b`` () =
    Check.QuickThrowOnFailure(fun xs ->
        let a = evenNumbersCountA xs
        let b = evenNumbersCountB xs
        a = b)

[<Test>]
let ``variant a equivalent to variant c`` () =
    Check.QuickThrowOnFailure(fun xs ->
        let a = evenNumbersCountA xs
        let c = evenNumbersCountC xs
        a = c)

[<Test>]
let ``variant a returns 0 for empty list`` () =
    [] |> evenNumbersCountA |> should equal 0

[<Test>]
let ``variant a returns 0 for list with no even numbers`` () =
    [ 1; 3; 5 ] |> evenNumbersCountA |> should equal 0

[<Test>]
let ``variant a returns 1 for list with one even number`` () =
    [ 1; 2; 3 ] |> evenNumbersCountA |> should equal 1

[<Test>]
let ``variant a returns 1 for list with single even number`` () =
    [ 0 ] |> evenNumbersCountA |> should equal 1

[<Test>]
let ``variant a returns 2 for list with two even numbers`` () =
    [ 0; 2 ] |> evenNumbersCountA |> should equal 2

[<Test>]
let ``variant a returns n for list with n even numbers`` () =
    let n = 20

    [ 0 .. n - 1 ]
    |> List.map (fun x -> x * 2)
    |> evenNumbersCountA
    |> should equal n

[<Test>]
let ``variant a returns n for list of size 2*n with n even numbers`` () =
    let n = 40

    [ 0 .. 2 * (n - 1) ] |> evenNumbersCountA |> should equal n

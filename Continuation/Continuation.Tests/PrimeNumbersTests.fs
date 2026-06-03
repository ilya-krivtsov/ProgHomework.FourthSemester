// <copyright file="PrimeNumbersTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module PrimeNumbers.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``first prime is 2`` () =
    primeNumbers () |> Seq.head |> should equal 2

[<Test>]
let ``second prime is 3`` () =
    primeNumbers () |> Seq.take 2 |> Seq.last |> should equal 3

[<Test>]
let ``first five primes are 2 3 5 7 11`` () =
    primeNumbers () |> Seq.take 5 |> Seq.toList |> should equal [ 2; 3; 5; 7; 11 ]

[<Test>]
let ``first ten primes are correct`` () =
    primeNumbers ()
    |> Seq.take 10
    |> Seq.toList
    |> should equal [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]

[<Test>]
let ``one hundredth prime is 541`` () =
    primeNumbers () |> Seq.item 99 |> should equal 541

[<Test>]
let ``one thousandth prime is 7919`` () =
    primeNumbers () |> Seq.item 999 |> should equal 7919

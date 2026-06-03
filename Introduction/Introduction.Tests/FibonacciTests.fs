// <copyright file="FibonacciTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module Fibonacci.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``fibonacci 0 equals 0`` () = fibonacci 0u |> should equal 0u

[<Test>]
let ``fibonacci 1 equals 1`` () = fibonacci 1u |> should equal 1u

[<Test>]
let ``fibonacci 10 equals 55`` () = fibonacci 10u |> should equal 55u

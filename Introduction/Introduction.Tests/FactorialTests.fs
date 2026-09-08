// <copyright file="FactorialTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module Factorial.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``factorial of 0 equals 1`` () = factorial 0u |> should equal 1u

[<Test>]
let ``factorial of 1 equals 1`` () = factorial 1u |> should equal 1u

[<Test>]
let ``factorial of 5 equals 120`` () = factorial 5u |> should equal 120u

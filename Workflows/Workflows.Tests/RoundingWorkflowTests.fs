// <copyright file="RoundingWorkflowTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module RoundingWorkflow.Tests

open NUnit.Framework
open FsUnit

let rounding = RoundingWorkflow

[<Test>]
let ``(2.0 / 12.0) / 3.5 with precision 3 should return 0.048`` () =
    let result =
        rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    result |> should equal 0.048

[<Test>]
let ``10.0 / 3.0 with precision 3 should return 3.333`` () =
    let result =
        rounding 3 {
            let! a = 10.0
            let! b = 3.0
            return a / b
        }

    result |> should equal 3.333

[<Test>]
let ``10.0 / 3.0 with precision 0 should return 3`` () =
    let result =
        rounding 0 {
            let! a = 10.0
            let! b = 3.0
            return a / b
        }

    result |> should equal 3

[<Test>]
let ``(10.0 / 3.0) * 3.0 with precision 1 should return 9.9`` () =
    let result =
        rounding 1 {
            let! a = 10.0
            let! b = 3.0
            let! c = a / b
            return c * 3.0
        }

    result |> should equal 9.9

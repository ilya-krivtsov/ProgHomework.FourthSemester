// <copyright file="StringExpressionWorkflowTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module StringExpressionWorkflow.Tests

open NUnit.Framework
open FsUnit

let calculate = StringExpressionWorkflow()

[<Test>]
let ``"1" + "2" should return 3`` () =
    let result =
        calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }

    result |> should equal (Some 3)

[<Test>]
let ``"1" + "b" should return None`` () =
    let result =
        calculate {
            let! x = "1"
            let! y = "b"
            let z = x + y
            return z
        }

    result |> should equal None

[<Test>]
let ``"44" - "25" should return 19`` () =
    let result =
        calculate {
            let! x = "44"
            let! y = "25"
            let z = x - y
            return z
        }

    result |> should equal (Some 19)

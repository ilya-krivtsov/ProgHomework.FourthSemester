// <copyright file="InterpreterTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module LambdaInterpreter.Interpreter.Tests

open NUnit.Framework

open LambdaInterpreter.Term

let private f = Var "f"
let private m = Var "m"
let private n = Var "n"
let private p = Var "p"
let private x = Var "x"
let private y = Var "y"

let shouldAlphaEqual expected actual =
    Assert.That(alphaEqual expected actual, $"Expected {expected} and {actual} to be alpha-equivalent")

[<Test>]
let ``I -> I`` () = normalize I |> shouldAlphaEqual I

[<Test>]
let ``S K K -> I`` () =
    makeApplication [ S; K; K ] |> normalize |> shouldAlphaEqual I

[<Test>]
let ``K I -> Kstar`` () =
    Application(K, I) |> normalize |> shouldAlphaEqual Kstar


[<Test>]
let `` IF TRUE x y -> x`` () =
    [ IF; TRUE; Variable x; Variable y ]
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual (Variable x)

[<Test>]
let ``IF FALSE x y -> y`` () =
    [ IF; FALSE; Variable x; Variable y ]
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual (Variable y)


let booleans = [ FALSE; TRUE ]
let testBooleans = booleans |> List.map TestCaseData
let testBooleanPairs = List.allPairs booleans booleans |> List.map TestCaseData

[<TestCaseSource(nameof testBooleans)>]
let ``NOT Alpha -> (!Alpha)`` v =
    [ NOT; v ]
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual (if v = TRUE then FALSE else TRUE)

[<TestCaseSource(nameof testBooleanPairs)>]
let ``AND Alpha Beta -> (Alpha && Beta)`` a b =
    [ AND; a; b ]
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual (if a = TRUE && b = TRUE then TRUE else FALSE)

[<TestCaseSource(nameof testBooleanPairs)>]
let ``OR Alpha Beta -> (Alpha || Beta)`` a b =
    [ OR; a; b ]
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual (if a = TRUE || b = TRUE then TRUE else FALSE)


let numbers = [ 0u .. 4u ]
let testNumbers = numbers |> List.map TestCaseData
let testNumberPairs = List.allPairs numbers numbers |> List.map TestCaseData

[<TestCaseSource(nameof testNumbers)>]
let ``SUCC Alpha -> (Alpha + 1)`` v =
    [ SUCC; makeNumeral v ] |> makeApplication |> normalize |> shouldAlphaEqual
    <| makeNumeral (v + 1u)

[<TestCaseSource(nameof testNumberPairs)>]
let ``Alpha + Beta -> (Alpha + Beta)`` a b =
    ADD :: ([ a; b ] |> List.map makeNumeral)
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual
    <| makeNumeral (a + b)

[<TestCaseSource(nameof testNumberPairs)>]
let ``Alpha * Beta -> (Alpha * Beta)`` a b =
    MUL :: ([ a; b ] |> List.map makeNumeral)
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual
    <| makeNumeral (a * b)


let private zp = makeApplication [ PAIR; ZERO; ZERO ]

let private sp =
    let secondP = Application(SECOND, Variable p)

    makeAbstraction [ p ]
    <| makeApplication [ PAIR; secondP; Application(SUCC, secondP) ]

let private pred =
    makeAbstraction [ m ]
    <| makeApplication [ FIRST; makeApplication [ Variable m; sp; zp ] ]

let private factorial =
    let F =
        makeAbstraction [ f; n ]
        <| makeApplication
            [ Application(ISZERO, Variable n)
              ONE
              makeApplication [ MUL; Variable n; Application(Variable f, Application(pred, Variable n)) ] ]

    makeApplication [ Y; F ]

[<TestCaseSource(nameof testNumbers)>]
let ``factorial Alpha -> (Alpha!)`` v =
    let rec fact n =
        if n <= 1u then 1u else n * fact (n - 1u)

    [ factorial; makeNumeral v ]
    |> makeApplication
    |> normalize
    |> shouldAlphaEqual (makeNumeral (fact v))


[<Test>]
let ``HEAD [1;2] -> 1`` () =
    let list =
        [ 1u; 2u ]
        |> List.rev
        |> List.map makeNumeral
        |> List.fold (fun acc x -> makeApplication <| [ CONS; x; acc ]) NIL

    Application(HEAD, list) |> normalize |> shouldAlphaEqual ONE

[<Test>]
let ``ISEMPTY NIL -> TRUE`` () =
    Application(ISEMPTY, NIL) |> normalize |> shouldAlphaEqual TRUE

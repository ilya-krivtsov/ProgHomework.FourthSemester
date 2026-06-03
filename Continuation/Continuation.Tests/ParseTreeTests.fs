module ParseTree.Tests

open NUnit.Framework
open FsUnit

let private parse (s: string) =
    let tokens = ResizeArray(s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries))

    let rec loop () =
        let t = tokens[0]
        tokens.RemoveAt 0

        if t = "(" then
            let left = loop ()

            let op = tokens[0]
            tokens.RemoveAt 0

            let right = loop ()

            let close = tokens[0]
            tokens.RemoveAt 0

            let opType =
                match op with
                | "+" -> Addition
                | "-" -> Subtraction
                | "*" -> Multiplication
                | "/" -> Division
                | _ -> failwithf "Unknown operator: %s" op

            Operation
                { Type = opType
                  LeftOperand = left
                  RightOperand = right }
        else
            Value(int t)

    loop ()

[<Test>]
let ``( 2 + 3 ) equals 5`` () =
    parse "( 2 + 3 )" |> evaluate |> should equal 5

[<Test>]
let ``( 10 - 4 ) equals 6`` () =
    parse "( 10 - 4 )" |> evaluate |> should equal 6

[<Test>]
let ``( 3 * 7 ) equals 21`` () =
    parse "( 3 * 7 )" |> evaluate |> should equal 21

[<Test>]
let ``( 20 / 4 ) equals 5`` () =
    parse "( 20 / 4 )" |> evaluate |> should equal 5

[<Test>]
let ``( ( 2 + 3 ) * 4 ) equals 20`` () =
    parse "( ( 2 + 3 ) * 4 )" |> evaluate |> should equal 20

[<Test>]
let ``( 2 * ( 3 + 4 ) ) equals 14`` () =
    parse "( 2 * ( 3 + 4 ) )" |> evaluate |> should equal 14

[<Test>]
let ``( ( 10 - 2 ) / 2 ) equals 4`` () =
    parse "( ( 10 - 2 ) / 2 )" |> evaluate |> should equal 4

[<Test>]
let ``( ( 2 + 3 ) * ( 4 - 1 ) ) equals 15`` () =
    parse "( ( 2 + 3 ) * ( 4 - 1 ) )" |> evaluate |> should equal 15

[<Test>]
let ``( ( ( 2 + 3 ) * ( 4 - 1 ) ) / 5 ) equals 3`` () =
    parse "( ( ( 2 + 3 ) * ( 4 - 1 ) ) / 5 )" |> evaluate |> should equal 3

[<Test>]
let ``( 42 ) equals 42`` () = Value 42 |> evaluate |> should equal 42

[<Test>]
let ``( 0 + 0 ) equals 0`` () =
    parse "( 0 + 0 )" |> evaluate |> should equal 0

[<Test>]
let ``( 1 - 1 ) equals 0`` () =
    parse "( 1 - 1 )" |> evaluate |> should equal 0

[<Test>]
let ``( ( ( 1 + 2 ) + 3 ) + 4 ) equals 10`` () =
    parse "( ( ( 1 + 2 ) + 3 ) + 4 )" |> evaluate |> should equal 10

[<Test>]
let ``( ( 100 / 5 ) / 2 ) equals 10`` () =
    parse "( ( 100 / 5 ) / 2 )" |> evaluate |> should equal 10

[<Test>]
let ``( ( 12 * 3 ) - ( 8 / 2 ) ) equals 32`` () =
    parse "( ( 12 * 3 ) - ( 8 / 2 ) )" |> evaluate |> should equal 32

let private rng = System.Random(42)

let private randomOp () =
    match rng.Next(4) with
    | 0 -> Addition, (+)
    | 1 -> Subtraction, (-)
    | 2 -> Multiplication, (*)
    | _ -> Division, (/)

let private randomLeaf () =
    let v = rng.Next(1, 21)
    Value v, v

let rec private randomTree depth =
    if depth = 0 then
        randomLeaf ()
    else
        match rng.Next(3) with
        | 0 -> randomLeaf ()
        | _ ->
            let l, lv = randomTree (depth - 1)
            let r, rv = randomTree (depth - 1)
            let op, opF = randomOp ()

            let expected = if op = Division && rv = 0 then lv else opF lv rv

            Operation
                { Type = op
                  LeftOperand = l
                  RightOperand = r },
            expected

[<Test>]
let ``random test 1`` () =
    let tree, v = randomTree 3
    evaluate tree |> should equal v

[<Test>]
let ``random test 2`` () =
    let tree, v = randomTree 3
    evaluate tree |> should equal v

[<Test>]
let ``random test 3`` () =
    let tree, v = randomTree 4
    evaluate tree |> should equal v

[<Test>]
let ``random test 4`` () =
    let tree, v = randomTree 4
    evaluate tree |> should equal v

[<Test>]
let ``random test 5`` () =
    let tree, v = randomTree 5
    evaluate tree |> should equal v

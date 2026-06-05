// <copyright file="TermTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module LambdaInterpreter.Term.Tests

open System
open FsCheck
open FsUnit
open NUnit.Framework

let private x = Var "x"
let private y = Var "y"
let private z = Var "z"
let private a = Var "a"
let private b = Var "b"
let private c = Var "c"
let private d = Var "d"

let I = Abstraction(x, Variable x)

[<Test>]
let ``var is created normally for allowed names`` () =
    [ "x"; "y"; "123"; "aa_aa_b"; "qwerty"; "[{__}]" ]
    |> List.map (fun name -> (Var name).Name |> should equal name)
    |> ignore

[<Test>]
let ``var constructor throws on incorrect name`` () =
    [ "x()"; "(y)"; "12.12"; "aa aa b" ]
    |> List.map (fun name -> (fun () -> Var name |> ignore) |> should throw typeof<ArgumentException>)
    |> ignore

[<Test>]
let ``makeAbstraction should throw on empty variable list`` () =
    (fun () -> makeAbstraction [] I |> ignore)
    |> should throw typeof<ArgumentException>

[<Test>]
let ``makeAbstraction works correctly for single variable`` () =
    let termA = makeAbstraction [ x ] I
    let termB = Abstraction(x, I)

    termA |> should equal termB

[<Test>]
let ``makeAbstraction works correctly for 2 variables`` () =
    let termA = makeAbstraction [ x; y ] I
    let termB = Abstraction(x, Abstraction(y, I))

    termA |> should equal termB

[<Test>]
let ``makeAbstraction works correctly for 3 variables`` () =
    let termA = makeAbstraction [ x; y; z ] I
    let termB = Abstraction(x, Abstraction(y, Abstraction(z, I)))

    termA |> should equal termB

[<Test>]
let ``makeApplication should throw on empty term list`` () =
    (fun () -> makeApplication [] |> ignore)
    |> should throw typeof<ArgumentException>

[<Test>]
let ``makeApplication should throw on single term`` () =
    (fun () -> makeApplication [ I ] |> ignore)
    |> should throw typeof<ArgumentException>


[<Test>]
let ``makeApplication works correctly for 2 terms`` () =
    let termA = makeApplication [ I; I ]
    let termB = Application(I, I)

    termA |> should equal termB

[<Test>]
let ``makeApplication works correctly for 3 terms`` () =
    let termA = makeApplication [ I; I; I ]
    let termB = Application(Application(I, I), I)

    termA |> should equal termB

[<Test>]
let ``variable is formatted correctly`` () =
    [ "x"; "y"; "123"; "aa_aa_b" ]
    |> List.map (fun x -> x, Variable <| Var x)
    |> List.map (fun (name, term) -> term.ToString() |> should equal name)
    |> ignore

[<Test>]
let ``λx.y formatted correctly`` () =
    let term = Abstraction(x, Variable y)
    term.ToString() |> should equal "λx.y"

[<Test>]
let ``λx.λy.x formatted correctly`` () =
    let term = makeAbstraction [ x; y ] (Variable x)
    term.ToString() |> should equal "λx.λy.x"

[<Test>]
let ``<λx.λy.x y> formatted correctly`` () =
    let term = makeAbstraction [ x; y ] <| makeVarApplication [ x; y ]
    term.ToString() |> should equal "λx.λy.x y"

[<Test>]
let ``<λx.λy.λz.x y z> formatted correctly`` () =
    let term = makeAbstraction [ x; y; z ] <| makeVarApplication [ x; y; z ]

    term.ToString() |> should equal "λx.λy.λz.x y z"

[<Test>]
let ``<x y z> formatted correctly`` () =
    let term = makeVarApplication [ x; y; z ]
    term.ToString() |> should equal "x y z"

[<Test>]
let ``<x y (y z)> formatted correctly`` () =
    let term = makeApplication [ Variable x; Variable y; makeVarApplication [ y; z ] ]

    term.ToString() |> should equal "x y (y z)"

[<Test>]
let ``<λx.λy.λz.x y (y z)> formatted correctly`` () =
    let term =
        makeAbstraction [ x; y; z ]
        <| makeApplication [ Variable x; Variable y; makeVarApplication [ y; z ] ]

    term.ToString() |> should equal "λx.λy.λz.x y (y z)"

[<Test>]
let ``<λx.λy.λz.x (z y x) z> formatted correctly`` () =
    let term =
        makeAbstraction [ x; y; z ]
        <| makeApplication [ Variable x; makeVarApplication [ z; y; x ]; Variable z ]

    term.ToString() |> should equal "λx.λy.λz.x (z y x) z"

[<Test>]
let ``<λf.λx.f (f (f x))> formatted correctly`` () =
    let f = Var "f"

    let term =
        makeAbstraction [ f; x ]
        <| Application(Variable f, Application(Variable f, Application(Variable f, Variable x)))

    term.ToString() |> should equal "λf.λx.f (f (f x))"

[<Test>]
let ``<λx.λy.λz.(λw.w) y (y z)> formatted correctly`` () =
    let w = Var "w"

    let term =
        makeAbstraction [ x; y; z ]
        <| makeApplication [ Abstraction(w, Variable w); Variable y; makeVarApplication [ y; z ] ]

    term.ToString() |> should equal "λx.λy.λz.(λw.w) y (y z)"

[<Test>]
let ``λx.x alpha equal to λy.y`` () =
    let termA = Abstraction(x, Variable x)
    let termB = Abstraction(y, Variable y)

    alphaEqual termA termB |> should be True

[<Test>]
let ``<λx.x y> alpha equal to <λz.z y>`` () =
    let termA = Abstraction(x, Application(Variable x, Variable y))
    let termB = Abstraction(z, Application(Variable z, Variable y))

    alphaEqual termA termB |> should be True

[<Test>]
let ``λx.λy.λx.x alpha equal to λa.λb.λc.c`` () =
    let termA = makeAbstraction [ x; y; x ] <| Variable x
    let termB = makeAbstraction [ a; b; c ] <| Variable c

    alphaEqual termA termB |> should be True

[<Test>]
let ``<λx.x a> not alpha equal to <λy.y b>`` () =
    let termA = Abstraction(x, Application(Variable x, Variable a))
    let termB = Abstraction(y, Application(Variable y, Variable b))

    alphaEqual termA termB |> should be False

[<Test>]
let ``λx.x not alpha equal to <λx.x b>`` () =
    let termB = Abstraction(x, Variable x)
    let termA = Abstraction(x, Application(Variable x, Variable b))

    alphaEqual termA termB |> should be False

[<Test>]
let ``λx.λx.x not alpha equal to λx.x`` () =
    let termA = Abstraction(x, Abstraction(x, Variable x))
    let termB = Abstraction(x, Variable x)

    alphaEqual termA termB |> should be False

[<Test>]
let ``<λx.λy.λx.x y> alpha equal to <λa.λb.λc.c b>`` () =
    let termA = makeAbstraction [ x; y; x ] <| makeVarApplication [ x; y ]
    let termB = makeAbstraction [ a; b; c ] <| makeVarApplication [ c; b ]

    alphaEqual termA termB |> should be True

[<Test>]
let ``<λx.λy.x (λz.y (λx.x))> alpha equal to <λa.λb.a (λc.b (λd.d))>`` () =
    let termA =
        makeAbstraction [ x; y ]
        <| Application(Variable x, Abstraction(z, Application(Variable y, Abstraction(x, Variable x))))

    let termB =
        makeAbstraction [ a; b ]
        <| Application(Variable a, Abstraction(c, Application(Variable b, Abstraction(d, Variable d))))

    alphaEqual termA termB |> should be True

[<Test>]
let ``basic combinators are defined correctly`` () =
    I.ToString() |> should equal "λx.x"
    omega.ToString() |> should equal "λs.s s"
    Omega.ToString() |> should equal "(λs.s s) (λs.s s)"
    K.ToString() |> should equal "λx.λy.x"
    Kstar.ToString() |> should equal "λx.λy.y"
    S.ToString() |> should equal "λx.λy.λz.x z (y z)"
    B.ToString() |> should equal "λf.λg.λx.f (g x)"
    Y.ToString() |> should equal "λf.(λx.f (x x)) (λx.f (x x))"

[<Test>]
let ``logic terms are defined correctly`` () =
    TRUE.ToString() |> should equal "λx.λy.x"
    FALSE.ToString() |> should equal "λx.λy.y"
    IF.ToString() |> should equal "λb.λt.λf.b t f"
    AND.ToString() |> should equal $"λa.λb.({IF}) a b ({FALSE})"
    OR.ToString() |> should equal $"λa.λb.({IF}) a ({TRUE}) b"
    NOT.ToString() |> should equal $"λb.({IF}) b ({FALSE}) ({TRUE})"

[<Test>]
let ``numerals are defined correctly`` () =
    ZERO.ToString() |> should equal "λs.λz.z"
    ONE.ToString() |> should equal "λs.λz.s z"

    (makeNumeral 2u).ToString() |> should equal "λs.λz.s (s z)"
    (makeNumeral 3u).ToString() |> should equal "λs.λz.s (s (s z))"
    (makeNumeral 4u).ToString() |> should equal "λs.λz.s (s (s (s z)))"

[<Test>]
let ``arithmetic terms are defined correctly`` () =
    SUCC.ToString() |> should equal "λn.λf.λx.f (n f x)"
    ADD.ToString() |> should equal "λm.λn.λf.λx.m f (n f x)"
    MUL.ToString() |> should equal $"λm.λn.m (({ADD}) n) ({ZERO})"
    POWER.ToString() |> should equal $"λm.λn.m (({MUL}) n) ({ONE})"
    ISZERO.ToString() |> should equal $"λn.n (λx.{FALSE}) ({TRUE})"

[<Test>]
let ``pair terms are defined correctly`` () =
    PAIR.ToString() |> should equal "λx.λy.λf.f x y"
    FIRST.ToString() |> should equal $"λp.p ({TRUE})"
    SECOND.ToString() |> should equal $"λp.p ({FALSE})"

[<Test>]
let ``list terms are defined correctly`` () =
    NIL.ToString() |> should equal "λc.λn.n"
    CONS.ToString() |> should equal "λe.λl.λc.λn.c e (l c n)"
    ISEMPTY.ToString() |> should equal $"λl.l (λh.λt.{FALSE}) ({TRUE})"
    HEAD.ToString() |> should equal $"λl.l (λh.λt.h) ({ZERO})"

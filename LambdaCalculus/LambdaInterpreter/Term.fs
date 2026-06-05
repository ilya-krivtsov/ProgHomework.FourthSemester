// <copyright file="Term.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module LambdaInterpreter.Term

open System

/// <summary>
/// Type that denotes variable names.
/// </summary>
type Var =
    val private name: string

    public new(name: string) =
        { name =
            if name.IndexOfAny [| ' '; '.'; '('; ')' |] <> -1 then
                invalidArg (nameof name) "Variable name cannot contain spaces, dots, or parentheses"
            else
                name }

    member this.Name = this.name

    override this.Equals(obj: obj) =
        match obj with
        | :? Var as v -> this.name = v.name
        | _ -> false

    override this.GetHashCode() = hash this.name

    override this.ToString() = this.name

    interface IComparable with
        member this.CompareTo(obj: obj) =
            match obj with
            | :? Var as v -> compare this.name v.name
            | _ -> invalidArg (nameof obj) "Not a Var"

/// <summary>
/// Lambda term.
/// </summary>
type Term =
    | Variable of Var
    | Abstraction of Var * Term
    | Application of Term * Term

    /// <inheritdoc/>
    override this.ToString() =
        let rec toStr term =
            match term with
            | Variable v -> v.ToString()
            | Abstraction(param, body) -> sprintf "λ%s.%s" (param.ToString()) (toStr body)
            | Application(left, right) ->
                // try to avoid unnecessary parentheses
                sprintf
                    (match left, right with
                     | (* x y           <-> x y           *) Variable _, Variable _ -> "%s %s"
                     | (* x (λy.A)      <-> x (λy.A)      *) Variable _, Abstraction _ -> "%s (%s)"
                     | (* x (A B)       <-> x (A B)       *) Variable _, Application _ -> "%s (%s)"

                     | (* (λx.A) y      <-> (λx.A) y      *) Abstraction _, Variable _ -> "(%s) %s"
                     | (* (λx.A) (λy.B) <-> (λx.A) (λy.B) *) Abstraction _, Abstraction _ -> "(%s) (%s)"
                     | (* (λx.A) (B C)  <-> (λx.A) (B C)  *) Abstraction _, Application _ -> "(%s) (%s)"

                     | (* (A B) x       <-> A B x         *) Application _, Variable _ -> "%s %s"
                     | (* (A B) (λx.C)  <-> A B (λx.C)    *) Application _, Abstraction _ -> "%s (%s)"
                     | (* (A B) (C D)   <-> A B (C D)     *) Application _, Application _ -> "%s (%s)")
                    (toStr left)
                    (toStr right)

        toStr this

/// <summary>
/// Finds all free variables in the term.
/// </summary>
/// <param name="term">Term to find free variables in.</param>
/// <returns>All free variables in the term.</returns>
let rec freeVariables term =
    match term with
    | Variable var -> Set.singleton var
    | Application(left, right) -> Set.union (freeVariables left) (freeVariables right)
    | Abstraction(var, body) -> Set.remove var (freeVariables body)

/// <summary>
/// For parameters [a;b;c;...;z] make lambda term λa.λb.λc.<...>λz.body.
/// </summary>
/// <param name="parameters">Abstraction parameters.</param>
/// <param name="body">Abstraction body.</param>
/// <returns>Abstraction in the form of λa.λb.λc.<...>λz.body.</returns>
let makeAbstraction parameters body =
    match parameters with
    | [] -> invalidArg (nameof parameters) "Cannot create abstraction with no parameters"
    | parameters -> List.foldBack (fun param acc -> Abstraction(param, acc)) parameters body

/// <summary>
/// For terms [A;B;C;...;Z] make lambda term (A B C <...> Z).
/// </summary>
/// <param name="terms">Application terms.</param>
/// <returns>Abstraction in the form of (A B C <...> Z).</returns>
let makeApplication terms =
    match terms with
    | [] -> invalidArg (nameof terms) "Cannot create application with no terms"
    | [ _ ] -> invalidArg (nameof terms) "Cannot create application with one term"
    | args -> List.fold (fun acc arg -> Application(acc, arg)) args.Head args.Tail

/// <summary>
/// For variables [a;b;c;...;z] make lambda term (a b c <...> c).
/// </summary>
/// <param name="variables">Application variables.</param>
/// <returns>Abstraction in the form of (a b c <...> z).</returns>
let makeVarApplication variables =
    variables |> List.map Variable |> makeApplication

/// <summary>
/// Check if two lamda terms are alpha equivalent.
/// </summary>
/// <param name="termA">Left term.</param>
/// <param name="termB">Right term.</param>
/// <returns>Returns value indicating whether two terms are alpha equivalent.</returns>
let alphaEqual termA termB =
    let toDeBruijn term =
        let rec conv env depth =
            function
            | Variable x ->
                match Map.tryFind x env with
                | Some idx -> depth - idx - 1 |> string |> Var |> Variable
                | None -> Variable x // free variable stays symbolic
            | Abstraction(x, body) ->
                let newEnv = Map.add x depth env
                Abstraction(Var "_", conv newEnv (depth + 1) body)
            | Application(f, a) -> Application(conv env depth f, conv env depth a)

        conv Map.empty 0 term

    toDeBruijn termA = toDeBruijn termB

// combinators

let private f = Var "f"
let private g = Var "g"
let private s = Var "s"
let private x = Var "x"
let private y = Var "y"
let private z = Var "z"

let I = Abstraction(x, Variable x)
let omega = Abstraction(s, Application(Variable s, Variable s))
let Omega = Application(omega, omega)
let K = makeAbstraction [ x; y ] <| Variable x
let Kstar = makeAbstraction [ x; y ] <| Variable y

let S =
    makeAbstraction [ x; y; z ]
    <| makeApplication [ Variable x; Variable z; Application(Variable y, Variable z) ]

let B =
    makeAbstraction [ f; g; x ]
    <| makeApplication [ Variable f; Application(Variable g, Variable x) ]

let Y =
    let t = Abstraction(x, Application(Variable f, Application(Variable x, Variable x)))
    Abstraction(f, Application(t, t))

// logic

let private a = Var "a"
let private b = Var "b"
let private t = Var "t"

let TRUE = K
let FALSE = Kstar

let IF = makeAbstraction [ b; t; f ] <| makeVarApplication [ b; t; f ]

let AND =
    makeAbstraction [ a; b ]
    <| makeApplication [ IF; Variable a; Variable b; FALSE ]

let OR =
    makeAbstraction [ a; b ] <| makeApplication [ IF; Variable a; TRUE; Variable b ]

let NOT = makeAbstraction [ b ] <| makeApplication [ IF; Variable b; FALSE; TRUE ]

// numerals and arithmetic

let private m = Var "m"
let private n = Var "n"

let ZERO = makeAbstraction [ s; z ] <| Variable z
let ONE = makeAbstraction [ s; z ] <| Application(Variable s, Variable z)

let makeNumeral =
    function
    | 0u -> ZERO
    | 1u -> ONE
    | n ->
        [ 2u .. n ]
        |> List.fold (fun acc _ -> Application(Variable s, acc)) (Application(Variable s, Variable z))
        |> makeAbstraction [ s; z ]

let SUCC =
    makeAbstraction [ n; f; x ]
    <| Application(Variable f, makeVarApplication [ n; f; x ])

let ADD =
    makeAbstraction [ m; n; f; x ]
    <| Application(Application(Variable m, Variable f), makeVarApplication [ n; f; x ])

let MUL =
    makeAbstraction [ m; n ]
    <| makeApplication [ Variable m; Application(ADD, Variable n); ZERO ]

let POWER =
    makeAbstraction [ m; n ]
    <| makeApplication [ Variable m; Application(MUL, Variable n); ONE ]

let ISZERO =
    makeAbstraction [ n ]
    <| makeApplication [ Variable n; Abstraction(x, FALSE); TRUE ]

// pairs

let private p = Var "p"

let PAIR = makeAbstraction [ x; y; f ] <| makeVarApplication [ f; x; y ]
let FIRST = Abstraction(p, Application(Variable p, TRUE))
let SECOND = Abstraction(p, Application(Variable p, FALSE))

// lists

let private c = Var "c"
let private h = Var "h"
let private e = Var "e"
let private l = Var "l"

let NIL = makeAbstraction [ c; n ] <| Variable n

let CONS =
    makeAbstraction [ e; l; c; n ]
    <| makeApplication [ Variable c; Variable e; makeVarApplication [ l; c; n ] ]

let ISEMPTY =
    Abstraction(l, makeApplication [ Variable l; makeAbstraction [ h; t ] FALSE; TRUE ])

let HEAD =
    Abstraction(l, makeApplication [ Variable l; makeAbstraction [ h; t ] (Variable h); ZERO ])

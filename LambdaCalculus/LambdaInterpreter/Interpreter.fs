// <copyright file="Interpreter.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module LambdaInterpreter.Interpreter

open LambdaInterpreter.Term

let private findNewName termA termB =
    let usedNames = Set.union (freeVariables termA) (freeVariables termB)

    let latin = [ 'a' .. 'z' ]

    // encode index in base 26
    let rec createName index =
        let ch = latin[index % latin.Length]

        if index < latin.Length then
            string ch
        else
            let index = index / latin.Length - 1
            string ch + createName index

    let rec findName index =
        let candidate = createName index |> Var

        if usedNames.Contains candidate then
            findName <| index + 1
        else
            candidate

    findName 0

let rec private substitute term variableToReplace replacement =
    let rec substituteInner term =
        match term with
        | Variable v when v = variableToReplace -> replacement
        | Variable _ -> term // no change
        | Abstraction(abstractionVar, _) when abstractionVar = variableToReplace -> term // no change
        | Abstraction(abstractionVar, body) when
            not (Set.contains abstractionVar (freeVariables replacement))
            || not (Set.contains variableToReplace (freeVariables body))
            ->
            Abstraction(abstractionVar, substituteInner body)
        | Abstraction(abstractionVar, body) ->
            let newVariable = findNewName term replacement
            let newBody = substitute body abstractionVar (Variable newVariable)
            let newBody = substitute newBody variableToReplace replacement
            Abstraction(newVariable, newBody)
        | Application(left, right) -> Application(substituteInner left, substituteInner right)

    substituteInner term

let rec private betaReduce term =
    match term with
    | Variable _ -> term, false
    | Abstraction(var, body) ->
        match betaReduce body with
        | newBody, true -> Abstraction(var, newBody), true
        | _, false -> term, false
    | Application(Abstraction(var, body), rightTerm) -> substitute body var rightTerm, true
    | Application(left, right) ->
        match betaReduce left with
        | newLeft, true -> Application(newLeft, right), true
        | _, false ->
            match betaReduce right with
            | newRight, true -> Application(left, newRight), true
            | _, false -> term, false

/// <summary>
/// Normalizes term using normal strategy.
/// </summary>
/// <param name="term">Term to normalize.</param>
/// <returns>Normalized term.</returns>
let rec normalize term =
    match betaReduce term with
    | term, true -> normalize term
    | term, false -> term

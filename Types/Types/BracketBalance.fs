// <copyright file="BracketBalance.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module BracketBalance

let checkBalance str =
    let availableBrackets = Map.ofList [ '(', ')'; '[', ']'; '{', '}' ]

    let rec checkInternal stack index =
        if index >= String.length str then
            List.isEmpty stack
        else
            let currentChar = str.Chars index

            match availableBrackets.TryFind currentChar with
            | Some closingBracket -> checkInternal (closingBracket :: stack) (index + 1)
            | None when availableBrackets.Values.Contains currentChar ->
                match stack with
                | [] -> false
                | top :: rest when top = currentChar -> checkInternal rest (index + 1)
                | _ -> false
            | _ -> checkInternal stack (index + 1)

    checkInternal [] 0

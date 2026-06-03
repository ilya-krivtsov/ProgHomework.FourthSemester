module ParseTree

type MathOperationType =
    | Addition
    | Subtraction
    | Multiplication
    | Division

type ParseTree =
    | Value of int
    | Operation of MathOperation

and MathOperation =
    { Type: MathOperationType
      LeftOperand: ParseTree
      RightOperand: ParseTree }

type private ContinuationStep =
    | Finished
    | Step of ParseTree * (unit -> ContinuationStep)

let evaluate tree =
    let rec linearize node continuation =
        match node with
        | Value v -> Step(node, continuation)
        | Operation op ->
            Step(node, fun () -> linearize op.LeftOperand (fun () -> linearize op.RightOperand continuation))

    let rec traverse step =
        match step with
        | Finished -> []
        | Step(value, nextStep) ->
            let stack = traverse <| nextStep ()

            match value with
            | Value v -> v :: stack
            | Operation op ->
                let f =
                    match op.Type with
                    | Addition -> (+)
                    | Subtraction -> (-)
                    | Multiplication -> (*)
                    | Division -> (/)

                assert (stack.Length >= 2)
                let left, stack = stack.Head, stack.Tail
                let right, stack = stack.Head, stack.Tail

                f left right :: stack

    let result = traverse <| linearize tree (fun () -> Finished)
    assert (result.Length = 1)
    result.Head

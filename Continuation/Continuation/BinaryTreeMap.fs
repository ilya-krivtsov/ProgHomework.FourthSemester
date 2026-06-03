// <copyright file="BinaryTreeMap.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module BinaryTreeMap

/// <summary>
/// Binary tree that has value in node and two optional children.
/// </summary>
type BinaryTree<'a> =
    { Value: 'a
      LeftChild: Option<BinaryTree<'a>>
      RightChild: Option<BinaryTree<'a>> }

type private ContinuationStep<'a> =
    | Finished
    | Step of BinaryTree<'a> * (unit -> ContinuationStep<'a>)

/// <summary>
/// Maps given function over all values in nodes in the tree.
/// </summary>
/// <param name="func">Map function.</param>
/// <param name="tree">Binary tree to map.</param>
/// <returns>New binary tree with mapped values.</returns>
let map func tree =
    let rec linearize node continuation =
        match node with
        | None -> continuation ()
        | Some node -> Step(node, fun () -> linearize node.LeftChild (fun () -> linearize node.RightChild continuation))

    let rec traverse step =
        match step with
        | Finished -> []
        | Step(value, nextStep) ->
            let stack = traverse (nextStep ())
            let newValue = func value.Value

            match value.LeftChild, value.RightChild with
            | None, None ->
                { Value = newValue
                  LeftChild = None
                  RightChild = None }
                :: stack
            | Some _, None ->
                assert (stack.Length >= 1)
                let left, stack = stack.Head, stack.Tail

                { Value = newValue
                  LeftChild = Some left
                  RightChild = None }
                :: stack
            | None, Some _ ->
                assert (stack.Length >= 1)
                let right, stack = stack.Head, stack.Tail

                { Value = newValue
                  LeftChild = None
                  RightChild = Some right }
                :: stack
            | Some _, Some _ ->
                assert (stack.Length >= 2)
                let left, stack = stack.Head, stack.Tail
                let right, stack = stack.Head, stack.Tail

                { Value = newValue
                  LeftChild = Some left
                  RightChild = Some right }
                :: stack

    let result = traverse <| linearize (Some tree) (fun () -> Finished)
    assert (result.Length = 1)
    result.Head

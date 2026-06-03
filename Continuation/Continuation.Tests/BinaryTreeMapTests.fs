// <copyright file="BinaryTreeMapTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module BinaryTreeMap.Tests

open NUnit.Framework
open FsUnit
open FsCheck.NUnit

[<Test>]
let ``map over single-node tree`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 5
          LeftChild = None
          RightChild = None }

    let result = BinaryTreeMap.map (fun x -> x * 2) tree
    result.Value |> should equal 10

[<Test>]
let ``map over left-leaning tree`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 2
          LeftChild =
            Some
                { Value = 1
                  LeftChild = None
                  RightChild = None }
          RightChild = None }

    let result = BinaryTreeMap.map (fun x -> x * 10) tree
    result.Value |> should equal 20
    result.LeftChild.Value.Value |> should equal 10
    result.RightChild |> should equal None

[<Test>]
let ``map over right-leaning tree`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 1
          LeftChild = None
          RightChild =
            Some
                { Value = 2
                  LeftChild = None
                  RightChild = None } }

    let result = BinaryTreeMap.map (fun x -> x * 10) tree
    result.Value |> should equal 10
    result.LeftChild |> should equal None
    result.RightChild.Value.Value |> should equal 20

[<Test>]
let ``map over full binary tree`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 2
          LeftChild =
            Some
                { Value = 1
                  LeftChild = None
                  RightChild = None }
          RightChild =
            Some
                { Value = 3
                  LeftChild = None
                  RightChild = None } }

    let result = BinaryTreeMap.map string tree
    result.Value |> should equal "2"
    result.LeftChild.Value.Value |> should equal "1"
    result.RightChild.Value.Value |> should equal "3"

[<Test>]
let ``map over deeper tree`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 4
          LeftChild =
            Some
                { Value = 2
                  LeftChild =
                    Some
                        { Value = 1
                          LeftChild = None
                          RightChild = None }
                  RightChild =
                    Some
                        { Value = 3
                          LeftChild = None
                          RightChild = None } }
          RightChild =
            Some
                { Value = 6
                  LeftChild =
                    Some
                        { Value = 5
                          LeftChild = None
                          RightChild = None }
                  RightChild =
                    Some
                        { Value = 7
                          LeftChild = None
                          RightChild = None } } }

    let result = BinaryTreeMap.map (fun x -> x * x) tree
    result.Value |> should equal 16
    result.LeftChild.Value.Value |> should equal 4
    result.LeftChild.Value.LeftChild.Value.Value |> should equal 1
    result.LeftChild.Value.RightChild.Value.Value |> should equal 9
    result.RightChild.Value.Value |> should equal 36
    result.RightChild.Value.LeftChild.Value.Value |> should equal 25
    result.RightChild.Value.RightChild.Value.Value |> should equal 49

[<Property>]
let ``map identity is identity`` (x: int) =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = x
          LeftChild =
            Some
                { Value = x + 1
                  LeftChild = None
                  RightChild = None }
          RightChild =
            Some
                { Value = x + 2
                  LeftChild = None
                  RightChild = None } }

    let mapped = BinaryTreeMap.map id tree

    mapped.Value = tree.Value
    && mapped.LeftChild.Value.Value = tree.LeftChild.Value.Value
    && mapped.RightChild.Value.Value = tree.RightChild.Value.Value

[<Property>]
let ``map preserves structure`` (x: int) =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = x
          LeftChild =
            Some
                { Value = x + 1
                  LeftChild = None
                  RightChild = None }
          RightChild =
            Some
                { Value = x + 2
                  LeftChild = None
                  RightChild = None } }

    let f (v: int) = v * 3
    let result = BinaryTreeMap.map f tree

    result.Value = f tree.Value
    && result.LeftChild.IsSome = tree.LeftChild.IsSome
    && result.RightChild.IsSome = tree.RightChild.IsSome
    && result.LeftChild.Value.Value = f tree.LeftChild.Value.Value
    && result.RightChild.Value.Value = f tree.RightChild.Value.Value

[<Test>]
let ``map over root with only left child, which has only right child`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 1
          LeftChild =
            Some
                { Value = 2
                  LeftChild = None
                  RightChild =
                    Some
                        { Value = 3
                          LeftChild = None
                          RightChild = None } }
          RightChild = None }

    let result = BinaryTreeMap.map (fun x -> x * 10) tree
    result.Value |> should equal 10
    result.LeftChild.Value.Value |> should equal 20
    result.LeftChild.Value.RightChild.Value.Value |> should equal 30
    result.LeftChild.Value.LeftChild |> should equal None
    result.RightChild |> should equal None

[<Test>]
let ``map over root with only right child, which has only left child`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 1
          LeftChild = None
          RightChild =
            Some
                { Value = 2
                  LeftChild =
                    Some
                        { Value = 3
                          LeftChild = None
                          RightChild = None }
                  RightChild = None } }

    let result = BinaryTreeMap.map (fun x -> x * 10) tree
    result.Value |> should equal 10
    result.RightChild.Value.Value |> should equal 20
    result.RightChild.Value.LeftChild.Value.Value |> should equal 30
    result.RightChild.Value.RightChild |> should equal None
    result.LeftChild |> should equal None

[<Test>]
let ``map over tree where left subtree is deeper than right`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 5
          LeftChild =
            Some
                { Value = 3
                  LeftChild =
                    Some
                        { Value = 1
                          LeftChild = None
                          RightChild = None }
                  RightChild = None }
          RightChild =
            Some
                { Value = 7
                  LeftChild = None
                  RightChild = None } }

    let result = BinaryTreeMap.map string tree
    result.Value |> should equal "5"
    result.LeftChild.Value.Value |> should equal "3"
    result.LeftChild.Value.LeftChild.Value.Value |> should equal "1"
    result.LeftChild.Value.RightChild |> should equal None
    result.RightChild.Value.Value |> should equal "7"

[<Test>]
let ``map over tree where right subtree is deeper than left`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 5
          LeftChild =
            Some
                { Value = 3
                  LeftChild = None
                  RightChild = None }
          RightChild =
            Some
                { Value = 7
                  LeftChild = None
                  RightChild =
                    Some
                        { Value = 9
                          LeftChild = None
                          RightChild = None } } }

    let result = BinaryTreeMap.map string tree
    result.Value |> should equal "5"
    result.LeftChild.Value.Value |> should equal "3"
    result.RightChild.Value.Value |> should equal "7"
    result.RightChild.Value.RightChild.Value.Value |> should equal "9"
    result.RightChild.Value.LeftChild |> should equal None

[<Test>]
let ``map over tree with only left spine`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 1
          LeftChild =
            Some
                { Value = 2
                  LeftChild =
                    Some
                        { Value = 3
                          LeftChild =
                            Some
                                { Value = 4
                                  LeftChild = None
                                  RightChild = None }
                          RightChild = None }
                  RightChild = None }
          RightChild = None }

    let result = BinaryTreeMap.map (fun x -> x * 3) tree
    result.Value |> should equal 3
    result.LeftChild.Value.Value |> should equal 6
    result.LeftChild.Value.LeftChild.Value.Value |> should equal 9
    result.LeftChild.Value.LeftChild.Value.LeftChild.Value.Value |> should equal 12
    result.LeftChild.Value.LeftChild.Value.RightChild |> should equal None

[<Test>]
let ``map over tree with only right spine`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 1
          LeftChild = None
          RightChild =
            Some
                { Value = 2
                  LeftChild = None
                  RightChild =
                    Some
                        { Value = 3
                          LeftChild = None
                          RightChild =
                            Some
                                { Value = 4
                                  LeftChild = None
                                  RightChild = None } } } }

    let result = BinaryTreeMap.map (fun x -> x * 3) tree
    result.Value |> should equal 3
    result.RightChild.Value.Value |> should equal 6
    result.RightChild.Value.RightChild.Value.Value |> should equal 9

    result.RightChild.Value.RightChild.Value.RightChild.Value.Value
    |> should equal 12

    result.RightChild.Value.RightChild.Value.LeftChild |> should equal None

[<Test>]
let ``map over tree with missing intermediate children`` () =
    let tree: BinaryTreeMap.BinaryTree<int> =
        { Value = 1
          LeftChild =
            Some
                { Value = 2
                  LeftChild = None
                  RightChild =
                    Some
                        { Value = 4
                          LeftChild =
                            Some
                                { Value = 5
                                  LeftChild = None
                                  RightChild = None }
                          RightChild = None } }
          RightChild =
            Some
                { Value = 3
                  LeftChild =
                    Some
                        { Value = 6
                          LeftChild = None
                          RightChild = None }
                  RightChild = None } }

    let result = BinaryTreeMap.map (fun x -> x * 10) tree
    result.Value |> should equal 10
    result.LeftChild.Value.Value |> should equal 20
    result.LeftChild.Value.LeftChild |> should equal None
    result.LeftChild.Value.RightChild.Value.Value |> should equal 40
    result.LeftChild.Value.RightChild.Value.LeftChild.Value.Value |> should equal 50
    result.LeftChild.Value.RightChild.Value.RightChild |> should equal None
    result.RightChild.Value.Value |> should equal 30
    result.RightChild.Value.LeftChild.Value.Value |> should equal 60
    result.RightChild.Value.RightChild |> should equal None

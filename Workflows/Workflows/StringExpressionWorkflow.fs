// <copyright file="StringExpressionWorkflow.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module StringExpressionWorkflow

open System

type StringExpressionWorkflow() =

    member this.Bind(x: string, f: int -> int option) =
        match Int32.TryParse x with
        | true, v -> v |> f
        | _ -> None

    member this.Return(x: int) = Some x

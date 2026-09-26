// <copyright file="RoundingWorkflow.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module RoundingWorkflow

open System

type RoundingWorkflow(precision: int) =
    member this.Bind(x: float, f: float -> float) = f (Math.Round(x, precision))

    member this.Return(x: float) = Math.Round(x, precision)

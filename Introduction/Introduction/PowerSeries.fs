// <copyright file="PowerSeries.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module PowerSeries

open System

/// <summary>
/// Generates a list of the first m powers of 2 starting from 2^n.
/// </summary>
/// <param name="n">The exponent from which to start generating powers of 2.</param>
/// <param name="m">The number of additional powers of 2 to generate.</param>
/// <returns>A list of the first m + 1 powers of 2 starting from 2^n.</returns>
let generatePowerSeries (n: int) (m: uint) =
    let rec generateInternal accum index =
        if index = 0u then
            accum
        else
            let currentPower = List.head accum
            let nextPower = currentPower / 2.0
            generateInternal (nextPower :: accum) (index - 1u)

    let initialPower = Math.Pow(2.0, float (n + int m))
    generateInternal [ initialPower ] m

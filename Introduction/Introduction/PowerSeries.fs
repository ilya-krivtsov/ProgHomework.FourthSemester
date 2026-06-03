// <copyright file="PowerSeries.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module PowerSeries

/// <summary>
/// Generates a list of the first m powers of 2 starting from 2^n.
/// </summary>
/// <param name="n">The exponent from which to start generating powers of 2.</param>
/// <param name="m">The number of powers of 2 to generate.</param
/// <returns>A list of the first m powers of 2 starting from 2^n.</returns>
let generatePowerSeries (n: uint) m =
    [ n .. (n + m) ] |> List.map (int >> (<<<) 1u)

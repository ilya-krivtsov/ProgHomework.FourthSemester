// <copyright file="Factorial.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module Factorial

/// <summary>
/// Computes the factorial of a non-negative integer n, denoted as n!, which is the
/// product of all positive integers less than or equal to n.
/// </summary>
/// <param name="n">The non-negative integer for which to compute the factorial.</param>
/// <returns>The factorial of n.</returns>
let factorial n =
    let rec factorialInternal acc n =
        if n <= 1u then
            acc
        else
            factorialInternal (acc * n) (n - 1u)

    factorialInternal 1u n

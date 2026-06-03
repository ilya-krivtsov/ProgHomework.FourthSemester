// <copyright file="Fibonacci.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module Fibonacci

/// <summary>
/// Computes the nth Fibonacci number, where F(0) = 0 and F(1) = 1. The Fibonacci sequence is defined as F(n) = F(n-1) + F(n-2) for n > 1.
/// </summary>
/// <param name="n">The non-negative integer for which to compute the Fibonacci number.</param>
/// <returns>The n'th Fibonacci number.</returns>
let fibonacci (n: uint) =
    let rec fibonacciInternal prev current iteration =
        if iteration = n then
            current
        else
            fibonacciInternal current (prev + current) (iteration + 1u)

    if n <= 1u then n else fibonacciInternal 0u 1u 1u // since we assume current = 1, so iteration = 1

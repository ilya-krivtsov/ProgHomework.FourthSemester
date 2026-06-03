// <copyright file="EvenNumbers.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module EvenNumbers

/// <summary>
/// Counts even numbers in the list using List.map and List.sum.
/// </summary>
/// <param name="xs">List to count even numbers in.</param>
/// <returns>Number of even numbers in the list.</returns>
let evenNumbersCountA = List.map (fun x -> 1 - abs x % 2) >> List.sum

/// <summary>
/// Counts even numbers in the list using List.filter and List.length.
/// </summary>
/// <param name="xs">List to count even numbers in.</param>
/// <returns>Number of even numbers in the list.</returns>
let evenNumbersCountB = List.filter (fun x -> x % 2 = 0) >> List.length

/// <summary>
/// Counts even numbers in the list using List.fold.
/// </summary>
/// <param name="xs">List to count even numbers in.</param>
/// <returns>Number of even numbers in the list.</returns>
let evenNumbersCountC = List.fold (fun acc value -> acc + (1 - abs value % 2)) 0

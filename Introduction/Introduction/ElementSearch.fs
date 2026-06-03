// <copyright file="ElementSearch.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module ElementSearch

/// <summary>
/// Finds the first occurrence of the specified element in the list, or None if none found.
/// </summary>
/// <param name="element">The element to find.</param>
/// <param name="list">The list to search through.</param>
/// <returns>Some(element) if found, otherwise None.</returns>
let rec findElement element list =
    match list with
    | [] -> None
    | head :: tail ->
        if head = element then
            Some head
        else
            findElement element tail

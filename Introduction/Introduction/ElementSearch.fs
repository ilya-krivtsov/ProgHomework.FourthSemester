// <copyright file="ElementSearch.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module ElementSearch

/// <summary>
/// Finds index of the first occurrence of the specified element in the list, or None if none found.
/// </summary>
/// <param name="element">The element to find.</param>
/// <param name="list">The list to search through.</param>
/// <returns>Some(index) if found, otherwise None.</returns>
let findElement element list =
    let rec findElementInternal element list index =
        match list with
        | [] -> None
        | head :: tail ->
            if head = element then
                Some index
            else
                findElementInternal element tail (index + 1)

    findElementInternal element list 0

// <copyright file="ListReversal.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module ListReversal

/// <summary>
/// Reverses the order of elements in a list.
/// </summary>
/// <param name="list">The list to reverse.</param>
/// <returns>A new list with the elements in reverse order.</returns>
let listReverse list =
    let rec listReverseInternal newList oldList =
        match oldList with
        | [] -> newList
        | head :: tail -> listReverseInternal (head :: newList) tail

    listReverseInternal [] list

// <copyright file="EvenNumbers.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module EvenNumbers

let evenNumbersCountA = List.map (fun x -> 1 - abs x % 2) >> List.sum

let evenNumbersCountB = List.filter (fun x -> x % 2 = 0) >> List.length

let evenNumbersCountC = List.fold (fun acc value -> acc + (1 - abs value % 2)) 0

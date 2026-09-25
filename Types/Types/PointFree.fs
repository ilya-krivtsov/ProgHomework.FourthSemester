// <copyright file="PointFree.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module PointFree

let originalFunc x l = List.map (fun y -> y * x) l

let pointfreeFunc = (*) >> List.map

// <copyright file="PointFreeTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module PointFree.Tests

open NUnit.Framework
open FsCheck

[<Test>]
let ``originalFunc equal to pointfreeFunc`` () =
    Check.QuickThrowOnFailure(fun x l ->
        let a = originalFunc x l
        let b = pointfreeFunc x l
        a = b)

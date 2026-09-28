// <copyright file="LazyTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module Lazy.Tests

open System
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent
open NUnit.Framework
open FsUnit

let private callerCount = 8

let private runConcurrently (get: unit -> int) =
    let observed = ConcurrentBag<int>()
    let startGate = new ManualResetEventSlim(false)

    let callers =
        [| for _ in 1..callerCount ->
               Task.Factory.StartNew(
                   (fun () ->
                       startGate.Wait()
                       observed.Add(get ())),
                   TaskCreationOptions.LongRunning
               ) |]

    startGate.Set()
    Task.WaitAll callers

    observed |> Seq.toList

let private freshSupplier () =
    let callCount = ref 0

    let supplier () =
        let mine = Interlocked.Increment(callCount)
        Thread.Sleep(20)
        mine

    supplier, callCount

[<Test>]
let ``lazy should compute value only once`` () =
    let mutable counter = 0

    let supplier () =
        do counter <- counter + 1
        42

    let lazies: ILazy<int> list =
        [ PrimitiveLazy supplier; SynchronizedLazy supplier; LockFreeLazy supplier ]

    for l in lazies do
        counter <- 0
        l.Get() |> should equal 42
        counter |> should equal 1
        l.Get() |> should equal 42
        counter |> should equal 1

[<Test>]
let ``lock free lazy: every concurrent caller observes the same value`` () =
    let supplier, _ = freshSupplier ()
    let lockFree: ILazy<int> = new LockFreeLazy<int>(supplier)

    let observed = runConcurrently (fun () -> lockFree.Get())

    observed |> List.distinct |> should haveLength 1

[<Test>]
let ``lock free lazy: the published value is stable for later callers`` () =
    let supplier, _ = freshSupplier ()
    let lockFree: ILazy<int> = new LockFreeLazy<int>(supplier)

    let observed = runConcurrently (fun () -> lockFree.Get())
    let published = List.head observed

    lockFree.Get() |> should equal published
    lockFree.Get() |> should equal published

[<Test>]
let ``lock free lazy: supplier runs at least once and not more than the caller count`` () =
    let supplier, callCount = freshSupplier ()
    let lockFree: ILazy<int> = new LockFreeLazy<int>(supplier)

    runConcurrently (fun () -> lockFree.Get()) |> ignore

    callCount.Value |> should be (greaterThan 0)
    callCount.Value |> should be (lessThanOrEqualTo callerCount)

[<Test>]
let ``synchronized lazy: supplier runs exactly once under concurrency`` () =
    let supplier, callCount = freshSupplier ()
    let sync: ILazy<int> = new SynchronizedLazy<int>(supplier)

    let observed = runConcurrently (fun () -> sync.Get())

    callCount.Value |> should equal 1
    observed |> List.distinct |> should haveLength 1
    sync.Get() |> should equal (List.head observed)

[<Test>]
let ``synchronized lazy: retries the supplier after it throws`` () =
    let shouldThrow = ref true

    let supplier () =
        if !shouldThrow then
            shouldThrow := false
            failwith "supplier failed"
        else
            7

    let sync: ILazy<int> = new SynchronizedLazy<int>(supplier)

    Assert.Throws<Exception>(fun () -> sync.Get() |> ignore) |> ignore

    sync.Get() |> should equal 7

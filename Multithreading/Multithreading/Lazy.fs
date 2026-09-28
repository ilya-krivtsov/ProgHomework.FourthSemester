// <copyright file="Lazy.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module Lazy

open System.Threading

/// A value that is computed on first access and cached for every access after that.
type ILazy<'a> =
    /// <summary>
    /// Returns the cached value, invoking the supplier if this is the first call.
    /// </summary>
    abstract member Get: unit -> 'a

/// <summary>
/// A lazy that memoizes the supplier result without any synchronization.
/// </summary>
/// <param name="supplier">Computes the value. May be invoked multiple times.</param>
type PrimitiveLazy<'a>(supplier: unit -> 'a) =
    let mutable value: 'a option = None

    interface ILazy<'a> with
        /// <inheritdoc />
        member this.Get() =
            if Option.isSome value then
                value.Value
            else
                let result = supplier ()
                value <- Some result
                result

/// <summary>
/// A lazy that evaluates the supplier exactly once, serializing concurrent
/// callers on a monitor so that every caller observes the same result.
/// </summary>
/// <param name="supplier">Computes the value. Invoked exactly once.</param>
type SynchronizedLazy<'a>(supplier: unit -> 'a) =
    let mutable value: 'a option = None
    let lockObj = obj ()

    interface ILazy<'a> with
        /// <inheritdoc />
        member this.Get() =
            if value.IsNone then
                lock lockObj (fun () ->
                    if value.IsNone then
                        value <- Some <| supplier ())

            value.Value

/// <summary>
/// A lazy that publishes the supplier result without taking a lock, using a
/// single compare-and-swap to let exactly one caller win the publication.
/// </summary>
/// <param name="supplier">
/// Computes the value. May be invoked concurrently by several callers, so it
/// should be free of side effects that must happen exactly once.
/// </param>
type LockFreeLazy<'a>(supplier: unit -> 'a) =
    let mutable value: 'a option = None

    interface ILazy<'a> with
        /// <inheritdoc />
        member this.Get() =
            let currentValue = value

            if Option.isNone currentValue then
                let newValue = Some <| supplier ()
                let seenValue = Interlocked.CompareExchange(&value, newValue, currentValue)

                if obj.ReferenceEquals(currentValue, seenValue) then
                    newValue.Value
                else
                    seenValue.Value

            else
                currentValue.Value

module EvenNumbers.Tests

open NUnit.Framework
open FsCheck

[<Test>]
let ``variant a equivalent to variant b`` () =
    Check.QuickThrowOnFailure(fun xs ->
        let a = evenNumbersCountA xs
        let b = evenNumbersCountB xs
        a = b)

[<Test>]
let ``variant a equivalent to variant c`` () =
    Check.QuickThrowOnFailure(fun xs ->
        let a = evenNumbersCountA xs
        let c = evenNumbersCountC xs
        a = c)

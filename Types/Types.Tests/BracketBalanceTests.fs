// <copyright file="BracketBalanceTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module BracketBalance.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``empty string is balanced`` () = checkBalance "" |> should be True

[<Test>]
let ``no-brackets string is balanced`` () = checkBalance "abcdef" |> should be True

[<TestCase("()")>]
[<TestCase("[]")>]
[<TestCase("{}")>]
let ``() and similar are balanced`` str = checkBalance str |> should be True

[<TestCase("()()")>]
[<TestCase("[][]")>]
[<TestCase("{}{}")>]
let ``()() and similar are balanced`` str = checkBalance str |> should be True

[<TestCase("((()))")>]
[<TestCase("[[[]]]")>]
[<TestCase("{{{}}}")>]
let ``((())) and similar are balanced`` str = checkBalance str |> should be True

[<TestCase("({[()]})")>]
[<TestCase("[{([])}]")>]
[<TestCase("{[({})]}")>]
let ``({[()]}) and similar are balanced`` str = checkBalance str |> should be True

[<TestCase("(()())")>]
[<TestCase("([]{}[])")>]
[<TestCase("{[()[]]}")>]
let ``(()()) and similar are balanced`` str = checkBalance str |> should be True

[<TestCase("a(b(c)d(e)f)g")>]
[<TestCase("a[b{c}d[e]f]g")>]
[<TestCase("a{b(c)d[e]f}g")>]
let ``(()()) and similar with interleaved are balanced`` str = checkBalance str |> should be True

[<TestCase("(")>]
[<TestCase("[")>]
[<TestCase("{")>]
[<TestCase(")")>]
[<TestCase("]")>]
[<TestCase("}")>]
let ``( and similar are unbalanced`` str = checkBalance str |> should be False

[<TestCase(")(")>]
[<TestCase("][")>]
[<TestCase("}{")>]
let ``)( and similar are unbalanced`` str = checkBalance str |> should be False

[<TestCase("(][)")>]
[<TestCase("[(])")>]
[<TestCase("{(})")>]
let ``(][) and similar are unbalanced`` str = checkBalance str |> should be False

[<TestCase("(((}}}")>]
[<TestCase("[[[)))")>]
[<TestCase("{{{]]]")>]
let ``((())) and similar are unbalanced`` str = checkBalance str |> should be False

[<TestCase("({]()[})")>]
[<TestCase("[{)[](}]")>]
[<TestCase("{]({})[}")>]
let ``({[()]}) and similar are unbalanced`` str = checkBalance str |> should be False

[<TestCase("(}{())")>]
[<TestCase("([]}{[])")>]
[<TestCase("{[(][)]}")>]
let ``(}{()) and similar are unbalanced`` str = checkBalance str |> should be False

[<TestCase("a(b{c)d[e)f]g")>]
[<TestCase("a(b{c]d{e)f]g")>]
let ``({)[)] and similar with interleaved are unbalanced`` str = checkBalance str |> should be False

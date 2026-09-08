// <copyright file="PowerSeriesTests.fs" company="Ilya Krivtsov">
// Copyright (c) Ilya Krivtsov. All rights reserved.
// </copyright>

module PowerSeries.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``power series from 0 with m = 3`` () =
    generatePowerSeries 0 3u |> should equal [ 1.0; 2.0; 4.0; 8.0 ]

[<Test>]
let ``power series from 1 with m = 2`` () =
    generatePowerSeries 1 2u |> should equal [ 2.0; 4.0; 8.0 ]

[<Test>]
let ``power series from 5 with m = 0`` () =
    generatePowerSeries 5 0u |> should equal [ 32.0 ]

[<Test>]
let ``power series from 0 with m = 0`` () =
    generatePowerSeries 0 0u |> should equal [ 1.0 ]

[<Test>]
let ``power series from 3 with m = 4`` () =
    generatePowerSeries 3 4u |> should equal [ 8.0; 16.0; 32.0; 64.0; 128.0 ]

[<Test>]
let ``power series from -3 with m = 4`` () =
    generatePowerSeries -3 4u |> should equal [ 0.125; 0.25; 0.5; 1.0; 2.0 ]

[<Test>]
let ``power series from -4 with m = 1`` () =
    generatePowerSeries -4 1u |> should equal [ 0.0625; 0.125 ]

[<Test>]
let ``power series from -5 with m = 0`` () =
    generatePowerSeries -5 0u |> should equal [ 0.03125 ]

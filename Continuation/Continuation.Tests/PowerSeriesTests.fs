module PowerSeries.Tests

open NUnit.Framework
open FsUnit

[<Test>]
let ``power series of 0 with 3 elements`` () =
    generatePowerSeries 0u 3u |> should equal [ 1u; 2u; 4u; 8u ]

[<Test>]
let ``power series of 1 with 2 elements`` () =
    generatePowerSeries 1u 2u |> should equal [ 2u; 4u; 8u ]

[<Test>]
let ``power series of 5 with 0 elements`` () =
    generatePowerSeries 5u 0u |> should equal [ 32u ]

[<Test>]
let ``power series of 0 with 0 elements`` () =
    generatePowerSeries 0u 0u |> should equal [ 1u ]

[<Test>]
let ``power series of 3 with 4 elements`` () =
    generatePowerSeries 3u 4u |> should equal [ 8u; 16u; 32u; 64u; 128u ]

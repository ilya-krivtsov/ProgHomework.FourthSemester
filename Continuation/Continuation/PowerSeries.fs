module PowerSeries

let generatePowerSeries (n: uint) m =
    [ n .. (n + m) ] |> List.map (int >> (<<<) 1u)

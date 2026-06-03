module PrimeNumbers

type private State =
    { AccumulatedPrimes: int list
      LastPrime: int }

let primeNumbers () =
    let nextPrimeGenerator (state: State) =
        let rec findNextPrime primeCandidate =
            if
                state.AccumulatedPrimes
                |> Seq.tryFind (fun prime -> primeCandidate % prime = 0)
                |> Option.isNone
            then
                primeCandidate
            else
                findNextPrime <| primeCandidate + 2

        let foundPrime = findNextPrime <| state.LastPrime + 2

        Some(
            foundPrime,
            { AccumulatedPrimes = foundPrime :: state.AccumulatedPrimes
              LastPrime = foundPrime }
        )

    let firstPrimes = [ 2; 3 ]

    seq {
        yield! firstPrimes

        yield!
            Seq.unfold
                nextPrimeGenerator
                { AccumulatedPrimes = firstPrimes
                  LastPrime = firstPrimes |> List.last }
    }

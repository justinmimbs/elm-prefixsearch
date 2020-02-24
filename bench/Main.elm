module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner
import Types exposing (Search, exampleText, fill)


main : Benchmark.Runner.BenchmarkProgram
main =
    Benchmark.Runner.program benchmarks


benchmarks : Benchmark
benchmarks =
    Benchmark.describe "prefix search"
        ([ "a", "am", "ampl", "amplifica", "amplifications" ]
            |> List.map
                (compare Types.strings Types.trie)
        )


compare : Search a -> Search b -> String -> Benchmark
compare a b =
    let
        x =
            a.empty |> fill exampleText a.insert

        y =
            b.empty |> fill exampleText b.insert
    in
    \keyword ->
        Benchmark.compare keyword
            a.name
            (\() -> x |> a.search keyword)
            b.name
            (\() -> y |> b.search keyword)

module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner
import ExampleData exposing (exampleText, fill)
import TextIndex.Versions as Versions exposing (Version)


main : Benchmark.Runner.BenchmarkProgram
main =
    Benchmark.Runner.program <|
        benchmark_searchAll Versions.seekSet Versions.seekList


benchmark_search : Version a -> Version b -> Benchmark
benchmark_search a b =
    let
        x =
            a.empty |> fill exampleText a.insert

        y =
            b.empty |> fill exampleText b.insert
    in
    Benchmark.describe "search"
        ([ "a"
         , "am"
         , "ampl"
         , "amplifica"
         , "amplifications"
         ]
            |> List.map
                (\keyword ->
                    Benchmark.compare keyword
                        a.name
                        (\() -> x |> a.search keyword)
                        b.name
                        (\() -> y |> b.search keyword)
                )
        )


benchmark_searchAll : Version a -> Version b -> Benchmark
benchmark_searchAll a b =
    let
        x =
            a.empty |> fill exampleText a.insert

        y =
            b.empty |> fill exampleText b.insert
    in
    Benchmark.describe "searchAll"
        ([ [ "d", "c" ]
         , [ "de", "co" ]
         , [ "def", "con" ]
         , [ "defa", "cons" ]
         , [ "default", "conscience" ]
         , [ "defaults", "consciences" ]
         ]
            |> List.map
                (\keywords ->
                    Benchmark.compare (keywords |> String.join ", ")
                        a.name
                        (\() -> x |> a.searchAll keywords)
                        b.name
                        (\() -> y |> b.searchAll keywords)
                )
        )

module Tests exposing (suite)

import SearchStrings
import SearchTrie


suite : List ( String, ( Int, List String ) )
suite =
    [ ( "SearchStrings"
      , { empty = SearchStrings.empty
        , insert = SearchStrings.insert
        , search = SearchStrings.search
        }
            |> test
      )
    , ( "SearchTrie"
      , { empty = SearchTrie.empty
        , insert = SearchTrie.insert
        , search = SearchTrie.search
        }
            |> test
      )
    ]


type alias Search a =
    { empty : a
    , insert : Int -> String -> a -> a
    , search : String -> a -> List Int
    }


test : Search a -> ( Int, List String )
test t =
    let
        items =
            [ ( 1, "Jane Doe" )
            , ( 2, "Jan Dyk" )
            , ( 3, "Jon Doe" )
            ]

        x =
            List.foldl
                (\( id, str ) x1 ->
                    let
                        terms =
                            str |> String.toLower |> String.words
                    in
                    List.foldl
                        (\term x2 -> x2 |> t.insert id term)
                        x1
                        terms
                )
                t.empty
                items

        tests =
            [ ( "", [] )
            , ( "j", [ 1, 2, 3 ] )
            , ( "ja", [ 1, 2 ] )
            , ( "jan", [ 1, 2 ] )
            , ( "jane", [ 1 ] )
            , ( "janet", [] )
            , ( "a", [] )
            , ( "d", [ 1, 2, 3 ] )
            , ( "doe", [ 1, 3 ] )
            , ( "doer", [] )
            ]

        failures =
            List.filterMap
                (\( keyword, expected ) ->
                    let
                        result =
                            x |> t.search keyword
                    in
                    if expected == result then
                        Nothing

                    else
                        Just (( keyword, expected, result ) |> Debug.toString)
                )
                tests
    in
    ( List.length tests, failures )

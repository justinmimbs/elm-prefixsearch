module Tests exposing (suite)

import Types exposing (Search, exampleText, fill)


suite : List ( String, Int, List String )
suite =
    [ test Types.strings
    , test Types.trie
    , test Types.trieset
    ]


test : Search a -> ( String, Int, List String )
test t =
    let
        x =
            t.empty
                |> (t.insert 1 "jane" >> t.insert 1 "doe")
                |> (t.insert 2 "jan" >> t.insert 2 "dyk")
                |> (t.insert 3 "jon" >> t.insert 3 "doe")

        y =
            t.empty |> fill exampleText t.insert

        tests =
            [ ( x, "", [ 1, 2, 3 ] )
            , ( x, "j", [ 1, 2, 3 ] )
            , ( x, "ja", [ 1, 2 ] )
            , ( x, "jan", [ 1, 2 ] )
            , ( x, "jane", [ 1 ] )
            , ( x, "janet", [] )
            , ( x, "a", [] )
            , ( x, "d", [ 1, 2, 3 ] )
            , ( x, "doe", [ 1, 3 ] )
            , ( x, "doer", [] )

            --
            , ( y, "a", [ 3, 4, 8, 9, 11, 15, 22, 23, 25, 31, 36, 37, 44, 48, 50, 51, 53, 55, 56, 57, 60, 62, 66, 67, 68, 72, 74, 76, 86, 91, 92, 94, 102, 104, 105, 107, 108, 113, 118, 119, 121, 122, 125, 126, 127, 129, 130, 134, 136, 141, 144, 145, 148, 150 ] )
            , ( y, "am", [ 48, 136, 145 ] )
            , ( y, "amp", [ 48, 136 ] )
            , ( y, "amplifi", [ 48, 136 ] )
            , ( y, "amplific", [ 136 ] )
            , ( y, "amplification", [ 136 ] )
            , ( y, "amplifications", [] )
            ]

        failures =
            List.filterMap
                (\( struct, keyword, expected ) ->
                    let
                        result =
                            struct |> t.search keyword
                    in
                    if expected == result then
                        Nothing

                    else
                        Just (( keyword, expected, result ) |> Debug.toString)
                )
                tests
    in
    ( t.name, List.length tests, failures )

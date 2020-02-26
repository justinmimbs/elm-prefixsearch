module Tests exposing (suite)

import SortedList
import Types exposing (Index, exampleText, fill)


suite : List ( String, Int, List String )
suite =
    [ test Types.strings
    , test Types.trie
    , test Types.trieset
    , test Types.trielist
    , tests_sortedList
    ]


test : Index a -> ( String, Int, List String )
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
                    testEqual keyword (struct |> t.search keyword) expected
                )
                tests
    in
    ( t.name, List.length tests, failures )


tests_sortedList : ( String, Int, List String )
tests_sortedList =
    let
        fromList =
            List.foldl SortedList.insert []

        tests_insert =
            List.map
                (\( items, expected ) ->
                    testEqual "insert" (fromList items) expected
                )
                [ ( [], [] )
                , ( [ 1, 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ] )
                , ( [ 5, 1, 3, 4, 2 ], [ 1, 2, 3, 4, 5 ] )
                , ( [ 2, 3, 5, 1, 3, 3, 4, 1, 2, 1, 1 ], [ 1, 2, 3, 4, 5 ] )
                ]

        tests_union =
            List.concatMap
                (\( a, b, expected ) ->
                    testCommutativeMonoid "union" SortedList.union (fromList a) (fromList b) expected
                )
                [ ( [ 1, 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ] )
                , ( [ 1, 2, 3, 4, 5 ], [], [ 1, 2, 3, 4, 5 ] )
                , ( [ 1, 3, 4 ], [ 2, 5 ], [ 1, 2, 3, 4, 5 ] )
                , ( [ 1, 2, 3, 4 ], [ 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ] )
                , ( [ 1, 2, 3, 4, 5 ], [ 3 ], [ 1, 2, 3, 4, 5 ] )
                ]

        tests_intersect =
            List.concatMap
                (\( a, b, expected ) ->
                    testCommutativeMonoid "intersect" SortedList.intersect (fromList a) (fromList b) expected
                )
                [ ( [ 1, 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ], [ 1, 2, 3, 4, 5 ] )
                , ( [ 1, 2, 3, 4, 5 ], [], [] )
                , ( [ 1, 3, 4 ], [ 2, 5 ], [] )
                , ( [ 1, 2, 3, 4 ], [ 2, 3, 4, 5 ], [ 2, 3, 4 ] )
                , ( [ 1, 2, 3, 4, 5 ], [ 3 ], [ 3 ] )
                ]

        tests =
            tests_insert ++ tests_union ++ tests_intersect

        failures =
            tests |> List.filterMap identity
    in
    ( "SortedList", List.length tests, failures )


testEqual : String -> a -> a -> Maybe String
testEqual name left right =
    if left == right then
        Nothing

    else
        Just (( name, left, right ) |> Debug.toString)


testCommutativeMonoid : String -> (a -> a -> a) -> a -> a -> a -> List (Maybe String)
testCommutativeMonoid name f a b expected =
    [ testEqual name (f a b) expected
    , testEqual name (f b a) expected
    ]

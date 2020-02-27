module IndexStrings exposing (Index, empty, insert, search)

import Dict exposing (Dict)


type alias Index =
    Dict Int (List String)


empty : Index
empty =
    Dict.empty


insert : Int -> String -> Index -> Index
insert id term dict =
    dict
        |> Dict.update id
            (\maybeList ->
                case maybeList of
                    Just list ->
                        Just (term :: list)

                    Nothing ->
                        Just [ term ]
            )


search : String -> Index -> List Int
search keyword dict =
    if keyword == "" then
        dict |> Dict.keys

    else
        dict
            |> Dict.filter
                (\_ terms ->
                    terms
                        |> List.any
                            (\term ->
                                term |> String.startsWith keyword
                            )
                )
            |> Dict.keys

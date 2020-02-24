module SearchStrings exposing (Search, empty, insert, search)

import Dict exposing (Dict)


type alias Search =
    Dict Int (List String)


empty : Search
empty =
    Dict.empty


insert : Int -> String -> Search -> Search
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


search : String -> Search -> List Int
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

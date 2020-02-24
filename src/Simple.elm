module Simple exposing (Simple, empty, insert, search)

import Dict exposing (Dict)


type Simple
    = Simple (Dict Int (List String))


empty : Simple
empty =
    Simple Dict.empty


insert : Int -> String -> Simple -> Simple
insert id term (Simple dict) =
    Simple
        (dict
            |> Dict.update id
                (\maybeList ->
                    case maybeList of
                        Just list ->
                            Just (term :: list)

                        Nothing ->
                            Just [ term ]
                )
        )


search : String -> Simple -> List Int
search keyword (Simple dict) =
    if keyword == "" then
        []

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

module SearchStrings exposing (SearchStrings, empty, insert, search)

import Dict exposing (Dict)


type SearchStrings
    = SearchStrings (Dict Int (List String))


empty : SearchStrings
empty =
    SearchStrings Dict.empty


insert : Int -> String -> SearchStrings -> SearchStrings
insert id term (SearchStrings dict) =
    SearchStrings
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


search : String -> SearchStrings -> List Int
search keyword (SearchStrings dict) =
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

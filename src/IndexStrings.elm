module IndexStrings exposing (Index, empty, insert, search)

import Dict exposing (Dict)


type alias Index =
    Dict Int (List String)


empty : Index
empty =
    Dict.empty


insert : Int -> String -> Index -> Index
insert id string dict =
    case string |> toWords of
        [] ->
            dict

        terms ->
            dict
                |> Dict.update id
                    (\maybeList ->
                        case maybeList of
                            Just list ->
                                Just (terms ++ list)

                            Nothing ->
                                Just terms
                    )


search : String -> Index -> List Int
search string dict =
    case string |> toWords of
        [] ->
            dict |> Dict.keys

        keywords ->
            dict
                |> Dict.filter
                    (\_ terms ->
                        keywords
                            |> List.all
                                (\keyword ->
                                    terms
                                        |> List.any
                                            (\term ->
                                                term |> String.startsWith keyword
                                            )
                                )
                    )
                |> Dict.keys


toWords : String -> List String
toWords string =
    case string |> String.trim |> String.toLower of
        "" ->
            []

        nonempty ->
            String.words nonempty

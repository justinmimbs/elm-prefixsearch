module IndexTrie exposing (Index, empty, insert, search)

import Dict exposing (Dict)
import Trie exposing (Trie)


type alias Index =
    Dict Int Trie


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
                    (\maybeTrie ->
                        List.foldl
                            Trie.insert
                            (case maybeTrie of
                                Just trie ->
                                    trie

                                Nothing ->
                                    Trie.empty
                            )
                            terms
                            |> Just
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
                                    terms |> Trie.match keyword
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

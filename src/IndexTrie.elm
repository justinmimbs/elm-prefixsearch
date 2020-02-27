module IndexTrie exposing (Index, empty, insert, search)

import Dict exposing (Dict)
import Trie exposing (Trie)


type alias Index =
    Dict Int Trie


empty : Index
empty =
    Dict.empty


insert : Int -> String -> Index -> Index
insert id term dict =
    dict
        |> Dict.update id
            (\maybeTrie ->
                (case maybeTrie of
                    Just trie ->
                        trie

                    Nothing ->
                        Trie.empty
                )
                    |> Trie.insert term
                    |> Just
            )


search : String -> Index -> List Int
search keyword dict =
    if keyword == "" then
        dict |> Dict.keys

    else
        dict
            |> Dict.filter
                (\_ terms ->
                    terms |> Trie.match keyword
                )
            |> Dict.keys

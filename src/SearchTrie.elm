module SearchTrie exposing (Search, empty, insert, search)

import Dict exposing (Dict)
import Trie exposing (Trie)


type alias Search =
    Dict Int Trie


empty : Search
empty =
    Dict.empty


insert : Int -> String -> Search -> Search
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


search : String -> Search -> List Int
search keyword dict =
    if keyword == "" then
        []

    else
        dict
            |> Dict.filter
                (\_ terms ->
                    terms |> Trie.match keyword
                )
            |> Dict.keys

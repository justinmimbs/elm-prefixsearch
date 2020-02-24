module SearchTrie exposing (SearchTrie, empty, insert, search)

import Dict exposing (Dict)
import Trie exposing (Trie)


type SearchTrie
    = SearchTrie (Dict Int Trie)


empty : SearchTrie
empty =
    SearchTrie Dict.empty


insert : Int -> String -> SearchTrie -> SearchTrie
insert id term (SearchTrie dict) =
    SearchTrie
        (dict
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
        )


search : String -> SearchTrie -> List Int
search keyword (SearchTrie dict) =
    if keyword == "" then
        []

    else
        dict
            |> Dict.filter
                (\_ terms ->
                    terms |> Trie.match keyword
                )
            |> Dict.keys

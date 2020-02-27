module TextIndex.V2ScanTrie exposing (TextIndex, empty, insert, search, searchAll)

import Dict exposing (Dict)
import TextIndex.Trie as Trie exposing (Trie)


type alias TextIndex =
    Dict Int Trie


empty : TextIndex
empty =
    Dict.empty


insert : Int -> String -> TextIndex -> TextIndex
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


search : String -> TextIndex -> List Int
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


searchAll : List String -> TextIndex -> List Int
searchAll keywords dict =
    Dict.foldr
        (\id terms result ->
            if
                keywords
                    |> List.all
                        (\keyword ->
                            terms |> Trie.match keyword
                        )
            then
                id :: result

            else
                result
        )
        []
        dict

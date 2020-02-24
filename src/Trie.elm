module Trie exposing (Trie, empty, insert, match)

import Dict exposing (Dict)


type Trie
    = Trie (Dict Char Trie)


empty : Trie
empty =
    Trie Dict.empty


insert : String -> Trie -> Trie
insert term trie =
    insertHelp (term |> String.toList) trie


insertHelp : List Char -> Trie -> Trie
insertHelp chars ((Trie dict) as trie) =
    case chars of
        [] ->
            trie

        char :: rest ->
            Trie
                (dict
                    |> Dict.update char
                        (\maybeNext ->
                            (case maybeNext of
                                Just next ->
                                    next

                                Nothing ->
                                    empty
                            )
                                |> insertHelp rest
                                |> Just
                        )
                )


match : String -> Trie -> Bool
match keyword trie =
    matchHelp (keyword |> String.toList) trie


matchHelp : List Char -> Trie -> Bool
matchHelp chars (Trie dict) =
    case chars of
        [] ->
            True

        char :: rest ->
            case dict |> Dict.get char of
                Just next ->
                    matchHelp rest next

                Nothing ->
                    False

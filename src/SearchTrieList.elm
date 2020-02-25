module SearchTrieList exposing (Search, empty, insert, search)

import Dict exposing (Dict)


type Search
    = Search
        { ends : List Int
        , cont : Dict Char Search
        }


empty : Search
empty =
    Search
        { ends = []
        , cont = Dict.empty
        }


insert : Int -> String -> Search -> Search
insert id term x =
    insertHelp id (term |> String.toList) x


insertHelp : Int -> List Char -> Search -> Search
insertHelp id chars (Search { ends, cont }) =
    case chars of
        [] ->
            Search
                { ends = ends |> listInsert id
                , cont = cont
                }

        char :: rest ->
            Search
                { ends = ends
                , cont =
                    cont
                        |> Dict.update char
                            (\maybeNext ->
                                (case maybeNext of
                                    Just next ->
                                        next

                                    Nothing ->
                                        empty
                                )
                                    |> insertHelp id rest
                                    |> Just
                            )
                }


search : String -> Search -> List Int
search keyword x =
    seek (keyword |> String.toList) x
        |> collect


seek : List Char -> Search -> Search
seek chars ((Search { ends, cont }) as x) =
    case chars of
        [] ->
            x

        char :: rest ->
            case cont |> Dict.get char of
                Just next ->
                    seek rest next

                Nothing ->
                    empty


collect : Search -> List Int
collect (Search { ends, cont }) =
    Dict.foldl
        (\_ next result ->
            listMerge (collect next) result
        )
        ends
        cont



-- sorted, distinct list


listInsert : comparable -> List comparable -> List comparable
listInsert x list =
    listInsertHelp list [] x list


listInsertHelp : List comparable -> List comparable -> comparable -> List comparable -> List comparable
listInsertHelp original rev x list =
    case list of
        [] ->
            listPour rev (x :: [])

        head :: rest ->
            case compare x head of
                LT ->
                    listPour rev (x :: list)

                EQ ->
                    original

                GT ->
                    listInsertHelp original (head :: rev) x rest


listPour : List a -> List a -> List a
listPour source target =
    case source of
        [] ->
            target

        head :: rest ->
            listPour rest (head :: target)


listMerge : List comparable -> List comparable -> List comparable
listMerge listA listB =
    listMergeHelp [] listA listB


listMergeHelp : List comparable -> List comparable -> List comparable -> List comparable
listMergeHelp rev listA listB =
    case listA of
        [] ->
            listPour rev listB

        a :: restA ->
            case listB of
                [] ->
                    listPour rev listA

                b :: restB ->
                    case compare a b of
                        LT ->
                            listMergeHelp (a :: rev) restA listB

                        EQ ->
                            listMergeHelp (a :: rev) restA restB

                        GT ->
                            listMergeHelp (b :: rev) listA restB

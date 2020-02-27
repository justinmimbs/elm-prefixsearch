module IndexTrieSet exposing (Index, empty, insert, search)

import Dict exposing (Dict)
import Set exposing (Set)


type Index
    = Index
        { ends : Set Int
        , cont : Dict Char Index
        }


empty : Index
empty =
    Index
        { ends = Set.empty
        , cont = Dict.empty
        }


insert : Int -> String -> Index -> Index
insert id term x =
    insertHelp id (term |> String.toList) x


insertHelp : Int -> List Char -> Index -> Index
insertHelp id chars (Index { ends, cont }) =
    case chars of
        [] ->
            Index
                { ends = ends |> Set.insert id
                , cont = cont
                }

        char :: rest ->
            Index
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


search : String -> Index -> List Int
search keyword x =
    seek (keyword |> String.toList) x
        |> collect
        |> Set.toList


seek : List Char -> Index -> Index
seek chars ((Index { ends, cont }) as x) =
    case chars of
        [] ->
            x

        char :: rest ->
            case cont |> Dict.get char of
                Just next ->
                    seek rest next

                Nothing ->
                    empty


collect : Index -> Set Int
collect (Index { ends, cont }) =
    Dict.foldl
        (\_ next result ->
            Set.union (collect next) result
        )
        ends
        cont

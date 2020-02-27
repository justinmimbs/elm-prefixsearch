module TextIndex.V3SeekSet exposing (TextIndex, empty, insert, search, searchAll)

import Dict exposing (Dict)
import Set exposing (Set)


type TextIndex
    = TextIndex
        { ends : Set Int
        , cont : Dict Char TextIndex
        }


empty : TextIndex
empty =
    TextIndex
        { ends = Set.empty
        , cont = Dict.empty
        }


insert : Int -> String -> TextIndex -> TextIndex
insert id term x =
    insertHelp id (term |> String.toList) x


insertHelp : Int -> List Char -> TextIndex -> TextIndex
insertHelp id chars (TextIndex { ends, cont }) =
    case chars of
        [] ->
            TextIndex
                { ends = ends |> Set.insert id
                , cont = cont
                }

        char :: rest ->
            TextIndex
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


search_ : String -> TextIndex -> Set Int
search_ keyword x =
    seek (keyword |> String.toList) x
        |> collect


search : String -> TextIndex -> List Int
search keyword x =
    search_ keyword x |> Set.toList


searchAll : List String -> TextIndex -> List Int
searchAll keywords x =
    (case keywords of
        [] ->
            collect x

        first :: rest ->
            List.foldl
                (\next result ->
                    search_ next x |> Set.intersect result
                )
                (search_ first x)
                rest
    )
        |> Set.toList


seek : List Char -> TextIndex -> TextIndex
seek chars ((TextIndex { ends, cont }) as x) =
    case chars of
        [] ->
            x

        char :: rest ->
            case cont |> Dict.get char of
                Just next ->
                    seek rest next

                Nothing ->
                    empty


collect : TextIndex -> Set Int
collect (TextIndex { ends, cont }) =
    Dict.foldl
        (\_ next result ->
            Set.union (collect next) result
        )
        ends
        cont

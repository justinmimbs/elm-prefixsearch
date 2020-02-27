module TextIndex exposing (TextIndex, empty, insert, search, searchAll)

import Dict exposing (Dict)
import SortedList


type TextIndex
    = TextIndex
        { ends : List Int
        , cont : Dict Char TextIndex
        }


empty : TextIndex
empty =
    TextIndex
        { ends = []
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
                { ends = ends |> SortedList.insert id
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


search : String -> TextIndex -> List Int
search keyword x =
    seek (keyword |> String.toList) x
        |> collect


searchAll : List String -> TextIndex -> List Int
searchAll keywords x =
    case keywords of
        [] ->
            collect x

        first :: rest ->
            List.foldl
                (\next result ->
                    search next x |> SortedList.intersect result
                )
                (search first x)
                rest


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


collect : TextIndex -> List Int
collect (TextIndex { ends, cont }) =
    Dict.foldl
        (\_ next result ->
            case result of
                [] ->
                    collect next

                _ ->
                    collect next |> SortedList.union result
        )
        ends
        cont

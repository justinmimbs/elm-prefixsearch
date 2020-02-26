module SearchTrieList exposing (Search, empty, insert, search)

import Dict exposing (Dict)
import SortedList


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
                { ends = ends |> SortedList.insert id
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
            SortedList.union (collect next) result
        )
        ends
        cont

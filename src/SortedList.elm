module SortedList exposing (insert, intersect, union)

{-| A sorted, distinct list.
-}


insert : comparable -> List comparable -> List comparable
insert x list =
    insertHelp list [] x list


insertHelp : List comparable -> List comparable -> comparable -> List comparable -> List comparable
insertHelp original rev x list =
    case list of
        [] ->
            drain rev (x :: [])

        head :: rest ->
            case compare x head of
                LT ->
                    drain rev (x :: list)

                EQ ->
                    original

                GT ->
                    insertHelp original (head :: rev) x rest


drain : List a -> List a -> List a
drain source target =
    case source of
        [] ->
            target

        head :: rest ->
            drain rest (head :: target)


union : List comparable -> List comparable -> List comparable
union listA listB =
    unionHelp [] listA listB


unionHelp : List comparable -> List comparable -> List comparable -> List comparable
unionHelp rev listA listB =
    case listA of
        [] ->
            drain rev listB

        a :: restA ->
            case listB of
                [] ->
                    drain rev listA

                b :: restB ->
                    case compare a b of
                        LT ->
                            unionHelp (a :: rev) restA listB

                        EQ ->
                            unionHelp (a :: rev) restA restB

                        GT ->
                            unionHelp (b :: rev) listA restB


intersect : List comparable -> List comparable -> List comparable
intersect listA listB =
    intersectHelp [] listA listB


intersectHelp : List comparable -> List comparable -> List comparable -> List comparable
intersectHelp rev listA listB =
    case listA of
        [] ->
            List.reverse rev

        a :: restA ->
            case listB of
                [] ->
                    List.reverse rev

                b :: restB ->
                    case compare a b of
                        LT ->
                            intersectHelp rev restA listB

                        EQ ->
                            intersectHelp (a :: rev) restA restB

                        GT ->
                            intersectHelp rev listA restB

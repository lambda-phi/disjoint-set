module DisjointSet exposing (DisjointSet, empty, union, find, fromList, toList)

{-| A disjoint set implementation with path compression.

@docs DisjointSet, empty, union, find, fromList, toList

-}

-- ℹ️ Since not every type is `comparable` in Elm (like union types),
-- we have to use `List (a, a)` instead of `Dict a a`.
-- Searches are O(n) instead of O(1), but we can use any type.


{-| A data structure that stores non-overlapping sets.
-}
type DisjointSet a
    = DisjointSet (List ( a, a ))


{-| Creats an empty disjoint set.
-}
empty : DisjointSet a
empty =
    DisjointSet []


{-| Unifies two elements. If an element is not part of any set, it is created.

The root element is always the first element that was added.

This operation does path compression as an optimization.

    union "a" "a" empty |> toList --> [ ("a", "a") ]
    union "a" "b" empty |> toList --> [ ("a", "a"), ("b", "a") ]

    empty
        |> union "a" "b"
        |> union "b" "c"
        |> toList
    --> [ ("a", "a"), ("b", "a"), ("c", "a") ]

    empty
        |> union "a" "b"
        |> union "x" "y"
        |> union "b" "y"
        |> toList
    --> [ ("a", "a"), ("b", "a"), ("x", "a"), ("y", "a") ]

-}
union : a -> a -> DisjointSet a -> DisjointSet a
union item1 item2 (DisjointSet pairs) =
    let
        path1 =
            path item1 pairs |> Maybe.withDefault [ item1 ]

        root =
            List.head path1 |> Maybe.withDefault item1

        compressed =
            List.map2 (\x y -> [ ( x, root ), ( y, root ) ])
                path1
                (path item2 pairs |> Maybe.withDefault [ item2 ])
                |> List.concat
    in
    DisjointSet (merge compressed pairs)


{-| Finds the root element from a given element.

    import Dict

    set : DisjointSet String
    set =
        empty
            |> union "a" "b"
            |> union "b" "c"
            |> union "c" "d"
            |> union "e" "e"

    find "a" set --> Just "a"
    find "b" set --> Just "a"
    find "c" set --> Just "a"
    find "d" set --> Just "a"
    find "e" set --> Just "e"
    find "x" set --> Nothing

-}
find : a -> DisjointSet a -> Maybe a
find item (DisjointSet pairs) =
    path item pairs
        |> Maybe.andThen List.head


{-| Creates a disjoint set from a list of `(element, equivalent)` pairs.
-}
fromList : List ( a, a ) -> DisjointSet a
fromList list =
    List.foldl (\( x, y ) -> union x y) empty list


{-| Creates a list of `(element, equivalent)` pairs from a disjoint set.
-}
toList : DisjointSet a -> List ( a, a )
toList (DisjointSet list) =
    list


path : a -> List ( a, a ) -> Maybe (List a)
path item pairs =
    let
        path_ : a -> List a -> Maybe (List a)
        path_ head tail =
            get item pairs
                |> Maybe.andThen
                    (\parent ->
                        if head == parent then
                            Just (head :: tail)

                        else
                            path_ parent (head :: tail)
                    )
    in
    path_ item []


get : a -> List ( a, a ) -> Maybe a
get x pairs =
    case pairs of
        ( head, parent ) :: tail ->
            if x == head then
                Just parent

            else
                get x tail

        [] ->
            Nothing


merge : List ( a, a ) -> List ( a, a ) -> List ( a, a )
merge pairs1 pairs2 =
    List.foldl addOrReplace pairs2 pairs1


addOrReplace : ( a, a ) -> List ( a, a ) -> List ( a, a )
addOrReplace ( x, y ) pairs =
    case pairs of
        ( head, parent ) :: tail ->
            if x == head then
                ( x, y ) :: tail

            else
                ( head, parent ) :: addOrReplace ( x, y ) tail

        [] ->
            [ ( x, y ) ]

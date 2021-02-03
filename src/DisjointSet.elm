module DisjointSet exposing
    ( DisjointSet, empty
    , union, add
    , find, has, items
    , toList, fromList
    )

{-| A disjoint set implementation with path compression.

@docs DisjointSet, empty


# Adding elements

@docs union, add


# Querying for elements

@docs find, has, items


# List operations

@docs toList, fromList

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
union x1 x2 (DisjointSet pairs) =
    let
        path1 =
            path x1 pairs |> Maybe.withDefault [ x1 ]

        root =
            List.head path1 |> Maybe.withDefault x1

        compressed =
            List.map2 (\x y -> [ ( x, root ), ( y, root ) ])
                path1
                (path x2 pairs |> Maybe.withDefault [ x2 ])
                |> List.concat
    in
    DisjointSet (merge compressed pairs)


{-| Adds a list of elements into their own set.

    add [ "a" ] empty |> toList --> [ ("a", "a") ]
    add [ "a", "b" ] empty |> toList --> [ ("a", "a"), ("b", "b") ]

    empty
        |> add [ "a" ]
        |> add [ "b", "c" ]
        |> toList
    --> [ ("a", "a"), ("b", "b"), ("c", "c") ]

-}
add : List a -> DisjointSet a -> DisjointSet a
add xs set =
    List.foldl (\x -> union x x) set xs


{-| Finds the root element from a given element if it exists.

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
find x (DisjointSet pairs) =
    path x pairs
        |> Maybe.andThen List.head


{-| Checks whether or not the disjoint set contains an element.

    set : DisjointSet String
    set =
        add [ "a" ] empty

    has "a" set --> True
    has "b" set --> False

-}
has : a -> DisjointSet a -> Bool
has x set =
    case find x set of
        Just _ ->
            True

        Nothing ->
            False


{-| Returns a list of all the elements contained in the disjoint set.

    items empty --> []
    items (add [ "a", "b", "c" ] empty) --> [ "a", "b", "c" ]

    empty
        |> union "a" "b"
        |> union "c" "d"
        |> items
    --> [ "a", "b", "c", "d" ]

-}
items : DisjointSet a -> List a
items (DisjointSet pairs) =
    List.map Tuple.first pairs



-- List operations


{-| Creates a list of `(element, equivalent)` pairs from a disjoint set.
-}
toList : DisjointSet a -> List ( a, a )
toList (DisjointSet list) =
    list


{-| Creates a disjoint set from a list of `(element, equivalent)` pairs.
-}
fromList : List ( a, a ) -> DisjointSet a
fromList list =
    List.foldl (\( x, y ) -> union x y) empty list



-- Local helper functions


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

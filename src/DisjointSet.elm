module DisjointSet exposing (DisjointSet, empty, union, find, fromDict, toDict)

{-| A disjoint set implementation with path compression.

@docs DisjointSet, empty, union, find, fromDict, toDict

-}

import Dict exposing (Dict)


{-| The `DisjointSet` type definition.
-}
type DisjointSet a
    = DisjointSet (Dict a a)


{-| Creats an empty disjoint set.
-}
empty : DisjointSet a
empty =
    DisjointSet Dict.empty


{-| Unifies two elements. If an element is not part of any set, it is created.

The root element is always the first element that was added.

This operation does path compression as an optimization.

    import Dict

    union "a" "a" empty |> toDict --> Dict.fromList [ ("a", "a") ]
    union "a" "b" empty |> toDict --> Dict.fromList [ ("a", "a"), ("b", "a") ]

    empty
        |> union "a" "b"
        |> union "b" "c"
        |> toDict
    --> Dict.fromList [ ("a", "a"), ("b", "a"), ("c", "a") ]

    empty
        |> union "a" "b"
        |> union "x" "y"
        |> union "b" "y"
        |> toDict
    --> Dict.fromList [ ("a", "a"), ("b", "a"), ("x", "a"), ("y", "a") ]

-}
union : comparable -> comparable -> DisjointSet comparable -> DisjointSet comparable
union item1 item2 set =
    let
        dict =
            toDict set

        path : comparable -> List comparable -> List comparable
        path item tail =
            Dict.get item dict
                |> Maybe.map
                    (\parent ->
                        if item == parent then
                            item :: tail

                        else
                            path parent (item :: tail)
                    )
                |> Maybe.withDefault [ item ]

        path1 =
            path item1 []

        root =
            List.head path1 |> Maybe.withDefault item1

        compressed =
            List.map2 (\x y -> [ ( x, root ), ( y, root ) ])
                path1
                (path item2 [])
                |> List.concat
                |> Dict.fromList
    in
    DisjointSet (Dict.union compressed dict)


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
find : comparable -> DisjointSet comparable -> Maybe comparable
find item set =
    let
        dict =
            toDict set

        find_ : comparable -> Maybe comparable
        find_ x =
            Dict.get x dict
                |> Maybe.andThen
                    (\root ->
                        if x == root then
                            Just root

                        else
                            find_ root
                    )
    in
    find_ item


{-| Creates a disjoint set from a dictionary of `(element, equivalent)` pairs.
-}
fromDict : Dict comparable comparable -> DisjointSet comparable
fromDict dict =
    Dict.foldl union empty dict


{-| Creates a dictionary of `(element, equivalent)` pairs from a disjoint set.
-}
toDict : DisjointSet comparable -> Dict comparable comparable
toDict set =
    case set of
        DisjointSet dict ->
            dict

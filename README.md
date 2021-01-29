# DisjointSet

A [disjoint set](https://en.wikipedia.org/wiki/Disjoint-set_data_structure),
also called union-find or merge-find, is a data structure that stores non-overlapping sets.

```elm
import Dict
import DisjointSet exposing (DisjointSet, empty, find, union)

-- The preferred way of creating a DisjointSet is with `union`.
abc : DisjointSet String
abc =
    empty
        |> union "a" "b"
        |> union "b" "c"

-- You can also create if from a `List`.
xyz : DisjointSet String
xyz =
    DisjointSet.fromList
        [ ("x", "x")
        , ("y", "x")
        , ("z", "y")
        ]

-- Finding elements is easy.
find "a" abc --> Just "a"
find "c" abc --> Just "a"
find "x" abc --> Nothing

-- Or you can convert it back to a `Dict`.
DisjointSet.toList abc --> [ ("a", "a"), ("b", "a"), ("c","a") ]
```

# DisjointSet

A [disjoint set](https://en.wikipedia.org/wiki/Disjoint-set_data_structure),
also called union-find or merge-find, is a data structure that stores non-overlapping sets.

```elm
import DisjointSet exposing (DisjointSet, empty, find, fromList, toList, union)

-- `empty` creates a new empty `DisjointSet`.
-- `union` unifies two elements, and creates the elements if they don't exist.
abc : DisjointSet String
abc =
    empty
        |> union "a" "b"
        |> union "b" "c"

-- You can convert it to a `List` of (element, root) pairs.
toList abc --> [ ("a", "a"), ("b", "a"), ("c","a") ]

-- `add` creates new elements into their own set.
add [ "a" ] empty |> toList --> [ ("a", "a") ]
add [ "a", "b" ] empty |> toList --> [ ("a", "a"), ("b", "b") ]

-- `find` gets you the "root" element for a set.
find "a" abc --> Just "a"
find "c" abc --> Just "a"
find "x" abc --> Nothing

-- `has` helps you know if the disjoint set contains an element.
has "a" abc --> True
has "c" abc --> True
has "x" abc --> False

-- `items` gives you a list of all the elements in the disjoint set.
items abc --> [ "a", "b", "c" ]

-- You can also create a `DisjointSet` from a `List` of pairs,
-- but it's usually easier using `union` or `add`.
xyz : DisjointSet String
xyz =
    fromList
        [ ("x", "x")
        , ("y", "x")
        , ("z", "y")
        ]
```

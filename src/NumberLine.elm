module NumberLine exposing
    ( NumberLine
    , empty, singleton, fromList
    , pushStart, pushEnd
    , insertLeftOf, insertRightOf
    , toListFromLeftToRight, toListFromRightToLeft
    , getItemInMiddle
    , size
    )

{-| I think this might be similar to a "doubly linked list" or a
"double-ended queue"– but I didn't want to call it something
misleading! Here's how this data structure works:

    myNumberLine : NumberLine Char
    myNumberLine =
        (empty -- .......................... <------------->)
            |> (pushStart 'C' -- ........... <------C------>)
            |> (insertLeftOf 'C' 'A' -- .... <----A---C---->)
            |> (insertRightOf 'A' 'B' -- ... <--A---B---C-->)

The original need for this data structure was ranking a big
list of choices (from least to most favorite) by inserting
them one at a time.

@docs NumberLine
@docs empty, singleton, fromList
@docs pushStart, pushEnd
@docs insertLeftOf, insertRightOf
@docs toListFromLeftToRight, toListFromRightToLeft
@docs getItemInMiddle
@docs size


### Implementation note:

This data structure works by tracking the `first` and `last` items on the number line,
and then keeps track of two dictionaries: a `left` lookup and a `right` lookup.

For a given value, you use `left` to know which item is to the left of it.
For a given value, you use `right` to know which item is to the right of it.

This makes relative inserts fast, and allows number line traversal
in either direction, but means that the values must be `comparable`
because they will be used as `Dict` keys.

-}

import Dict exposing (Dict)


{-| The goal for this data structure is to make it
easy to add items along a "number line", and read it
in either direction.
-}
type NumberLine comparable
    = NumberLine
        { first : Maybe comparable
        , left : Dict comparable comparable
        , right : Dict comparable comparable
        , last : Maybe comparable
        }


{-| Creates an empty number line.

    -- <------------->
    empty

-}
empty : NumberLine comparable
empty =
    NumberLine
        { first = Nothing
        , left = Dict.empty
        , right = Dict.empty
        , last = Nothing
        }


{-| Creates a number line with a single value.

    -- <------A------>
    singleton 'A'

-}
singleton : comparable -> NumberLine comparable
singleton value =
    empty
        |> pushStart value


{-| Creates a number line from an existing list.

    -- <--A---B---C-->
    fromList [ 'A', 'B', 'C' ]

-}
fromList : List comparable -> NumberLine comparable
fromList list =
    case list of
        [] ->
            empty

        firstItem :: [] ->
            singleton firstItem

        firstItem :: _ ->
            let
                reversed : List comparable
                reversed =
                    List.reverse list
            in
            NumberLine
                { first = Just firstItem
                , left = toLookupDict reversed
                , right = toLookupDict list
                , last = List.head reversed
                }


{-| Adds a value to the start of a number line

    -- <--A---B---C-->
    empty
        |> pushStart 'C'
        |> pushStart 'B'
        |> pushStart 'A'

-}
pushStart : comparable -> NumberLine comparable -> NumberLine comparable
pushStart item (NumberLine line) =
    case line.first of
        Nothing ->
            NumberLine { line | first = Just item, last = Just item }

        Just oldFirst ->
            NumberLine
                { line
                    | first = Just item
                    , left = Dict.insert oldFirst item line.left
                    , right = Dict.insert item oldFirst line.right
                }


{-| Adds a value to the end of a number line

    -- <--D---E---F-->
    empty
        |> pushEnd 'D'
        |> pushEnd 'E'
        |> pushEnd 'F'

-}
pushEnd : comparable -> NumberLine comparable -> NumberLine comparable
pushEnd item (NumberLine line) =
    case line.last of
        Nothing ->
            NumberLine
                { line | first = Just item, last = Just item }

        Just oldLast ->
            NumberLine
                { line
                    | last = Just item
                    , left = Dict.insert item oldLast line.left
                    , right = Dict.insert oldLast item line.right
                }


{-| Adds a value to the left of another value.

    -- <--A---B---C-->
    fromList [ 'A', 'C' ]
        |> insertLeftOf 'C' 'B'

**Note:** If the target value is not found, no
changes are made to the list.

    -- <----A---C---->
    fromList [ 'A', 'C' ]
        |> insertLeftOf 'D' 'B'

-}
insertLeftOf : comparable -> comparable -> NumberLine comparable -> NumberLine comparable
insertLeftOf itemToFind item ((NumberLine line) as nl) =
    if isEmpty nl then
        NumberLine { line | first = Just item, last = Just item }

    else if Just itemToFind == line.first then
        pushStart item nl

    else
        case Dict.get itemToFind line.left of
            Just leftOfTarget ->
                NumberLine
                    { line
                        | left =
                            line.left
                                |> Dict.insert itemToFind item
                                |> Dict.insert item leftOfTarget
                        , right =
                            line.right
                                |> Dict.insert leftOfTarget item
                                |> Dict.insert item itemToFind
                    }

            Nothing ->
                nl


{-| Adds a value to the left of another value.

    -- <--A---B---C-->
    fromList [ 'A', 'C' ]
        |> insertRightOf 'A' 'B'

**Note:** If the target value is not found, no
changes are made to the list.

    -- <----A---C---->
    fromList [ 'A', 'C' ]
        |> insertRightOf 'D' 'B'

-}
insertRightOf : comparable -> comparable -> NumberLine comparable -> NumberLine comparable
insertRightOf itemToFind item ((NumberLine line) as nl) =
    if isEmpty nl then
        NumberLine { line | first = Just item, last = Just item }

    else if Just itemToFind == line.last then
        pushEnd item nl

    else
        case Dict.get itemToFind line.right of
            Just rightOfTarget ->
                NumberLine
                    { line
                        | right =
                            line.right
                                |> Dict.insert itemToFind item
                                |> Dict.insert item rightOfTarget
                        , left =
                            line.left
                                |> Dict.insert rightOfTarget item
                                |> Dict.insert item itemToFind
                    }

            Nothing ->
                nl


{-| Returns `True` if there are no number items in the line:

    -- <------------->
    isEmpty (fromList [])
        == True

    -- <--A---B---C-->
    isEmpty (fromList [ 'A', 'B', 'C' ])
        == False

-}
isEmpty : NumberLine comparable -> Bool
isEmpty (NumberLine line) =
    line.first == Nothing


{-| Returns the number of items in the line:

    -- <--A---B---C-->
    size (fromList [ 'A', 'B', 'C' ])
        == 3

-}
size : NumberLine comparable -> Int
size (NumberLine line) =
    if line.first == Nothing then
        0

    else
        Dict.size line.left + 1


{-| Returns a list of items from left to right:

    -- <--A---B---C-->
    toListFromLeftToRight (fromList [ 'A', 'B', 'C' ])
        == [ 'A', 'B', 'C' ]

-}
toListFromLeftToRight : NumberLine comparable -> List comparable
toListFromLeftToRight (NumberLine line) =
    case line.first of
        Nothing ->
            []

        Just first ->
            case Dict.get first line.right of
                Just next ->
                    first :: toListFromDict next line.right

                Nothing ->
                    first :: []


{-| Returns a list of items from right to left:

    -- <--A---B---C-->
    toListFromRightToLeft (fromList [ 'A', 'B', 'C' ])
        == [ 'C', 'B', 'A' ]

-}
toListFromRightToLeft : NumberLine comparable -> List comparable
toListFromRightToLeft (NumberLine line) =
    case line.last of
        Nothing ->
            []

        Just last ->
            case Dict.get last line.left of
                Just next ->
                    last :: toListFromDict next line.left

                Nothing ->
                    last :: []


{-| Given optional bounds, this returns the item closest to
the middle of the range.

    fromList [ 'A', 'B', 'C', 'D', 'E' ]
        |> getItemInMiddle Nothing Nothing
        == 'C'

    fromList [ 'A', 'B', 'C', 'D', 'E' ]
        |> getItemInMiddle (Just 'C') Nothing
        == 'D'

    fromList [ 'A', 'B', 'C', 'D', 'E' ]
        |> getItemInMiddle Nothing (Just 'C')
        == 'B'

If no items are found within that range, it returns `Nothing`:

    fromList [ 'A', 'B', 'C', 'D', 'E' ]
        |> getItemInMiddle (Just 'C') (Just 'D')
        == Nothing

In the event of two middle items, it picks the one more
on the right.

    fromList [ 'A', 'B', 'C', 'D' ]
        |> getItemInMiddle (Just 'A') (Just 'D')
        == Just 'C'

If either bound provided isn't in the number line,
nothing will be returned.

    fromList [ 'A', 'B', 'C', 'D' ]
        |> getItemInMiddle (Just 'X') Nothing
        == Nothing

    fromList [ 'A', 'B', 'C', 'D' ]
        |> getItemInMiddle Nothing (Just 'X')
        == Nothing

-}
getItemInMiddle : Maybe comparable -> Maybe comparable -> NumberLine comparable -> Maybe comparable
getItemInMiddle maybeRightOf maybeLeftOf ((NumberLine line) as nl) =
    case ( maybeRightOf, maybeLeftOf ) of
        ( Nothing, Nothing ) ->
            findMiddle (toListFromLeftToRight nl)

        ( Nothing, Just leftOf ) ->
            findMiddle (toListFromDict leftOf line.left |> List.drop 1)

        ( Just rightOf, Nothing ) ->
            findMiddle (toListFromDict rightOf line.right |> List.drop 1)

        ( Just rightOf, Just leftOf ) ->
            findMiddle (toListFromDictStopAt leftOf rightOf line.right |> List.drop 1)



-- INTERNAL UTILITIES


toListFromDict : comparable -> Dict comparable comparable -> List comparable
toListFromDict item dict =
    case Dict.get item dict of
        Just next ->
            item :: toListFromDict next dict

        Nothing ->
            item :: []


toListFromDictStopAt : comparable -> comparable -> Dict comparable comparable -> List comparable
toListFromDictStopAt stop item dict =
    if Dict.member stop dict then
        case Dict.get item dict of
            Just next ->
                if next == stop then
                    item :: []

                else
                    item :: toListFromDictStopAt stop next dict

            Nothing ->
                item :: []

    else
        []


findMiddle : List comparable -> Maybe comparable
findMiddle list =
    list
        |> List.drop (List.length list // 2)
        |> List.head


toLookupDict : List comparable -> Dict comparable comparable
toLookupDict items =
    List.drop 1 items
        |> List.map2 Tuple.pair items
        |> Dict.fromList


isSingleton : NumberLine comparable -> Bool
isSingleton (NumberLine line) =
    line.first /= Nothing && Dict.isEmpty line.left

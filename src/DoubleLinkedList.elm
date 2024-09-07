module DoubleLinkedList exposing
    ( DoubleLinkedList, empty
    , pushStart, pushEnd
    , insertLeftOf, insertRightOf
    , size, toList
    , getItemBetween
    )

{-|

@docs DoubleLinkedList, empty
@docs pushStart, pushEnd
@docs insertLeftOf, insertRightOf
@docs size, toList
@docs getItemBetween
@docs toProgress

-}

import Dict exposing (Dict)


type alias DoubleLinkedList comparable =
    { first : Maybe comparable
    , left : Dict comparable comparable
    , right : Dict comparable comparable
    , last : Maybe comparable
    }


empty : DoubleLinkedList comparable
empty =
    { first = Nothing
    , left = Dict.empty
    , right = Dict.empty
    , last = Nothing
    }


pushStart : comparable -> DoubleLinkedList comparable -> DoubleLinkedList comparable
pushStart item dll =
    case dll.first of
        Nothing ->
            { dll | first = Just item, last = Just item }

        Just oldFirst ->
            { dll
                | first = Just item
                , left = Dict.insert oldFirst item dll.left
                , right = Dict.insert item oldFirst dll.right
            }


pushEnd : comparable -> DoubleLinkedList comparable -> DoubleLinkedList comparable
pushEnd item dll =
    case dll.last of
        Nothing ->
            { dll | first = Just item, last = Just item }

        Just oldLast ->
            { dll
                | last = Just item
                , left = Dict.insert item oldLast dll.left
                , right = Dict.insert oldLast item dll.right
            }


insertLeftOf : comparable -> comparable -> DoubleLinkedList comparable -> DoubleLinkedList comparable
insertLeftOf itemToFind item dll =
    if isEmpty dll then
        { dll | first = Just item, last = Just item }

    else if Just itemToFind == dll.first then
        pushStart item dll

    else
        case Dict.get itemToFind dll.left of
            Just leftOfTarget ->
                { dll
                    | left =
                        dll.left
                            |> Dict.insert itemToFind item
                            |> Dict.insert item leftOfTarget
                    , right =
                        dll.right
                            |> Dict.insert leftOfTarget item
                            |> Dict.insert item itemToFind
                }

            Nothing ->
                dll


insertRightOf : comparable -> comparable -> DoubleLinkedList comparable -> DoubleLinkedList comparable
insertRightOf itemToFind item dll =
    if isEmpty dll then
        { dll | first = Just item, last = Just item }

    else if Just itemToFind == dll.last then
        pushEnd item dll

    else
        case Dict.get itemToFind dll.right of
            Just rightOfTarget ->
                { dll
                    | right =
                        dll.right
                            |> Dict.insert itemToFind item
                            |> Dict.insert item rightOfTarget
                    , left =
                        dll.left
                            |> Dict.insert rightOfTarget item
                            |> Dict.insert item itemToFind
                }

            Nothing ->
                dll


size : DoubleLinkedList comparable -> Int
size dll =
    if isEmpty dll then
        0

    else
        Dict.size dll.left + 1


toList : DoubleLinkedList comparable -> List comparable
toList dll =
    case dll.last of
        Nothing ->
            []

        Just last ->
            case Dict.get last dll.left of
                Just next ->
                    last :: toListFromDict next dll.left

                Nothing ->
                    last :: []


toListFromDict : comparable -> Dict comparable comparable -> List comparable
toListFromDict item dict =
    case Dict.get item dict of
        Just next ->
            item :: toListFromDict next dict

        Nothing ->
            item :: []


toListFromDictStopAt : comparable -> comparable -> Dict comparable comparable -> List comparable
toListFromDictStopAt stop item dict =
    case Dict.get item dict of
        Just next ->
            if next == stop then
                item :: []

            else
                item :: toListFromDictStopAt stop next dict

        Nothing ->
            item :: []


isEmpty : DoubleLinkedList comparable -> Bool
isEmpty dll =
    dll.first == Nothing


isSingleton : DoubleLinkedList comparable -> Bool
isSingleton dll =
    dll.first /= Nothing && Dict.isEmpty dll.left


getItemBetween : Maybe comparable -> Maybe comparable -> DoubleLinkedList comparable -> Maybe comparable
getItemBetween maybeRightOf maybeLeftOf dll =
    case ( maybeRightOf, maybeLeftOf ) of
        ( Nothing, Nothing ) ->
            findMiddle (toList dll)

        ( Nothing, Just leftOf ) ->
            findMiddle (toListFromDict leftOf dll.left |> List.drop 1)

        ( Just rightOf, Nothing ) ->
            findMiddle (toListFromDict rightOf dll.right |> List.drop 1)

        ( Just rightOf, Just leftOf ) ->
            findMiddle (toListFromDictStopAt leftOf rightOf dll.right |> List.drop 2)


findMiddle : List comparable -> Maybe comparable
findMiddle list =
    list
        |> List.drop (List.length list // 2)
        |> List.head

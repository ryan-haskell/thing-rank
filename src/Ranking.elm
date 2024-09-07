module Ranking exposing
    ( Ranking, init
    , insertLikesLessThan, insertLikesMoreThan
    , Status(..), toStatus
    , toProgress
    )

{-|

@docs Ranking, init
@docs insertLikesLessThan, insertLikesMoreThan
@docs Status, toStatus
@docs toProgress

-}

import Array exposing (Array)
import NumberLine exposing (NumberLine)


{-| choices = [A,B,C]
-}
type alias Ranking choice =
    { size : Int
    , fallback : choice
    , array : Array choice
    , remaining : List Index
    , line : NumberLine Index
    , comparing : Index
    , bounds :
        { rightOf : Maybe Index
        , leftOf : Maybe Index
        }
    }


type alias Index =
    Int


init : List choice -> Result String (Ranking choice)
init choices =
    case choices of
        [] ->
            Err "Cannot sort an empty list"

        firstItem :: [] ->
            Err "Lists with one item are already sorted"

        firstItem :: _ ->
            let
                size : Int
                size =
                    List.length choices
            in
            Ok
                { size = size
                , fallback = firstItem
                , array = Array.fromList choices
                , remaining = List.range 2 (size - 1)
                , line =
                    NumberLine.empty
                        |> NumberLine.pushStart 0
                , comparing = 1
                , bounds = { rightOf = Nothing, leftOf = Nothing }
                }


insertLikesLessThan : choice -> Ranking choice -> Ranking choice
insertLikesLessThan choice ranking =
    let
        bounds =
            ranking.bounds

        index =
            toIndex choice ranking.array
    in
    { ranking | bounds = { bounds | leftOf = index } }
        |> attemptToResolve


insertLikesMoreThan : choice -> Ranking choice -> Ranking choice
insertLikesMoreThan choice ranking =
    let
        bounds =
            ranking.bounds

        index =
            toIndex choice ranking.array
    in
    { ranking | bounds = { bounds | rightOf = index } }
        |> attemptToResolve


attemptToResolve : Ranking choice -> Ranking choice
attemptToResolve ranking =
    case
        NumberLine.getItemInMiddle
            ranking.bounds.rightOf
            ranking.bounds.leftOf
            ranking.line
    of
        Nothing ->
            let
                rankingWithNewComparing : Ranking choice
                rankingWithNewComparing =
                    case ranking.remaining of
                        [] ->
                            ranking

                        firstItem :: remaining ->
                            { ranking
                                | bounds = { leftOf = Nothing, rightOf = Nothing }
                                , comparing = firstItem
                                , remaining = remaining
                            }
            in
            case ( ranking.bounds.leftOf, ranking.bounds.rightOf ) of
                ( Just leftOf, _ ) ->
                    { rankingWithNewComparing
                        | line = NumberLine.insertLeftOf leftOf ranking.comparing ranking.line
                    }

                ( _, Just rightOf ) ->
                    { rankingWithNewComparing
                        | line = NumberLine.insertRightOf rightOf ranking.comparing ranking.line
                    }

                ( Nothing, Nothing ) ->
                    { rankingWithNewComparing
                        | line = NumberLine.pushStart ranking.comparing ranking.line
                    }

        Just otherChoice ->
            ranking


type Status choice
    = Finished (List choice)
    | VotingOn ( choice, choice )


toStatus : Ranking choice -> Status choice
toStatus ranking =
    if ranking.size == NumberLine.size ranking.line then
        Finished
            (ranking.line
                |> NumberLine.toListFromRightToLeft
                |> List.filterMap (\i -> Array.get i ranking.array)
            )

    else
        let
            currentChoice : choice
            currentChoice =
                ranking.array
                    |> Array.get ranking.comparing
                    |> Maybe.withDefault ranking.fallback
        in
        case
            NumberLine.getItemInMiddle
                ranking.bounds.rightOf
                ranking.bounds.leftOf
                ranking.line
        of
            Just otherChoiceIndex ->
                let
                    otherChoice : choice
                    otherChoice =
                        ranking.array
                            |> Array.get otherChoiceIndex
                            |> Maybe.withDefault ranking.fallback
                in
                VotingOn ( currentChoice, otherChoice )

            Nothing ->
                -- Note: This should only happen if "attemptToResolve" is not
                -- called after insertion!
                VotingOn ( currentChoice, currentChoice )


{-| This tells the user how many items have been placed so far on the ranking timeline–
but the number of remaining votes depends on their rankings so far.
-}
toProgress : Ranking choice -> ( Int, Int )
toProgress ranking =
    ( NumberLine.size ranking.line
    , ranking.size
    )


toIndex : choice -> Array choice -> Maybe Int
toIndex choice array =
    let
        findIndex idx =
            if idx >= Array.length array then
                Nothing

            else if Array.get idx array == Just choice then
                Just idx

            else
                findIndex (idx + 1)
    in
    findIndex 0

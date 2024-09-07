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

import DoubleLinkedList exposing (DoubleLinkedList)


{-| choices = [A,B,C]
-}
type alias Ranking =
    { size : Int
    , remaining : List String
    , dll : DoubleLinkedList String
    , comparing : String
    , bounds : { rightOf : Maybe String, leftOf : Maybe String }
    }


init : List String -> Result (List String) Ranking
init choices =
    case choices of
        [] ->
            Err choices

        first :: [] ->
            Err choices

        firstItem :: secondItem :: rest ->
            Ok
                { size = List.length choices
                , remaining = rest
                , dll =
                    DoubleLinkedList.empty
                        |> DoubleLinkedList.pushStart firstItem
                , comparing = secondItem
                , bounds = { rightOf = Nothing, leftOf = Nothing }
                }


insertLikesLessThan : String -> Ranking -> Ranking
insertLikesLessThan value ranking =
    let
        bounds =
            ranking.bounds
    in
    { ranking | bounds = { bounds | leftOf = Just value } }
        |> attemptToResolve


insertLikesMoreThan : String -> Ranking -> Ranking
insertLikesMoreThan value ranking =
    let
        bounds =
            ranking.bounds
    in
    { ranking | bounds = { bounds | rightOf = Just value } }
        |> attemptToResolve


attemptToResolve : Ranking -> Ranking
attemptToResolve ranking =
    case
        DoubleLinkedList.getItemBetween
            ranking.bounds.rightOf
            ranking.bounds.leftOf
            ranking.dll
    of
        Nothing ->
            let
                rankingWithNewComparing : Ranking
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
                        | dll = DoubleLinkedList.insertLeftOf leftOf ranking.comparing ranking.dll
                    }

                ( _, Just rightOf ) ->
                    { rankingWithNewComparing
                        | dll = DoubleLinkedList.insertRightOf rightOf ranking.comparing ranking.dll
                    }

                ( Nothing, Nothing ) ->
                    { rankingWithNewComparing
                        | dll = DoubleLinkedList.pushStart ranking.comparing ranking.dll
                    }

        Just otherChoice ->
            ranking


type Status
    = Finished (List String)
    | VotingOn ( String, String )


toStatus : Ranking -> Status
toStatus ranking =
    if ranking.size == DoubleLinkedList.size ranking.dll then
        Finished (DoubleLinkedList.toList ranking.dll)

    else
        case
            DoubleLinkedList.getItemBetween
                ranking.bounds.rightOf
                ranking.bounds.leftOf
                ranking.dll
        of
            Just otherChoice ->
                VotingOn ( ranking.comparing, otherChoice )

            Nothing ->
                -- Note: This should only happen if "attemptToResolve" is not
                -- called after insertion!
                VotingOn ( ranking.comparing, ranking.comparing )


{-| This tells the user how many items have been placed so far on the ranking timeline–
but the number of remaining votes depends on their rankings so far.
-}
toProgress : Ranking -> ( Int, Int )
toProgress ranking =
    ( DoubleLinkedList.size ranking.dll
    , ranking.size
    )

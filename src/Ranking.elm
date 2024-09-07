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

import NumberLine exposing (NumberLine)


{-| choices = [A,B,C]
-}
type alias Ranking =
    { size : Int
    , remaining : List String
    , line : NumberLine String
    , comparing : String
    , bounds :
        { rightOf : Maybe String
        , leftOf : Maybe String
        }
    }


init : List String -> Result String Ranking
init choices =
    case choices of
        [] ->
            Err "Cannot sort an empty list"

        firstItem :: [] ->
            Err "Lists with one item are already sorted"

        firstItem :: secondItem :: rest ->
            Ok
                { size = List.length choices
                , remaining = rest
                , line =
                    NumberLine.empty
                        |> NumberLine.pushStart firstItem
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
        NumberLine.getItemInMiddle
            ranking.bounds.rightOf
            ranking.bounds.leftOf
            ranking.line
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


type Status
    = Finished (List String)
    | VotingOn ( String, String )


toStatus : Ranking -> Status
toStatus ranking =
    if ranking.size == NumberLine.size ranking.line then
        Finished (NumberLine.toListFromRightToLeft ranking.line)

    else
        case
            NumberLine.getItemInMiddle
                ranking.bounds.rightOf
                ranking.bounds.leftOf
                ranking.line
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
    ( NumberLine.size ranking.line
    , ranking.size
    )

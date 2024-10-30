module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import Html.Keyed
import Random
import Random.List
import Ranking exposing (Ranking)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type Model
    = Shuffling
    | Shuffled
        { ranking : Result (List Choice) (Ranking Choice)
        , votes : Int
        }


type alias Choice =
    ( Int, String )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Shuffling
    , pokemon
        |> getShuffledItems
    )


getShuffledItems : List Choice -> Cmd Msg
getShuffledItems choices =
    Random.List.shuffle choices
        |> Random.generate GotItems



-- UPDATE


type Msg
    = GotItems (List Choice)
    | VoteLikesMoreThan Choice
    | VoteLikesLessThan Choice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    case ( msg, model_ ) of
        ( GotItems itemsToRank, _ ) ->
            ( Shuffled
                { ranking =
                    Ranking.init itemsToRank
                        |> Result.mapError (\reason -> itemsToRank)
                , votes = 0
                }
            , Cmd.none
            )

        ( VoteLikesMoreThan otherChoice, Shuffled model ) ->
            ( Shuffled
                { model
                    | votes = model.votes + 1
                    , ranking =
                        model.ranking
                            |> Result.map (Ranking.insertLikesMoreThan otherChoice)
                }
            , Cmd.none
            )

        ( VoteLikesLessThan otherChoice, Shuffled model ) ->
            ( Shuffled
                { model
                    | votes = model.votes + 1
                    , ranking =
                        model.ranking
                            |> Result.map (Ranking.insertLikesLessThan otherChoice)
                }
            , Cmd.none
            )

        _ ->
            ( model_, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model_ =
    case model_ of
        Shuffling ->
            div [] []

        Shuffled model ->
            div
                [ style "padding" "1rem"
                , style "margin" "2rem auto"
                , style "max-width" "840px"
                , style "font-family" "Avenir, Montserrat, Corbel, 'URW Gothic', source-sans-pro, sans-serif"
                ]
                [ h1 [] [ text "Thing Rank: Pokémon" ]
                , case model.ranking of
                    Ok ranking ->
                        let
                            ( placed, size ) =
                                Ranking.toProgress ranking

                            count =
                                String.fromInt placed

                            total =
                                String.fromInt size
                        in
                        div []
                            [ div [ Attr.style "display" "flex", Attr.style "gap" "1rem" ]
                                [ progress
                                    [ value count
                                    , Attr.max total
                                    , Attr.style "flex" "1 1 auto"
                                    ]
                                    []
                                , span []
                                    [ text (String.fromInt (size - placed) ++ " remaining") ]
                                ]
                            , case Ranking.toStatus ranking of
                                Ranking.Finished list ->
                                    viewFinalRankList model list

                                Ranking.VotingOn choices ->
                                    viewChoice model choices
                            ]

                    Err list ->
                        viewFinalRankList model list
                ]


viewFinalRankList : { model | votes : Int } -> List Choice -> Html Msg
viewFinalRankList model rankedItems =
    div []
        [ p []
            [ text
                ("After ${votes} votes, here's your ranking!"
                    |> String.replace "${votes}" (String.fromInt model.votes)
                )
            ]
        , div [] (List.indexedMap viewFinalListPokemon rankedItems)
        ]


viewChoice : { model | votes : Int } -> ( Choice, Choice ) -> Html Msg
viewChoice model ( ( id1, name1 ) as choice1, ( id2, name2 ) as choice2 ) =
    Html.Keyed.node "div"
        []
        [ ( "0", p [ style "text-align" "center" ] [ text "Tap your favorite!" ] )
        , ( Tuple.second choice1, viewPokemonButton { onClick = VoteLikesMoreThan choice2, choice = choice1 } )
        , ( Tuple.second choice2, viewPokemonButton { onClick = VoteLikesLessThan choice2, choice = choice2 } )
        , ( "debug", p [ style "text-align" "center", style "opacity" "0.5" ] [ text ("Votes so far: " ++ String.fromInt model.votes) ] )
        ]



-- POKEMON DATA


viewPokemonButton : { choice : Choice, onClick : msg } -> Html msg
viewPokemonButton props =
    let
        ( id, name ) =
            props.choice
    in
    button [ Events.onClick props.onClick, style "max-width" "min(400px, calc(50vw - 1rem))" ]
        [ img [ src (toPokemonImage id), title name, alt name, style "max-width" "100%", style "height" "auto", width 400, height 400 ] []
        ]


viewFinalListPokemon : Int -> Choice -> Html msg
viewFinalListPokemon index ( id, name ) =
    div [ style "display" "flex", style "align-items" "center" ]
        [ strong [] [ text (String.fromInt (index + 1) ++ ". ") ]
        , img [ src (toPokemonSprite id), title name, alt name, width 40, height 40 ] []
        , span [] [ text name ]
        ]


toPokemonImage : Int -> String
toPokemonImage id =
    let
        idString =
            id
                |> String.fromInt
                |> String.padLeft 3 '0'
    in
    "https://raw.githubusercontent.com/fanzeyi/pokemon.json/refs/heads/master/images/" ++ idString ++ ".png"


toPokemonSprite : Int -> String
toPokemonSprite id =
    let
        idString =
            id
                |> String.fromInt
                |> String.padLeft 3 '0'
    in
    "https://raw.githubusercontent.com/fanzeyi/pokemon.json/refs/heads/master/sprites/" ++ idString ++ "MS.png"


pokemon : List Choice
pokemon =
    List.indexedMap (\i name -> ( i + 1, name ))
        [ "Bulbasaur"
        , "Ivysaur"
        , "Venusaur"
        , "Charmander"
        , "Charmeleon"
        , "Charizard"
        , "Squirtle"
        , "Wartortle"
        , "Blastoise"
        , "Caterpie"
        , "Metapod"
        , "Butterfree"
        , "Weedle"
        , "Kakuna"
        , "Beedrill"
        , "Pidgey"
        , "Pidgeotto"
        , "Pidgeot"
        , "Rattata"
        , "Raticate"
        , "Spearow"
        , "Fearow"
        , "Ekans"
        , "Arbok"
        , "Pikachu"
        , "Raichu"
        , "Sandshrew"
        , "Sandslash"
        , "Nidoran♀"
        , "Nidorina"
        , "Nidoqueen"
        , "Nidoran♂"
        , "Nidorino"
        , "Nidoking"
        , "Clefairy"
        , "Clefable"
        , "Vulpix"
        , "Ninetales"
        , "Jigglypuff"
        , "Wigglytuff"
        , "Zubat"
        , "Golbat"
        , "Oddish"
        , "Gloom"
        , "Vileplume"
        , "Paras"
        , "Parasect"
        , "Venonat"
        , "Venomoth"
        , "Diglett"
        , "Dugtrio"
        , "Meowth"
        , "Persian"
        , "Psyduck"
        , "Golduck"
        , "Mankey"
        , "Primeape"
        , "Growlithe"
        , "Arcanine"
        , "Poliwag"
        , "Poliwhirl"
        , "Poliwrath"
        , "Abra"
        , "Kadabra"
        , "Alakazam"
        , "Machop"
        , "Machoke"
        , "Machamp"
        , "Bellsprout"
        , "Weepinbell"
        , "Victreebel"
        , "Tentacool"
        , "Tentacruel"
        , "Geodude"
        , "Graveler"
        , "Golem"
        , "Ponyta"
        , "Rapidash"
        , "Slowpoke"
        , "Slowbro"
        , "Magnemite"
        , "Magneton"
        , "Farfetch'd"
        , "Doduo"
        , "Dodrio"
        , "Seel"
        , "Dewgong"
        , "Grimer"
        , "Muk"
        , "Shellder"
        , "Cloyster"
        , "Gastly"
        , "Haunter"
        , "Gengar"
        , "Onix"
        , "Drowzee"
        , "Hypno"
        , "Krabby"
        , "Kingler"
        , "Voltorb"
        , "Electrode"
        , "Exeggcute"
        , "Exeggutor"
        , "Cubone"
        , "Marowak"
        , "Hitmonlee"
        , "Hitmonchan"
        , "Lickitung"
        , "Koffing"
        , "Weezing"
        , "Rhyhorn"
        , "Rhydon"
        , "Chansey"
        , "Tangela"
        , "Kangaskhan"
        , "Horsea"
        , "Seadra"
        , "Goldeen"
        , "Seaking"
        , "Staryu"
        , "Starmie"
        , "Mr. Mime"
        , "Scyther"
        , "Jynx"
        , "Electabuzz"
        , "Magmar"
        , "Pinsir"
        , "Tauros"
        , "Magikarp"
        , "Gyarados"
        , "Lapras"
        , "Ditto"
        , "Eevee"
        , "Vaporeon"
        , "Jolteon"
        , "Flareon"
        , "Porygon"
        , "Omanyte"
        , "Omastar"
        , "Kabuto"
        , "Kabutops"
        , "Aerodactyl"
        , "Snorlax"
        , "Articuno"
        , "Zapdos"
        , "Moltres"
        , "Dratini"
        , "Dragonair"
        , "Dragonite"
        , "Mewtwo"
        ]

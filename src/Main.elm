module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events
import Ranking exposing (Ranking)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- INIT


type alias Model =
    { ranking : Result (List Choice) (Ranking Choice)
    , votes : Int
    }


type alias Choice =
    String


init : Model
init =
    let
        itemsToRank : List String
        itemsToRank =
            -- [ "Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise", "Caterpie", "Metapod", "Butterfree", "Weedle", "Kakuna", "Beedrill", "Pidgey", "Pidgeotto", "Pidgeot", "Rattata", "Raticate", "Spearow", "Fearow", "Ekans", "Arbok", "Pikachu", "Raichu", "Sandshrew", "Sandslash", "Nidoran♀", "Nidorina", "Nidoqueen", "Nidoran♂", "Nidorino", "Nidoking", "Clefairy", "Clefable", "Vulpix", "Ninetales", "Jigglypuff", "Wigglytuff", "Zubat", "Golbat", "Oddish", "Gloom", "Vileplume", "Paras", "Parasect", "Venonat", "Venomoth", "Diglett", "Dugtrio", "Meowth", "Persian", "Psyduck", "Golduck", "Mankey", "Primeape", "Growlithe", "Arcanine", "Poliwag", "Poliwhirl", "Poliwrath", "Abra", "Kadabra", "Alakazam", "Machop", "Machoke", "Machamp", "Bellsprout", "Weepinbell", "Victreebel", "Tentacool", "Tentacruel", "Geodude", "Graveler", "Golem", "Ponyta", "Rapidash", "Slowpoke", "Slowbro", "Magnemite", "Magneton", "Farfetch'd", "Doduo", "Dodrio", "Seel", "Dewgong", "Grimer", "Muk", "Shellder", "Cloyster", "Gastly", "Haunter", "Gengar", "Onix", "Drowzee", "Hypno", "Krabby", "Kingler", "Voltorb", "Electrode", "Exeggcute", "Exeggutor", "Cubone", "Marowak", "Hitmonlee", "Hitmonchan", "Lickitung", "Koffing", "Weezing", "Rhyhorn", "Rhydon", "Chansey", "Tangela", "Kangaskhan", "Horsea", "Seadra", "Goldeen", "Seaking", "Staryu", "Starmie", "Mr. Mime", "Scyther", "Jynx", "Electabuzz", "Magmar", "Pinsir", "Tauros", "Magikarp", "Gyarados", "Lapras", "Ditto", "Eevee", "Vaporeon", "Jolteon", "Flareon", "Porygon", "Omanyte", "Omastar", "Kabuto", "Kabutops", "Aerodactyl", "Snorlax", "Articuno", "Zapdos", "Moltres", "Dratini", "Dragonair", "Dragonite", "Mewtwo" ]
            [ "Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise" ]
    in
    { ranking =
        Ranking.init itemsToRank
            |> Result.mapError (\reason -> itemsToRank)
    , votes = 0
    }



-- UPDATE


type Msg
    = VoteLikesMoreThan Choice
    | VoteLikesLessThan Choice


update : Msg -> Model -> Model
update msg model =
    case msg of
        VoteLikesMoreThan otherChoice ->
            { model
                | votes = model.votes + 1
                , ranking =
                    model.ranking
                        |> Result.map (Ranking.insertLikesMoreThan otherChoice)
            }

        VoteLikesLessThan otherChoice ->
            { model
                | votes = model.votes + 1
                , ranking =
                    model.ranking
                        |> Result.map (Ranking.insertLikesLessThan otherChoice)
            }



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "1rem" ]
        [ h1 [] [ text "Thing Rank" ]
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
                    [ div [ Attr.style "display" "flex" ]
                        [ progress
                            [ value count
                            , Attr.max total
                            ]
                            []
                        , span []
                            [ text ("(" ++ count ++ "/" ++ total ++ ")") ]
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


viewFinalRankList : Model -> List String -> Html Msg
viewFinalRankList model rankedItems =
    div []
        [ p []
            [ text
                ("After ${votes} votes, here's your ranking!"
                    |> String.replace "${votes}" (String.fromInt model.votes)
                )
            ]
        , ol [] (List.map (\item -> li [] [ text item ]) rankedItems)
        ]


viewChoice : Model -> ( String, String ) -> Html Msg
viewChoice model ( currentChoice, otherChoice ) =
    div []
        [ p [] [ text "Pick your favorite:" ]
        , button [ Events.onClick (VoteLikesMoreThan otherChoice) ] [ text currentChoice ]
        , button [ Events.onClick (VoteLikesLessThan otherChoice) ] [ text otherChoice ]
        , p [] [ text ("Votes: " ++ String.fromInt model.votes) ]
        ]

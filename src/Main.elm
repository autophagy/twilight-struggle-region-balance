module Main exposing (Model, Msg, main)

import Browser
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (classList, id, name)



---- MODEL ----


type Superpower
    = US
    | USSR


type alias Country =
    { name : String
    , battleground : Bool
    , stability : Int
    , usInfluence : Int
    , ussrInfluence : Int
    , adjacentTo : Maybe Superpower
    }

type alias Region =
    { name : String
    , presence : Int
    , domination : Int
    , control : Int
    , countries: List Country
    }

type alias Model = List Region


initCountry : String -> Int -> (Int, Int) -> Country
initCountry name stability (usInf, ussrInf) =
    { name = name, battleground = False, stability = stability, usInfluence = usInf, ussrInfluence = ussrInf, adjacentTo = Nothing }

initBattleground : String -> Int -> (Int, Int) -> Country
initBattleground name stability (usInf, ussrInf) =
    { name = name, battleground = True, stability = stability, usInfluence = usInf, ussrInfluence = ussrInf, adjacentTo = Nothing }

initAdjCountry : String -> Superpower -> Int -> (Int, Int) -> Country
initAdjCountry name superpower stability (usInf, ussrInf) =
    { name = name, battleground = False, stability = stability, usInfluence = usInf, ussrInfluence = ussrInf, adjacentTo = Just superpower }

initAdjBattleground : String -> Superpower -> Int -> (Int, Int) -> Country
initAdjBattleground name superpower stability (usInf, ussrInf) =
    { name = name, battleground = True, stability = stability, usInfluence = usInf, ussrInfluence = ussrInf, adjacentTo = Just superpower }


init : ( Model, Cmd Msg )
init =
    let
        europe =
            { name = "Europe"
            , presence = 3
            , domination = 7
            , control = 9999
            , countries =
                [ initAdjBattleground "Poland"     USSR 3 (0, 0)
                , initBattleground    "France"          3 (0, 0)
                , initBattleground    "W. Germany"      4 (0, 0)
                , initBattleground    "Italy"           2 (0, 0)
                , initBattleground    "E. Germany"      3 (0, 3)
                , initAdjCountry      "Canada"     US   4 (2, 0)
                , initAdjCountry      "Finland"    USSR 4 (0, 1)
                , initAdjCountry      "Romania"    USSR 3 (0, 0)
                , initCountry         "U.K"             5 (5, 0)
                , initCountry         "Spain/Portugal"  2 (0, 0)
                , initCountry         "Benelux"         3 (0, 0)
                , initCountry         "Norway"          4 (0, 0)
                , initCountry         "Denmark"         3 (0, 0)
                , initCountry         "Austria"         4 (0, 0)
                , initCountry         "Sweden"          4 (0, 0)
                , initCountry         "Czechoslovakia"  3 (0, 0)
                , initCountry         "Hungary"         3 (0, 0)
                , initCountry         "Yugoslavia"      3 (0, 0)
                , initCountry         "Greece"          2 (0, 0)
                , initCountry         "Bulgaria"        3 (0, 0)
                , initCountry         "Turkey"          2 (0, 0)
                ]
            }

        asia =
            { name = "Asia"
            , presence = 3
            , domination = 7
            , control = 9
            , countries =
                [ initAdjBattleground "N. Korea"    USSR 3 (0, 3)
                , initAdjBattleground "Japan"       US   4 (1, 0)
                , initBattleground    "S. Korea"         3 (1, 0)
                , initBattleground    "Pakistan"         2 (0, 0)
                , initBattleground    "India"            3 (0, 0)
                , initBattleground    "Thailand"         2 (0, 0)
                , initAdjCountry      "Afghanistan" USSR 2 (0, 0)
                , initCountry         "Burma"            2 (0, 0)
                , initCountry         "Laos/Combodia"    1 (0, 0)
                , initCountry         "Taiwan"           3 (0, 0)
                , initCountry         "Vietnam"          1 (0, 0)
                , initCountry         "Philippines"      2 (1, 0)
                , initCountry         "Malaysia"         2 (0, 0)
                , initCountry         "Indonesia"        1 (0, 0)
                , initCountry         "Australia"        4 (4, 0)
                ]
            }
    in
    ( [ europe, asia ], Cmd.none )


controlsCountry : Superpower -> Country -> Bool
controlsCountry superpower country =
    case superpower of
        US ->
            (country.usInfluence - country.ussrInfluence) >= country.stability

        USSR ->
            (country.ussrInfluence - country.usInfluence) >= country.stability


hasPresence : Superpower -> Region -> Bool
hasPresence superpower region =
    List.length (List.filter (controlsCountry superpower) region.countries) > 0


hasDomination : Superpower -> Region -> Bool
hasDomination superpower region =
    let
        usNonBattlegrounds : Int
        usNonBattlegrounds =
            List.length <|
                List.filter (controlsCountry US) <|
                    List.filter (\c -> not c.battleground) region.countries

        ussrNonBattlegrounds : Int
        ussrNonBattlegrounds =
            List.length <|
                List.filter (controlsCountry USSR) <|
                    List.filter (\c -> not c.battleground) region.countries

        usBattlegrounds : Int
        usBattlegrounds =
            List.length <|
                List.filter (controlsCountry US) <|
                    List.filter (\c -> c.battleground) region.countries

        ussrBattlegrounds : Int
        ussrBattlegrounds =
            List.length <|
                List.filter (controlsCountry USSR) <|
                    List.filter (\c -> c.battleground) region.countries
    in
    case superpower of
        US ->
            ((usBattlegrounds + usNonBattlegrounds) > (ussrBattlegrounds + ussrNonBattlegrounds))
                && (usBattlegrounds > ussrBattlegrounds)
                && usBattlegrounds
                >= 1
                && usNonBattlegrounds
                >= 1

        USSR ->
            ((ussrBattlegrounds + ussrNonBattlegrounds) > (usBattlegrounds + usNonBattlegrounds))
                && (ussrBattlegrounds > usBattlegrounds)
                && ussrBattlegrounds
                >= 1
                && ussrNonBattlegrounds
                >= 1


hasControl : Superpower -> Region -> Bool
hasControl superpower region =
    let
        usCountries : Int
        usCountries =
            List.length <|
                List.filter (controlsCountry US) region.countries

        ussrCountries : Int
        ussrCountries =
            List.length <|
                List.filter (controlsCountry USSR) region.countries
    in
    case superpower of
        US ->
            (usCountries > ussrCountries)
                && List.all (controlsCountry US) (List.filter (\c -> c.battleground) region.countries)

        USSR ->
            (ussrCountries > usCountries)
                && List.all (controlsCountry USSR) (List.filter (\c -> c.battleground) region.countries)



---- UPDATE ----


type Msg
    = A


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        A ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "wrapper" ] (List.map viewRegion model)


viewRegion : Region -> Html Msg
viewRegion region =
    div [ id region.name ] (List.append [ h1 [] [ text region.name ] ] (List.map viewCountry region.countries))


viewCountry : Country -> Html Msg
viewCountry country =
    let
        controlledBy : String
        controlledBy =
            if controlsCountry US country then
                "US Controlled"

            else if controlsCountry USSR country then
                "USSR Controlled"

            else
                "No Control"
    in
    div
        [ id country.name
        , classList
            [ ( "battleground", country.battleground )
            , ( "us-controlled", controlsCountry US country )
            , ( "ussr-controlled", controlsCountry USSR country )
            ]
        ]
        [ text <| country.name ++ " :: " ++ controlledBy ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }

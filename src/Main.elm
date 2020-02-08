module Main exposing (..)

import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json
import List.Extra
import Tuple exposing (first, second)


type alias Model =
    { playerPosition : Coordinate
    , boardSize : Coordinate
    , crates : List Coordinate
    , goals : List Coordinate
    , walls : List Coordinate
    }


type Msg
    = Move Direction
    | Win


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Coordinate =
    ( Int, Int )


type Tile
    = Empty
    | Player
    | Crate
    | Wall
    | Goal


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd msg )
init _ =
    ( loadModel
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Move direction ->
            ( movePlayer direction model, Cmd.none )

        Win ->
            ( { model | crates = [ ( -1, -1 ) ], walls = [], goals = [] }, Cmd.none )

view : Model -> Browser.Document Msg
view model =
    { title = "Sokoelm"
    , body =
        [ table []
            [ renderBoard model ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onKeyDown keyDecoder


keyDecoder : Json.Decoder Msg
keyDecoder =
    Json.field "key" Json.string
        |> Json.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Json.succeed (Move Left)

                    "ArrowRight" ->
                        Json.succeed (Move Right)

                    "ArrowUp" ->
                        Json.succeed (Move Up)

                    "ArrowDown" ->
                        Json.succeed (Move Down)

                    _ ->
                        Json.fail "Invalid Msg"
            )


loadModel : Model
loadModel =
    { playerPosition = ( 4, 2 )
    , boardSize = ( 6, 9 )
    , crates = [ ( 2, 2 ), ( 2, 6 ), ( 4, 3 ), ( 4, 5 ) ]
    , goals = [ ( 2, 1 ), ( 2, 3 ), ( 4, 4 ), ( 4, 6 ) ]
    , walls =
        [ ( 0, 0 )
        , ( 1, 0 )
        , ( 2, 0 )
        , ( 3, 0 )
        , ( 4, 0 )
        , ( 5, 0 )
        , ( 0, 1 )
        , ( 0, 2 )
        , ( 0, 3 )
        , ( 0, 4 )
        , ( 0, 5 )
        , ( 0, 6 )
        , ( 0, 7 )
        , ( 0, 8 )
        , ( 5, 1 )
        , ( 5, 2 )
        , ( 5, 3 )
        , ( 5, 4 )
        , ( 5, 5 )
        , ( 5, 6 )
        , ( 5, 7 )
        , ( 1, 8 )
        , ( 2, 8 )
        , ( 3, 8 )
        , ( 4, 8 )
        , ( 5, 8 )
        , ( 1, 1 )
        , ( 3, 2 )
        , ( 1, 4 )
        , ( 3, 5 )
        , ( 3, 6 )
        ]
    }


inBounds : Coordinate -> Coordinate -> Bool
inBounds boardSize coordinates =
    first coordinates
        < first boardSize
        && first coordinates
        >= 0
        && second coordinates
        < second boardSize
        && second coordinates
        >= 0


legal : Model -> Bool
legal { boardSize, playerPosition, crates, walls } =
    -- no collisions
    Debug.log "all" (List.Extra.allDifferent (walls ++ crates ++ [ playerPosition ]))
        && -- player in bounds
           inBounds boardSize playerPosition
        && -- crate in bounds
           List.all (\crate -> inBounds boardSize crate) crates


winCondition : Model -> Bool
winCondition model =
    List.all (\goal -> List.member goal model.crates) model.goals


movePlayer : Direction -> Model -> Model
movePlayer direction model =
    let
        newPlayerPosition =
            case direction of
                Up ->
                    ( first model.playerPosition + -1, second model.playerPosition )

                Down ->
                    ( first model.playerPosition + 1, second model.playerPosition )

                Left ->
                    ( first model.playerPosition, second model.playerPosition + -1 )

                Right ->
                    ( first model.playerPosition, second model.playerPosition + 1 )

        newCratePosition =
            case direction of
                Up ->
                    ( first newPlayerPosition + -1, second newPlayerPosition )

                Down ->
                    ( first newPlayerPosition + 1, second newPlayerPosition )

                Left ->
                    ( first newPlayerPosition, second newPlayerPosition + -1 )

                Right ->
                    ( first newPlayerPosition, second newPlayerPosition + 1 )

        newState =
            { model
                | playerPosition = newPlayerPosition
                , crates =
                    if List.member newPlayerPosition model.crates then
                        List.filter (\crate -> crate /= newPlayerPosition) model.crates
                            |> List.append [ newCratePosition ]

                    else
                        model.crates
            }
    in
    if newState |> legal then
        if winCondition newState then
            first (update Win newState)

        else
            newState

    else
        model


renderTile : Model -> Int -> Int -> Html msg
renderTile { boardSize, playerPosition, crates, goals, walls } col row =
    let
        tileState =
            if playerPosition == ( row, col ) then
                Player

            else if List.member ( row, col ) crates then
                Crate

            else if List.member ( row, col ) goals then
                Goal

            else if List.member ( row, col ) walls then
                Wall

            else
                Empty

        --
    in
    case tileState of
        Player ->
            text "@"

        Crate ->
            text "$"

        Wall ->
            text "#"

        Goal ->
            text "."

        Empty ->
            text " "


renderRow : Int -> Model -> List (Html msg)
renderRow rowNumber model =
    (List.repeat (second model.boardSize) rowNumber
        |> List.indexedMap (renderTile model)
    )
        ++ [ br [] [] ]


renderBoard : Model -> Html msg
renderBoard model =
    pre []
        (List.range 0 (first model.boardSize - 1)
            |> List.concatMap (\rowNumber -> renderRow rowNumber model)
        )

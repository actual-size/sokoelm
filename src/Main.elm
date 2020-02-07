module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Tuple exposing (first, second)


type alias Model =
    { playerPosition : Coordinate
    , boardSize : Int
    , crates : List Coordinate
    }


type Msg
    = Move Direction


type Direction
    = Up
    | Down
    | Left
    | Right
    | Other


type alias Coordinate =
    ( Int, Int )


type Tile
    = Empty
    | Player
    | Crate


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd msg )
init _ =
    ( { playerPosition = ( 1, 1 )
      , boardSize = 8
      , crates = [ ( 0, 0 ), ( 3, 2 ), ( 6, 3 ), ( 8, 5 ) ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Move direction ->
            ( movePlayer direction model, Cmd.none )


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


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            Move Left

        "ArrowRight" ->
            Move Right

        "ArrowUp" ->
            Move Up

        "ArrowDown" ->
            Move Down

        _ ->
            Move Other


inBounds : Int -> Coordinate -> Bool
inBounds boardSize coordinates =
    first coordinates
        < boardSize
    && first coordinates
        >= 0
    && second coordinates
        < boardSize
    && second coordinates
        >= 0


tryPushOrDefault : Direction -> Model -> Model -> Model
tryPushOrDefault direction state default =
    let
        newCratePosition = case direction of
            Up ->
                ( first state.playerPosition + -1, second state.playerPosition )

            Down ->
                ( first state.playerPosition + 1, second state.playerPosition )

            Left ->
                ( first state.playerPosition, second state.playerPosition + -1 )

            Right ->
                ( first state.playerPosition, second state.playerPosition + 1 )

            Other ->
                state.playerPosition

    in
        if List.any (\crate -> crate == newCratePosition) state.crates || not (inBounds state.boardSize newCratePosition) then
            default
        else
            { state | crates = List.append (List.filter (\crate -> crate /= state.playerPosition) state.crates) [newCratePosition] }

movePlayer : Direction -> Model -> Model
movePlayer direction model =
    let
        legalOrDefault =
            \default state ->
                if inBounds state.boardSize state.playerPosition then
                    if List.member state.playerPosition state.crates then
                        tryPushOrDefault direction state default
                    else
                        state
                else
                    default

        newState =
            case direction of
                Up ->
                    { model | playerPosition = ( first model.playerPosition + -1, second model.playerPosition ) }

                Down ->
                    { model | playerPosition = ( first model.playerPosition + 1, second model.playerPosition ) }

                Left ->
                    { model | playerPosition = ( first model.playerPosition, second model.playerPosition + -1 ) }

                Right ->
                    { model | playerPosition = ( first model.playerPosition, second model.playerPosition + 1 ) }

                Other ->
                    model
    in
    legalOrDefault model newState


renderTile : Model -> Int -> Int -> Html msg
renderTile { playerPosition, crates } col row =
    let
        playerIsHere =
            playerPosition == ( row, col )

        crateIsHere =
            List.member ( row, col ) crates

        tileState =
            if playerIsHere then
                Player

            else if crateIsHere then
                Crate

            else
                Empty

        --
    in
    td []
        [ case tileState of
            Player ->
                text "+"

            Crate ->
                text "☐"

            Empty ->
                text "_"
        ]


renderRow : Int -> Model -> Html msg
renderRow rowNumber model =
    tr [] (List.indexedMap (renderTile model) (List.repeat model.boardSize rowNumber))


renderBoard : Model -> Html msg
renderBoard model =
    div [] (List.map (\rowNumber -> renderRow rowNumber model) (List.range 0 (model.boardSize - 1)))

module Main exposing (..)

import Browser
import List.Extra
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json
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
      , crates = [ ( 0, 0 ), ( 3, 2 ), ( 6, 3 ) ]
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


legal : Model -> Bool
legal {boardSize, playerPosition, crates} =
    Debug.log "no crates stacked" (List.Extra.allDifferent crates) && -- crates stacked
    Debug.log "player in bounds" (inBounds boardSize playerPosition) && -- player out of bounds
    Debug.log "player not on crate" (not (List.member playerPosition crates)) && -- player on crate
    Debug.log "all crates in bounds" (List.all (\crate -> inBounds boardSize crate) (Debug.log "all crates" crates)) -- crate out of bounds


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
            { model | playerPosition = newPlayerPosition
            , crates = case List.member newPlayerPosition model.crates of
                True -> List.append (List.filter (\crate -> crate /= newPlayerPosition) model.crates) [newCratePosition]
                False -> model.crates
            }

    in
    case legal (Debug.log "newstate" newState) of
        True -> newState
        False -> model


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

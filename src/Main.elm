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
    , boardSize : Int
    , crates : List Coordinate
    , walls : List Coordinate
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
    | Wall


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd msg )
init _ =
    ( { playerPosition = ( 1, 1 )
      , boardSize = 8
      , crates = [ ( 3, 2 ), ( 6, 3 ), ( 3, 3 ) ]
      , walls = [ ( 0, 0 ), ( 0, 1 ) ]
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
legal { boardSize, playerPosition, crates, walls } =
    -- no crates stacked
    List.Extra.allDifferent crates
        && -- player in bounds
           inBounds boardSize playerPosition
        && -- no player collisions
           not
            (List.append walls crates
                |> List.member playerPosition
            )
        && -- crate in bounds
           List.all (\crate -> inBounds boardSize crate) crates


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
        newState

    else
        model


renderTile : Model -> Int -> Int -> Html msg
renderTile { playerPosition, crates, walls } col row =
    let
        tileState =
            if playerPosition == ( row, col ) then
                Player

            else if List.member ( row, col ) crates then
                Crate

            else if List.member ( row, col ) walls then
                Wall

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

            Wall ->
                text "#"

            Empty ->
                text "_"
        ]


renderRow : Int -> Model -> Html msg
renderRow rowNumber model =
    tr []
        (List.repeat model.boardSize rowNumber
            |> List.indexedMap (renderTile model)
        )


renderBoard : Model -> Html msg
renderBoard model =
    div []
        (List.range 0 (model.boardSize - 1)
            |> List.map (\rowNumber -> renderRow rowNumber model)
        )

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


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


init : () -> ( Model, Cmd msg )
init _ =
    ( { playerPosition = ( 1, 1 )
      , boardSize = 5
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
            [ renderBoard model.boardSize model.playerPosition ]
        , div [] [ button [ onClick (Move Right) ] [ text "go right" ] ]
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



-- TYPES


type alias Model =
    { playerPosition : Coordinate
    , boardSize : Int
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


movePlayer : Direction -> Model -> Model
movePlayer direction model =
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


renderTile : Coordinate -> Int -> Int -> Html msg
renderTile playerPosition col row =
    let
        playerIsHere =
            playerPosition == ( row, col )
    in
    td []
        [ case playerIsHere of
            True ->
                text "+"

            False ->
                text "_"
        ]


renderRow : Int -> Int -> Coordinate -> Html msg
renderRow boardSize row playerPosition =
    tr [] (List.indexedMap (renderTile playerPosition) (List.repeat boardSize row))


renderBoard : Int -> Coordinate -> Html msg
renderBoard boardSize playerPosition =
    div [] (List.map (\tile -> renderRow boardSize tile playerPosition) (List.range 0 (boardSize - 1)))

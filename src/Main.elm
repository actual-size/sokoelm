module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { board : Array Int }


type Msg
    = Off
    | On


init : Model
init =
    { board = Array.initialize 10 (always 0) }


update : Msg -> Model -> Model
update msg model =
    model


displaySquare : Int -> Html msg
displaySquare arrayValue =
    div []
        [ text (String.fromInt arrayValue)
        ]


view : Model -> Html Msg
view model =
    div [] (Array.toList (Array.map displaySquare model.board))

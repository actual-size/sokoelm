module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type Tile
    = Empty
    | Player


type alias Model =
    { board : Array (Array Tile), playerPosition : ( Int, Int ) }


startingBoard : Array (Array Tile)
startingBoard =
    Array.fromList
        [ Array.fromList [ Empty, Empty, Empty, Empty, Empty, Empty ]
        , Array.fromList [ Empty, Player, Empty, Empty, Empty, Empty ]
        , Array.fromList [ Empty, Empty, Empty, Empty, Empty, Empty ]
        , Array.fromList [ Empty, Empty, Empty, Empty, Empty, Empty ]
        , Array.fromList [ Empty, Empty, Empty, Empty, Empty, Empty ]
        ]


init : () -> ( Model, Cmd msg )
init _ =
    ( { board = startingBoard
      , playerPosition = ( 1, 1 )
      }
    , Cmd.none
    )


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


displaySquare : Tile -> Html msg
displaySquare arrayValue =
    td []
        [ case arrayValue of
            Player ->
                text "+"

            Empty ->
                text "_"
        ]


displayRow : Array Tile -> Html msg
displayRow row =
    tr []
        (Array.toList
            (Array.map displaySquare row)
        )


view : Model -> Browser.Document msg
view model =
    { title = "Sokoelm"
    , body =
        [ table []
            (Array.toList
                (Array.map displayRow model.board)
            )
        ]
    }

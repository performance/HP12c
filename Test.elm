module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard
import Json.Decode exposing (..)
import String
import List
import Json.Decode as Json
import Char

import Array exposing (..)

-- This file is built on top of https://github.com/chrisbuttery/elm-calculator

-- Events


-- Helpers


classNames : List String -> Attribute Msg
classNames strings =
  classList (List.map (\str -> ( str, True )) strings)


parseFloat : String -> Float
parseFloat string =
  case String.toFloat string of
    Ok value ->
      value

    Err error ->
      0


-- Model


type InputModes
  = White
  | Orange
  | Blue

type alias Model =
  { inputMode                     : InputModes
  , keyCode                       : Keyboard.KeyCode
  }


initialModel : Model
initialModel =
  { inputMode                     = White
  , keyCode                       = 0
  }

-- Operations


sum : Float -> Float -> Float
sum x y =
  x + y


multiply : Float -> Float -> Float
multiply x y =
  x * y


division : Float -> Float -> Float
division x y =
  x / y


subtraction : Float -> Float -> Float
subtraction x y =
  x - y


-- Action


type Msg -- Cal_C_Keys                    Blue                         Orange
-------------------------------- First Row of Keys
  = N_Key                             | Times_12_Key            |     AMORT_Key
  | I_Key                             | DIVIDE_BY_12_Key        |     INT_Key
  | KeyMsg Keyboard.KeyCode -- for any key not on the calc


-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyMsg code ->
      ( { model | keyCode = code }  , Cmd.none )
-------------------------------- First Row of Keys
    N_Key                 ->
      ( model, Cmd.none )
    Times_12_Key          ->
      ( model, Cmd.none )
    AMORT_Key             ->
      ( model, Cmd.none )
    I_Key                 ->
      ( model, Cmd.none )
    DIVIDE_BY_12_Key      ->
      ( model, Cmd.none )
    INT_Key               ->
      ( model, Cmd.none )
    _ ->
          ( model, Cmd.none )


-- SUBSCRIPTIONS

keyCodeToMsg : InputModes -> Keyboard.KeyCode -> Msg
keyCodeToMsg inputMode code =
  case code of
-------------------------------- First Row of Keys
    78  -> case inputMode of
              White  -> N_Key   -- "N"
              Blue   -> Times_12_Key
              Orange -> AMORT_Key
    110 -> case inputMode of
              White  -> N_Key   -- "N"
              Blue   -> Times_12_Key
              Orange -> AMORT_Key

    73  -> case inputMode of
              White  -> I_Key   -- "I"
              Blue   -> DIVIDE_BY_12_Key
              Orange -> INT_Key
    105 -> case inputMode of
              White  -> I_Key   -- "i"
              Blue   -> DIVIDE_BY_12_Key
              Orange -> INT_Key

------------------------------
    _  -> KeyMsg code -- for any key not on the calc

subscriptions:Model -> Sub Msg
subscriptions model =
  Keyboard.presses ( \code -> ( keyCodeToMsg model.inputMode code ) )

-- View

view : Model -> Html Msg
view model =
  div
    [
      classNames ["calculator"]
    ]
    [
      div [ classNames [ "model" ] ] [ text (toString model) ]
    , div
          []
          [
            text ( String.fromChar ( Char.fromCode model.keyCode ) )
          ]
    ]


-- Main


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  ( Maybe.withDefault initialModel savedModel, Cmd.none )


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions -- \_ -> Sub.none
    }

module Main exposing (..)

import String
import List
--import Json.Decode as Json
import Char
import Keyboard

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html

import Json.Decode exposing (..)

import HP12c_KeyBoardInput exposing (..)
import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)
import HP12c_Update exposing (..)

import Array exposing (..)




-- SUBSCRIPTIONS



subscriptions:Model -> Sub Msg
subscriptions model =
  Keyboard.presses ( \code -> ( keyCodeToMsg model.inputMode code ) )

-- View


-- Helpers


classNames : List String -> Attribute Msg
classNames strings =
  classList (List.map (\str -> ( str, True )) strings)


stylesheet =
  let
    tag = "link"
    attrs =
        [ attribute "rel"       "stylesheet"
        , attribute "property"  "stylesheet"
        , attribute "href"      "HP12c.css"
        ]
    children = []
  in
    node tag attrs children

transparent_box left top msg =
  div
    [ onClick msg, classNames [ "transparent_box"], Html.Attributes.style [ ( "left", ( toString left ) ++ "px"),( "top", ( toString top ) ++ "px") ] ]
    [  ]

enter_button_div model =
  let
    msg = ( keyCodeToMsg model.inputMode ( 13 ) )
    enter_style =
      Html.Attributes.style
        [ ( "left", "305px")
        , ( "top", "250px")
        , ( "height", "64" )
        , ("position",  "absolute" )
        , ("opacity",  "0.9" )
        , ("height",  "92px" )
        , ("width",  "34px" )
        , ("background-color", "lightBlue" )
        , ("z-index" , "1" )
        ]

  in
    div
      [ onClick msg,  enter_style ]
      [  ]

mktdiv inputMode y_loc (keyChar ,  x_loc ) =
  transparent_box x_loc y_loc  ( keyCodeToMsg inputMode ( Char.toCode keyChar ) )

first_row_divs model =
  let
    x_locs = [ ('N', 40 ), ('I', 95 ), ('P', 145 ), ('M', 200 ), ('V', 252 ), ('H', 305 ), ('7', 358 ), ('8', 410 ), ('9', 464 ),('/', 515 ) ]
    y_loc  = 134
  in
    List.map ( \kxlpair -> mktdiv model.inputMode y_loc kxlpair ) x_locs

second_row_divs model =
  let
    x_locs = [ ('!', 40 ), ('\\', 95 ), ('T', 145 ), ('$', 200 ), ('%', 252 ), ('E', 305 ), ('4', 358 ), ('5', 410 ), ('6', 464 ),('*', 515 ) ]
    y_loc  = 192
  in
    List.map ( \kxlpair -> mktdiv model.inputMode y_loc kxlpair ) x_locs

third_row_divs model =
  let
    x_locs = [ ('[', 40 ), (']', 95 ), ('R', 145 ), ('Y', 200 ), ('C', 252 ),               ('1', 358 ), ('2', 410 ), ('3', 464 ),('-', 515 ) ]
    y_loc  = 250
  in
    List.map ( \kxlpair -> mktdiv model.inputMode y_loc kxlpair ) x_locs

fourth_row_divs model =
  let
    x_locs = [ ('O', 40 ), ('F', 95 ), ('G', 145 ), ('S', 200 ), ('L', 252 ),               ('0', 358 ), ('.', 410 ), ('W', 464 ),('+', 515 ) ]
    y_loc  = 310
  in
    List.map ( \kxlpair -> mktdiv model.inputMode y_loc kxlpair ) x_locs


button_divs model =
  ( first_row_divs model ) ++ ( second_row_divs model ) ++ ( third_row_divs model ) ++ ( fourth_row_divs model ) ++ [ enter_button_div model ]


empty_br_node = br [] []

-- upgrade to show a beautifully labeled tzyx stack and last_x
stack_registers_div model =
    div
    [
       classNames [ "calc_model" ], Html.Attributes.style [ ( "left", "0px"),( "top", "574px"), ("position", "absolute")  ]
    ]
    
   ( List.intersperse  empty_br_node ( List.map text ( String.split "," ( toString model.automaticMemoryStackRegisters ) )  ) 

   )

-- upgrade this to show detailed computation state info
modelinfodiv model =
  div
    [
      classNames [ "calc_model" ], Html.Attributes.style [ ( "left", "0px"),( "top", "434px"), ("position", "absolute") ]
    ]
    [
        text ( toString model.inputMode ++ "[ code = " ++ String.fromChar ( Char.fromCode model.keyCode ) ++ "  ] " ++ model.message )
    ]


divs_to_show model =
  [ stylesheet ] ++ ( button_divs model ) ++ [ modelinfodiv model ] ++ [stack_registers_div model ]

view : Model -> Html Msg
view model =
  div
    [
      classNames [ "calculator" ]
    ]
    ( divs_to_show model )


-- Main


init : ( Model, Cmd Msg )
init  =
  ( initialModel, Cmd.none )


main =
  Html.program -- WithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions -- \_ -> Sub.none
    }

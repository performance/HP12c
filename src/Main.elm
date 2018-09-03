module Main exposing (..)

-- Link to manual: http://www.hp.com/ctg/Manual/bpia5309.pdf

import Keyboard


-- import Html.App as Html

import Html exposing (..)
import HP12c_KeyBoardInput exposing (..)
import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)
import HP12c_View exposing (..)
import HP12c_Update exposing (..)


-- import HP12c_Update_handlers exposing (..)
--import Array exposing (..)
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses (\code -> (keyCodeToMsg model.inputMode code))



-- Main


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        -- WithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions =
            subscriptions
            -- \_ -> Sub.none
        }

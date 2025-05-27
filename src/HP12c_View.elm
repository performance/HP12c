module HP12c_View exposing (..)

import Char
import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import HP12c_KeyBoardInput exposing (..)
import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)


-- Helpers


classNames : List String -> Attribute Msg
classNames strings =
    classList (List.map (\str -> ( str, True )) strings)


stylesheet : Html msg
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "HP12c.css"
            ]

        children =
            []
    in
        node tag attrs children


transparent_box : Model -> b -> c -> Char -> Html Msg
transparent_box model left top keyChar =
    let
        msg =
            (keyCodeToMsg model.inputMode (Char.toCode keyChar))

        opacity =
            ( "opacity"
            , toString
                (if True == model.shortcutVisible then
                    0.851
                 else
                    0
                )
            )
    in
        div
            [ onClick msg, classNames [ "transparent_box" ], Html.Attributes.style [ ( "left", (toString left) ++ "px" ), ( "top", (toString top) ++ "px" ), opacity, ( "background-color", "lightBlue" ) ] ]
            [ span
                [ classNames [ "hint_key" ], Html.Attributes.style [ opacity ] ]
                [ Html.text (String.fromChar keyChar) ]
            ]


enter_button_div : Model -> Html Msg
enter_button_div model =
    let
        msg =
            (keyCodeToMsg model.inputMode (13))

        opacity =
            ( "opacity"
            , toString
                (if True == model.shortcutVisible then
                    0.851
                 else
                    0
                )
            )

        enter_style =
            Html.Attributes.style
                [ ( "left", "305px" )
                , ( "top", "250px" )
                , ( "height", "64" )
                , ( "position", "absolute" )
                , opacity
                  -- ("opacity",  "0.9" )
                , ( "height", "92px" )
                , ( "width", "34px" )
                , ( "background-color", "lightBlue" )
                , ( "z-index", "1" )
                ]
    in
        div
            [ onClick msg, enter_style ]
            [ span
                [ classNames [ "hint_key" ], Html.Attributes.style [ opacity, ( "font-size", "10px" ) ] ]
                [ Html.text (toString "Enter") ]
            ]


mktdiv : Model -> b -> ( Char, a ) -> Html Msg
mktdiv model y_loc ( keyChar, x_loc ) =
    transparent_box model x_loc y_loc keyChar


first_row_divs : Model -> List (Html Msg)
first_row_divs model =
    let
        x_locs =
            [ ( 'N', 40 ), ( 'I', 95 ), ( 'P', 145 ), ( 'M', 200 ), ( 'V', 252 ), ( 'H', 305 ), ( '7', 358 ), ( '8', 410 ), ( '9', 464 ), ( '/', 515 ) ]

        y_loc =
            134
    in
        List.map (\kxlpair -> mktdiv model y_loc kxlpair) x_locs


second_row_divs : Model -> List (Html Msg)
second_row_divs model =
    let
        x_locs =
            [ ( '!', 40 ), ( '\\', 95 ), ( 'T', 145 ), ( '$', 200 ), ( '%', 252 ), ( 'E', 305 ), ( '4', 358 ), ( '5', 410 ), ( '6', 464 ), ( '*', 515 ) ]

        y_loc =
            192
    in
        List.map (\kxlpair -> mktdiv model y_loc kxlpair) x_locs


third_row_divs : Model -> List (Html Msg)
third_row_divs model =
    let
        x_locs =
            [ ( '[', 40 ), ( ']', 95 ), ( 'R', 145 ), ( 'Y', 200 ), ( 'C', 252 ), ( '1', 358 ), ( '2', 410 ), ( '3', 464 ), ( '-', 515 ) ]

        y_loc =
            250
    in
        List.map (\kxlpair -> mktdiv model y_loc kxlpair) x_locs


fourth_row_divs : Model -> List (Html Msg)
fourth_row_divs model =
    let
        x_locs =
            [ ( 'O', 40 ), ( 'F', 95 ), ( 'G', 145 ), ( 'S', 200 ), ( 'L', 252 ), ( '0', 358 ), ( '.', 410 ), ( 'W', 464 ), ( '+', 515 ) ]

        y_loc =
            310
    in
        List.map (\kxlpair -> mktdiv model y_loc kxlpair) x_locs


all_rows_divs : Model -> List (Html Msg)
-- LCD Display Div
lcdDisplayDiv : Model -> Html Msg
lcdDisplayDiv model =
    div [ classNames [ "lcd-display" ] ]
        [ Html.div [ classNames ["modifier-indicators"] ]
            [ if model.inputMode == Orange then
                Html.span [ classNames ["f-indicator"] ] [ Html.text "f" ]
              else
                Html.span [] [] -- Empty span to maintain structure if needed, or Html.text ""
            , if model.inputMode == Blue then
                Html.span [ classNames ["g-indicator"] ] [ Html.text "g" ]
              else
                Html.span [] [] -- Empty span
            ]
        , Html.span [ classNames ["main-lcd-text"] ] [ Html.text model.displayString ]
        ]

all_rows_divs : Model -> List (Html Msg)
all_rows_divs model =
    (first_row_divs model) ++ (second_row_divs model) ++ (third_row_divs model) ++ (fourth_row_divs model) ++ [ enter_button_div model ]


button_divs : Model -> Html Msg
button_divs model =
    div
        [ classNames [ "calculator" ] 
        ]
        (lcdDisplayDiv model :: all_rows_divs model) -- Prepend LCD display to other calculator elements


empty_br_node : Html Msg
empty_br_node =
    br [] []



-- upgrade to show a beautifully labeled tzyx stack and last_x


stack_registers_div : Model -> Html Msg
stack_registers_div model =
    div
        [ classNames [ "calc_model" ]
        , Html.Attributes.style [ ( "left", "0px" ), ( "top", "574px" ), ( "position", "absolute" ) ]
        ]
        -- Removed "display: " ++ model.displayString
        (List.intersperse empty_br_node (List.map text (String.split "," (toString model.automaticMemoryStackRegisters))))


input_queue_div : Model -> Html Msg
input_queue_div model =
    div
        [ classNames [ "calc_model" ]
        , Html.Attributes.style [ ( "left", "610px" ), ( "top", "0px" ), ( "position", "absolute" ) ]
        ]
        -- Removed "display: " ++ model.displayString
        (List.intersperse empty_br_node (List.map text (String.split "," (toString model.inputQueue))))


financial_registers_div : Model -> Html Msg
financial_registers_div model =
    div
        [ classNames [ "calc_model" ]
        , Html.Attributes.style [ ( "left", "400px" ), ( "top", "574px" ), ( "width", "400" ), ( "position", "absolute" ) ]
        ]
        (model.financialRegisters
            |> Basics.toString
            |> String.split ","
            |> List.take 5
            |> List.map Html.text
            |> List.intersperse empty_br_node
         -- List.intersperse  empty_br_node ( List.map text ( String.split "," ( toString model.financialRegisters ) )  )
        )



-- upgrade this to show detailed computation state info


modelinfodiv : Model -> Html Msg
modelinfodiv model =
    div
        [ classNames [ "calc_model" ]
        , Html.Attributes.style [ ( "left", "0px" ), ( "top", "434px" ), ( "position", "absolute" ) ]
        ]
        [ Html.text (" Current Key       : " ++ model.message ++ " pressed ")
        , empty_br_node
        , Html.text (" Mode for next key : " ++ toString model.inputMode)
        ]


-- NLP View Components

nlpInputSection : Model -> Html Msg
nlpInputSection model =
    div [ classNames [ "nlp-section" ], style [ ( "margin-bottom", "20px" ) ] ]
        [ Html.input
            [ type_ "text"
            , id "nlp-input" -- Corresponds to index.html
            , placeholder "e.g., 2 plus 2"
            , value model.nlpInputString
            , onInput NLPInput -- Sends the current value of the input field
            , style [ ( "margin-right", "10px" ), ( "padding", "5px" ) ]
            ]
            []
        , button
            [ id "nlp-execute" -- Corresponds to index.html
            , onClick ProcessNLP
            , style [ ( "padding", "5px" ) ]
            ]
            [ text "Translate & Execute" ]
        ]

nlpDisplaySection : Model -> Html Msg
nlpDisplaySection model =
    div [ classNames [ "nlp-display-section" ] ]
        [ div
            [ id "rpn-commands-display" -- Corresponds to index.html
            , style [ ( "margin-top", "20px" ), ( "padding", "10px" ), ( "border", "1px solid #ccc" ), ( "min-height", "30px" ) ]
            ]
            [ text model.nlpRpnCommands ] -- Changed from String.join to direct use of model.nlpRpnCommands
        , div
            [ id "rpn-result-display" -- Corresponds to index.html
            , style [ ( "margin-top", "10px" ), ( "padding", "10px" ), ( "border", "1px solid #ccc" ), ( "font-weight", "bold" ), ( "min-height", "30px" ) ]
            ]
            [ text model.nlpResultString ]
        ]


divs_to_show : Model -> List (Html Msg)
divs_to_show model =
    [ stylesheet ] 
    ++ [ nlpInputSection model ] -- NLP input fields
    ++ [ button_divs model ] 
    ++ [ nlpDisplaySection model ] -- NLP display areas
    ++ [ modelinfodiv model ] 
    ++ [ stack_registers_div model ] 
    ++ [ financial_registers_div model ] 
    ++ [ input_queue_div model ]


view : Model -> Html Msg
view model =
    div
        []
        (divs_to_show model)

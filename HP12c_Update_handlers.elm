module HP12c_Update_handlers exposing (..)

import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)
import HP12c_Update_utils exposing (..)


--import Keyboard


type UpdateHandler
    = UpdateHandler (Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg ))


chainHandler : Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg )
chainHandler msg model handlers =
    let
        firstHandler =
            List.head handlers

        remainingHandlers =
            List.tail handlers
    in
        case ( firstHandler, remainingHandlers ) of
            ( Just (UpdateHandler handler), Just rest_handler ) ->
                handler msg model rest_handler

            ( Just (UpdateHandler handler), _ ) ->
                handler msg model []

            ( Nothing, _ ) ->
                ( model, Cmd.none )


handleDigitKeys : Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg )
handleDigitKeys msg model nonDigitHandlers =
    let
        ( digit, handled ) =
            case msg of
                Number_7_Key ->
                    ( 7, True )

                Number_8_Key ->
                    ( 8, True )

                Number_9_Key ->
                    ( 9, True )

                Number_4_Key ->
                    ( 4, True )

                Number_5_Key ->
                    ( 5, True )

                Number_6_Key ->
                    ( 6, True )

                Number_1_Key ->
                    ( 1, True )

                Number_2_Key ->
                    ( 2, True )

                Number_3_Key ->
                    ( 3, True )

                Number_0_Key ->
                    ( 0, True )

                _ ->
                    ( -1, False )
    in
        if (handled) then
            let
                newModel =
                    handleDigitInput digit model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )
        else
            chainHandler msg model nonDigitHandlers


handleSetPrecisionKeys : Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg )
handleSetPrecisionKeys msg model remainingHandlers =
    let
        ( digit, handled ) =
            case msg of
                SetPrecision_7_Key ->
                    ( 7, True )

                SetPrecision_8_Key ->
                    ( 8, True )

                SetPrecision_9_Key ->
                    ( 9, True )

                SetPrecision_4_Key ->
                    ( 4, True )

                SetPrecision_5_Key ->
                    ( 5, True )

                SetPrecision_6_Key ->
                    ( 6, True )

                SetPrecision_1_Key ->
                    ( 1, True )

                SetPrecision_2_Key ->
                    ( 2, True )

                SetPrecision_3_Key ->
                    ( 3, True )

                SetPrecision_0_Key ->
                    ( 0, True )

                SetDisplayScientific_Key ->
                    ( 10, True )

                _ ->
                    ( -1, False )
    in
        if (handled) then
            let
                newModel =
                    update_Display_Precision digit model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )
        else
            chainHandler msg model remainingHandlers


handleUnaryOperatorKeys : Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg )
handleUnaryOperatorKeys msg model remainingHandlers =
    let
        ( unary_op, handled ) =
            case msg of
                LN_Key ->
                    ( natural_log, True )

                Square_Root_Key ->
                    ( square_root, True )

                Reciprocal_Key ->
                    ( reciprocal, True )

                E_to_the_x_Key ->
                    ( e_to_the_x, True )

                FRAC_Key ->
                    ( fractional_part, True )

                INTG_Key ->
                    ( integral_part, True )

                X_Squared_Key ->
                    ( x_squared, True )

                N_Factorial_Key ->
                    ( n_factorial, True )

                RND_Key ->
                    ( round_function model.displayPrecision, True )

                _ ->
                    ( Basics.identity, False )
    in
        if (handled) then
            let
                newModel =
                    unaryOperator unary_op model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )
        else
            chainHandler msg model remainingHandlers


handleBinaryOperatorKeys : Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg )
handleBinaryOperatorKeys msg model remainingHandlers =
    let
        ( binary_op, handled ) =
            case msg of
                Divide_Key ->
                    ( y_divided_by_x, True )

                Y_toThe_X_Key ->
                    ( y_to_the_x, True )

                Multiply_Key ->
                    ( y_times_x, True )

                Subtract_Key ->
                    ( y_minus_x, True )

                Sum_Key ->
                    ( y_plus_x, True )

                _ ->
                    ( y_plus_x, False )
    in
        if (handled) then
            let
                newModel =
                    binaryOperator binary_op model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )
        else
            chainHandler msg model remainingHandlers


defaultHandler : Msg -> Model -> List UpdateHandler -> ( Model, Cmd Msg )
defaultHandler msg model remainingHandlers =
    case msg of
        _ ->
            ( { model | message = "UNHANDLED!!" ++ Basics.toString msg }, Cmd.none )


newUpdate : Msg -> Model -> ( Model, Cmd Msg )
newUpdate msg model =
    case msg of
        KeyMsg code ->
            if code == 32 then
                -- space bar toggles keyboard shortcuts
                ( { model
                    | keyCode = code
                    , shortcutVisible = not model.shortcutVisible
                    , message = "Space bar pressed : " ++ Basics.toString (model.shortcutVisible)
                  }
                , Cmd.none
                )
            else
                ( { model | keyCode = code }, Cmd.none )

        Roll_Down_Key ->
            let
                rolledStackModel =
                    roll_Down_Stack model
            in
                ( { rolledStackModel | message = Basics.toString msg }, Cmd.none )

        Exchange_X_Y_Key ->
            let
                newModel =
                    exchange_X_Y_Regs model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )

        CLEAR_REG_Key ->
            let
                newModel =
                    clearAllRegisters model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )

        CLEAR_PREFIX_Key ->
            ( { model
                | message = Basics.toString msg
                , inputMode = White
              }
            , Cmd.none
            )

        Enter_Key ->
            let
                newModel =
                    numericalInputTerminated model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )

        BackSpace_Key ->
            let
                newModel =
                    backSpaceReg_X model
            in
                ( { newModel | message = Basics.toString msg }, Cmd.none )

        _ ->
            List.map UpdateHandler [ handleDigitKeys, handleSetPrecisionKeys, handleUnaryOperatorKeys, handleBinaryOperatorKeys, defaultHandler ]
                |> chainHandler msg model

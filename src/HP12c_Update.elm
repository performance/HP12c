module HP12c_Update exposing (..)

import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)
import HP12c_Update_utils exposing (..)
import NaturalLanguageParser exposing (parse, beautifyRpnCommands) -- Updated import


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handler =
            case msg of
                NLPInput newString ->
                    \m -> { m | nlpInputString = newString }

                ProcessNLP ->
                    \m ->
                        let
                            parsedCmdsList = parse m.nlpInputString
                            rpnCommandsAsString = List.map Basics.toString parsedCmdsList
                        in
                        ( { m | nlpRpnCommands = rpnCommandsAsString, nlpResultString = "Executing..." }, Cmd.msg (ExecuteNLPCommands parsedCmdsList) )

                ExecuteNLPCommands cmdsList ->
                    \m ->
                        let
                            -- Apply each command in the list to the model sequentially
                            -- Note: update returns (Model, Cmd Msg). We'll ignore Cmds from individual steps for now.
                            processedModel =
                                List.foldl
                                    (\cmd currentModel -> Tuple.first (update cmd currentModel))
                                    m -- Start with the current model
                                    cmdsList
                        in
                        { processedModel | nlpResultString = processedModel.displayString }

                KeyMsg code ->
                    handleKeyCode code

                N_Key ->
                    defaultModelTransformer

                I_Key ->
                    defaultModelTransformer

                PV_Key ->
                    defaultModelTransformer

                PMT_Key ->
                    defaultModelTransformer

                FV_Key ->
                    defaultModelTransformer

                Times_12_Key ->
                    defaultModelTransformer

                DIVIDE_BY_12_Key ->
                    defaultModelTransformer

                CF_0_Key ->
                    defaultModelTransformer

                CF_j_Key ->
                    defaultModelTransformer

                N_j_Key ->
                    defaultModelTransformer

                AMORT_Key ->
                    defaultModelTransformer

                INT_Key ->
                    defaultModelTransformer

                NPV_Key ->
                    defaultModelTransformer

                IRR_Key ->
                    defaultModelTransformer

                DATE_Key ->
                    defaultModelTransformer

                BEG_Key ->
                    defaultModelTransformer

                END_Key ->
                    defaultModelTransformer

                Delta_Days_Key ->
                    defaultModelTransformer

                D_MY_Key ->
                    defaultModelTransformer

                M_DY_Key ->
                    defaultModelTransformer

                RunMode_Key ->
                    defaultModelTransformer

                GTO_Key ->
                    defaultModelTransformer

                PSE_Key ->
                    defaultModelTransformer

                BST_Key ->
                    defaultModelTransformer

                SST_Key ->
                    defaultModelTransformer

                Program_Mode_Key ->
                    defaultModelTransformer

                CL_x_Key ->
                    clearXRegister

                CLEAR_Σ_Key ->
                    clearSigma

                CLEAR_PRGM_Key ->
                    clearProgramMemory

                CLEAR_FIN_Key ->
                    clearFinancialRegisters

                CLEAR_REG_Key ->
                    clearAllRegisters

                CLEAR_PREFIX_Key ->
                    clearPrefix

                -- TODO: easing for display
                STO_Key ->
                    defaultModelTransformer

                RCL_Key ->
                    defaultModelTransformer

                MEM_Key ->
                    defaultModelTransformer

                Divide_Key ->
                    binaryOperator y_divided_by_x

                Y_toThe_X_Key ->
                    binaryOperator y_to_the_x

                Multiply_Key ->
                    binaryOperator y_times_x

                Subtract_Key ->
                    binaryOperator y_minus_x

                Sum_Key ->
                    binaryOperator y_plus_x

                LN_Key ->
                    unaryOperator natural_log

                Square_Root_Key ->
                    unaryOperator square_root

                Reciprocal_Key ->
                    unaryOperator reciprocal

                E_to_the_x_Key ->
                    unaryOperator e_to_the_x

                FRAC_Key ->
                    unaryOperator fractional_part

                INTG_Key ->
                    unaryOperator integral_part

                X_Squared_Key ->
                    unaryOperator x_squared

                N_Factorial_Key ->
                    unaryOperator n_factorial

                RND_Key ->
                    unaryOperator (round_function model.displayPrecision)

                PRICE_Key ->
                    defaultModelTransformer

                YTM_Key ->
                    defaultModelTransformer

                SL_Key ->
                    defaultModelTransformer

                SOYD_Key ->
                    defaultModelTransformer

                DB_Key ->
                    defaultModelTransformer

                Weighted_Mean_Key ->
                    defaultModelTransformer

                Mean_of_X_Key ->
                    defaultModelTransformer

                Std_Dev_Key ->
                    defaultModelTransformer

                Sigma_Plus_Key ->
                    defaultModelTransformer

                Sigma_Minus_Key ->
                    defaultModelTransformer

                X_lte_Y_Key ->
                    defaultModelTransformer

                X_eq_0_Key ->
                    defaultModelTransformer

                Linear_Estimate_X_Key ->
                    defaultModelTransformer

                Linear_Estimate_Y_Key ->
                    defaultModelTransformer

                RPN_Key ->
                    setComputationMode RPN_Mode

                ALG_Key ->
                    setComputationMode ALG_Mode

                Equals_Key ->
                    numericalInputTerminated

                Enter_Key ->
                    numericalInputTerminated

                BackSpace_Key ->
                    backSpaceReg_X

                Roll_Down_Key ->
                    roll_Down_Stack

                Exchange_X_Y_Key ->
                    exchange_X_Y_Regs

                Last_X_Key ->
                    last_X

                Left_Paren_Key ->
                    defaultModelTransformer

                Right_Paren_Key ->
                    defaultModelTransformer

                Number_1_Key ->
                    handleDigitInput 1

                Number_2_Key ->
                    handleDigitInput 2

                Number_3_Key ->
                    handleDigitInput 3

                Number_4_Key ->
                    handleDigitInput 4

                Number_5_Key ->
                    handleDigitInput 5

                Number_6_Key ->
                    handleDigitInput 6

                Number_7_Key ->
                    handleDigitInput 7

                Number_8_Key ->
                    handleDigitInput 8

                Number_9_Key ->
                    handleDigitInput 9

                Number_0_Key ->
                    handleDigitInput 0

                Decimal_Point_Key ->
                    handleDecimalPoint

                EEX_Key ->
                    handleEEX_Key

                CHS_Key ->
                    handle_CHS_Key

                ON_Key ->
                    handlePOWERONKey

                OFF_Key ->
                    handlePOWERONKey

                Percentage_T_Key ->
                    binaryOperator_No_Down_Shift percentage_of_total

                Delta_Percentage_Key ->
                    binaryOperator_No_Down_Shift delta_percentage

                Percent_Key ->
                    binaryOperator_No_Down_Shift x_percent_of_y

                SetPrecision_0_Key ->
                    update_Display_Precision 0

                SetPrecision_1_Key ->
                    update_Display_Precision 1

                SetPrecision_2_Key ->
                    update_Display_Precision 2

                SetPrecision_3_Key ->
                    update_Display_Precision 3

                SetPrecision_4_Key ->
                    update_Display_Precision 4

                SetPrecision_5_Key ->
                    update_Display_Precision 5

                SetPrecision_6_Key ->
                    update_Display_Precision 6

                SetPrecision_7_Key ->
                    update_Display_Precision 7

                SetPrecision_8_Key ->
                    update_Display_Precision 8

                SetPrecision_9_Key ->
                    update_Display_Precision 9

                SetDisplayScientific_Key ->
                    update_Display_Precision 10

                Orange_F_Key ->
                    setPrefix Orange

                Blue_G_Key ->
                    setPrefix Blue

                Undo_Key ->
                    defaultModelTransformer

        (updatedModelFromHandler, cmdFromHandler) =
            -- Special handling for ProcessNLP as it returns a command directly
            if msg == ProcessNLP then
                case handler model of
                    ( m, cmd ) -> ( m, cmd ) -- This pattern is a bit off for ProcessNLP's lambda
                    _ -> (model, Cmd.none) -- Should not happen if handler for ProcessNLP is correct
            else if case msg of ExecuteNLPCommands _ -> True; _ -> False then
                 (handler model, Cmd.none) -- ExecuteNLPCommands lambda returns Model directly
            else if case msg of NLPInput _ -> True; _ -> False then
                 (handler model, Cmd.none) -- NLPInput lambda returns Model directly
            else
                (handler model, Cmd.none) -- Default for simple model transformers

        -- Refactored logic for ProcessNLP to fit the structure
        finalModelAndCmd =
            case msg of
                ProcessNLP ->
                    let
                        parsedCmdsList = parse model.nlpInputString
                        -- Use the new beautifyRpnCommands function
                        beautifiedRpnString = beautifyRpnCommands parsedCmdsList
                    in
                    ( { model | nlpRpnCommands = beautifiedRpnString, nlpResultString = "Executing..." }
                    , Cmd.msg (ExecuteNLPCommands parsedCmdsList)
                    )

                ExecuteNLPCommands cmdsList ->
                    let
                        processedModel =
                            List.foldl
                                (\cmd currentModel -> Tuple.first (update cmd currentModel))
                                model
                                cmdsList
                    in
                    ( { processedModel | nlpResultString = processedModel.displayString }, Cmd.none )
                
                NLPInput newString ->
                    ( { model | nlpInputString = newString }, Cmd.none )

                _ ->
                    -- Original handler logic for all other messages
                    let
                        -- The 'handler model' call executes the specific function for the message,
                        -- which might now return a model already in an error state (with displayString="Error"
                        -- and a specific message set by setErrorState or defaultModelTransformer).
                        processedModelByHandler = handler model
                        
                        -- If the handler did not set a specific error message,
                        -- and it's not an NLP message (which have their own message/result fields),
                        -- then we might set a default message based on the action.
                        -- However, setErrorState and defaultModelTransformer now handle their own messages.
                        -- So, we mostly preserve the message from the handler.
                        -- The previous logic for 'unimplemented' is removed as the field is gone.
                        finalMessage =
                            case processedModelByHandler.calculatorOperationalState of
                                Error _ ->
                                    processedModelByHandler.message -- Preserve message from setErrorState
                                _ ->
                                    -- For non-error states, a generic message based on the key pressed can be used,
                                    -- or specific handlers can set more descriptive messages.
                                    -- For now, let's keep it simple or use what handler provided.
                                    -- If handler didn't change message, it's original model.message.
                                    -- If it did, it's processedModelByHandler.message.
                                    -- Let's assume specific handlers or setErrorState set messages appropriately.
                                    -- If no specific message was set by a simple defaultModelTransformer,
                                    -- it's now "Unimplemented Op".
                                    if processedModelByHandler.message == model.message then
                                        "Key: " ++ Basics.toString msg -- Generic message if not changed by handler
                                    else
                                        processedModelByHandler.message -- Use message from handler
                    in
                    ( { processedModelByHandler
                        | message = finalMessage -- Ensure message is correctly propagated
                        , inputQueue =
                            if processedModelByHandler.addToInputQueue then
                                msg :: processedModelByHandler.inputQueue
                            else
                                processedModelByHandler.inputQueue
                      }
                    , Cmd.none
                    )
    in
    finalModelAndCmd

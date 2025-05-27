module HP12c_Update_utils exposing (..)

import String
import Keyboard
import Formatting exposing (..)
import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)


-- Update
---------- Stack operations


liftAutomaticMemoryStack : AutomaticMemoryStackRegisters -> AutomaticMemoryStackRegisters
liftAutomaticMemoryStack currentStackRegs =
    let
        liftedStack =
            { currentStackRegs
                | reg_T = currentStackRegs.reg_Z
                , reg_Z = currentStackRegs.reg_Y
                , reg_Y = currentStackRegs.reg_X
            }
    in
        liftedStack


liftStack : Model -> Model
liftStack model =
    let
        stackRegs =
            model.automaticMemoryStackRegisters

        liftedStack =
            liftAutomaticMemoryStack stackRegs

        liftedModel =
            { model | automaticMemoryStackRegisters = liftedStack }
    in
        liftedModel


exchange_X_Y_Regs : Model -> Model
exchange_X_Y_Regs model =
    let
        stackRegs =
            model.automaticMemoryStackRegisters

        tmp_Y =
            stackRegs.reg_Y

        exchangedStack =
            { stackRegs
                | reg_Y = stackRegs.reg_X
                , reg_X = tmp_Y
            }

        promotedModel =
            { model | automaticMemoryStackRegisters = exchangedStack, addToInputQueue = True }
    in
        promotedModel


roll_Down_Stack : Model -> Model
roll_Down_Stack model =
    let
        stackRegs =
            model.automaticMemoryStackRegisters

        tmp_X =
            stackRegs.reg_X

        rolledStack =
            { stackRegs
                | reg_X = stackRegs.reg_Y
                , reg_Y = stackRegs.reg_Z
                , reg_Z = stackRegs.reg_T
                , reg_T = tmp_X
            }

        promotedModel =
            { model | automaticMemoryStackRegisters = rolledStack, addToInputQueue = True }
    in
        promotedModel



-- g LST x lifts the stack ( unless Enter, CLx Σ+ Σ-, 12× , 12÷, was the last key pressed )


last_X : Model -> Model
last_X model =
    let
        lift_blockers =
            [ Enter_Key, CL_x_Key, Sigma_Plus_Key, Sigma_Minus_Key, Times_12_Key, DIVIDE_BY_12_Key ]

        last_key_input =
            List.head model.inputQueue

        ( should_we_even_lift_bro, dbgmsg ) =
            case last_key_input of
                Just test_this_key ->
                    let
                        filtered_blockers =
                            List.filter (\x -> x == test_this_key) lift_blockers

                        flag =
                            List.isEmpty <| filtered_blockers

                        msg =
                            " TEST:---> " ++ Basics.toString test_this_key ++ Basics.toString flag ++ " <---:" ++ Basics.toString filtered_blockers
                    in
                        ( flag, msg )

                Nothing ->
                    ( False, "No Head " )

        maybeLiftedModel =
            if should_we_even_lift_bro then
                liftStack model
            else
                model

        stackRegs =
            maybeLiftedModel.automaticMemoryStackRegisters

        maybePromotedStack =
            { stackRegs
                | reg_X = stackRegs.reg_Last_X
            }

        maybePromotedModel =
            { maybeLiftedModel
                | automaticMemoryStackRegisters = maybePromotedStack
                , inputMode = White
                , scratchRegisters = initializeScratchRegisters
                , addToInputQueue = True
                , message = dbgmsg
            }

        newModel =
            update_Display_Precision model.displayPrecision maybePromotedModel
    in
        newModel



---------- Stack operations
---------- unaryOperators


reciprocal : Float -> Result String Float
reciprocal x =
    if x == 0 then
        Err "DIV BY ZERO" -- Specific error message for model.message
    else
        Ok (1 / x)

x_squared : Float -> Result String Float
x_squared x =
    Ok (x * x) -- Squaring typically doesn't error for real numbers unless overflow is considered

square_root : Float -> Result String Float
square_root x =
    if x < 0 then
        Err "SQRT OF NEG"
    else
        Ok (Basics.sqrt x)

natural_log : Float -> Result String Float
natural_log x =
    if x <= 0 then
        Err "LOG OF NON-POS"
    else
        Ok (Basics.logBase e x)

e_to_the_x : Float -> Result String Float
e_to_the_x x =
    Ok (e ^ x) -- e^x is defined for all real x, typically doesn't error unless overflow

-- Internal helper for n_factorial and integral_part checks
fractional_part_internal_check : Float -> Float
fractional_part_internal_check x = x - (Basics.toFloat (Basics.truncate x))

n_factorial : Float -> Result String Float
n_factorial x =
    if x < 0 || fractional_part_internal_check x /= 0 then
        Err "FACTORIAL DOMAIN"
    else if x > 170 then -- Approximation: 170! is around max float64
        Err "FACTORIAL OVERFLOW"
    else
        let
            n = Basics.floor x
        in
        Ok (List.product (List.range 1 n) |> Basics.toFloat)

-- Helper function to set the calculator into an error state
setErrorState : ErrorState -> String -> Model -> Model
setErrorState errorType errorMsg model =
    { model
        | calculatorOperationalState = Error errorType
        , displayString = "Error" -- Set displayString to "Error"
        , message = errorMsg -- Keep detailed message in model.message
        , inputMode = White -- Typically reset input mode on error
        , scratchRegisters = initializeScratchRegisters -- Clear scratch/entry on error
    }


-- in the hp12c platinum, the round function rounds the number to
-- the number of decimals sepcified in the display precision


round_function : Int -> Float -> Result String Float
round_function n x =
    -- Elm's rounding is well-behaved. Errors would typically be for NaN/Infinity inputs,
    -- which should ideally be caught before this or result from previous errors.
    if isNaN x || isInfinite x then
        Err "INVALID INPUT"
    else
        let
            scaler = 10 ^ Basics.toFloat n
        in
        Ok (Basics.toFloat (Basics.round (x * scaler)) / scaler)


integral_part : Float -> Result String Float
integral_part x =
    if isNaN x || isInfinite x then
        Err "INVALID INPUT"
    else
        Ok (Basics.toFloat (Basics.truncate x))

fractional_part : Float -> Result String Float
fractional_part x =
    if isNaN x || isInfinite x then
        Err "INVALID INPUT"
    else
        Ok (x - Basics.toFloat (Basics.truncate x))


unaryOperator : (Float -> Result String Float) -> Model -> Model
unaryOperator op model =
    -- First, check if the calculator is already in an error state.
    -- If so, most operations should not proceed or should clear error first.
    -- For now, let's assume operations are blocked if in error state.
    case model.calculatorOperationalState of
        Error _ ->
            model -- Do nothing if already in error state (or clear error, TBD)

        _ ->
            let
                stackRegs = model.automaticMemoryStackRegisters
                opResult = op stackRegs.reg_X
            in
            case opResult of
                Ok result ->
                    let
                        unReducededStack =
                            { stackRegs
                                | reg_Last_X = stackRegs.reg_X
                                , reg_X = result
                            }
                        scratchRegs = model.scratchRegisters
                        unReducedModel =
                            { model
                                | automaticMemoryStackRegisters = unReducededStack
                                , inputMode = White
                                , scratchRegisters = { scratchRegs | acceptNewDigitInto = NewNumber }
                                , addToInputQueue = True
                                , calculatorOperationalState = AcceptingOperationsOrNumbers -- Ensure state is normal
                            }
                    in
                    update_Display_Precision model.displayPrecision unReducedModel

                Err errorMsg ->
                    -- Use Error_0_Mathematics for general math errors from unary ops
                    setErrorState Error_0_Mathematics ("Error 0 " ++ errorMsg) model


---------- unaryOperators
-----------  Binary operators lhs is reg_y, rhs is reg_x


y_to_the_x : Float -> Float -> Result String Float
y_to_the_x y x =
    -- Could add checks for 0^0 (conventionally 1 or error) or x^y resulting in complex if y is non-integer and x is negative.
    -- For HP12c, y^x typically works for positive y. If y is negative, x must be integer.
    -- Elm's (^) operator handles negative bases if exponent is integer, otherwise NaN.
    let
        result = y ^ x
    in
    if isNaN result then Err "DOMAIN ERROR Y^X" else Ok result

y_plus_x : Float -> Float -> Result String Float
y_plus_x y x = Ok (y + x)

y_minus_x : Float -> Float -> Result String Float
y_minus_x y x = Ok (y - x)

y_times_x : Float -> Float -> Result String Float
y_times_x y x = Ok (y * x)

y_divided_by_x : Float -> Float -> Result String Float
y_divided_by_x y x =
    if x == 0 then
        Err "DIV BY ZERO"
    else
        Ok (y / x)

x_percent_of_y : Float -> Float -> Result String Float
x_percent_of_y y x = Ok (y * x / 100) -- Standard percentage, unlikely to error unless inputs are non-finite

delta_percentage : Float -> Float -> Result String Float
delta_percentage y x =
    if y == 0 then
        Err "DIV BY ZERO IN %Δ"
    else
        Ok (100 * (x - y) / y)

percentage_of_total : Float -> Float -> Result String Float
percentage_of_total y x =
    if y == 0 then
        Err "DIV BY ZERO IN %T"
    else
        Ok (100 * x / y)


binaryOperator_No_Down_Shift : (Float -> Float -> Result String Float) -> Model -> Model
binaryOperator_No_Down_Shift op model =
    case model.calculatorOperationalState of
        Error _ -> model 
        _ ->
            let
                stackRegs = model.automaticMemoryStackRegisters
                opResult = op stackRegs.reg_Y stackRegs.reg_X
            in
            case opResult of
                Ok result ->
                    let
                        unPromotedStack =
                            { stackRegs
                                | reg_Last_X = stackRegs.reg_X
                                , reg_X = result
                            }
                        promotedModel =
                            { model
                                | automaticMemoryStackRegisters = unPromotedStack
                                , inputMode = White
                                , addToInputQueue = True
                                , calculatorOperationalState = AcceptingOperationsOrNumbers
                            }
                    in
                    update_Display_Precision model.displayPrecision promotedModel
                Err errorMsg ->
                    setErrorState Error_0_Mathematics ("Error 0 " ++ errorMsg) model


binaryOperator : (Float -> Float -> Result String Float) -> Model -> Model
binaryOperator op model =
    case model.calculatorOperationalState of
        Error _ -> model
        _ ->
            let
                stackRegs = model.automaticMemoryStackRegisters
                opResult = op stackRegs.reg_Y stackRegs.reg_X
            in
            case opResult of
                Ok result ->
                    let
                        reducedStack =
                            { stackRegs
                                | reg_T = stackRegs.reg_T
                                , reg_Z = stackRegs.reg_T
                                , reg_Y = stackRegs.reg_Z
                                , reg_Last_X = stackRegs.reg_X
                                , reg_X = result
                            }
                        scratchRegs = model.scratchRegisters
                        promotedModel =
                            { model
                                | automaticMemoryStackRegisters = reducedStack
                                , inputMode = White
                                , scratchRegisters = { scratchRegs | acceptNewDigitInto = NewNumber, reg_X_is_Positive = True }
                                , addToInputQueue = True
                                , calculatorOperationalState = AcceptingOperationsOrNumbers
                            }
                    in
                    update_Display_Precision model.displayPrecision promotedModel
                Err errorMsg ->
                    setErrorState Error_0_Mathematics ("Error 0 " ++ errorMsg) model

-----------  Binary operators lhs is reg_y, rhs is reg_x


setComputationMode : ComputationMode -> Model -> Model
setComputationMode computationMode model =
    { model | computationMode = computationMode, addToInputQueue = False }


until_evaluate_X_register : ScratchRegisters -> Float
until_evaluate_X_register modelScratchRegs =
    let
        decimal_place =
            (10 ^ (modelScratchRegs.number_of_decimals + 1))

        sign_of_X =
            if (modelScratchRegs.reg_X_is_Positive) then
                1
            else
                -1

        newReg_X =
            sign_of_X
                * (Basics.toFloat modelScratchRegs.integral_part_of_X)
                + ((Basics.toFloat modelScratchRegs.fractional_part_of_X)
                    / (Basics.toFloat decimal_place)
                  )

        newReg_X_with_EEX =
            newReg_X * (10 ^ (Basics.toFloat modelScratchRegs.exponent_for_EEX))

        reg_X_to_Use =
            if (modelScratchRegs.acceptNewDigitInto == ExponentForEEX) then
                newReg_X_with_EEX
            else
                newReg_X
    in
        reg_X_to_Use



-- TODO: change this to handle STO RCL GTO etc


handleDigitInput : Int -> Model -> Model
handleDigitInput newDigit model =
    let
        stackRegs =
            model.automaticMemoryStackRegisters

        scratchRegs =
            model.scratchRegisters

        decimal_place =
            (10 ^ (scratchRegs.number_of_decimals + 1))

        sign_of_X =
            if (scratchRegs.reg_X_is_Positive) then
                1
            else
                -1

        ( newScratchRegs, newStackRegs ) =
            case scratchRegs.acceptNewDigitInto of
                FractionalPart ->
                    let
                        newScratchRegs =
                            { scratchRegs
                                | fractional_part_of_X = (10 * scratchRegs.fractional_part_of_X + newDigit)
                                , number_of_decimals = scratchRegs.number_of_decimals + 1
                            }

                        newReg_X =
                            sign_of_X * (Basics.toFloat newScratchRegs.integral_part_of_X) + ((Basics.toFloat newScratchRegs.fractional_part_of_X) / (Basics.toFloat decimal_place))

                        newStackRegs =
                            { stackRegs | reg_X = newReg_X }
                    in
                        ( newScratchRegs, newStackRegs )

                IntegralPart ->
                    let
                        newScratchRegs =
                            { scratchRegs
                                | fractional_part_of_X = 0
                                , integral_part_of_X = (10 * scratchRegs.integral_part_of_X + newDigit)
                            }

                        newReg_X =
                            sign_of_X * (Basics.toFloat newScratchRegs.integral_part_of_X) + ((Basics.toFloat newScratchRegs.fractional_part_of_X) / (Basics.toFloat decimal_place))

                        newStackRegs =
                            { stackRegs | reg_X = newReg_X }
                    in
                        ( newScratchRegs, newStackRegs )

                NewNumber ->
                    let
                        newScratchRegs =
                            { scratchRegs
                                | fractional_part_of_X = 0
                                , integral_part_of_X = newDigit
                                , acceptNewDigitInto = IntegralPart
                            }

                        liftedStack =
                            liftAutomaticMemoryStack stackRegs

                        newReg_X =
                            (Basics.toFloat newScratchRegs.integral_part_of_X)

                        newStackRegs =
                            { liftedStack | reg_X = newReg_X }
                    in
                        ( newScratchRegs, newStackRegs )

                ExponentForEEX ->
                    let
                        newScratchRegs =
                            { scratchRegs
                                | exponent_for_EEX = (10 * scratchRegs.exponent_for_EEX + newDigit) % 100
                            }

                        newReg_X =
                            sign_of_X * (Basics.toFloat newScratchRegs.integral_part_of_X) + ((Basics.toFloat newScratchRegs.fractional_part_of_X) / (Basics.toFloat decimal_place))

                        newReg_X_with_EEX =
                            newReg_X * (10 ^ (Basics.toFloat newScratchRegs.exponent_for_EEX))

                        newStackRegs =
                            { stackRegs | reg_X = newReg_X_with_EEX }
                    in
                        ( newScratchRegs, newStackRegs )

                STO_Reg ->
                    ( scratchRegs, stackRegs )

                -- TODO: unhandled for now
                STO_Dot_Reg ->
                    ( scratchRegs, stackRegs )

                -- TODO: unhandled for now
                RCL_Reg ->
                    ( scratchRegs, stackRegs )

                -- TODO: unhandled for now
                RCL_Dot_Reg ->
                    ( scratchRegs, stackRegs )

                -- TODO: unhandled for now
                GTO_Addrs ->
                    ( scratchRegs, stackRegs )

        -- TODO: unhandled for now
        newPrecision =
            if
                (scratchRegs.acceptNewDigitInto == ExponentForEEX)
                -- && if magnitude of reg_X is > 10 ^ 10
            then
                10
            else
                model.displayPrecision

        updatedModel =
            { model
                | automaticMemoryStackRegisters = newStackRegs
                , scratchRegisters = newScratchRegs
                , addToInputQueue = True
                , displayPrecision = newPrecision
                , message = "[>>> sign_of_X is " ++ Basics.toString sign_of_X ++ " <<<] "
            }

        -- TODO: for now EEX always shows in scientific
        newModel =
            update_Display_Precision newPrecision updatedModel
    in
        newModel



-- TODO: change this to handle STO RCL GTO etc


handleDecimalPoint : Model -> Model
handleDecimalPoint model =
    let
        modelScratchRegs =
            model.scratchRegisters

        newScratchRegs =
            { modelScratchRegs | acceptNewDigitInto = FractionalPart }

        newModel =
            { model | scratchRegisters = newScratchRegs, addToInputQueue = True }
    in
        newModel



-- TODO: change this to handle STO RCL GTO etc


handle_CHS_Key : Model -> Model
handle_CHS_Key model =
    let
        modelScratchRegs =
            model.scratchRegisters

        newScratchRegs =
            if (modelScratchRegs.acceptNewDigitInto == ExponentForEEX) then
                { modelScratchRegs | exponent_for_EEX = -1 * modelScratchRegs.exponent_for_EEX }
            else
                { modelScratchRegs | reg_X_is_Positive = not modelScratchRegs.reg_X_is_Positive }

        newReg_X =
            until_evaluate_X_register newScratchRegs

        modelStackRegs =
            model.automaticMemoryStackRegisters

        revaluatedStackRegs =
            { modelStackRegs | reg_X = newReg_X }

        toggledModel =
            { model
                | automaticMemoryStackRegisters = revaluatedStackRegs
                , scratchRegisters = newScratchRegs
                , addToInputQueue = True
                , message = "[>>> out of CHS handler sign_of_X is " ++ Basics.toString newScratchRegs.reg_X_is_Positive ++ " <<<] "
            }

        newModel =
            update_Display_Precision model.displayPrecision toggledModel
    in
        newModel



-- TODO: change this to handle STO RCL GTO etc


handleEEX_Key : Model -> Model
handleEEX_Key model =
    let
        modelScratchRegs =
            model.scratchRegisters

        newScratchRegs =
            { modelScratchRegs | acceptNewDigitInto = ExponentForEEX }

        newModel =
            { model | scratchRegisters = newScratchRegs, addToInputQueue = True }
    in
        newModel


numericalInputTerminated : Model -> Model
numericalInputTerminated model =
    let
        liftedModel =
            liftStack model

        stackRegs =
            liftedModel.automaticMemoryStackRegisters

        lastXUpdatedStack =
            { stackRegs | reg_Last_X = stackRegs.reg_X }
    in
        { liftedModel
            | scratchRegisters = initializeScratchRegisters
            , automaticMemoryStackRegisters = lastXUpdatedStack
        }


backSpaceReg_X : Model -> Model
backSpaceReg_X model =
    let
        stackRegs =
            model.automaticMemoryStackRegisters

        newStackRegs =
            { stackRegs | reg_X = toFloat (floor (stackRegs.reg_X / 10)) }

        newModel =
            { model | automaticMemoryStackRegisters = newStackRegs, addToInputQueue = True }
    in
        newModel


clearAllRegisters : Model -> Model
clearAllRegisters model =
    let
        -- Clearing all registers should also clear any error state and reset display.
        cleared_model_state =
            { model
                | automaticMemoryStackRegisters = initializeAutomaticMemoryStackRegisters
                , dataStorageRegisters = initializeDataStorageRegisters
                , statisticalRegisters = initializeStatisticalRegisters
                , financialRegisters = initializeFinancialRegisters
                , scratchRegisters = initializeScratchRegisters
                , calculatorOperationalState = AcceptingOperationsOrNumbers -- Reset error state
                , message = "All Clear" 
                , inputMode = White
                , addToInputQueue = True
            }
        -- Set displayString to "0.00" (or based on current precision) after clearing.
        -- Need to ensure reg_X is 0 in initializeAutomaticMemoryStackRegisters for this to be correct.
        -- initializeAutomaticMemoryStackRegisters sets reg_X = 0.
    in
    update_Display_Precision model.displayPrecision { cleared_model_state | displayString = "0.00" } -- Tentatively set displayString, update_Display_Precision will format based on new reg_X


clearFinancialRegisters : Model -> Model
clearFinancialRegisters model =
    let
        cleared_model_state =
            { model
                | financialRegisters = initializeFinancialRegisters
                , scratchRegisters = initializeScratchRegisters
                , calculatorOperationalState = AcceptingOperationsOrNumbers
                , message = "Financial Clear"
                , inputMode = White
                , addToInputQueue = True
            }
    in
    update_Display_Precision model.displayPrecision { cleared_model_state | displayString = "0.00" }


clearProgramMemory : Model -> Model
clearProgramMemory model =
    if (model.computationMode == PRGM_MODE) then
        { model
            | programMemory = initializeProgramMemory
            , calculatorOperationalState = AcceptingOperationsOrNumbers -- Clear error if in PRGM mode
            , message = "Program Memory Cleared"
            , inputMode = White
            , addToInputQueue = True
        }
        -- displayString should probably remain unchanged or show 0.00 if PRGM clear also clears X
        -- For now, assume it doesn't affect displayString directly, but clears error state.
    else
        model


clearPrefix : Model -> Model
clearPrefix model =
    -- Clearing a prefix (f, g, STO) should not affect an existing error message on display.
    -- It should only reset the inputMode.
    if model.calculatorOperationalState == Error Error_9_Service && model.message == "Unimplemented Op" then
        -- If the error was due to an unimplemented prefixed key, clearing prefix might make sense to clear this specific error.
        -- However, general rule: clearPrefix itself does not clear general errors.
        { model | inputMode = White, addToInputQueue = True, message = "Prefix Cleared" }
    else
        { model | inputMode = White, addToInputQueue = True }


clearSigma : Model -> Model
clearSigma model =
    let
        cleared_model_state =
            { model
                | automaticMemoryStackRegisters = initializeAutomaticMemoryStackRegisters -- As per HP manual, CLΣ clears stack
                , statisticalRegisters = initializeStatisticalRegisters
                , scratchRegisters = initializeScratchRegisters
                , calculatorOperationalState = AcceptingOperationsOrNumbers
                , message = "Sigma Cleared"
                , inputMode = White
                , addToInputQueue = True
            }
    in
    update_Display_Precision model.displayPrecision { cleared_model_state | displayString = "0.00" }


clearXRegister : Model -> Model
clearXRegister model =
    let
        stackRegs = model.automaticMemoryStackRegisters
        newStackRegs = { stackRegs | reg_X = 0 }
        cleared_model_state =
            { model
                | automaticMemoryStackRegisters = newStackRegs
                , scratchRegisters = initializeScratchRegisters -- CLx also initializes scratch for new number entry
                , calculatorOperationalState = AcceptingOperationsOrNumbers -- Clearing X register clears error state
                , message = "X Cleared"
                , inputMode = White
                , addToInputQueue = True
            }
    in
    update_Display_Precision model.displayPrecision { cleared_model_state | displayString = "0.00" }


update_Display_Precision : Int -> Model -> Model
update_Display_Precision n model =
    case model.calculatorOperationalState of
        Error _ ->
            model -- Do not change displayString if already in error state

        _ ->
            if (n < 10) then
                update_Display_Precision_util n model
            else
                update_Display_to_Scientific model


update_Display_Precision_util : Int -> Model -> Model
update_Display_Precision_util n model =
    -- This function should not be called if model is in Error state,
    -- but as a safeguard, or if called directly:
    case model.calculatorOperationalState of
        Error _ ->
            model
        _ ->
    let
        displayPrecision =
            n

        stackRegs =
            model.automaticMemoryStackRegisters

        reg_X =
            stackRegs.reg_X

        newDisplayString =
            (print (Formatting.roundTo displayPrecision) reg_X)

            let
                displayPrecision = n
                stackRegs = model.automaticMemoryStackRegisters
                reg_X = stackRegs.reg_X
                newDisplayString = print (Formatting.roundTo displayPrecision) reg_X
            in
            { model
                | displayString = newDisplayString
                , displayPrecision = displayPrecision
                , inputMode = White
                , calculatorOperationalState = AcceptingOperationsOrNumbers -- Ensure normal state
            }


update_Display_to_Scientific : Model -> Model
update_Display_to_Scientific model =
    case model.calculatorOperationalState of
        Error _ ->
            model
        _ ->
    let
        displayPrecision =
            10

        stackRegs =
            model.automaticMemoryStackRegisters

        reg_X =
            stackRegs.reg_X

        decimal_place =
            reg_X |> Basics.abs |> Basics.logBase 10

        sign_of_exponent =
            if (decimal_place < 0) then
                1
            else
                -1

        newExponent =
            if (reg_X < 1) then
                (decimal_place |> Basics.ceiling)
            else
                (decimal_place |> Basics.floor)

        newNumber =
            reg_X * (10 ^ (sign_of_exponent * newExponent |> Basics.toFloat))

        exp_display_part =
            print (padLeft 2 '0' int) newExponent |> String.right 2

        newDisplayString =
            (print (Formatting.roundTo 6) newNumber) ++ "  " ++ (exp_display_part)

            let
                displayPrecisionSetting = 10 -- Indicates scientific mode
                stackRegs = model.automaticMemoryStackRegisters
                reg_X = stackRegs.reg_X
                -- Simplified scientific string representation
                newDisplayString =
                    if reg_X == 0.0 then
                        "0.000000  00" -- HP12c representation of 0 in sci mode
                    else
                        -- This is a simplified representation. Real HP12c scientific format is specific.
                        -- Elm's default toString for small/large numbers might be scientific.
                        -- For now, let's use a placeholder or a basic scientific format.
                        -- Example: Basics.toString reg_X might already be "1.23456e+7"
                        -- We need to format it like "1.234567 07" (mantissa and exponent)
                        -- This requires more complex string manipulation than available directly.
                        -- Using a simplified version for now.
                        -- Using the existing logic which was an attempt at custom scientific formatting:
                        let
                            absRegX = Basics.abs reg_X
                            (mantissa, exponent) = 
                                if absRegX == 0.0 then (0.0, 0)
                                else 
                                    let exp = Basics.floor (Basics.logBase 10 absRegX)
                                    in (reg_X / (10 ^ Basics.toFloat exp), Basics.round exp)
                            
                            formattedMantissa = print (Formatting.roundTo 6) mantissa -- Display 6 decimal places for mantissa
                            formattedExponent = print (padLeft 2 '0' int) (Basics.abs exponent) |> String.right 2
                            sign = if reg_X < 0.0 then "-" else "" -- Mantissa sign handled by print
                            expSign = if exponent < 0 then "-" else " " -- Exponent sign often space or minus
                        in
                        formattedMantissa ++ expSign ++ formattedExponent
            in
            { model
                | displayString = newDisplayString
                , displayPrecision = displayPrecisionSetting 
                , inputMode = White
                , calculatorOperationalState = AcceptingOperationsOrNumbers -- Ensure normal state
            }


setPrefix : InputMode -> Model -> Model
setPrefix inputMode model =
    { model
        | inputMode = inputMode
        , addToInputQueue = False
    }


handleKeyCode : Keyboard.KeyCode -> Model -> Model
handleKeyCode code model =
    if code == 32 then
        -- space bar toggles keyboard shortcuts
        { model
            | keyCode = code
            , shortcutVisible = not model.shortcutVisible
            , addToInputQueue = False
            , message = "Space bar pressed : " ++ Basics.toString (model.shortcutVisible)
        }
    else
        { model | keyCode = code }


handlePOWERONKey : Model -> Model
handlePOWERONKey model =
    -- ON key always resets the calculator to its initial state, clearing errors.
    { initialModel | nlpInputString = model.nlpInputString, nlpRpnCommands = model.nlpRpnCommands, nlpResultString = model.nlpResultString }
    -- Preserve NLP state across ON key, as it's external to calculator core state


defaultModelTransformer : Model -> Model
defaultModelTransformer model =
    -- For unimplemented operations, use a specific error type, e.g., Error_9_Service
    -- This function will now also set displayString = "Error" via setErrorState
    setErrorState Error_9_Service "Unimplemented Op" model
    |> \m -> { m | addToInputQueue = True } -- Ensure addToInputQueue is handled as before

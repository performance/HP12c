module HP12c_Update_utils exposing (..)

import String
import Keyboard
import Formatting         exposing (..)

import HP12c_KeyTypes     exposing (..)
import HP12c_Model        exposing (..)



-- Update

---------- Stack operations 



liftAutomaticMemoryStack : AutomaticMemoryStackRegisters -> AutomaticMemoryStackRegisters
liftAutomaticMemoryStack currentStackRegs =
  let 
    liftedStack = { currentStackRegs | 
      reg_T = currentStackRegs.reg_Z
    , reg_Z = currentStackRegs.reg_Y
    , reg_Y = currentStackRegs.reg_X
    }
  in 
    liftedStack



liftStack : Model -> Model
liftStack model =
  let
    stackRegs = model.automaticMemoryStackRegisters
    liftedStack = liftAutomaticMemoryStack stackRegs
    liftedModel = { model | automaticMemoryStackRegisters = liftedStack }
  in
    liftedModel

exchange_X_Y_Regs : Model -> Model
exchange_X_Y_Regs model =
  let
    stackRegs = model.automaticMemoryStackRegisters
    tmp_Y = stackRegs.reg_Y
    exchangedStack = { stackRegs | 
      reg_Y = stackRegs.reg_X
    , reg_X = tmp_Y
    } 
    promotedModel = { model | automaticMemoryStackRegisters = exchangedStack, addToInputQueue   = True }
  in
    promotedModel

roll_Down_Stack : Model -> Model
roll_Down_Stack model =
  let
    stackRegs = model.automaticMemoryStackRegisters
    tmp_X = stackRegs.reg_X
    rolledStack = { stackRegs | 
      reg_X = stackRegs.reg_Y
    , reg_Y = stackRegs.reg_Z
    , reg_Z = stackRegs.reg_T
    , reg_T = tmp_X
    } 
    promotedModel = { model | automaticMemoryStackRegisters = rolledStack, addToInputQueue   = True }
  in
    promotedModel

-- g LST x lifts the stack ( unless Enter, CLx Σ+ Σ-, 12× , 12÷, was the last key pressed )
last_X : Model -> Model
last_X model =
  let
    lift_blockers =  [ Enter_Key, CL_x_Key, Sigma_Plus_Key, Sigma_Minus_Key, Times_12_Key, DIVIDE_BY_12_Key ]
    last_key_input = List.head model.inputQueue
    ( should_we_even_lift_bro, dbgmsg ) = 
      case last_key_input of 
        Just test_this_key -> 
          let 
            filtered_blockers = List.filter ( \x -> x == test_this_key ) lift_blockers
            flag =  List.isEmpty <| filtered_blockers
            msg  = " TEST:---> " ++ Basics.toString test_this_key ++  Basics.toString flag ++  " <---:" ++ Basics.toString filtered_blockers
          in 
            ( flag, msg )
        Nothing            ->  ( False, "No Head " )

    maybeLiftedModel = if should_we_even_lift_bro 
                  then liftStack model 
                  else model
    stackRegs = maybeLiftedModel.automaticMemoryStackRegisters
    maybePromotedStack = { stackRegs | 
      reg_X = stackRegs.reg_Last_X
    } 
    maybePromotedModel = 
      { maybeLiftedModel | 
        automaticMemoryStackRegisters = maybePromotedStack
      , inputMode                     = White
      , scratchRegisters  = initializeScratchRegisters
      , addToInputQueue   = True
      , message = dbgmsg
      }
    newModel = update_Display_Precision model.displayPrecision maybePromotedModel
  in
    newModel
---------- Stack operations 

---------- unaryOperators

reciprocal  x = 1/x
x_squared   x = x * x
square_root x = Basics.sqrt x
natural_log x = Basics.logBase e x
e_to_the_x  x = e ^ x
n_factorial x = 
  let 
    n = Basics.floor x
  in
    List.product [1..n] |> Basics.toFloat

-- in the hp12c platinum, the round function rounds the number to 
-- the number of decimals sepcified in the display precision
round_function : Int -> Float -> Float
round_function n x =
  let 
    float_n = Basics.toFloat n
  in 
    Basics.toFloat ( Basics.floor( x * ( 10 ^ float_n ) ) ) / ( 10 ^ float_n )
    -- TODO: check if the calc rounds it by adding 0.5

integral_part   x = Basics.toFloat ( Basics.floor x )
fractional_part x = x - ( integral_part x )


unaryOperator : ( Float -> Float ) -> Model -> Model
unaryOperator op model =
  let
    stackRegs        = model.automaticMemoryStackRegisters
    result           = ( op stackRegs.reg_X ) 
    unReducededStack = 
      { stackRegs | 
        reg_Last_X = stackRegs.reg_X
      , reg_X      = result
      } 
    scratchRegs = model.scratchRegisters
    unReducedModel = 
      { model | 
        automaticMemoryStackRegisters = unReducededStack
      , inputMode = White
      , scratchRegisters = { scratchRegs | acceptNewDigitInto = NewNumber }
      , addToInputQueue   = True
      }
    newModel = update_Display_Precision model.displayPrecision unReducedModel
  in
    newModel

---------- unaryOperators

-----------  Binary operators lhs is reg_y, rhs is reg_x

y_to_the_x     y x = y ^ x
y_plus_x       y x = y + x
y_minus_x      y x = y - x
y_times_x      y x = y * x 
y_divided_by_x y x = y / x

x_percent_of_y y x = y * x / 100
delta_percentage y x = 
  let
    sign = if y > x 
           then 1
           else -1
  in
    sign * y * ( y - x )/ 100

percentage_of_total y x = 100 * x / y 


binaryOperator_No_Down_Shift : ( Float -> Float -> Float ) -> Model -> Model 
binaryOperator_No_Down_Shift op model =
  let
    stackRegs     = model.automaticMemoryStackRegisters
    result        = ( op stackRegs.reg_Y stackRegs.reg_X )
    unPromotedStack = 
      { stackRegs | 
        reg_Last_X = stackRegs.reg_X
      , reg_X      = result
      } 
    promotedModel = 
      { model | 
        automaticMemoryStackRegisters = unPromotedStack
      , inputMode = White
      , addToInputQueue   = True
      }
    newModel = update_Display_Precision model.displayPrecision promotedModel
  in
    newModel



binaryOperator : ( Float -> Float -> Float ) -> Model -> Model 
binaryOperator op model =
  let
    stackRegs     = model.automaticMemoryStackRegisters
    result        = ( op stackRegs.reg_Y stackRegs.reg_X )
    reducedStack = 
      { stackRegs | 
        reg_T      = stackRegs.reg_T
      , reg_Z      = stackRegs.reg_T
      , reg_Y      = stackRegs.reg_Z
      , reg_Last_X = stackRegs.reg_X
      , reg_X      = result
      } 
    scratchRegs = model.scratchRegisters
    promotedModel = 
      { model | 
        automaticMemoryStackRegisters = reducedStack
      , inputMode = White
      , scratchRegisters = { scratchRegs | acceptNewDigitInto = NewNumber }
      , addToInputQueue  = True
      }
    newModel = update_Display_Precision model.displayPrecision promotedModel
  in
    newModel

-----------  Binary operators lhs is reg_y, rhs is reg_x

setComputationMode: ComputationMode -> Model -> Model
setComputationMode computationMode model =
  { model | computationMode = computationMode, addToInputQueue   = False }


until_evaluate_X_register : ScratchRegisters -> Float
until_evaluate_X_register modelScratchRegs = 
  let 
    decimal_place  = ( 10 ^ ( modelScratchRegs.number_of_decimals + 1 ) )
    sign_of_X      = if ( modelScratchRegs.reg_X_is_Positive ) then 1  else -1
    newReg_X       = sign_of_X * ( Basics.toFloat modelScratchRegs.integral_part_of_X )  + ( ( Basics.toFloat modelScratchRegs.fractional_part_of_X ) / decimal_place )    
  in 
    newReg_X

-- TODO: change this to handle STO RCL GTO etc
handleDigitInput: Int -> Model -> Model
handleDigitInput newDigit model = 
  let 
    stackRegs      = model.automaticMemoryStackRegisters
    scratchRegs    = model.scratchRegisters
    decimal_place  = ( 10 ^ ( scratchRegs.number_of_decimals + 1 ) )
    sign_of_X      = if ( scratchRegs.reg_X_is_Positive ) then 1  else -1
    
    ( newScratchRegs, newStackRegs ) =
      case scratchRegs.acceptNewDigitInto of 
        FractionalPart -> 
          let 
            newScratchRegs =
              { 
                scratchRegs | 
                fractional_part_of_X = ( 10 * scratchRegs.fractional_part_of_X + newDigit ) 
              , number_of_decimals   = scratchRegs.number_of_decimals + 1
              }
            newReg_X     = sign_of_X * ( Basics.toFloat newScratchRegs.integral_part_of_X )  + ( ( Basics.toFloat newScratchRegs.fractional_part_of_X ) / decimal_place )    
            newStackRegs = { stackRegs | reg_X = newReg_X }
          in 
            ( newScratchRegs, newStackRegs ) 
          
        IntegralPart   ->
          let 
            newScratchRegs =
              { 
                scratchRegs | 
                fractional_part_of_X = 0  
              , integral_part_of_X   = ( 10 * scratchRegs.integral_part_of_X + newDigit )
              }
            newReg_X     = sign_of_X * ( Basics.toFloat newScratchRegs.integral_part_of_X )  + ( ( Basics.toFloat newScratchRegs.fractional_part_of_X ) / decimal_place )    
            newStackRegs = { stackRegs | reg_X = newReg_X }
          in 
            ( newScratchRegs, newStackRegs ) 

        NewNumber      ->
          let             
            newScratchRegs =
              { 
                scratchRegs | 
                fractional_part_of_X = 0  
              , integral_part_of_X   = newDigit
              , acceptNewDigitInto   = IntegralPart
              }
            liftedStack  = liftAutomaticMemoryStack stackRegs
            newReg_X     = ( Basics.toFloat newScratchRegs.integral_part_of_X ) 
            newStackRegs = { liftedStack | reg_X = newReg_X }
          in 
            ( newScratchRegs, newStackRegs ) 

        ExponentForEEX ->
          ( scratchRegs, stackRegs )  -- TODO: unhandled for now
        STO_Reg -> 
          ( scratchRegs, stackRegs )  -- TODO: unhandled for now
        STO_Dot_Reg ->
          ( scratchRegs, stackRegs )  -- TODO: unhandled for now
        RCL_Reg     -> 
          ( scratchRegs, stackRegs )  -- TODO: unhandled for now
        RCL_Dot_Reg     -> 
          ( scratchRegs, stackRegs )  -- TODO: unhandled for now
        GTO_Addrs     -> 
          ( scratchRegs, stackRegs )  -- TODO: unhandled for now

    updatedModel = 
      { model |  
        automaticMemoryStackRegisters = newStackRegs
      , scratchRegisters              = newScratchRegs  
      , addToInputQueue               = True
      , message = "[>>> sign_of_X is " ++ Basics.toString sign_of_X ++ " <<<] "
      }
    newModel = update_Display_Precision model.displayPrecision updatedModel 
  in 
    newModel

-- TODO: change this to handle STO RCL GTO etc
handleDecimalPoint : Model -> Model
handleDecimalPoint model = 
  let 
    modelScratchRegs = model.scratchRegisters 
    newScratchRegs = { modelScratchRegs | acceptNewDigitInto = FractionalPart }
    newModel    = { model | scratchRegisters = newScratchRegs, addToInputQueue   = True }
  in 
    newModel

-- TODO: change this to handle STO RCL GTO etc
handle_CHS_Key : Model -> Model
handle_CHS_Key model = 
  let 
    modelScratchRegs = model.scratchRegisters 
    newScratchRegs   = { modelScratchRegs | reg_X_is_Positive = not modelScratchRegs.reg_X_is_Positive }
    newReg_X =  until_evaluate_X_register newScratchRegs
    modelStackRegs = model.automaticMemoryStackRegisters
    revaluatedStackRegs = { modelStackRegs | reg_X = newReg_X }
    toggledModel     = 
      { model | 
        automaticMemoryStackRegisters = revaluatedStackRegs
      , scratchRegisters = newScratchRegs
      , addToInputQueue   = True
      , message = "[>>> out of CHS handler sign_of_X is " ++ Basics.toString newScratchRegs.reg_X_is_Positive ++ " <<<] "
      }
    newModel = update_Display_Precision model.displayPrecision toggledModel
  in 
    newModel



-- TODO: change this to handle STO RCL GTO etc
handleEEX_Key : Model -> Model
handleEEX_Key model = 
  let 
    modelScratchRegs = model.scratchRegisters 
    newScratchRegs = { modelScratchRegs | acceptNewDigitInto = ExponentForEEX }
    newModel    = { model | scratchRegisters = newScratchRegs, addToInputQueue   = True }
  in 
    newModel



numericalInputTerminated : Model -> Model
numericalInputTerminated model =
  let
    liftedModel = liftStack model
    stackRegs     = liftedModel.automaticMemoryStackRegisters 
    lastXUpdatedStack =  { stackRegs | reg_Last_X = stackRegs.reg_X }
  in
    { liftedModel | scratchRegisters = initializeScratchRegisters
    , automaticMemoryStackRegisters = lastXUpdatedStack
    }

backSpaceReg_X : Model -> Model
backSpaceReg_X model =
  let 
    stackRegs    = model.automaticMemoryStackRegisters
    newStackRegs = { stackRegs | reg_X = toFloat ( floor ( stackRegs.reg_X / 10 ) )}
    newModel     = { model | automaticMemoryStackRegisters = newStackRegs, addToInputQueue   = True }
  in 
    newModel

clearAllRegisters : Model -> Model
clearAllRegisters model =
  let 
    cleared_model =
      { model | 
                automaticMemoryStackRegisters = initializeAutomaticMemoryStackRegisters
      ,         dataStorageRegisters          = initializeDataStorageRegisters 
      ,         statisticalRegisters          = initializeStatisticalRegisters
      ,         financialRegisters            = initializeFinancialRegisters
      ,         scratchRegisters              = initializeScratchRegisters
      , addToInputQueue   = True
      }
    newModel = update_Display_Precision model.displayPrecision cleared_model
  in 
    newModel

clearFinancialRegisters : Model -> Model
clearFinancialRegisters model =
  let 
    cleared_model =
      { model | 
                financialRegisters            = initializeFinancialRegisters
      ,         scratchRegisters              = initializeScratchRegisters
      ,         addToInputQueue   = True
      }
    newModel = update_Display_Precision model.displayPrecision cleared_model
  in 
    newModel

clearProgramMemory : Model -> Model
clearProgramMemory model =
  if ( model.computationMode == PRGM_MODE )
    then
      let 
        cleared_model =
          { model | 
                    programMemory            = initializeProgramMemory
                  , addToInputQueue   = True
          }
      in 
        cleared_model
    else
      model


--CLEAR PREFIX after f, g, STO, RCL or GTO cancels that key (page 18).
--f CLEAR PREFIX also displays mantissa of number in the displayed X-register

clearPrefix : Model -> Model
clearPrefix model = 
  { model | 
    inputMode = White
  , addToInputQueue   = True
  , message = "DOES NOT YET HANDLE STO RCL AND GTO, and need to figure out how to display mantissa for one sec only " 
  }


clearSigma : Model -> Model
clearSigma model =
  let 
    cleared_model =
      { model | 
                automaticMemoryStackRegisters = initializeAutomaticMemoryStackRegisters
      ,         statisticalRegisters          = initializeStatisticalRegisters
      ,         scratchRegisters              = initializeScratchRegisters
      ,         addToInputQueue               = True
      }
    newModel = update_Display_Precision model.displayPrecision cleared_model
  in 
    newModel

clearXRegister : Model -> Model
clearXRegister model =
  let 
    stackRegs     = model.automaticMemoryStackRegisters
    newStackRegs  = { stackRegs | reg_X = 0 }
    cleared_model = 
      { model |
        automaticMemoryStackRegisters = newStackRegs
      , addToInputQueue               = True
      }
    newModel = update_Display_Precision model.displayPrecision cleared_model
  in 
    newModel

update_Display_Precision : Int -> Model -> Model
update_Display_Precision n model =
  let 
    newModel = if ( n < 10 ) 
                then update_Display_Precision_util n model 
                else update_Display_to_Scientific model 
  in 
    newModel

update_Display_Precision_util : Int -> Model -> Model
update_Display_Precision_util n model =
  let
    displayPrecision = n
    stackRegs = model.automaticMemoryStackRegisters
    reg_X = stackRegs.reg_X
    newDisplayString = ( print ( Formatting.roundTo displayPrecision ) reg_X )

    newModel = { model | displayString    = newDisplayString
               ,         displayPrecision = displayPrecision 
               ,         inputMode        = White
               }
  in 
    newModel

update_Display_to_Scientific : Model -> Model
update_Display_to_Scientific model =
  let
    displayPrecision = 10
    stackRegs = model.automaticMemoryStackRegisters
    reg_X = stackRegs.reg_X
    decimal_place = reg_X |> Basics.abs |> Basics.logBase 10
    sign_of_exponent = if ( decimal_place < 0 ) then 1 else -1
    newExponent = if ( reg_X < 1 ) then ( decimal_place |> Basics.ceiling ) else ( decimal_place |> Basics.floor )
    newNumber = reg_X * ( 10 ^ ( sign_of_exponent * newExponent |> Basics.toFloat ) )
    exp_display_part = print (padLeft 2 '0' int ) newExponent |> String.right 2
    newDisplayString = ( print ( Formatting.roundTo 6 ) newNumber ) ++ "  " ++ (exp_display_part )

    newModel = { model | displayString    = newDisplayString
               ,         displayPrecision = displayPrecision 
               ,         inputMode = White
               }
  in 
    newModel

setPrefix: InputMode -> Model -> Model
setPrefix inputMode model =
  { model | 
    inputMode       = inputMode
  , addToInputQueue = False  
  }

handleKeyCode : Keyboard.KeyCode -> Model -> Model
handleKeyCode code model =
  if code == 32 then -- space bar toggles keyboard shortcuts
    { model | keyCode = code
            , shortcutVisible = not model.shortcutVisible 
            , addToInputQueue = False
            , message = "Space bar pressed : " ++ Basics.toString ( model.shortcutVisible )
    }
  
    else
    { model | keyCode = code }

handlePOWERONKey : Model -> Model
handlePOWERONKey model = initialModel

defaultModelTransformer : Model -> Model
defaultModelTransformer model = 
  { model | 
    unimplemented     = True 
  , addToInputQueue   = True
  }



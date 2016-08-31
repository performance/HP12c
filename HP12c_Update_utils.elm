module HP12c_Update_utils exposing (..)

import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)

import Formatting exposing (..)
import String
import Keyboard

-- Update

---------- Stack operations 
liftStack : Model -> Model
liftStack model =
  let
    stackRegs = model.automaticMemoryStackRegisters
    promotedStack = { stackRegs | 
      reg_T = stackRegs.reg_Z
    , reg_Z = stackRegs.reg_Y
    , reg_Y = stackRegs.reg_X
    } 
    promotedModel = { model | automaticMemoryStackRegisters = promotedStack }
  in
    promotedModel

exchange_X_Y_Regs : Model -> Model
exchange_X_Y_Regs model =
  let
    stackRegs = model.automaticMemoryStackRegisters
    tmp_Y = stackRegs.reg_Y
    exchangedStack = { stackRegs | 
      reg_Y = stackRegs.reg_X
    , reg_X = tmp_Y
    } 
    promotedModel = { model | automaticMemoryStackRegisters = exchangedStack }
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
    promotedModel = { model | automaticMemoryStackRegisters = rolledStack }
  in
    promotedModel


last_X : Model -> Model
last_X model =
  let
    stackRegs = model.automaticMemoryStackRegisters
    promotedStack = { stackRegs | 
      reg_T = stackRegs.reg_Z
    , reg_Z = stackRegs.reg_Y
    , reg_Y = stackRegs.reg_X
    , reg_X = stackRegs.reg_Last_X
    } 
    promotedModel = 
      { model | 
        automaticMemoryStackRegisters = promotedStack
      , inputMode                     = White
      }
  in
    promotedModel

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
    stackRegs     = model.automaticMemoryStackRegisters
    promotedStack = 
      { stackRegs | 
        reg_T      = stackRegs.reg_Z
      , reg_Z      = stackRegs.reg_Y
      , reg_Y      = stackRegs.reg_X
      , reg_Last_X = stackRegs.reg_X
      , reg_X      = ( op stackRegs.reg_X )
      } 
    promotedModel = 
      { model | 
        automaticMemoryStackRegisters = promotedStack 
      , inputMode = White
      }
  in
    promotedModel

---------- unaryOperators

-----------  Binary operators lhs is reg_y, rhs is reg_x

y_to_the_x     y x = y ^ x
y_plus_x       y x = y + x
y_minus_x      y x = y - x
y_times_x      y x = y * x 
y_divided_by_x y x = y / x



binaryOperator : ( Float -> Float -> Float ) -> Model -> Model 
binaryOperator op model =
  let
    stackRegs     = model.automaticMemoryStackRegisters
    result        = ( op stackRegs.reg_Y stackRegs.reg_X )
    promotedStack = 
      { stackRegs | 
        reg_T      = stackRegs.reg_T
      , reg_Z      = stackRegs.reg_T
      , reg_Y      = stackRegs.reg_Z
      , reg_Last_X = stackRegs.reg_X
      , reg_X      = result
      } 
    promotedModel = 
      { model | 
        automaticMemoryStackRegisters = promotedStack
      , inputMode = White
      }
  in
    promotedModel

-----------  Binary operators lhs is reg_y, rhs is reg_x


-- TODO: change this to handle STO RCL GTO etc
handleDigitInput: Int -> Model -> Model
handleDigitInput newDigit model = 
  let 
    stackRegs      = model.automaticMemoryStackRegisters
    scratchRegs    = model.scratchRegisters
    decimal_place  = ( 10 ^ ( scratchRegs.number_of_decimals + 1 ) )
    
    newScratchRegs =
      if scratchRegs.acceptNewDigitInto == FractionalPart 
        then 
            { 
              scratchRegs | 
              fractional_part_of_X = ( 10 * scratchRegs.fractional_part_of_X + newDigit ) 
            , number_of_decimals   = scratchRegs.number_of_decimals + 1
            }
        else 
            { 
              scratchRegs | 
              integral_part_of_X = ( 10 * scratchRegs.integral_part_of_X + newDigit )
            }

    newReg_X     = ( Basics.toFloat newScratchRegs.integral_part_of_X )  + ( ( Basics.toFloat newScratchRegs.fractional_part_of_X ) / decimal_place )
    newStackRegs = { stackRegs | reg_X = newReg_X }
    updatedModel = 
      { model |  
        automaticMemoryStackRegisters = newStackRegs 
      , scratchRegisters              = newScratchRegs  
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
    newModel    = { model | scratchRegisters = newScratchRegs }
  in 
    newModel



numericalInputTerminated : Model -> Model
numericalInputTerminated model =
  let
    promotedModel = liftStack model
  in
    { promotedModel | scratchRegisters = initializeScratchRegisters
    }

backSpaceReg_X : Model -> Model
backSpaceReg_X model =
  let 
    stackRegs    = model.automaticMemoryStackRegisters
    newStackRegs = { stackRegs | reg_X = toFloat ( floor ( stackRegs.reg_X / 10 ) )}
    newModel     = { model | automaticMemoryStackRegisters = newStackRegs }
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
      }
    newModel = update_Display_Precision model.displayPrecision cleared_model
  in 
    newModel

clearXRegister : Model -> Model
clearXRegister model =
  let 
    stackRegs     = model.automaticMemoryStackRegisters
    newStackRegs  = { stackRegs | reg_X = 0 }
    cleared_model = { model | automaticMemoryStackRegisters = newStackRegs }
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
               ,         inputMode = White
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
  { model | inputMode = inputMode }

handleKeyCode : Keyboard.KeyCode -> Model -> Model
handleKeyCode code model =
  if code == 32 then -- space bar toggles keyboard shortcuts
    { model | keyCode = code
            , shortcutVisible = not model.shortcutVisible 
            , message = "Space bar pressed : " ++ Basics.toString ( model.shortcutVisible )
    }
  
    else
    { model | keyCode = code }

handlePOWERONKey : Model -> Model
handlePOWERONKey model = initialModel

defaultModelTransformer : Model -> Model
defaultModelTransformer model = 
  { model | unimplemented = True }

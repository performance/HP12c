module HP12c_Update exposing (..)

import HP12c_KeyTypes exposing (..)
import HP12c_Model exposing (..)

-- Update

-- unaryOperators
reciprocal x = 1/x
x_squared x = x * x
square_root = Basics.sqrt
natural_log x = Basics.logBase e x
e_to_the_x x = e ^ x
n_factorial x = 
  let 
    n = Basics.floor x
  in
    List.product [1..n]

-- in the hp12c platinum, the round function rounds the number to 
-- the number of decimals sepcified in the display precision
round_function x n =
  Basics.toFloat ( Basics.floor( x * ( 10 ^ n ) ) ) / ( 10 ^ n )
-- TODO: check if the calc rounds it by adding 0.5

integral_part x = Basics.toFloat ( Basics.floor x )

fractional_part x = x - ( integral_part x )

-----------  Binary operators lhs is reg_y, rhs is reg_x

y_to_the_x     y x = y ^ x
y_plus_x       y x = y + x
y_minus_x      y x = y - x
y_times_x      y x = y * x 
y_divided_by_x y x = y / x


binaryOperator model op =
  let
    stackRegs = model.automaticMemoryStackRegisters
    result = ( op stackRegs.reg_Y stackRegs.reg_X )
    promotedStack = { stackRegs | 
      reg_T = stackRegs.reg_T
    , reg_Z = stackRegs.reg_T
    , reg_Y = stackRegs.reg_Z
    , reg_Last_X = stackRegs.reg_X
    , reg_X = result
    } 
    promotedModel = { model 
                    | automaticMemoryStackRegisters = promotedStack 
                    , inputMode = White
                    --, calculatorOperationalState = AcceptingOperationsOrNumbers
                    }
  in
    promotedModel

updateReg_X: Model -> Int -> Model
updateReg_X model n = 
  let 
    stackRegs = model.automaticMemoryStackRegisters
    scratchRegs = model.scratchRegisters
    decimal_place = ( 10 ^ ( scratchRegs.number_of_decimals + 1 ) )
    
    newScratchRegs =
      if scratchRegs.acceptNewDigitInto == FractionalPart 
        then 
            { scratchRegs | fractional_part_of_X = ( 10 * scratchRegs.fractional_part_of_X + n ) 
                          , number_of_decimals = scratchRegs.number_of_decimals + 1
            }
        else 
            { scratchRegs | integral_part_of_X = ( 10 * scratchRegs.integral_part_of_X + n )
            }

    newReg_X = ( Basics.toFloat newScratchRegs.integral_part_of_X )  + ( ( Basics.toFloat newScratchRegs.fractional_part_of_X ) / decimal_place )
    newStackRegs = { stackRegs | reg_X = newReg_X }
    newModel = { model |  automaticMemoryStackRegisters = newStackRegs 
               ,          scratchRegisters              = newScratchRegs  
               }
  in 
    newModel

numericalInputTerminated model =
  let
    promotedModel = liftStack model
  in
    { promotedModel | scratchRegisters = initializeScratchRegisters
    }


backSpaceReg_X model =
  let 
    stackRegs = model.automaticMemoryStackRegisters
  in 
    { stackRegs | reg_X = toFloat ( floor ( stackRegs.reg_X / 10 ) )} 

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


unaryOperator model op =
  let
    stackRegs = model.automaticMemoryStackRegisters
    promotedStack = { stackRegs | 
      reg_T = stackRegs.reg_Z
    , reg_Z = stackRegs.reg_Y
    , reg_Y = stackRegs.reg_X
    , reg_Last_X = stackRegs.reg_X
    , reg_X = ( op stackRegs.reg_X )
    } 
    promotedModel = { model 
                    | automaticMemoryStackRegisters = promotedStack 
                    , inputMode = White
                    --, calculatorOperationalState = AcceptingOperationsOrNumbers
                    }
  in
    promotedModel

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyMsg code ->
      if code == 32 then -- space bar toggles keyboard shortcuts
        ( { model | keyCode = code
                  , shortcutVisible = not model.shortcutVisible 
                  , message = "Space bar pressed : " ++ Basics.toString ( model.shortcutVisible )
          }, Cmd.none 
        )
      else
        ( { model | keyCode = code }  , Cmd.none )
-------------------------------- First Row of Keys
    N_Key                 ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Times_12_Key          ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    AMORT_Key             ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    I_Key                 ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    DIVIDE_BY_12_Key      ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    INT_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    PV_Key                ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CF_0_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    NPV_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    PMT_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CF_j_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    RND_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    FV_Key                ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    N_j_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    IRR_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CHS_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    DATE_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    RPN_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_7_Key          ->
      let 
        newModel = updateReg_X model 7
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    BEG_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_8_Key          ->
      let 
        newModel = updateReg_X model 8
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    END_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_9_Key          ->
      let 
        newModel = updateReg_X model 9
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    MEM_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Divide_Key            ->
      let 
        newModel = binaryOperator model y_divided_by_x
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    Undo_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
-------------------------------- Second Row of Keys
    Y_toThe_X_Key         ->
      let 
        newModel = binaryOperator model y_to_the_x
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    Square_Root_Key       ->
      let 
        newModel = unaryOperator model square_root
      in 
      ( { newModel | message = Basics.toString msg    }, Cmd.none 
      )
    PRICE_Key             ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Reciprocal_Key        ->
      let 
        newModel = unaryOperator model reciprocal
      in 
      ( { newModel | message = Basics.toString msg }, Cmd.none 
      )
    E_to_the_x_Key        ->
      let 
        newModel = unaryOperator model e_to_the_x
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )
    YTM_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Percentage_T_Key      ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    LN_Key                ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    SL_Key                ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Delta_Percentage_Key  ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    FRAC_Key              ->
      let 
        newModel = unaryOperator model fractional_part
      in 
        ( { newModel | message = Basics.toString msg
          ,            inputMode = White  
          }, Cmd.none 
        )

    SOYD_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Percent_Key           ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    INTG_Key              ->
      let 
        newModel = unaryOperator model integral_part
      in 
        ( { newModel | message = Basics.toString msg
          ,            inputMode = White  
          }, Cmd.none 
        )
    DB_Key                ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    EEX_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Delta_Days_Key        ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    ALG_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_4_Key          ->
      let 
        newModel = updateReg_X model 4
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    D_MY_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_5_Key          ->
      let 
        newModel = updateReg_X model 5
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    M_DY_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_6_Key          ->
      let 
        newModel = updateReg_X model 6
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    Weighted_Mean_Key     ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Multiply_Key          ->
      let 
        newModel = binaryOperator model y_times_x
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    X_Squared_Key         ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
-------------------------------- Third Row of Keys
    RunMode_Key           ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    PSE_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Program_Mode_Key      ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    SST_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    BST_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CLEAR_Σ_Key           ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Roll_Down_Key         ->
      let 
        rolledStackModel = roll_Down_Stack model
      in
        ( { rolledStackModel | message = Basics.toString msg  }, Cmd.none )

    GTO_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CLEAR_PRGM_Key        ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Exchange_X_Y_Key      ->
      let 
        newModel =  exchange_X_Y_Regs model
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    X_lte_Y_Key           ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CLEAR_FIN_Key         ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CL_x_Key              ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    X_eq_0_Key            ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CLEAR_REG_Key         ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Enter_Key             ->
      let 
        newModel = numericalInputTerminated model 
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    Equals_Key            ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    CLEAR_PREFIX_Key      ->
      ( { model | message = Basics.toString msg  
        ,         inputMode = White
        }, Cmd.none 
      )
    Number_1_Key          ->
      let 
        newModel = updateReg_X model 1
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    Linear_Estimate_X_Key ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_2_Key          ->
      let 
        newModel = updateReg_X model 2
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    Linear_Estimate_Y_Key ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_3_Key          ->
      let 
        newModel = updateReg_X model 3
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )


    N_Factorial_Key       ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Subtract_Key          ->
      let 
        newModel = binaryOperator model y_minus_x
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    BackSpace_Key         ->
      ( { model | message = Basics.toString msg  
        ,         automaticMemoryStackRegisters = backSpaceReg_X model 
        }, Cmd.none )
-------------------------------- Fourth Row of Keys
    ON_Key                ->
      ( { initialModel | message = Basics.toString msg  }, Cmd.none )
    OFF_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Orange_F_Key          ->
      ( { model | message   = Basics.toString msg  
        ,         inputMode = Orange
        }, Cmd.none 
      )
    Blue_G_Key            ->
      ( { model | message   = Basics.toString msg
        ,         inputMode = Blue
        }, Cmd.none 
      )
    STO_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Left_Paren_Key        ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    RCL_Key               ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Right_Paren_Key       ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Number_0_Key          ->
      let 
        newModel = updateReg_X model 0
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    Mean_of_X_Key         ->
      ( { model | message = Basics.toString msg  }, Cmd.none )

    Decimal_Point_Key     ->
      let 
        modelScratchRegs = model.scratchRegisters 
        newScratchRegs = { modelScratchRegs | acceptNewDigitInto = FractionalPart }
        newModel    = { model | scratchRegisters = newScratchRegs }
      in 
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    Std_Dev_Key           ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Sigma_Plus_Key        ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Sigma_Minus_Key       ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    Sum_Key               ->
      let 
        newModel = binaryOperator model y_plus_x
      in
        ( { newModel | message = Basics.toString msg  }, Cmd.none )

    Last_X_Key            ->
      ( { model | message = Basics.toString msg  }, Cmd.none )
    _ ->
      ( { model | message = " Not Yet implemented "           }, Cmd.none )

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
                    , calculatorOperationalState = AcceptingOperationsOrNumbers
                    }
  in
    promotedModel


updateReg_X: Model -> Int -> Model -- AutomaticMemoryStackRegisters 
updateReg_X model n = 
  let 
    stackRegs = model.automaticMemoryStackRegisters
    newStackRegs = if ( model.calculatorOperationalState == AcceptingNumericalInputOnly)
      then { stackRegs | reg_X = 10 * stackRegs.reg_X + toFloat n } 
      else { stackRegs | reg_X = toFloat n} 
    newModel = { model |  calculatorOperationalState = AcceptingNumericalInputOnly, automaticMemoryStackRegisters = newStackRegs }
  in 
    newModel
    

digitEntered model n = 
  let 
    liftedStack      = liftStack model
    promotedModel    = if   model.calculatorOperationalState == AcceptingOperationsOrNumbers
                       then { liftedStack | calculatorOperationalState = AcceptingNumericalInputOnly } 
                       else model
    stackRegs        = promotedModel.automaticMemoryStackRegisters
    updatedStackRegs = { stackRegs | reg_X = 10 * stackRegs.reg_X + toFloat n } 
  in 
    { promotedModel | automaticMemoryStackRegisters = updatedStackRegs }

numericalInputTerminated model =
  let
    promotedModel = liftStack model
  in
    { promotedModel | calculatorOperationalState = AcceptingOperationsOrNumbers }


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
                    , calculatorOperationalState = AcceptingOperationsOrNumbers
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
      ( { model | message = " N_Key pressed "                 }, Cmd.none )
    Times_12_Key          ->
      ( { model | message = " Times_12_Key pressed "          }, Cmd.none )
    AMORT_Key             ->
      ( { model | message = " AMORT_Key pressed "             }, Cmd.none )
    I_Key                 ->
      ( { model | message = " I_Key pressed "                 }, Cmd.none )
    DIVIDE_BY_12_Key      ->
      ( { model | message = " DIVIDE_BY_12_Key pressed "      }, Cmd.none )
    INT_Key               ->
      ( { model | message = " INT_Key pressed "               }, Cmd.none )
    PV_Key                ->
      ( { model | message = " PV_Key pressed "                }, Cmd.none )
    CF_0_Key              ->
      ( { model | message = " CF_0_Key pressed "              }, Cmd.none )
    NPV_Key               ->
      ( { model | message = " NPV_Key pressed "               }, Cmd.none )
    PMT_Key               ->
      ( { model | message = " PMT_Key pressed "               }, Cmd.none )
    CF_j_Key              ->
      ( { model | message = " CF_j_Key pressed "              }, Cmd.none )
    RND_Key               ->
      ( { model | message = " RND_Key pressed "               }, Cmd.none )
    FV_Key                ->
      ( { model | message = " FV_Key pressed "                }, Cmd.none )
    N_j_Key               ->
      ( { model | message = " N_j_Key pressed "               }, Cmd.none )
    IRR_Key               ->
      ( { model | message = " IRR_Key pressed "               }, Cmd.none )
    CHS_Key               ->
      ( { model | message = " CHS_Key pressed "               }, Cmd.none )
    DATE_Key              ->
      ( { model | message = " DATE_Key pressed "              }, Cmd.none )
    RPN_Key               ->
      ( { model | message = " RPN_Key pressed "               }, Cmd.none )
    Number_7_Key          ->
      let 
        newModel = updateReg_X model 7
      in
        ( { newModel | message = " Number_7_Key pressed " }, Cmd.none )

    BEG_Key               ->
      ( { model | message = " BEG_Key pressed "               }, Cmd.none )
    Number_8_Key          ->
      let 
        newModel = updateReg_X model 8
      in
        ( { newModel | message = " Number_8_Key pressed " }, Cmd.none )


    END_Key               ->
      ( { model | message = " END_Key pressed "               }, Cmd.none )
    Number_9_Key          ->
      let 
        newModel = updateReg_X model 9
      in
        ( { newModel | message = " Number_9_Key pressed " }, Cmd.none )


    MEM_Key               ->
      ( { model | message = " MEM_Key pressed "               }, Cmd.none )
    Divide_Key            ->
      let 
        newModel = binaryOperator model y_divided_by_x
      in
        ( { newModel | message = " Divide_Key pressed "        }, Cmd.none )

    Undo_Key              ->
      ( { model | message = " Undo_Key pressed "              }, Cmd.none )
-------------------------------- Second Row of Keys
    Y_toThe_X_Key         ->
      let 
        newModel = binaryOperator model y_to_the_x
      in
        ( { newModel | message = " Y_toThe_X_Key pressed "        }, Cmd.none )

    Square_Root_Key       ->
      let 
        newModel = unaryOperator model square_root
      in 
      ( { newModel | message = " Square_Root_Key pressed "
        --,            inputMode = White
        }, Cmd.none 
      )
    PRICE_Key             ->
      ( { model | message = " PRICE_Key pressed "             }, Cmd.none )
    Reciprocal_Key        ->
      let 
        newModel = unaryOperator model reciprocal
      in 
      ( { newModel | message = " Reciprocal_Key pressed "
        --,            inputMode = White
        }, Cmd.none 
      )
    E_to_the_x_Key        ->
      let 
        newModel = unaryOperator model e_to_the_x
      in
        ( { newModel | message = " E_to_the_x_Key pressed "        }, Cmd.none )
    YTM_Key               ->
      ( { model | message = " YTM_Key pressed "               }, Cmd.none )
    Percentage_T_Key      ->
      ( { model | message = " Percentage_T_Key pressed "      }, Cmd.none )
    LN_Key                ->
      ( { model | message = " LN_Key pressed "                }, Cmd.none )
    SL_Key                ->
      ( { model | message = " SL_Key pressed "                }, Cmd.none )
    Delta_Percentage_Key  ->
      ( { model | message = " Delta_Percentage_Key pressed "  }, Cmd.none )
    FRAC_Key              ->
      let 
        newModel = unaryOperator model fractional_part
      in 
        ( { newModel | message = " FRAC_Key pressed "
          ,            inputMode = White  
          }, Cmd.none 
        )

    SOYD_Key              ->
      ( { model | message = " SOYD_Key pressed "              }, Cmd.none )
    Percent_Key           ->
      ( { model | message = " Percent_Key pressed "           }, Cmd.none )
    INTG_Key              ->
      let 
        newModel = unaryOperator model integral_part
      in 
        ( { newModel | message = " INTG_Key pressed "
          ,            inputMode = White  
          }, Cmd.none 
        )
    DB_Key                ->
      ( { model | message = " DB_Key pressed "                }, Cmd.none )
    EEX_Key               ->
      ( { model | message = " EEX_Key pressed "               }, Cmd.none )
    Delta_Days_Key        ->
      ( { model | message = " Delta_Days_Key pressed "        }, Cmd.none )
    ALG_Key               ->
      ( { model | message = " ALG_Key pressed "               }, Cmd.none )
    Number_4_Key          ->
      let 
        newModel = updateReg_X model 4
      in
        ( { newModel | message = " Number_4_Key pressed " }, Cmd.none )


    D_MY_Key              ->
      ( { model | message = " D_MY_Key pressed "              }, Cmd.none )
    Number_5_Key          ->
      let 
        newModel = updateReg_X model 5
      in
        ( { newModel | message = " Number_5_Key pressed " }, Cmd.none )


    M_DY_Key              ->
      ( { model | message = " M_DY_Key pressed "              }, Cmd.none )
    Number_6_Key          ->
      let 
        newModel = updateReg_X model 6
      in
        ( { newModel | message = " Number_6_Key pressed " }, Cmd.none )


    Weighted_Mean_Key     ->
      ( { model | message = " Weighted_Mean_Key pressed "     }, Cmd.none )
    Multiply_Key          ->
      let 
        newModel = binaryOperator model y_times_x
      in
        ( { newModel | message = " Multiply_Key pressed "        }, Cmd.none )

    X_Squared_Key         ->
      ( { model | message = " X_Squared_Key pressed "         }, Cmd.none )
-------------------------------- Third Row of Keys
    RunMode_Key           ->
      ( { model | message = " RunMode_Key pressed "           }, Cmd.none )
    PSE_Key               ->
      ( { model | message = " PSE_Key pressed "               }, Cmd.none )
    Program_Mode_Key      ->
      ( { model | message = " Program_Mode_Key pressed "      }, Cmd.none )
    SST_Key               ->
      ( { model | message = " SST_Key pressed "               }, Cmd.none )
    BST_Key               ->
      ( { model | message = " BST_Key pressed "               }, Cmd.none )
    CLEAR_Σ_Key           ->
      ( { model | message = " CLEAR_Σ_Key pressed "           }, Cmd.none )
    Roll_Down_Key         ->
      let 
        rolledStackModel = roll_Down_Stack model
      in
        ( { rolledStackModel | message = " Roll_Down_Key pressed "         }, Cmd.none )

    GTO_Key               ->
      ( { model | message = " GTO_Key pressed "               }, Cmd.none )
    CLEAR_PRGM_Key        ->
      ( { model | message = " CLEAR_PRGM_Key pressed "        }, Cmd.none )
    Exchange_X_Y_Key      ->
      let 
        newModel =  exchange_X_Y_Regs model
      in
        ( { newModel | message = " Exchange_X_Y_Key pressed "      }, Cmd.none )

    X_lte_Y_Key           ->
      ( { model | message = " X_lte_Y_Key pressed "           }, Cmd.none )
    CLEAR_FIN_Key         ->
      ( { model | message = " CLEAR_FIN_Key pressed "         }, Cmd.none )
    CL_x_Key              ->
      ( { model | message = " CL_x_Key pressed "              }, Cmd.none )
    X_eq_0_Key            ->
      ( { model | message = " X_eq_0_Key pressed "            }, Cmd.none )
    CLEAR_REG_Key         ->
      ( { model | message = " CLEAR_REG_Key pressed "         }, Cmd.none )
    Enter_Key             ->
      let 
        newModel = numericalInputTerminated model 
      in
        ( { newModel | message = " Enter_Key pressed "             }, Cmd.none )

    Equals_Key            ->
      ( { model | message = " Equals_Key pressed "            }, Cmd.none )
    CLEAR_PREFIX_Key      ->
      ( { model | message = " CLEAR_PREFIX_Key pressed ",
                  inputMode = White
        }, Cmd.none 
      )
    Number_1_Key          ->
      let 
        newModel = updateReg_X model 1
      in
        ( { newModel | message = " Number_1_Key pressed " }, Cmd.none )


    Linear_Estimate_X_Key ->
      ( { model | message = " Linear_Estimate_X_Key pressed " }, Cmd.none )
    Number_2_Key          ->
      let 
        newModel = updateReg_X model 2
      in
        ( { newModel | message = " Number_2_Key pressed " }, Cmd.none )


    Linear_Estimate_Y_Key ->
      ( { model | message = " Linear_Estimate_Y_Key pressed " }, Cmd.none )
    Number_3_Key          ->
      let 
        newModel = updateReg_X model 3
      in
        ( { newModel | message = " Number_3_Key pressed " }, Cmd.none )


    N_Factorial_Key       ->
      ( { model | message = " N_Factorial_Key pressed "       }, Cmd.none )
    Subtract_Key          ->
      let 
        newModel = binaryOperator model y_minus_x
      in
        ( { newModel | message = " Subtract_Key pressed "        }, Cmd.none )

    BackSpace_Key         ->
      ( { model | message = " BackSpace_Key pressed ",
                  automaticMemoryStackRegisters = backSpaceReg_X model 
        }, Cmd.none )
-------------------------------- Fourth Row of Keys
    ON_Key                ->
      ( { initialModel | message = " ON_Key pressed "         }, Cmd.none )
    OFF_Key               ->
      ( { model | message = " OFF_Key pressed "               }, Cmd.none )
    Orange_F_Key          ->
      ( { model | 
            message   = " Orange_F_Key pressed ",
            inputMode = Orange
        }, Cmd.none 
      )
    Blue_G_Key            ->
      ( { model | 
            message   = " Blue_G_Key pressed ",
            inputMode = Blue
        }, Cmd.none 
      )
    STO_Key               ->
      ( { model | message = " STO_Key pressed "               }, Cmd.none )
    Left_Paren_Key        ->
      ( { model | message = " Left_Paren_Key pressed "        }, Cmd.none )
    RCL_Key               ->
      ( { model | message = " RCL_Key pressed "               }, Cmd.none )
    Right_Paren_Key       ->
      ( { model | message = " Right_Paren_Key pressed "       }, Cmd.none )
    Number_0_Key          ->
      let 
        newModel = updateReg_X model 0
      in
        ( { newModel | message = " Number_0_Key pressed " }, Cmd.none )

    Mean_of_X_Key         ->
      ( { model | message = " Mean_of_X_Key pressed "         }, Cmd.none )
    Decimal_Point_Key     ->
      ( { model | message = " Decimal_Point_Key pressed "     }, Cmd.none )
    Std_Dev_Key           ->
      ( { model | message = " Std_Dev_Key pressed "           }, Cmd.none )
    Sigma_Plus_Key        ->
      ( { model | message = " Sigma_Plus_Key pressed "        }, Cmd.none )
    Sigma_Minus_Key       ->
      ( { model | message = " Sigma_Minus_Key pressed "       }, Cmd.none )
    Sum_Key               ->
      let 
        newModel = binaryOperator model y_plus_x
      in
        ( { newModel | message = " Sum_Key pressed "        }, Cmd.none )

    Last_X_Key            ->
      ( { model | message = " Last_X_Key pressed "            }, Cmd.none )
    _ ->
      ( { model | message = " Not Yet implemented "           }, Cmd.none )

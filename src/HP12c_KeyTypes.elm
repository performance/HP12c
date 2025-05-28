module HP12c_KeyTypes exposing (..)

import Keyboard

type Msg -- Cal_C_Keys                    Blue                         Orange
-------------------------------- First Row of Keys
  = N_Key                             | Times_12_Key            |     AMORT_Key
  | I_Key                             | DIVIDE_BY_12_Key        |     INT_Key
  | PV_Key                            | CF_0_Key                |     NPV_Key
  | PMT_Key                           | CF_j_Key                |     RND_Key
  | FV_Key                            | N_j_Key                 |     IRR_Key
  | CHS_Key                           | DATE_Key                |     RPN_Key
  | Number_7_Key                      | BEG_Key                 |     SetPrecision_7_Key
  | Number_8_Key                      | END_Key                 |     SetPrecision_8_Key
  | Number_9_Key                      | MEM_Key                 |     SetPrecision_9_Key
  | Divide_Key                        | Undo_Key                
-------------------------------- Second Row of Keys
  | Y_toThe_X_Key                     | Square_Root_Key         |     PRICE_Key
  | Reciprocal_Key                    | E_to_the_x_Key          |     YTM_Key
  | Percentage_T_Key                  | LN_Key                  |     SL_Key      -- % of Total    |
  | Delta_Percentage_Key              | FRAC_Key                |     SOYD_Key
  | Percent_Key                       | INTG_Key                |     DB_Key
  | EEX_Key                           | Delta_Days_Key          |     ALG_Key
  | Number_4_Key                      | D_MY_Key                |     SetPrecision_4_Key
  | Number_5_Key                      | M_DY_Key                |     SetPrecision_5_Key
  | Number_6_Key                      | Weighted_Mean_Key       |     SetPrecision_6_Key
  | Multiply_Key                      | X_Squared_Key           
-------------------------------- Third Row of Keys
  | RunMode_Key                       | PSE_Key                 |     Program_Mode_Key    -- R/S    |
  | SST_Key                           | BST_Key                 |     CLEAR_Σ_Key         -- Single Step
  | Roll_Down_Key                     | GTO_Key                 |     CLEAR_PRGM_Key
  | Exchange_X_Y_Key                  | X_lte_Y_Key             |     CLEAR_FIN_Key
  | CL_x_Key                          | X_eq_0_Key              |     CLEAR_REG_Key
  | Enter_Key                         | Equals_Key              |     CLEAR_PREFIX_Key
  | Number_1_Key                      | Linear_Estimate_X_Key   |     SetPrecision_1_Key
  | Number_2_Key                      | Linear_Estimate_Y_Key   |     SetPrecision_2_Key
  | Number_3_Key                      | N_Factorial_Key         |     SetPrecision_3_Key
  | Subtract_Key                      | BackSpace_Key           
-------------------------------- Fourth Row of Keys
  | ON_Key                                                      | OFF_Key
  | Orange_F_Key
  | Blue_G_Key
  | STO_Key                           | Left_Paren_Key
  | RCL_Key                           | Right_Paren_Key
  | Number_0_Key                      | Mean_of_X_Key           |     SetPrecision_0_Key 
  | Decimal_Point_Key                 | Std_Dev_Key             |     SetDisplayScientific_Key
  | Sigma_Plus_Key                    | Sigma_Minus_Key
  | Sum_Key                           | Last_X_Key
  | KeyMsg Keyboard.KeyCode -- for any key not on the calc
  -- NLP specific messages
  | NLPInput String
  | ProcessNLP
  | ExecuteNLPCommands (List Msg)
  | UnrecognizedToken_Key String -- For reporting unrecognized tokens

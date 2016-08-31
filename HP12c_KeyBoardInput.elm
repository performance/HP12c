module HP12c_KeyBoardInput exposing (..)



-- TODO: read manual to figure out what to do with STO etc keys in orange mode

import Keyboard
import HP12c_KeyTypes exposing (..)
import HP12c_Model    exposing (..)


keyCodeToMsg : InputMode -> Keyboard.KeyCode -> Msg
keyCodeToMsg inputMode code =
  case code of
-------------------------------- First Row of Keys
    78  -> case inputMode of
              White  -> N_Key   -- "N"
              Blue   -> Times_12_Key
              Orange -> AMORT_Key
    110 -> case inputMode of
              White  -> N_Key   -- "N"
              Blue   -> Times_12_Key
              Orange -> AMORT_Key

    73  -> case inputMode of
              White  -> I_Key   -- "I"
              Blue   -> DIVIDE_BY_12_Key
              Orange -> INT_Key
    105 -> case inputMode of
              White  -> I_Key   -- "i"
              Blue   -> DIVIDE_BY_12_Key
              Orange -> INT_Key

    80  -> case inputMode of
              White  -> PV_Key   -- "P"
              Blue   -> CF_0_Key
              Orange -> NPV_Key
    112 -> case inputMode of
              White  -> PV_Key   -- "p"
              Blue   -> CF_0_Key
              Orange -> NPV_Key

    77  -> case inputMode of
              White  -> PMT_Key -- "M"
              Blue   -> CF_j_Key
              Orange -> RND_Key
    109 -> case inputMode of
              White  -> PMT_Key -- "m"
              Blue   -> CF_j_Key
              Orange -> RND_Key

    86  -> case inputMode of
              White  -> FV_Key  -- "V"
              Blue   -> N_j_Key
              Orange -> IRR_Key
    118 -> case inputMode of
              White  -> FV_Key  -- "v"
              Blue   -> N_j_Key
              Orange -> IRR_Key

    72  -> case inputMode of
              White  -> CHS_Key -- "H"
              Blue   -> DATE_Key
              Orange -> RPN_Key
    104 -> case inputMode of
              White  -> CHS_Key -- "H"
              Blue   -> DATE_Key
              Orange -> RPN_Key

    55  -> case inputMode of
              White  -> Number_7_Key -- "7"
              Blue   -> BEG_Key
              Orange -> SetPrecision_7_Key -- KeyMsg code

    56  -> case inputMode of
              White  -> Number_8_Key -- "8"
              Blue   -> END_Key
              Orange -> SetPrecision_8_Key -- KeyMsg code

    57  -> case inputMode of
              White  -> Number_9_Key -- "9"
              Blue   -> MEM_Key
              Orange -> SetPrecision_9_Key -- KeyMsg code

    47  -> case inputMode of
              White  -> Divide_Key   -- "/"
              Blue   -> Undo_Key
              Orange -> Divide_Key -- KeyMsg code

-------------------------------- Second Row of Keys
    33  -> case inputMode of
              White  -> Y_toThe_X_Key  -- "!"
              Blue   -> Square_Root_Key
              Orange -> PRICE_Key

    92  -> case inputMode of
              White  -> Reciprocal_Key -- "\\"
              Blue   -> E_to_the_x_Key
              Orange -> YTM_Key

    84  -> case inputMode of
              White  -> Percentage_T_Key -- % of Total -- "T"
              Blue   -> LN_Key
              Orange -> SL_Key

    116 -> case inputMode of
              White  -> Percentage_T_Key -- % of Total -- "T"
              Blue   -> LN_Key
              Orange -> SL_Key
    --35  -> "#" -- %T
    36  -> case inputMode of
              White  -> Delta_Percentage_Key -- delta% -- "$"
              Blue   -> FRAC_Key
              Orange -> SOYD_Key

    37  -> case inputMode of
              White  -> Percent_Key -- "%"
              Blue   -> INTG_Key
              Orange -> DB_Key

    69  -> case inputMode of
              White  -> EEX_Key            -- "E"
              Blue   -> Delta_Days_Key
              Orange -> ALG_Key

    101 -> case inputMode of
              White  -> EEX_Key            -- "E"
              Blue   -> Delta_Days_Key
              Orange -> ALG_Key

    52  -> case inputMode of
              White  -> Number_4_Key       -- "4"
              Blue   -> D_MY_Key
              Orange -> SetPrecision_4_Key -- KeyMsg code

    53  -> case inputMode of
              White  -> Number_5_Key       -- "5"
              Blue   -> M_DY_Key
              Orange -> SetPrecision_5_Key -- KeyMsg code

    54  -> case inputMode of
              White  -> Number_6_Key       -- "6"
              Blue   -> Weighted_Mean_Key
              Orange -> SetPrecision_6_Key -- KeyMsg code

    42  -> case inputMode of
              White  -> Multiply_Key       -- "*"
              Blue   -> X_Squared_Key
              Orange -> Multiply_Key -- KeyMsg code

-------------------------------- Third Row of Keys
    91  -> case inputMode of
              White  -> RunMode_Key  -- "[" -- R/S
              Blue   -> PSE_Key
              Orange -> Program_Mode_Key

    93  -> case inputMode of
              White  -> SST_Key      -- "]" -- SST
              Blue   -> BST_Key
              Orange -> CLEAR_Σ_Key

    81  -> case inputMode of
              White  -> Roll_Down_Key--"R"
              Blue   -> GTO_Key
              Orange -> CLEAR_PRGM_Key

    114 -> case inputMode of
              White  -> Roll_Down_Key-- "r" -- R_downarrow roll down stack
              Blue   -> GTO_Key
              Orange -> CLEAR_PRGM_Key

    89  -> case inputMode of
              White  -> Exchange_X_Y_Key -- "Y"
              Blue   -> X_lte_Y_Key
              Orange -> CLEAR_FIN_Key

    121 -> case inputMode of
              White  -> Exchange_X_Y_Key -- "y" -- Exchange x and y in the stack
              Blue   -> X_lte_Y_Key
              Orange -> CLEAR_FIN_Key

    67  -> case inputMode of
              White  -> CL_x_Key -- "C" -- CLx
              Blue   -> X_eq_0_Key
              Orange -> CLEAR_REG_Key

    99  -> case inputMode of
              White  -> CL_x_Key -- "c" -- CLx
              Blue   -> X_eq_0_Key
              Orange -> CLEAR_REG_Key

    13  -> case inputMode of
              White  -> Enter_Key -- Enter key
              Blue   -> Equals_Key
              Orange -> CLEAR_PREFIX_Key

    49  -> case inputMode of
              White  -> Number_1_Key -- "1"
              Blue   -> Linear_Estimate_X_Key
              Orange -> SetPrecision_1_Key -- KeyMsg code

    50  -> case inputMode of
              White  -> Number_2_Key -- "2"
              Blue   -> Linear_Estimate_Y_Key
              Orange -> SetPrecision_2_Key -- KeyMsg code

    51  -> case inputMode of
              White  -> Number_3_Key -- "3"
              Blue   -> N_Factorial_Key
              Orange -> SetPrecision_3_Key -- KeyMsg code

    45  -> case inputMode of
              White  -> Subtract_Key -- "-"
              Blue   -> BackSpace_Key
              Orange -> KeyMsg code

-------------------------------- Fourth Row of Keys
    79  -> case inputMode of
              White  -> ON_Key -- "O" -- Upper case O
              Blue   -> KeyMsg code
              Orange -> OFF_Key
    11  -> case inputMode of
              White  -> ON_Key -- "o" -- lower case O -- ON
              Blue   -> KeyMsg code
              Orange -> OFF_Key

    70  -> case inputMode of
              White  -> Orange_F_Key -- "F"
              Blue   -> Orange_F_Key
              Orange -> Orange_F_Key
    102 -> case inputMode of
              White  -> Orange_F_Key -- "f" -- orange f key
              Blue   -> Orange_F_Key
              Orange -> Orange_F_Key

    71  -> case inputMode of
              White  -> Blue_G_Key -- "G"
              Blue   -> Blue_G_Key
              Orange -> Blue_G_Key
    103 -> case inputMode of
              White  -> Blue_G_Key -- "g" -- blue g key
              Blue   -> Blue_G_Key
              Orange -> Blue_G_Key

    83  -> case inputMode of
              White  -> STO_Key -- "S"
              Blue   -> Left_Paren_Key
              Orange -> STO_Key --KeyMsg code

    115 -> case inputMode of
              White  -> STO_Key -- "s" -- STO
              Blue   -> Left_Paren_Key
              Orange -> STO_Key -- KeyMsg code

    76  -> case inputMode of
              White  -> RCL_Key -- "L"
              Blue   -> Right_Paren_Key
              Orange -> RCL_Key --KeyMsg code

    108 -> case inputMode of
              White  -> RCL_Key -- "l" -- RCL
              Blue   -> Right_Paren_Key
              Orange -> RCL_Key -- KeyMsg code

    -- 61 -> Equals -- "="
    48  -> case inputMode of
              White  -> Number_0_Key      -- "0" -- zero
              Blue   -> Mean_of_X_Key
              Orange -> SetPrecision_0_Key -- KeyMsg code

    46  -> case inputMode of
              White  -> Decimal_Point_Key -- ".""
              Blue   -> Std_Dev_Key
              Orange -> SetDisplayScientific_Key

    87  -> case inputMode of
              White  -> Sigma_Plus_Key    -- "W"
              Blue   -> Sigma_Minus_Key
              Orange -> KeyMsg code

    119 -> case inputMode of
              White  -> Sigma_Plus_Key    -- "w" -- Sigma+
              Blue   -> Sigma_Minus_Key
              Orange -> KeyMsg code

    43  -> case inputMode of
              White  -> Sum_Key -- "+"
              Blue   -> Last_X_Key
              Orange -> Sum_Key -- KeyMsg code
------------------------------
    _  -> KeyMsg code -- for any key not on the calc
module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard
import Json.Decode exposing (..)
import String
import List
import Json.Decode as Json
import Char

import Array exposing (..)

-- This file is built on top of https://github.com/chrisbuttery/elm-calculator

-- Events


-- Helpers


classNames : List String -> Attribute Msg
classNames strings =
  classList (List.map (\str -> ( str, True )) strings)


parseFloat : String -> Float
parseFloat string =
  case String.toFloat string of
    Ok value ->
      value

    Err error ->
      0


-- Model

type alias AutomaticMemoryStackRegisters =
  { reg_T : Float
  , reg_Z : Float
  , reg_Y : Float
  , reg_X : Float
  , reg_Last_X : Float
  }

type alias FinancialRegisters =
  { reg_n   : Float
  , reg_i   : Float
  , reg_PV  : Float
  , reg_PMT : Float
  , reg_FV  : Float
  , regs_N  : Array Float
  }
numberOfFinacialRegisters_N = 80

type alias DataStorageRegisters =
  { reg_R   : Array Float -- 10
  , reg_R_Decimal : Array Float -- 10
  }


-- in hardware, these are aliases,
-- so for now, try to mimic hardware behaviour
type alias StatisticalRegisters =
  { reg_n           : Float -- same as reg_R[1]
  , reg_Σ_X         : Float -- reg_R[2]
  , reg_Σ_X_Squared : Float -- reg_R[3]
  , reg_Σ_Y         : Float -- reg_R[4]
  , reg_Σ_Y_Squared : Float -- reg_R[5]
  , reg_Σ_XY        : Float -- reg_R[6]
  }

type alias ProgramMemory =
  { prog_mem : Array Int
  }

type InputModes
  = White
  | Orange
  | Blue

type alias Model =
  { inputMode                     : InputModes
  , automaticMemoryStackRegisters : AutomaticMemoryStackRegisters
  , financialRegisters            : FinancialRegisters
  , dataStorageRegisters          : DataStorageRegisters
  , statisticalRegisters          : StatisticalRegisters
  , programMemory                 : ProgramMemory
  , keyCode                       : Keyboard.KeyCode
  }

initializeAutomaticMemoryStackRegisters : AutomaticMemoryStackRegisters
initializeAutomaticMemoryStackRegisters =
  { reg_T      = 0
  , reg_Z      = 0
  , reg_Y      = 0
  , reg_X      = 0
  , reg_Last_X = 0
  }

initializeFinancialRegisters : FinancialRegisters
initializeFinancialRegisters =
  { reg_n   = 0
  , reg_i   = 0
  , reg_PV  = 0
  , reg_PMT = 0
  , reg_FV  = 0
  , regs_N  = Array.initialize 80  ( always 0 )
  }
initializeDataStorageRegisters : DataStorageRegisters
initializeDataStorageRegisters =
  { reg_R          = Array.initialize  10 ( always 0 )
  , reg_R_Decimal  = Array.initialize  10 ( always 0 )
  }

initializeStatisticalRegisters : StatisticalRegisters
initializeStatisticalRegisters =
  { reg_n           = 0
  , reg_Σ_X         = 0
  , reg_Σ_X_Squared = 0
  , reg_Σ_Y         = 0
  , reg_Σ_Y_Squared = 0
  , reg_Σ_XY        = 0
  }
initializeProgramMemory : ProgramMemory
initializeProgramMemory =
  {
    prog_mem = Array.initialize 400 ( always 0 )
  }

initialModel : Model
initialModel =
  { inputMode                     = White
  , automaticMemoryStackRegisters = initializeAutomaticMemoryStackRegisters
  , financialRegisters            = initializeFinancialRegisters
  , dataStorageRegisters          = initializeDataStorageRegisters
  , statisticalRegisters          = initializeStatisticalRegisters
  , programMemory                 = initializeProgramMemory
  , keyCode                       = 0
  }

-- Operations


sum : Float -> Float -> Float
sum x y =
  x + y


multiply : Float -> Float -> Float
multiply x y =
  x * y


division : Float -> Float -> Float
division x y =
  x / y


subtraction : Float -> Float -> Float
subtraction x y =
  x - y


-- Action


type Msg -- Cal_C_Keys                    Blue                         Orange
-------------------------------- First Row of Keys
  = N_Key                             | Times_12_Key            |     AMORT_Key
  | I_Key                             | DIVIDE_BY_12_Key        |     INT_Key
  | PV_Key                            | CF_0_Key                |     NPV_Key
  | PMT_Key                           | CF_j_Key                |     RND_Key
  | FV_Key                            | N_j_Key                 |     IRR_Key
  | CHS_Key                           | DATE_Key                |     RPN_Key
  | Number_7_Key                      | BEG_Key -------------   ---------------------------------
  | Number_8_Key                      | END_Key -------------   ---------------------------------
  | Number_9_Key                      | MEM_Key -------------   ---------------------------------
  | Divide_Key                        | Undo_Key-------------   ---------------------------------
-------------------------------- Second Row of Keys
  | Y_toThe_X_Key                     | Square_Root_Key         |     PRICE_Key
  | Reciprocal_Key                    | E_to_the_x_Key          |     YTM_Key
  | Percentage_T_Key                  | LN_Key                  |     SL_Key      -- % of Total    |
  | Delta_Percentage_Key              | FRAC_Key                |     SOYD_Key
  | Percent_Key                       | INTG_Key                |     DB_Key
  | EEX_Key                           | Delta_Days_Key          |     ALG_Key
  | Number_4_Key                      | D_MY_Key                ----------------------------------------------
  | Number_5_Key                      | M_DY_Key                ----------------------------------------------
  | Number_6_Key                      | Weighted_Mean_Key       ----------------------------------------------
  | Multiply_Key                      | X_Squared_Key           ----------------------------------------------
-------------------------------- Third Row of Keys
  | RunMode_Key                       | PSE_Key                 |    Program_Mode_Key    -- R/S    |
  | SST_Key                           | BST_Key                 |    CLEAR_Σ_Key         -- Single Step
  | Roll_Down_Key                     | GTO_Key                 |    CLEAR_PRGM_Key
  | Exchange_X_Y_Key                  | X_lte_Y_Key             |    CLEAR_FIN_Key
  | CL_x_Key                          | X_eq_0_Key              |    CLEAR_REG_Key
  | Enter_Key                         | Equals_Key              |    CLEAR_PREFIX_Key
  | Number_1_Key                      | Linear_Estimate_X_Key
  | Number_2_Key                      | Linear_Estimate_Y_Key
  | Number_3_Key                      | N_Factorial_Key
  | Subtract_Key                      | BackSpace_Key
-------------------------------- Fourth Row of Keys
  | ON_Key                                                      | OFF_Key
  | Orange_F_Key
  | Blue_G_Key
  | STO_Key                           | Left_Paren_Key
  | RCL_Key                           | Right_Paren_Key
  | Number_0_Key                      | Mean_of_X_Key
  | Decimal_Point_Key                 | Std_Dev_Key
  | Sigma_Plus_Key                    | Sigma_Minus_Key
  | Sum_Key                           | Last_X_Key
  | KeyMsg Keyboard.KeyCode -- for any key not on the calc


-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyMsg code ->
      ( { model | keyCode = code }  , Cmd.none )
-------------------------------- First Row of Keys
    N_Key                 ->
      ( model, Cmd.none )
    Times_12_Key          ->
      ( model, Cmd.none )
    AMORT_Key             ->
      ( model, Cmd.none )
    I_Key                 ->
      ( model, Cmd.none )
    DIVIDE_BY_12_Key      ->
      ( model, Cmd.none )
    INT_Key               ->
      ( model, Cmd.none )
    PV_Key                ->
      ( model, Cmd.none )
    CF_0_Key              ->
      ( model, Cmd.none )
    NPV_Key               ->
      ( model, Cmd.none )
    PMT_Key               ->
      ( model, Cmd.none )
    CF_j_Key              ->
      ( model, Cmd.none )
    RND_Key               ->
      ( model, Cmd.none )
    FV_Key                ->
      ( model, Cmd.none )
    N_j_Key               ->
      ( model, Cmd.none )
    IRR_Key               ->
      ( model, Cmd.none )
    CHS_Key               ->
      ( model, Cmd.none )
    DATE_Key              ->
      ( model, Cmd.none )
    RPN_Key               ->
      ( model, Cmd.none )
    Number_7_Key          ->
      ( model, Cmd.none )
    BEG_Key               ->
      ( model, Cmd.none )
    Number_8_Key          ->
      ( model, Cmd.none )
    END_Key               ->
      ( model, Cmd.none )
    Number_9_Key          ->
      ( model, Cmd.none )
    MEM_Key               ->
      ( model, Cmd.none )
    Divide_Key            ->
      ( model, Cmd.none )
    Undo_Key              ->
      ( model, Cmd.none )
-------------------------------- Second Row of Keys
    Y_toThe_X_Key         ->
      ( model, Cmd.none )     
    Square_Root_Key       ->
      ( model, Cmd.none )     
    PRICE_Key             ->
      ( model, Cmd.none )     
    Reciprocal_Key        ->
      ( model, Cmd.none )     
    E_to_the_x_Key        ->
      ( model, Cmd.none )     
    YTM_Key               ->
      ( model, Cmd.none )     
    Percentage_T_Key      ->
      ( model, Cmd.none )    
    LN_Key                ->
      ( model, Cmd.none )     
    SL_Key                ->
      ( model, Cmd.none )
    Delta_Percentage_Key  ->
      ( model, Cmd.none )     
    FRAC_Key              ->
      ( model, Cmd.none )     
    SOYD_Key              ->
      ( model, Cmd.none )     
    Percent_Key           ->
      ( model, Cmd.none )     
    INTG_Key              ->
      ( model, Cmd.none )     
    DB_Key                ->
      ( model, Cmd.none )     
    EEX_Key               ->
      ( model, Cmd.none )     
    Delta_Days_Key        ->
      ( model, Cmd.none )     
    ALG_Key               ->
      ( model, Cmd.none )     
    Number_4_Key          ->
      ( model, Cmd.none )     
    D_MY_Key              ->
      ( model, Cmd.none )
    Number_5_Key          ->
      ( model, Cmd.none )     
    M_DY_Key              ->
      ( model, Cmd.none )
    Number_6_Key          ->
      ( model, Cmd.none )     
    Weighted_Mean_Key     ->
      ( model, Cmd.none )
    Multiply_Key          ->
      ( model, Cmd.none )
    X_Squared_Key         ->
      ( model, Cmd.none )
-------------------------------- Third Row of Keys
    RunMode_Key           ->
      ( model, Cmd.none )     
    PSE_Key               ->
      ( model, Cmd.none )     
    Program_Mode_Key      ->
      ( model, Cmd.none )
    SST_Key               ->
      ( model, Cmd.none )     
    BST_Key               ->
      ( model, Cmd.none )     
    CLEAR_Σ_Key           ->
      ( model, Cmd.none )
    Roll_Down_Key         ->
      ( model, Cmd.none )     
    GTO_Key               ->
      ( model, Cmd.none )     
    CLEAR_PRGM_Key        ->
      ( model, Cmd.none )     
    Exchange_X_Y_Key      ->
      ( model, Cmd.none )     
    X_lte_Y_Key           ->
      ( model, Cmd.none )     
    CLEAR_FIN_Key         ->
      ( model, Cmd.none )     
    CL_x_Key              ->
      ( model, Cmd.none )     
    X_eq_0_Key            ->
      ( model, Cmd.none )     
    CLEAR_REG_Key         ->
      ( model, Cmd.none )     
    Enter_Key             ->
      ( model, Cmd.none )     
    Equals_Key            ->
      ( model, Cmd.none )     
    CLEAR_PREFIX_Key      ->
      ( model, Cmd.none )     
    Number_1_Key          ->
      ( model, Cmd.none )     
    Linear_Estimate_X_Key ->
      ( model, Cmd.none )     
    Number_2_Key          ->
      ( model, Cmd.none )     
    Linear_Estimate_Y_Key ->
      ( model, Cmd.none )     
    Number_3_Key          ->
      ( model, Cmd.none )     
    N_Factorial_Key       ->
      ( model, Cmd.none )     
    Subtract_Key          ->
      ( model, Cmd.none )     
    BackSpace_Key         ->
      ( model, Cmd.none )     
-------------------------------- Fourth Row of Keys
    ON_Key                ->
      ( initialModel, Cmd.none )     
    OFF_Key               ->
      ( model, Cmd.none )     
    Orange_F_Key          ->
      ( model, Cmd.none )     
    Blue_G_Key            ->
      ( model, Cmd.none )     
    STO_Key               ->
      ( model, Cmd.none )     
    Left_Paren_Key        ->
      ( model, Cmd.none )     
    RCL_Key               ->
      ( model, Cmd.none )     
    Right_Paren_Key       ->
      ( model, Cmd.none )     
    Number_0_Key          ->
      ( model, Cmd.none )     
    Mean_of_X_Key         ->
      ( model, Cmd.none )     
    Decimal_Point_Key     ->
      ( model, Cmd.none )     
    Std_Dev_Key           ->
      ( model, Cmd.none )     
    Sigma_Plus_Key        ->
      ( model, Cmd.none )     
    Sigma_Minus_Key       ->
      ( model, Cmd.none )     
    Sum_Key               ->
      ( model, Cmd.none )     
    Last_X_Key            ->
      ( model, Cmd.none )     
    _ ->
          ( model, Cmd.none )


-- SUBSCRIPTIONS

keyCodeToMsg : InputModes -> Keyboard.KeyCode -> Msg
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
              Orange -> KeyMsg code

    56  -> case inputMode of
              White  -> Number_8_Key -- "8"
              Blue   -> END_Key
              Orange -> KeyMsg code

    57  -> case inputMode of
              White  -> Number_9_Key -- "9"
              Blue   -> MEM_Key
              Orange -> KeyMsg code

    47  -> case inputMode of
              White  -> Divide_Key   -- "/"
              Blue   -> Undo_Key
              Orange -> KeyMsg code

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
              Orange -> KeyMsg code

    53  -> case inputMode of
              White  -> Number_5_Key       -- "5"
              Blue   -> M_DY_Key
              Orange -> KeyMsg code

    54  -> case inputMode of
              White  -> Number_6_Key       -- "6"
              Blue   -> Weighted_Mean_Key
              Orange -> KeyMsg code

    42  -> case inputMode of
              White  -> Multiply_Key       -- "*"
              Blue   -> X_Squared_Key
              Orange -> KeyMsg code

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
              Orange -> KeyMsg code

    50  -> case inputMode of
              White  -> Number_2_Key -- "2"
              Blue   -> Linear_Estimate_Y_Key
              Orange -> KeyMsg code

    51  -> case inputMode of
              White  -> Number_3_Key -- "3"
              Blue   -> N_Factorial_Key
              Orange -> KeyMsg code

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
              Blue   -> KeyMsg code
              Orange -> KeyMsg code
    102 -> case inputMode of
              White  -> Orange_F_Key -- "f" -- orange f key
              Blue   -> KeyMsg code
              Orange -> KeyMsg code

    71  -> case inputMode of
              White  -> Blue_G_Key -- "G"
              Blue   -> KeyMsg code
              Orange -> KeyMsg code
    103 -> case inputMode of
              White  -> Blue_G_Key -- "g" -- blue g key
              Blue   -> KeyMsg code
              Orange -> KeyMsg code

    83  -> case inputMode of
              White  -> STO_Key -- "S"
              Blue   -> Left_Paren_Key
              Orange -> KeyMsg code

    115 -> case inputMode of
              White  -> STO_Key -- "s" -- STO
              Blue   -> Left_Paren_Key
              Orange -> KeyMsg code

    76  -> case inputMode of
              White  -> RCL_Key -- "L"
              Blue   -> Right_Paren_Key
              Orange -> KeyMsg code

    108 -> case inputMode of
              White  -> RCL_Key -- "l" -- RCL
              Blue   -> Right_Paren_Key
              Orange -> KeyMsg code

    -- 61 -> Equals -- "="
    48  -> case inputMode of
              White  -> Number_0_Key      -- "0" -- zero
              Blue   -> Mean_of_X_Key
              Orange -> KeyMsg code

    46  -> case inputMode of
              White  -> Decimal_Point_Key -- ".""
              Blue   -> Std_Dev_Key
              Orange -> KeyMsg code

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
              Orange -> KeyMsg code
------------------------------
    _  -> KeyMsg code -- for any key not on the calc

subscriptions:Model -> Sub Msg
subscriptions model =
  Keyboard.presses ( \code -> ( keyCodeToMsg model.inputMode code ) )

-- View

view : Model -> Html Msg
view model =
  div
    [
      classNames ["calculator"]
    ]
    [
      div [ classNames [ "model" ] ] [ text (toString model) ]
    , div
          []
          [
            text ( String.fromChar ( Char.fromCode model.keyCode ) )
          ]
    ]


-- Main


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  ( Maybe.withDefault initialModel savedModel, Cmd.none )


main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions -- \_ -> Sub.none
    }

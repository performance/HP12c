module HP12c_Model exposing (..)

import Array exposing (..)

import Keyboard

type alias AutomaticMemoryStackRegisters =
  { reg_T : Float
  , reg_Z : Float
  , reg_Y : Float
  , reg_X : Float
  , reg_Last_X : Float
  }

type AcceptNewDigitInto 
  = IntegralPart
  |  FractionalPart

type alias ScratchRegisters =
  { integral_part_of_X   : Int
  , fractional_part_of_X : Int
  , number_of_decimals   : Int
  , acceptNewDigitInto   : AcceptNewDigitInto
  }

type alias FinancialRegistersEntered =
  { reg_n_entered   : Bool
  , reg_i_entered   : Bool
  , reg_PV_entered  : Bool
  , reg_PMT_entered : Bool
  , reg_FV_entered  : Bool
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

type InputMode
  = White
  | Orange
  | Blue

type PrefixMode
  = Orange_PrefixMode
  | Blue_PrefixMode
  | STO_Mode

type ErrorState
  = Error_0_Mathematics
  | Error_1_Storage_Register_Overflow
  | Error_2_Statistics
  | Error_3_IRR
  | Error_4_Memory
  | Error_5_Compound_Interest
  | Error_6_Storage_Registers
  | Error_7_IRR
  | Error_8_Calendar
  | Error_9_Service
  | PrError

type CalculatorOperationalState
  = AcceptingNumericalInputOnly
  | AcceptingOperationsOrNumbers
  | InPrefixMode PrefixMode
  | RunningProgram
  | Error ErrorState

type ComputationMode
  = RPN_Mode
  | ALG_Mode

type alias Model =
  { inputMode                     : InputMode
  , computationMode               : ComputationMode
  --, calculatorOperationalState    : CalculatorOperationalState
  , scratchRegisters              : ScratchRegisters
  , automaticMemoryStackRegisters : AutomaticMemoryStackRegisters
  , financialRegisters            : FinancialRegisters
  , financialRegistersEntered     : FinancialRegistersEntered
  , dataStorageRegisters          : DataStorageRegisters
  , statisticalRegisters          : StatisticalRegisters
  , programMemory                 : ProgramMemory
  , keyCode                       : Keyboard.KeyCode
  , shortcutVisible               : Bool
  , message                       : String
  , displayString                 : String
  , displayPrecision              : Int
  , unimplemented                 : Bool
  }

initializeScratchRegisters : ScratchRegisters
initializeScratchRegisters = 
  { integral_part_of_X   = 0
  , fractional_part_of_X = 0
  , number_of_decimals   = 0
  , acceptNewDigitInto   = IntegralPart
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

initializeFinancialRegistersEntered : FinancialRegistersEntered
initializeFinancialRegistersEntered = 
  { reg_n_entered   = False
  , reg_i_entered   = False
  , reg_PV_entered  = False
  , reg_PMT_entered = False
  , reg_FV_entered  = False
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
  , computationMode               = RPN_Mode
  --, calculatorOperationalState    = AcceptingOperationsOrNumbers
  , scratchRegisters              = initializeScratchRegisters
  , automaticMemoryStackRegisters = initializeAutomaticMemoryStackRegisters
  , financialRegisters            = initializeFinancialRegisters
  , financialRegistersEntered     = initializeFinancialRegistersEntered 
  , dataStorageRegisters          = initializeDataStorageRegisters
  , statisticalRegisters          = initializeStatisticalRegisters
  , programMemory                 = initializeProgramMemory
  , keyCode                       = 0
  , shortcutVisible               = False
  , message                       = "No Keys"
  , displayString                 = "0.00"
  , displayPrecision              = 2
  , unimplemented                 = False
  }



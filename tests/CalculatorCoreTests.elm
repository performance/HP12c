module CalculatorCoreTests exposing (all)

import Expect
import Test exposing (..)
import HP12c_KeyTypes exposing (Msg(..))
import HP12c_Model exposing (Model, initialModel)
import HP12c_Update exposing (update)

-- Helper to run a sequence of commands against the initial model
runCoreCommands : List Msg -> Model -> Model
runCoreCommands cmds model =
    List.foldl (\cmd mdl -> Tuple.first (update cmd mdl)) model cmds

all : Test
all =
    describe "CalculatorCoreLogic Tests"
        [ test "Number entry 123" <|
            \_ ->
                let
                    commands = [ Number_1_Key, Number_2_Key, Number_3_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "123.00"
        , test "Simple addition: 1 + 2 = 3.00" <|
            \_ ->
                let
                    commands = [ Number_1_Key, Enter_Key, Number_2_Key, Sum_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "3.00"
        , test "Simple subtraction: 5 - 2 = 3.00" <|
            \_ ->
                let
                    commands = [ Number_5_Key, Enter_Key, Number_2_Key, Subtract_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "3.00"
        , test "CLx Key: 12 CLx = 0.00" <|
            \_ ->
                let
                    initialWith12 = runCoreCommands [Number_1_Key, Number_2_Key] initialModel
                    commandsCLx = [ CL_x_Key ]
                    finalModel = runCoreCommands commandsCLx initialWith12
                in
                Expect.equal finalModel.displayString "0.00"
        , test "CHS Key: 5 CHS = -5.00" <|
            \_ ->
                let
                    commands = [ Number_5_Key, CHS_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "-5.00"
        , test "Precision change: 123.456 Enter, f, 2 -> 123.45" <|
            \_ ->
                let
                    initialWithNum = runCoreCommands [Number_1_Key, Number_2_Key, Number_3_Key, Decimal_Point_Key, Number_4_Key, Number_5_Key, Number_6_Key, Enter_Key] initialModel
                    commandsPrecision = [ Orange_F_Key, SetPrecision_2_Key ]
                    finalModel = runCoreCommands commandsPrecision initialWithNum
                in
                Expect.equal finalModel.displayString "123.45"
        , test "CLx after operation: 1 Enter 2 + CLx = 0.00" <|
            \_ ->
                let
                    initialAfterPlus = runCoreCommands [Number_1_Key, Enter_Key, Number_2_Key, Sum_Key] initialModel
                    commandsCLxAfterOp = [ CL_x_Key ]
                    finalModel = runCoreCommands commandsCLxAfterOp initialAfterPlus
                in
                Expect.equal finalModel.displayString "0.00"
        ]

modifierKeyTests : Test
modifierKeyTests =
    describe "Modifier Key State Tests"
        [ test "Pressing 'f' key sets inputMode to Orange" <|
            \_ ->
                let
                    (modelAfterF, _) = update Orange_F_Key initialModel
                in
                Expect.equal modelAfterF.inputMode HP12c_Model.Orange
        , test "Pressing 'g' key sets inputMode to Blue" <|
            \_ ->
                let
                    (modelAfterG, _) = update Blue_G_Key initialModel
                in
                Expect.equal modelAfterG.inputMode HP12c_Model.Blue
        , test "Pressing 'f' then '1' (SetPrecision_1_Key) reverts inputMode to White and changes precision" <|
            \_ ->
                let
                    (modelAfterF, _) = update Orange_F_Key initialModel
                    -- Number_1_Key after Orange_F_Key should trigger SetPrecision_1_Key
                    (finalModel, _) = update Number_1_Key modelAfterF 
                in
                Expect.all
                    [ Expect.equal finalModel.inputMode HP12c_Model.White
                    , Expect.equal finalModel.displayPrecision 1
                    ]
                    ()
        , test "Pressing 'g' then 'CHS' (DATE_Key) reverts inputMode to White" <|
            \_ ->
                let
                    -- Assuming CHS_Key after Blue_G_Key triggers DATE_Key (which uses defaultModelTransformer)
                    (modelAfterG, _) = update Blue_G_Key initialModel
                    (finalModel, _) = update CHS_Key modelAfterG 
                in
                Expect.equal finalModel.inputMode HP12c_Model.White
        , test "Pressing 'f' then 'f' (another modifier) should remain Orange or reset based on specific logic" <|
            \_ ->
                let
                    (modelAfterF1, _) = update Orange_F_Key initialModel
                    (modelAfterF2, _) = update Orange_F_Key modelAfterF1
                in
                -- Typical HP calculators reset modifier if same modifier is pressed again, or stay.
                -- Assuming it resets or stays Orange. Let's assume it stays (common behavior for some).
                -- If it's meant to toggle off, this test would need to be Expect.equal modelAfterF2.inputMode HP12c_Model.White
                -- Current setPrefix in utils just sets it, doesn't toggle. So it will stay Orange.
                Expect.equal modelAfterF2.inputMode HP12c_Model.Orange
        ]

errorDisplayTests : Test
errorDisplayTests =
    describe "Error Display State Tests"
        [ test "Division by zero (1 / 0) sets displayString to Error and ErrorState" <|
            \_ ->
                let
                    commands = [ Number_1_Key, Enter_Key, Number_0_Key, Divide_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.all
                    [ Expect.equal finalModel.displayString "Error"
                    , Expect.equal finalModel.calculatorOperationalState (HP12c_Model.Error HP12c_Model.Error_0_Mathematics)
                    , Expect.stringStartsWith "Error 0 DIV BY ZERO" finalModel.message
                    ]
                    ()
        , test "Unimplemented key (AMORT_Key) sets displayString to Error and ErrorState" <|
            \_ ->
                let
                    -- AMORT_Key uses defaultModelTransformer, which should set Error_9_Service
                    (finalModel, _) = update AMORT_Key initialModel
                in
                Expect.all
                    [ Expect.equal finalModel.displayString "Error"
                    , Expect.equal finalModel.calculatorOperationalState (HP12c_Model.Error HP12c_Model.Error_9_Service)
                    , Expect.equal finalModel.message "Unimplemented Op"
                    ]
                    ()
        , test "Square root of negative number (-1 sqrt) sets displayString to Error" <|
            \_ ->
                let
                    commands = [ Number_1_Key, CHS_Key, Enter_Key, Blue_G_Key, Y_toThe_X_Key ] -- g y^x is sqrt
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.all
                    [ Expect.equal finalModel.displayString "Error"
                    , Expect.equal finalModel.calculatorOperationalState (HP12c_Model.Error HP12c_Model.Error_0_Mathematics)
                    , Expect.stringStartsWith "Error 0 SQRT OF NEG" finalModel.message
                    ]
                    ()
        ]

all : Test
all =
    describe "CalculatorCoreLogic Tests"
        [ test "Number entry 123" <|
            \_ ->
                let
                    commands = [ Number_1_Key, Number_2_Key, Number_3_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "123.00"
        , test "Simple addition: 1 + 2 = 3.00" <|
            \_ ->
                let
                    commands = [ Number_1_Key, Enter_Key, Number_2_Key, Sum_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "3.00"
        , test "Simple subtraction: 5 - 2 = 3.00" <|
            \_ ->
                let
                    commands = [ Number_5_Key, Enter_Key, Number_2_Key, Subtract_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "3.00"
        , test "CLx Key: 12 CLx = 0.00" <|
            \_ ->
                let
                    initialWith12 = runCoreCommands [Number_1_Key, Number_2_Key] initialModel
                    commandsCLx = [ CL_x_Key ]
                    finalModel = runCoreCommands commandsCLx initialWith12
                in
                Expect.equal finalModel.displayString "0.00"
        , test "CHS Key: 5 CHS = -5.00" <|
            \_ ->
                let
                    commands = [ Number_5_Key, CHS_Key ]
                    finalModel = runCoreCommands commands initialModel
                in
                Expect.equal finalModel.displayString "-5.00"
        , test "Precision change: 123.456 Enter, f, 2 -> 123.45" <|
            \_ ->
                let
                    initialWithNum = runCoreCommands [Number_1_Key, Number_2_Key, Number_3_Key, Decimal_Point_Key, Number_4_Key, Number_5_Key, Number_6_Key, Enter_Key] initialModel
                    commandsPrecision = [ Orange_F_Key, SetPrecision_2_Key ]
                    finalModel = runCoreCommands commandsPrecision initialWithNum
                in
                Expect.equal finalModel.displayString "123.45"
        , test "CLx after operation: 1 Enter 2 + CLx = 0.00" <|
            \_ ->
                let
                    initialAfterPlus = runCoreCommands [Number_1_Key, Enter_Key, Number_2_Key, Sum_Key] initialModel
                    commandsCLxAfterOp = [ CL_x_Key ]
                    finalModel = runCoreCommands commandsCLxAfterOp initialAfterPlus
                in
                Expect.equal finalModel.displayString "0.00"
        -- Adding the new test suites here
        , modifierKeyTests
        , errorDisplayTests
        ]

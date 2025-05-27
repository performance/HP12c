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

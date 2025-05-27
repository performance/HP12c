module NaturalLanguageParser exposing (parse, beautifyRpnCommands)

import HP12c_KeyTypes exposing (Msg(..))
import String
import Char


-- Helper to check if a string can be parsed as a number (positive or negative)
isNumberString : String -> Bool
isNumberString str =
    let
        coreStr = if String.startsWith "-" str then String.dropLeft 1 str else str
    in
    not (String.isEmpty coreStr) && String.all (\c -> Char.isDigit c || c == '.') coreStr && String.countOccurrences "." coreStr <= 1


-- Pre-processes a list of tokens to combine "minus" with a following number token.
-- Example: ["minus", "5", "plus", "2"] -> ["-5", "plus", "2"]
preprocessTokens : List String -> List String
preprocessTokens tokens =
    case tokens of
        "minus" :: nextToken :: rest ->
            if isNumberString nextToken && not (String.startsWith "-" nextToken) then
                ("-" ++ nextToken) :: preprocessTokens rest
            else
                "minus" :: preprocessTokens (nextToken :: rest) -- "minus" is an operator here or followed by non-number

        token :: rest ->
            token :: preprocessTokens rest

        [] ->
            []


parse : String -> List Msg
parse inputText =
    let
        initialTokens =
            String.split " " inputText
                |> List.filter (not << String.isEmpty)

        processedTokens =
            preprocessTokens initialTokens

        -- Converts a single token string into a list of HP12C_KeyTypes.Msg
        parseTokenInternal : String -> List Msg
        parseTokenInternal token =
            case token of
                "plus" -> [ Sum_Key ]
                "minus" -> [ Subtract_Key ] -- This should only be reached if "minus" is an operator
                "times" -> [ Multiply_Key ]
                "divided" -> [ Divide_Key ]
                "by" -> [] -- Skipped

                _ ->
                    if String.startsWith "-" token then
                        let
                            numberPart = String.dropLeft 1 token
                        in
                        if isNumberString numberPart && not (String.isEmpty numberPart) then -- ensure it's a valid number after stripping "-"
                            parsePositiveNumberString numberPart ++ [ CHS_Key ]
                        else
                            [ UnrecognizedToken_Key token ] -- e.g. "-" or "-abc"

                    else if isNumberString token then
                        parsePositiveNumberString token

                    else
                        [ UnrecognizedToken_Key token ]

        -- Helper to parse a string known to be a positive number
        parsePositiveNumberString : String -> List Msg
        parsePositiveNumberString numStr =
            String.foldl
                (\char acc ->
                    let
                        maybeKey =
                            case char of
                                '0' -> Just Number_0_Key
                                '1' -> Just Number_1_Key
                                '2' -> Just Number_2_Key
                                '3' -> Just Number_3_Key
                                '4' -> Just Number_4_Key
                                '5' -> Just Number_5_Key
                                '6' -> Just Number_6_Key
                                '7' -> Just Number_7_Key
                                '8' -> Just Number_8_Key
                                '9' -> Just Number_9_Key
                                '.' -> Just Decimal_Point_Key
                                _ -> Nothing
                    in
                    case maybeKey of
                        Just key -> acc ++ [ key ]
                        Nothing -> acc
                )
                []
                numStr
        
        -- Helper to determine if a list of Msgs represents a number entry
        -- (e.g., [N1,N2], [N1,DP,N2], [N1,N2,CHS], [N1,DP,N2,CHS])
        -- Excludes single CHS_Key if it's the only thing.
        isNumberEntry : List Msg -> Bool
        isNumberEntry msgs =
            case msgs of
                [] -> False
                [CHS_Key] -> False -- CHS_Key alone is not a number entry in this context
                _ -> List.any (\msg -> case msg of
                                        Number_0_Key -> True; Number_1_Key -> True; Number_2_Key -> True;
                                        Number_3_Key -> True; Number_4_Key -> True; Number_5_Key -> True;
                                        Number_6_Key -> True; Number_7_Key -> True; Number_8_Key -> True;
                                        Number_9_Key -> True; Decimal_Point_Key -> True;
                                        _ -> False
                                ) msgs


        processShuntingYard : List String -> List Msg -> List Msg -> List Msg
        processShuntingYard tokenList currentOutput opStack =
            case tokenList of
                [] ->
                    currentOutput ++ List.reverse opStack

                headToken :: tailTokens ->
                    let
                        parsedKeys = parseTokenInternal headToken
                    in
                    if List.isEmpty parsedKeys then -- Should only be "by" now
                        processShuntingYard tailTokens currentOutput opStack
                    
                    else if isNumberEntry parsedKeys then
                        processShuntingYard tailTokens (currentOutput ++ parsedKeys) opStack

                    else
                        -- It's an operator or UnrecognizedToken_Key
                        case List.head parsedKeys of 
                            Just (UnrecognizedToken_Key _) ->
                                -- Pass unrecognized tokens directly to output without Enter_Key logic
                                processShuntingYard tailTokens (currentOutput ++ parsedKeys) opStack
                            
                            _ -> 
                                -- It's an operator
                                let
                                    outputWithEnter = currentOutput ++ [ Enter_Key ]
                                    outputAfterExistingOps = outputWithEnter ++ List.reverse opStack
                                    newOpStack = parsedKeys
                                in
                                processShuntingYard tailTokens outputAfterExistingOps newOpStack
    in
    processShuntingYard processedTokens [] []


-- Takes a Msg and returns a human-readable string representation.
msgToReadableString : Msg -> String
msgToReadableString msg =
    case msg of
        UnrecognizedToken_Key tokenStr -> "UNKNOWN:" ++ tokenStr
        Number_0_Key -> "0"
        Number_1_Key -> "1"
        Number_2_Key -> "2"
        Number_3_Key -> "3"
        Number_4_Key -> "4"
        Number_5_Key -> "5"
        Number_6_Key -> "6"
        Number_7_Key -> "7"
        Number_8_Key -> "8"
        Number_9_Key -> "9"
        Decimal_Point_Key -> "."
        Enter_Key -> "ENTER"
        Sum_Key -> "+"
        Subtract_Key -> "-"
        Multiply_Key -> "*"
        Divide_Key -> "/"
        PV_Key -> "PV"
        FV_Key -> "FV"
        PMT_Key -> "PMT"
        N_Key -> "n"
        I_Key -> "i"
        STO_Key -> "STO"
        RCL_Key -> "RCL"
        CHS_Key -> "CHS"
        EEX_Key -> "EEX"
        CL_x_Key -> "CLx"
        Roll_Down_Key -> "R↓"
        Exchange_X_Y_Key -> "x<>y"
        Y_toThe_X_Key -> "y^x"
        Reciprocal_Key -> "1/x"
        Square_Root_Key -> "√x"
        Percent_Key -> "%"
        Delta_Percentage_Key -> "Δ%"
        Percentage_T_Key -> "%T"
        Orange_F_Key -> "f"
        Blue_G_Key -> "g"
        ON_Key -> "ON"
        -- For NLP specific messages, probably not expected in RPN command list from parser
        NLPInput _ -> "[NLP Input]"
        ProcessNLP -> "[Process NLP]"
        ExecuteNLPCommands _ -> "[Execute NLP Cmds]"
        -- Fallback for other keys not explicitly listed
        _ -> Basics.toString msg -- Or a more generic "[Key]" if Basics.toString is too noisy


-- Takes a list of Msgs and returns a comma-separated string of readable command names.
beautifyRpnCommands : List Msg -> String
beautifyRpnCommands msgs =
    List.map msgToReadableString msgs
        |> String.join ", "

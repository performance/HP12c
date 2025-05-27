module NaturalLanguageParserTests exposing (all)

import Expect
import Test exposing (..)
import NaturalLanguageParser exposing (parse, beautifyRpnCommands)
import HP12c_KeyTypes exposing (Msg(..))

all : Test
all =
    describe "NaturalLanguageParser Tests"
        [ parseTests
        , beautifyTests
        ]

parseTests : Test
parseTests =
    describe "NaturalLanguageParser.parse"
        [ test "Simple addition" <|
            \_ -> Expect.equal (parse "2 plus 3") [ Number_2_Key, Enter_Key, Number_3_Key, Sum_Key ]
        , test "Simple subtraction" <|
            \_ -> Expect.equal (parse "5 minus 1") [ Number_5_Key, Enter_Key, Number_1_Key, Subtract_Key ]
        , test "Simple multiplication" <|
            \_ -> Expect.equal (parse "4 times 2") [ Number_4_Key, Enter_Key, Number_2_Key, Multiply_Key ]
        , test "Simple division" <|
            \_ -> Expect.equal (parse "10 divided by 2") [ Number_1_Key, Number_0_Key, Enter_Key, Number_2_Key, Divide_Key ]
        , test "Numbers with decimals" <|
            \_ -> Expect.equal (parse "1.5 plus 2.5") [ Number_1_Key, Decimal_Point_Key, Number_5_Key, Enter_Key, Number_2_Key, Decimal_Point_Key, Number_5_Key, Sum_Key ]
        , test "Negative number (-2 plus 3)" <|
            \_ -> Expect.equal (parse "-2 plus 3") [ Number_2_Key, CHS_Key, Enter_Key, Number_3_Key, Sum_Key ]
        , test "Negative number (minus 2 plus 3)" <|
            \_ -> Expect.equal (parse "minus 2 plus 3") [ Number_2_Key, CHS_Key, Enter_Key, Number_3_Key, Sum_Key ]
        , test "Negative number (3 plus -2)" <|
            \_ -> Expect.equal (parse "3 plus -2") [ Number_3_Key, Enter_Key, Number_2_Key, CHS_Key, Sum_Key ]
        , test "Unrecognized token (2 foo 3)" <|
            \_ -> Expect.equal (parse "2 foo 3") [ Number_2_Key, UnrecognizedToken_Key "foo", Number_3_Key ]
        , test "Single number" <|
            \_ -> Expect.equal (parse "123") [ Number_1_Key, Number_2_Key, Number_3_Key ]
        , test "Single unrecognized token" <|
            \_ -> Expect.equal (parse "foo") [ UnrecognizedToken_Key "foo" ]
        , test "Unrecognized negative token" <|
            \_ -> Expect.equal (parse "- foo") [ UnrecognizedToken_Key "- foo" ]
        , test "Number plus operator (incomplete)" <|
            \_ -> Expect.equal (parse "2 plus") [ Number_2_Key, Enter_Key, Sum_Key ]
        , test "Operator plus number (0 + 2 essentially)" <|
            \_ -> Expect.equal (parse "plus 2") [ Enter_Key, Number_2_Key, Sum_Key ]
        , test "Invalid number format" <|
            \_ -> Expect.equal (parse "2. .3") [ Number_2_Key, Decimal_Point_Key, UnrecognizedToken_Key ".3" ]
        , test "Empty input" <|
            \_ -> Expect.equal (parse "") []
        , test "Space-only input" <|
            \_ -> Expect.equal (parse "   ") []
        , test "Leading/trailing spaces" <|
            \_ -> Expect.equal (parse " 2 plus 3 ") [ Number_2_Key, Enter_Key, Number_3_Key, Sum_Key ]
        , test "Longer expression (5 * 2 - 3)" <|
            \_ -> Expect.equal (parse "5 times 2 minus 3") [ Number_5_Key, Enter_Key, Number_2_Key, Multiply_Key, Enter_Key, Number_3_Key, Subtract_Key ]
        ]

beautifyTests : Test
beautifyTests =
    describe "NaturalLanguageParser.beautifyRpnCommands"
        [ test "Beautify: Simple addition" <|
            \_ -> Expect.equal (beautifyRpnCommands [ Number_2_Key, Enter_Key, Number_3_Key, Sum_Key ]) "2, ENTER, 3, +"
        , test "Beautify: Negative decimal" <|
            \_ -> Expect.equal (beautifyRpnCommands [ Number_1_Key, Decimal_Point_Key, Number_5_Key, CHS_Key ]) "1, ., 5, CHS"
        , test "Beautify: Unrecognized token" <|
            \_ -> Expect.equal (beautifyRpnCommands [ UnrecognizedToken_Key "foo" ]) "UNKNOWN:foo"
        , test "Beautify: Division" <|
            \_ -> Expect.equal (beautifyRpnCommands [ Number_1_Key, Number_0_Key, Enter_Key, Number_2_Key, Divide_Key ]) "1, 0, ENTER, 2, /"
        , test "Beautify: Empty list" <|
            \_ -> Expect.equal (beautifyRpnCommands []) ""
        , test "Beautify: Longer expression" <|
            \_ -> Expect.equal (beautifyRpnCommands [ Number_5_Key, Enter_Key, Number_2_Key, Multiply_Key, Enter_Key, Number_3_Key, Subtract_Key ]) "5, ENTER, 2, *, ENTER, 3, -"
        ]

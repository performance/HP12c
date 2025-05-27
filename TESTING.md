# Testing in the HP12c Elm Project

This document provides instructions for setting up the testing environment, running tests, and adding new tests to this project.

## Elm and elm-test Setup

In the automated testing environment, Elm and elm-test were successfully installed locally using the following commands:

1.  **Install Elm (local):**
    ```bash
    npm install --save-dev elm
    ```
    The Elm executable is then available at `./node_modules/.bin/elm`. This was verified to be Elm version 0.19.1.

2.  **Install elm-test (local):**
    ```bash
    npm install --save-dev elm-test
    ```

3.  **Initialize elm-test:**
    An `elm.json` file is required. If one doesn't exist for Elm 0.19 (the project previously used `elm-package.json` for Elm 0.18), you might need to run `./node_modules/.bin/elm init` first.
    Then, initialize `elm-test`:
    ```bash
    echo "Y" | ./node_modules/.bin/elm-test init --compiler=./node_modules/.bin/elm
    ```
    This command automatically adds necessary test dependencies to `elm.json` and creates a `tests/` directory with an example test. The `--compiler` flag points to the locally installed Elm executable.

If you are setting up the project in a new local environment, you will need to have Node.js and npm installed. Then, run the `npm install` commands above to get Elm and elm-test.

## Running Tests

With Elm and elm-test set up:

1.  **Ensure all dependencies are installed:**
    ```bash
    npm install
    # This will install Elm and elm-test based on package.json
    ```
    You may also need to ensure Elm project dependencies are up to date by running `./node_modules/.bin/elm make` or a similar command if `elm-test` reports issues with packages.

2.  **Run the tests:**
    Execute the following command from the project root:
    ```bash
    ./node_modules/.bin/elm-test
    ```
    Or, if you have npx installed or elm-test globally:
    ```bash
    npx elm-test
    # or
    # elm-test
    ```
    This will discover and run all tests defined in the `tests/` directory. Test results will be displayed in the console.

## Test File Structure

Tests are organized into modules within the `tests/` directory:

*   **Natural Language Parser Tests:** `tests/NaturalLanguageParserTests.elm`
*   **Calculator Core Logic Tests:** `tests/CalculatorCoreTests.elm`

## Adding New Tests

1.  **Identify the relevant test file:**
    *   For changes to `NaturalLanguageParser.elm`, add tests to `tests/NaturalLanguageParserTests.elm`.
    *   For changes to the calculator's core logic (e.g., in `HP12c_Update.elm` or `HP12c_Update_utils.elm`), add tests to `tests/CalculatorCoreTests.elm`.

2.  **Write tests using `elm-test` syntax:**
    *   Use `describe` to group related tests.
    *   Use `test` to define individual test cases.
    *   Use assertions from the `Expect` module (e.g., `Expect.equal`, `Expect.isTrue`, `Expect.listEqual`) to verify behavior.

    Example structure for a test:
    ```elm
    module MyModuleTests exposing (..)

    import Expect
    import Test exposing (..)
    -- Import the module you are testing, e.g., import NaturalLanguageParser

    myTestSuite : Test
    myTestSuite =
        describe "Description of the test suite"
            [ test "Description of a specific test case" <|
                \_ ->
                    -- Setup code or function call
                    let
                        actualValue = MyModule.myFunction inputValue
                        expectedValue = ...
                    in
                    Expect.equal actualValue expectedValue
            ]
    ```

3.  Ensure the new test suite is exposed or added to a main test runner if applicable (often `elm-test` discovers all `Test` values automatically).

module SkkTests.SkkTests exposing (..)

import Dict
import Expect
import Skk exposing (SkkConversionMode(..))
import SkkKanaRule
import SkkTests.TestHelper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module"
        [ describe "Skk.init"
            [ test "initialize Skk" <|
                \_ ->
                    Expect.equal
                        { mode = Skk.AsciiMode { kakutei = Nothing }
                        , context =
                            { kanaRules = SkkKanaRule.getDefaultRules
                            , dict = Dict.empty
                            }
                        }
                        (Skk.init { kanaRules = SkkKanaRule.getDefaultRules, dict = Dict.empty })
            ]
        ]

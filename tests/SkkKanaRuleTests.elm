module SkkKanaRuleTests exposing (..)

import Expect
import SkkKanaRule
import Test exposing (..)


suite : Test
suite =
    describe "The SkkKanaRule module"
        [ describe "SkkKanaRule.search"
            [ test "ルールに完全一致しているキーワードを指定すると、完全一致の結果を返すこと(1文字)" <|
                \_ ->
                    let
                        rules =
                            SkkKanaRule.getDefaultRules
                    in
                    Expect.equal
                        (SkkKanaRule.PerfectMatch
                            { key = "a"
                            , hiragana = "あ"
                            , katakana = "ア"
                            , next = Nothing
                            }
                        )
                        (SkkKanaRule.search "a" rules)
            , test "ルールに完全一致しているキーワードを指定すると、完全一致の結果を返すこと(複数文字)" <|
                \_ ->
                    let
                        rules =
                            SkkKanaRule.getDefaultRules
                    in
                    Expect.equal
                        (SkkKanaRule.PerfectMatch
                            { key = "ka"
                            , hiragana = "か"
                            , katakana = "カ"
                            , next = Nothing
                            }
                        )
                        (SkkKanaRule.search "ka" rules)
            , test "ルールに部分一致しているキーワードを指定すると、部分一致の結果を返すこと" <|
                \_ ->
                    let
                        rules =
                            SkkKanaRule.getDefaultRules
                    in
                    Expect.equal
                        SkkKanaRule.PartialMatch
                        (SkkKanaRule.search "k" rules)
            , test "ルールに一致しないキーワードを指定すると、一致なしの結果を返すこと" <|
                \_ ->
                    let
                        rules =
                            SkkKanaRule.getDefaultRules
                    in
                    Expect.equal
                        SkkKanaRule.NoMatch
                        (SkkKanaRule.search "kx" rules)
            , test "半角スペースを含んだキーワードを指定した時に、該当する変換ルールを返すこと" <|
                \_ ->
                    let
                        rules =
                            SkkKanaRule.getDefaultRules
                    in
                    Expect.equal
                        (SkkKanaRule.PerfectMatch
                            { key = "z "
                            , hiragana = "\u{3000}"
                            , katakana = "\u{3000}"
                            , next = Nothing
                            }
                        )
                        (SkkKanaRule.search "z " rules)
            ]
        ]

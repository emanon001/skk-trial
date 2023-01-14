module SkkTests exposing (..)

import Dict
import Expect
import Skk
import SkkKanaRule
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module"
        [ describe "Skk.init"
            [ test "initialize Skk" <|
                \_ ->
                    Expect.equal
                        { mode = Skk.AsciiInputMode { input = "" }
                        , context =
                            { kanaRules = SkkKanaRule.getDefaultRules
                            , dict = Dict.empty
                            }
                        }
                        (Skk.init { kanaRules = SkkKanaRule.getDefaultRules, dict = Dict.empty })
            ]
        , describe "Skk.update"
            [ describe "Ascii入力モード"
                [ test "入力したキーが確定済みの文字列に追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { input = "" })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal { skk | mode = Skk.AsciiInputMode { input = "a" } } (Skk.update skk key)
                , test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { input = "abc" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal { skk | mode = Skk.AsciiInputMode { input = "ab" } } (Skk.update skk key)
                , test "確定済みの文字列が空の時にBSキーを入力すると、確定済み文字列が空のままになること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { input = "" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal { skk | mode = Skk.AsciiInputMode { input = "" } } (Skk.update skk key)
                , test "Ctrl-jを入力すると、ひらがなモードに切り替わること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { input = "abc" })

                            key =
                                { key = "j", shift = False, ctrl = True }
                        in
                        Expect.equal { skk | mode = Skk.HiraganaInputMode { kakutei = "abc", mikakutei = "" } } (Skk.update skk key)
                ]
            , describe "ひらがな入力モード"
                [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", mikakutei = "" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal { skk | mode = Skk.HiraganaInputMode { kakutei = "あい", mikakutei = "" } } (Skk.update skk key)
                , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", mikakutei = "sy" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal { skk | mode = Skk.HiraganaInputMode { kakutei = "あいう", mikakutei = "s" } } (Skk.update skk key)
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", mikakutei = "s" })

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal { skk | mode = Skk.HiraganaInputMode { kakutei = "あいう ", mikakutei = "" } } (Skk.update skk key)
                ]
            ]
        ]



-- helper


initSkk : Skk.SkkMode -> Skk.Skk
initSkk mode =
    { mode = mode
    , context =
        { kanaRules = SkkKanaRule.getDefaultRules
        , dict = Dict.empty
        }
    }

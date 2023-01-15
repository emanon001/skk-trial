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
                        { mode = Skk.AsciiInputMode { kakutei = "" }
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
                                initSkk (Skk.AsciiInputMode { kakutei = "" })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiInputMode { kakutei = "a" }) (Skk.update skk key).mode
                , test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { kakutei = "abc" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiInputMode { kakutei = "ab" }) (Skk.update skk key).mode
                , test "確定済みの文字列が空の時にBSキーを入力すると、確定済み文字列が空のままになること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { kakutei = "" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiInputMode { kakutei = "" }) (Skk.update skk key).mode
                , test "Ctrl-jを入力すると、ひらがなモードに切り替わること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiInputMode { kakutei = "abc" })

                            key =
                                { key = "j", shift = False, ctrl = True }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "abc", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 確定入力モード)"
                [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あい", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } }) (Skk.update skk key).mode
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あいう ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あいうしゃ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あいうっ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない場合は、未確定の文字列に入力したキーが設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaInputMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "b" } }) (Skk.update skk key).mode
                ]
            ]
        ]



-- helper


initSkk : Skk.SkkInputMode -> Skk.Skk
initSkk mode =
    { mode = mode
    , context =
        { kanaRules = SkkKanaRule.getDefaultRules
        , dict = Dict.empty
        }
    }

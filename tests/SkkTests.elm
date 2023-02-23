module SkkTests exposing (..)

import Dict
import Expect
import Skk exposing (SkkConvertMode(..))
import SkkDict
import SkkKanaRule
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module"
        [ describe "Skk.init"
            [ test "initialize Skk" <|
                \_ ->
                    Expect.equal
                        { mode = Skk.AsciiMode { kakutei = "" }
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
                                initSkk (Skk.AsciiMode { kakutei = "" })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = "a" }) (Skk.update skk key).mode
                , test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiMode { kakutei = "abc" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = "ab" }) (Skk.update skk key).mode
                , test "確定済みの文字列が空の時にBSキーを入力すると、確定済み文字列が空のままになること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiMode { kakutei = "" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = "" }) (Skk.update skk key).mode
                , test "Ctrl-jを入力すると、ひらがなモードに切り替わること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiMode { kakutei = "abc" })

                            key =
                                { key = "j", shift = False, ctrl = True }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "abc", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 確定入力モード)"
                [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あい", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } }) (Skk.update skk key).mode
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいう ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいうしゃ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいうっ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の文字列に入力したキーが設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "b" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "y" } })

                            key =
                                { key = "i", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいうい", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "qキーを入力するとカタカナモードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sh" } })

                            key =
                                { key = "q", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "あいう", convertMode = KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "アルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "" } })

                            key =
                                { key = "S", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode { initMidashi | mikakutei = "s" } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在すると時にアルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.KakuteiInputMode { mikakutei = "sh" } })

                            key =
                                { key = "A", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode { initMidashi | kakutei = "しゃ" } }) (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 見出し語入力モード)"
                [ test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の見出し語の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "かきく", mikakutei = "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode =
                                    Skk.MidashiInputMode
                                        { kakutei = "かきく", mikakutei = "sy" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する場合は、確定済みの見出し語の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "かきく", mikakutei = "sy" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode =
                                    Skk.MidashiInputMode
                                        { kakutei = "かきくしゃ", mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの見出し語の末尾に変換結果が追加される かつ 未確定の見出し語に次の文字が設定されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "かきく", mikakutei = "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode =
                                    Skk.MidashiInputMode
                                        { kakutei = "かきくっ", mikakutei = "s" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の見出し語に入力したキーが設定されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "かきく", mikakutei = "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode =
                                    Skk.MidashiInputMode
                                        { kakutei = "かきく", mikakutei = "b" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの見出し語の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "かきく", mikakutei = "y" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "i", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode =
                                    Skk.MidashiInputMode
                                        { kakutei = "かきくい", mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "見出し語を入力中にEnterキーを入力すると、確定済みの文字列の末尾に確定した見出し語が追加され、確定入力モードに遷移すること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "ねこ", mikakutei = "cy" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "Enter", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいうねこ"
                                , convertMode = Skk.KakuteiInputMode { mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "未確定の見出し語が存在しない場合、BSキーを入力すると確定済みの見出し語が削除されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "ねこ", mikakutei = "" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.MidashiInputMode { kakutei = "ね", mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "未確定の見出し語が存在する場合、BSキーを入力すると見確定の見出し語が削除されること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "ね", mikakutei = "t" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.MidashiInputMode { kakutei = "ね", mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "見出し語を入力している最中にCtrl-gキーを入力すると、見出し入力モードがキャンセルされること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "ねこ", mikakutei = "" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "g", shift = False, ctrl = True }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.KakuteiInputMode { mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "見出し語を入力している最中にSpaceキーを入力すると、辞書変換モードに遷移すること" <|
                    \_ ->
                        let
                            convertValue =
                                { kakutei = "きょう", mikakutei = "" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiInputMode convertValue })

                            key =
                                { key = "Space", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode =
                                    Skk.DictConvertMode
                                        { prevMode = Skk.PreDictConvertMidashiInputMode convertValue
                                        , candidateList = [ "今日", "京", "強" ]
                                        , pos = 0
                                        }
                                }
                            )
                            (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 見出し語入力モード/送り仮名)"
                [ test "確定済みの送り仮名が存在する かつ 未確定の送り仮名が存在しない場合、BSキーを入力すると確定済みの送り仮名が削除されること" <|
                    \_ ->
                        let
                            midashi =
                                { kakutei = "はし", mikakutei = "" }

                            convertValue =
                                { midashi = midashi
                                , kakutei = "っ"
                                , mikakutei = ""
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiOkuriInputMode convertValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.MidashiOkuriInputMode { midashi = midashi, kakutei = "", mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "確定済みの送り仮名が存在する かつ 未確定の送り仮名が存在する場合、BSキーを入力すると未確定の送り仮名が削除されること" <|
                    \_ ->
                        let
                            midashi =
                                { kakutei = "はし", mikakutei = "" }

                            convertValue =
                                { midashi = midashi
                                , kakutei = "っ"
                                , mikakutei = "t"
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiOkuriInputMode convertValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.MidashiOkuriInputMode { midashi = midashi, kakutei = "っ", mikakutei = "" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "送り仮名を入力している最中にCtrl-gキーを入力すると、送り仮名の入力がキャンセルされること" <|
                    \_ ->
                        let
                            midashi =
                                { kakutei = "はし", mikakutei = "" }

                            convertValue =
                                { midashi = midashi
                                , kakutei = "t"
                                , mikakutei = ""
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = "あいう", convertMode = Skk.MidashiOkuriInputMode convertValue })

                            key =
                                { key = "g", shift = False, ctrl = True }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.MidashiInputMode midashi
                                }
                            )
                            (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 辞書変換モード)"
                [ test "次の変換候補が存在する場合、Spaceキーを入力すると、次の変換候補が選択されること" <|
                    \_ ->
                        let
                            preConvertValue =
                                { kakutei = "きょう", mikakutei = "" }

                            convertValue =
                                { prevMode = Skk.PreDictConvertMidashiInputMode preConvertValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 0
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = "あいう"
                                        , convertMode = Skk.DictConvertMode convertValue
                                        }
                                    )

                            key =
                                { key = "Space", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.DictConvertMode { convertValue | pos = 1 }
                                }
                            )
                            (Skk.update skk key).mode
                , test "次の変換候補が存在しない場合、Spaceキーを入力すると、状態が変わらないこと" <|
                    \_ ->
                        let
                            preConvertValue =
                                { kakutei = "きょう", mikakutei = "" }

                            convertValue =
                                { prevMode = Skk.PreDictConvertMidashiInputMode preConvertValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 2
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = "あいう"
                                        , convertMode = Skk.DictConvertMode convertValue
                                        }
                                    )

                            key =
                                { key = "Space", shift = False, ctrl = False }
                        in
                        Expect.equal
                            skk.mode
                            (Skk.update skk key).mode
                , test "前の変換候補が存在する場合、xキーを入力すると、前の変換候補が選択されること" <|
                    \_ ->
                        let
                            preConvertValue =
                                { kakutei = "きょう", mikakutei = "" }

                            convertValue =
                                { prevMode = Skk.PreDictConvertMidashiInputMode preConvertValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 1
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = "あいう"
                                        , convertMode = Skk.DictConvertMode convertValue
                                        }
                                    )

                            key =
                                { key = "x", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.DictConvertMode { convertValue | pos = 0 }
                                }
                            )
                            (Skk.update skk key).mode
                , test "前の変換候補が存在しない場合、xキーを入力すると、直前の変換モードに遷移すること" <|
                    \_ ->
                        let
                            preConvertValue =
                                { kakutei = "きょう", mikakutei = "" }

                            convertValue =
                                { prevMode = Skk.PreDictConvertMidashiInputMode preConvertValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 0
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = "あいう"
                                        , convertMode = Skk.DictConvertMode convertValue
                                        }
                                    )

                            key =
                                { key = "x", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = "あいう"
                                , convertMode = Skk.MidashiInputMode preConvertValue
                                }
                            )
                            (Skk.update skk key).mode
                ]
            , describe "カタカナ入力モード(変換モード: 確定入力モード)"
                [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } }) (Skk.update skk key).mode
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウ ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが部分的に存在する場合は、未確定の文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "sy" } })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウシャ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウッ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の文字列に入力したキーが設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "s" } })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "b" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "y" } })

                            key =
                                { key = "i", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウイ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "qキーを入力するとひらがなモードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "sh" } })

                            key =
                                { key = "q", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = "アイウ", convertMode = KakuteiInputMode { mikakutei = "" } }) (Skk.update skk key).mode
                , test "アルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "" } })

                            key =
                                { key = "S", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.MidashiInputMode { kakutei = "", mikakutei = "s" } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在すると時にアルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.KakuteiInputMode { mikakutei = "sh" } })

                            key =
                                { key = "A", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = "アイウ", convertMode = Skk.MidashiInputMode { kakutei = "シャ", mikakutei = "" } }) (Skk.update skk key).mode
                ]
            ]
        ]



-- helper


initSkk : Skk.SkkInputMode -> Skk.Skk
initSkk mode =
    let
        dictString =
            """
            ねこ /猫/
            だいすk /大好/
            きょう /今日/京/強/
            """
    in
    { mode = mode
    , context =
        { kanaRules = SkkKanaRule.getDefaultRules
        , dict = SkkDict.fromDictString dictString
        }
    }


initMidashiModeValue =
    { midashi = initMidashi
    , okuri = initOkuri
    }


initMidashi =
    { kakutei = ""
    , mikakutei = ""
    }


initOkuri =
    { kakutei = ""
    , mikakutei = ""
    }

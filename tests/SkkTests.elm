module SkkTests exposing (..)

import Dict
import Expect
import Skk exposing (SkkConversionMode(..))
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
                        { mode = Skk.AsciiMode { kakutei = Nothing }
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
                                initSkk (Skk.AsciiMode { kakutei = Nothing })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = Just "a" }) (Skk.update skk key).mode
                , test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiMode { kakutei = Just "abc" })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = Just "ab" }) (Skk.update skk key).mode
                , test "確定済みの文字列が空の時にBSキーを入力すると、確定済み文字列が空のままになること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiMode { kakutei = Nothing })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = Nothing }) (Skk.update skk key).mode
                , test "Ctrl-jを入力すると、ひらがなモードに切り替わること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.AsciiMode { kakutei = Just "abc" })

                            key =
                                { key = "j", shift = False, ctrl = True }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "abc", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 確定入力モード)"
                [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あい", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいうしゃ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいうっ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の文字列に入力したキーが設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "b" } }) (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "y" } })

                            key =
                                { key = "i", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいうい", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "lキーを入力するとAsciiモードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                            key =
                                { key = "l", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { kakutei = Just "あいう" }) (Skk.update skk key).mode
                , test "qキーを入力するとカタカナモードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                            key =
                                { key = "q", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "あいう", conversionMode = KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "アルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                            key =
                                { key = "S", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode { mikakuteiMidashi = Just "s", kakuteiMidashi = Nothing } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在すると時にアルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                            key =
                                { key = "A", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode { mikakuteiMidashi = Nothing, kakuteiMidashi = Just "しゃ" } }) (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 見出し語入力モード)"
                [ test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の見出し語の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.MidashiInputMode
                                        { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "sy" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する場合は、確定済みの見出し語の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "sy" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.MidashiInputMode
                                        { kakuteiMidashi = Just "かきくしゃ", mikakuteiMidashi = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの見出し語の末尾に変換結果が追加される かつ 未確定の見出し語に次の文字が設定されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.MidashiInputMode
                                        { kakuteiMidashi = Just "かきくっ", mikakuteiMidashi = Just "s" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の見出し語に入力したキーが設定されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.MidashiInputMode
                                        { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "b" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "確定済みの文字列のみ存在する場合は、Shift + アルファベット(母音を除く)を入力すると送り仮名を入力モードに遷移すること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "はし", mikakuteiMidashi = Nothing }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "R", shift = True, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.MidashiOkuriInputMode
                                        { midashi = conversionValue, headOkuri = Just "r", kakuteiOkuri = Nothing, mikakuteiOkuri = Just "r" }
                                }
                            )
                            (Skk.update skk key).mode
                , test "確定済みの文字列が存在しない場合は、Shift + アルファベット(母音を除く)を入力しても無視されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Nothing, mikakuteiMidashi = Nothing }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "R", shift = True, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode conversionValue
                                }
                            )
                            (Skk.update skk key).mode
                , test "未確定の文字列が存在する場合は、Shift + アルファベット(母音を除く)を入力しても無視されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "は", mikakuteiMidashi = Just "s" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "R", shift = True, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode conversionValue
                                }
                            )
                            (Skk.update skk key).mode
                , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの見出し語の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "y" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "i", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.MidashiInputMode
                                        { kakuteiMidashi = Just "かきくい", mikakuteiMidashi = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "未確定の見出し語が存在しない場合、BSキーを入力すると確定済みの見出し語が削除されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "ねこ", mikakuteiMidashi = Nothing }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode { kakuteiMidashi = Just "ね", mikakuteiMidashi = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "未確定の見出し語が存在する場合、BSキーを入力すると見確定の見出し語が削除されること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "ね", mikakuteiMidashi = Just "t" }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode { kakuteiMidashi = Just "ね", mikakuteiMidashi = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "見出し語を入力している最中にCtrl-gキーを入力すると、見出し入力モードがキャンセルされること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "ねこ", mikakuteiMidashi = Nothing }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "g", shift = False, ctrl = True }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "見出し語を入力している最中にSpaceキーを入力すると、辞書変換モードに遷移すること" <|
                    \_ ->
                        let
                            conversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                            key =
                                { key = "Space", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.DictConversionMode
                                        { prevMode = Skk.PrevDictConversionMidashiInputMode conversionValue
                                        , candidateList = [ "今日", "京", "強" ]
                                        , pos = 0
                                        , okuri = Nothing
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
                                { kakuteiMidashi = Just "はし", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { midashi = midashi
                                , headOkuri = Just "t"
                                , kakuteiOkuri = Just "っ"
                                , mikakuteiOkuri = Nothing
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiOkuriInputMode conversionValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiOkuriInputMode { midashi = midashi, headOkuri = Nothing, kakuteiOkuri = Nothing, mikakuteiOkuri = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "確定済みの送り仮名が存在する かつ 未確定の送り仮名が存在する場合、BSキーを入力すると未確定の送り仮名が削除されること" <|
                    \_ ->
                        let
                            midashi =
                                { kakuteiMidashi = Just "はし", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { midashi = midashi
                                , headOkuri = Just "t"
                                , kakuteiOkuri = Just "っ"
                                , mikakuteiOkuri = Just "t"
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiOkuriInputMode conversionValue })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiOkuriInputMode { midashi = midashi, headOkuri = Just "t", kakuteiOkuri = Just "っ", mikakuteiOkuri = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                , test "送り仮名を入力している最中にCtrl-gキーを入力すると、送り仮名の入力がキャンセルされること" <|
                    \_ ->
                        let
                            midashi =
                                { kakuteiMidashi = Just "はし", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { midashi = midashi
                                , headOkuri = Just "t"
                                , kakuteiOkuri = Nothing
                                , mikakuteiOkuri = Just "t"
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiOkuriInputMode conversionValue })

                            key =
                                { key = "g", shift = False, ctrl = True }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode midashi
                                }
                            )
                            (Skk.update skk key).mode
                , test "送り仮名が確定すると、辞書変換モードに遷移すること" <|
                    \_ ->
                        let
                            midashi =
                                { kakuteiMidashi = Just "はし", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { midashi = midashi
                                , headOkuri = Just "r"
                                , kakuteiOkuri = Nothing
                                , mikakuteiOkuri = Just "r"
                                }

                            skk =
                                initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiOkuriInputMode conversionValue })

                            key =
                                { key = "u", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode =
                                    Skk.DictConversionMode
                                        { prevMode = Skk.PrevDictConversionMidashiInputMode conversionValue.midashi
                                        , pos = 0
                                        , candidateList = [ "走", "奔" ]
                                        , okuri = Just "る"
                                        }
                                }
                            )
                            (Skk.update skk key).mode
                ]
            , describe "ひらがな入力モード(変換モード: 辞書変換モード)"
                [ test "次の変換候補が存在する場合、Spaceキーを入力すると、次の変換候補が選択されること" <|
                    \_ ->
                        let
                            preConversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { prevMode = Skk.PrevDictConversionMidashiInputMode preConversionValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 0
                                , okuri = Nothing
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = Just "あいう"
                                        , conversionMode = Skk.DictConversionMode conversionValue
                                        }
                                    )

                            key =
                                { key = "Space", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.DictConversionMode { conversionValue | pos = 1 }
                                }
                            )
                            (Skk.update skk key).mode
                , test "次の変換候補が存在しない場合、Spaceキーを入力すると、状態が変わらないこと" <|
                    \_ ->
                        let
                            preConversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { prevMode = Skk.PrevDictConversionMidashiInputMode preConversionValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 2
                                , okuri = Nothing
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = Just "あいう"
                                        , conversionMode = Skk.DictConversionMode conversionValue
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
                            preConversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { prevMode = Skk.PrevDictConversionMidashiInputMode preConversionValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 1
                                , okuri = Nothing
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = Just "あいう"
                                        , conversionMode = Skk.DictConversionMode conversionValue
                                        }
                                    )

                            key =
                                { key = "x", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.DictConversionMode { conversionValue | pos = 0 }
                                }
                            )
                            (Skk.update skk key).mode
                , test "前の変換候補が存在しない場合、xキーを入力すると、直前の変換モードに遷移すること" <|
                    \_ ->
                        let
                            preConversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { prevMode = Skk.PrevDictConversionMidashiInputMode preConversionValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 0
                                , okuri = Nothing
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = Just "あいう"
                                        , conversionMode = Skk.DictConversionMode conversionValue
                                        }
                                    )

                            key =
                                { key = "x", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode preConversionValue
                                }
                            )
                            (Skk.update skk key).mode
                , test "Ctrl-gキーを入力すると、直前の変換モードに遷移すること" <|
                    \_ ->
                        let
                            preConversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { prevMode = Skk.PrevDictConversionMidashiInputMode preConversionValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 0
                                , okuri = Nothing
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = Just "あいう"
                                        , conversionMode = Skk.DictConversionMode conversionValue
                                        }
                                    )

                            key =
                                { key = "g", shift = False, ctrl = True }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう"
                                , conversionMode = Skk.MidashiInputMode preConversionValue
                                }
                            )
                            (Skk.update skk key).mode
                , test "Enterキーを入力すると、選択中の変換候補を確定済みの文字列の末尾に追加すること" <|
                    \_ ->
                        let
                            preConversionValue =
                                { kakuteiMidashi = Just "きょう", mikakuteiMidashi = Nothing }

                            conversionValue =
                                { prevMode = Skk.PrevDictConversionMidashiInputMode preConversionValue
                                , candidateList = [ "今日", "京", "強" ]
                                , pos = 1
                                , okuri = Nothing
                                }

                            skk =
                                initSkk
                                    (Skk.HiraganaMode
                                        { kakutei = Just "あいう"
                                        , conversionMode = Skk.DictConversionMode conversionValue
                                        }
                                    )

                            key =
                                { key = "Enter", shift = False, ctrl = False }
                        in
                        Expect.equal
                            (Skk.HiraganaMode
                                { kakutei = Just "あいう京"
                                , conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing }
                                }
                            )
                            (Skk.update skk key).mode
                ]
            , describe "カタカナ入力モード(変換モード: 確定入力モード)"
                [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが部分的に存在する場合は、未確定の文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = "y", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウシャ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = "s", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウッ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の文字列に入力したキーが設定されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                            key =
                                { key = "b", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "b" } }) (Skk.update skk key).mode
                , test "ローマ字からカタカナへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの文字列の末尾に入力したキーが追加されること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "y" } })

                            key =
                                { key = "i", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウイ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "qキーを入力するとひらがなモードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                            key =
                                { key = "q", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { kakutei = Just "アイウ", conversionMode = KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
                , test "アルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                            key =
                                { key = "S", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.MidashiInputMode { kakuteiMidashi = Nothing, mikakuteiMidashi = Just "s" } }) (Skk.update skk key).mode
                , test "未確定の文字列が存在すると時にアルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                    \_ ->
                        let
                            skk =
                                initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                            key =
                                { key = "A", shift = True, ctrl = False }
                        in
                        Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.MidashiInputMode { kakuteiMidashi = Just "シャ", mikakuteiMidashi = Nothing } }) (Skk.update skk key).mode
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
            はしr /走/奔/
            """
    in
    { mode = mode
    , context =
        { kanaRules = SkkKanaRule.getDefaultRules
        , dict = SkkDict.fromDictString dictString
        }
    }

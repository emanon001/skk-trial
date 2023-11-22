module SkkTests.SkkHiraganaPrevDictConversionMidashiInputModeTests exposing (..)

import Expect
import Skk exposing (SkkConversionMode(..))
import SkkTests.TestHelper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module / ひらがな入力モード(変換モード: 辞書変換モード)"
        [ describe "Skk.update"
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
                            buildPlainInputKey "Space"
                    in
                    Expect.equal
                        (Skk.HiraganaMode
                            { kakutei = Just "あいう"
                            , conversionMode = Skk.DictConversionMode { conversionValue | pos = 1 }
                            }
                        )
                        (Skk.update skk key).mode
            , test "次の変換候補が存在しない場合、Spaceキーを入力すると、辞書登録モードに遷移すること" <|
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
                            buildPlainInputKey "Space"
                    in
                    Expect.equal
                        (Skk.HiraganaMode
                            { kakutei = Just "あいう"
                            , conversionMode =
                                Skk.DictRegistrationMode
                                    { prevMode = Skk.PrevDictRegistrationDictConversionMode conversionValue
                                    , inputMode =
                                        Skk.HiraganaMode
                                            { kakutei = Nothing
                                            , conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing }
                                            }
                                    }
                            }
                        )
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
                            buildPlainInputKey "x"
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
                            buildPlainInputKey "x"
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
                            buildPlainInputKey "Enter"
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
                            buildPlainInputKey "BackSpace"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                        key =
                            buildPlainInputKey "BackSpace"
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
                            buildPlainInputKey "y"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } }) (Skk.update skk key).mode
            , test "ローマ字からカタカナへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                        key =
                            buildPlainInputKey "a"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウシャ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "ローマ字からカタカナへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                        key =
                            buildPlainInputKey "s"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウッ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
            , test "ローマ字からカタカナへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の文字列に入力したキーが設定されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                        key =
                            buildPlainInputKey "b"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "b" } }) (Skk.update skk key).mode
            , test "ローマ字からカタカナへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの文字列の末尾に入力したキーが追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "y" } })

                        key =
                            buildPlainInputKey "i"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウイ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "qキーを入力するとひらがなモードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                        key =
                            buildPlainInputKey "q"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "アイウ", conversionMode = KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "アルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                        key =
                            buildMidashiInputKey "S"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.MidashiInputMode { kakuteiMidashi = Nothing, mikakuteiMidashi = Just "s" } }) (Skk.update skk key).mode
            , test "未確定の文字列が存在すると時にアルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                        key =
                            buildMidashiInputKey "A"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "アイウ", conversionMode = Skk.MidashiInputMode { kakuteiMidashi = Just "シャ", mikakuteiMidashi = Nothing } }) (Skk.update skk key).mode
            ]
        ]

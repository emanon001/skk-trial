module SkkTests.SkkHiraganaMidashiInputModeTests exposing (..)

import Expect
import Skk exposing (SkkConversionMode(..))
import SkkTests.TestHelper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module / ひらがな入力モード(変換モード: 見出し語入力モード)"
        [ describe "Skk.update"
            [ test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の見出し語の末尾に入力したキーが追加されること" <|
                \_ ->
                    let
                        conversionValue =
                            { kakuteiMidashi = Just "かきく", mikakuteiMidashi = Just "s" }

                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                        key =
                            buildPlainInputKey "y"
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
                            buildPlainInputKey "a"
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
                            buildPlainInputKey "s"
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
                            buildPlainInputKey "b"
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
                            buildMidashiInputKey "R"
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
                            buildMidashiInputKey "R"
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
                            buildMidashiInputKey "R"
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
                            buildPlainInputKey "i"
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
                            buildPlainInputKey "BackSpace"
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
                            buildPlainInputKey "BackSpace"
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
                            buildPlainInputKey "Space"
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
            , test "見出し語を入力している最中にSpaceキーを入力する かつ 変換候補がない場合、辞書登録モードに遷移すること" <|
                \_ ->
                    let
                        conversionValue =
                            { kakuteiMidashi = Just "こうほなし", mikakuteiMidashi = Nothing }

                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode conversionValue })

                        key =
                            buildPlainInputKey "Space"
                    in
                    Expect.equal
                        (Skk.HiraganaMode
                            { kakutei = Just "あいう"
                            , conversionMode =
                                Skk.DictRegistrationMode
                                    { prevMode = Skk.PrevDictRegistrationMidashiInputMode conversionValue
                                    , inputMode =
                                        Skk.HiraganaMode
                                            { kakutei = Nothing
                                            , conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing }
                                            }
                                    }
                            }
                        )
                        (Skk.update skk key).mode
            ]
        ]

module SkkTests.SkkHiraganaMidashiOkuriInputMode exposing (..)

import Expect
import Skk exposing (SkkConversionMode(..))
import SkkTests.TestHelper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module / ひらがな入力モード(変換モード: 見出し語入力モード/送り仮名)"
        [ describe "Skk.update"
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
                            buildPlainInputKey "BackSpace"
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
                            buildPlainInputKey "BackSpace"
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
                            buildPlainInputKey "u"
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
            , test "送り仮名が確定する かつ 変換候補がない場合、辞書登録モードに遷移すること" <|
                \_ ->
                    let
                        midashi =
                            { kakuteiMidashi = Just "こうほなし", mikakuteiMidashi = Nothing }

                        conversionValue =
                            { midashi = midashi
                            , headOkuri = Just "r"
                            , kakuteiOkuri = Nothing
                            , mikakuteiOkuri = Just "r"
                            }

                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiOkuriInputMode conversionValue })

                        key =
                            buildPlainInputKey "u"
                    in
                    Expect.equal
                        (Skk.HiraganaMode
                            { kakutei = Just "あいう"
                            , conversionMode =
                                Skk.DictRegistrationMode
                                    { prevMode = Skk.PrevDictRegistrationMidashiInputMode midashi
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

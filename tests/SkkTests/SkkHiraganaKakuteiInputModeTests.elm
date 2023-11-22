module SkkTests.SkkHiraganaKakuteiInputModeTests exposing (..)

import Expect
import Skk exposing (SkkConversionMode(..))
import SkkTests.TestHelper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module / ひらがな入力モード(変換モード: 確定入力モード)"
        [ describe "Skk.update"
            [ test "未確定の文字列が存在しない場合、BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                        key =
                            buildPlainInputKey "BackSpace"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あい", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "未確定の文字列が存在する場合、BSキーを入力すると、未確定文字列の末尾文字が削除されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                        key =
                            buildPlainInputKey "BackSpace"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
            , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                        key =
                            buildPlainInputKey " "
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "ローマ字からひらがなへの変換ルールが部分的に存在する場合は、未確定の文字列の末尾に入力したキーが追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                        key =
                            buildPlainInputKey "y"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } }) (Skk.update skk key).mode
            , test "ローマ字からひらがなへの変換ルールが存在する場合は、確定済みの文字列の末尾に変換結果が追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sy" } })

                        key =
                            buildPlainInputKey "a"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいうしゃ", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "ローマ字からひらがなへの変換ルールが存在する かつ 次の文字が存在する場合は、確定済みの文字列の末尾に変換結果が追加される かつ 未確定の文字列に次の文字が設定されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                        key =
                            buildPlainInputKey "s"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいうっ", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } }) (Skk.update skk key).mode
            , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が未確定の場合は、未確定の文字列に入力したキーが設定されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "s" } })

                        key =
                            buildPlainInputKey "b"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "b" } }) (Skk.update skk key).mode
            , test "ローマ字からひらがなへの変換ルールが存在しない かつ 入力した文字が確定する場合は、確定済みの文字列の末尾に入力したキーが追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "y" } })

                        key =
                            buildPlainInputKey "i"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいうい", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "lキーを入力するとAsciiモードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                        key =
                            buildPlainInputKey "l"
                    in
                    Expect.equal (Skk.AsciiMode { kakutei = Just "あいう" }) (Skk.update skk key).mode
            , test "qキーを入力するとカタカナモードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                        key =
                            buildPlainInputKey "q"
                    in
                    Expect.equal (Skk.KatakanaMode { kakutei = Just "あいう", conversionMode = KakuteiInputMode { mikakutei = Nothing } }) (Skk.update skk key).mode
            , test "アルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Nothing } })

                        key =
                            buildMidashiInputKey "S"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode { mikakuteiMidashi = Just "s", kakuteiMidashi = Nothing } }) (Skk.update skk key).mode
            , test "未確定の文字列が存在すると時にアルファベットの大文字を入力すると見出し語入力モードに遷移すること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.KakuteiInputMode { mikakutei = Just "sh" } })

                        key =
                            buildMidashiInputKey "A"
                    in
                    Expect.equal (Skk.HiraganaMode { kakutei = Just "あいう", conversionMode = Skk.MidashiInputMode { mikakuteiMidashi = Nothing, kakuteiMidashi = Just "しゃ" } }) (Skk.update skk key).mode
            ]
        ]

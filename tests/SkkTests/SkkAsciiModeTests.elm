module SkkTests.SkkAsciiModeTests exposing (..)

import Expect
import Skk exposing (SkkConversionMode(..))
import SkkTests.TestHelper exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module / Ascii入力モード"
        [ describe "Skk.update"
            [ test "入力したキーが確定済みの文字列に追加されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.AsciiMode { kakutei = Nothing })

                        key =
                            buildPlainInputKey "a"
                    in
                    Expect.equal (Skk.AsciiMode { kakutei = Just "a" }) (Skk.update skk key).mode
            , test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.AsciiMode { kakutei = Just "abc" })

                        key =
                            buildPlainInputKey "BackSpace"
                    in
                    Expect.equal (Skk.AsciiMode { kakutei = Just "ab" }) (Skk.update skk key).mode
            , test "確定済みの文字列が空の時にBSキーを入力すると、確定済み文字列が空のままになること" <|
                \_ ->
                    let
                        skk =
                            initSkk (Skk.AsciiMode { kakutei = Nothing })

                        key =
                            buildPlainInputKey "BackSpace"
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
        ]

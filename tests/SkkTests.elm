module SkkTests exposing (..)

import Expect
import Skk
import Test exposing (..)


suite : Test
suite =
    describe "The Skk module"
        [ describe "Skk.init"
            [ test "initialize Skk" <|
                \_ ->
                    Expect.equal (Skk.AsciiMode { input = "" }) Skk.init
            ]
        , describe "Skk.update"
            [ describe "ascii mode"
                [ test "入力したキーが確定済みの文字列に追加されること" <|
                    \_ ->
                        let
                            skk =
                                Skk.AsciiMode { input = "" }

                            key =
                                { key = "a", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { input = "a" }) (Skk.update skk key)
                , test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                Skk.AsciiMode { input = "abc" }

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { input = "ab" }) (Skk.update skk key)
                , test "確定済みの文字列が空の時にBSキーを入力すると、確定済み文字列が空のままになること" <|
                    \_ ->
                        let
                            skk =
                                Skk.AsciiMode { input = "" }

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.AsciiMode { input = "" }) (Skk.update skk key)
                , test "Ctrl-jを入力すると、ひらがなモードに切り替わること" <|
                    \_ ->
                        let
                            skk =
                                Skk.AsciiMode { input = "abc" }

                            key =
                                { key = "j", shift = False, ctrl = True }
                        in
                        Expect.equal (Skk.HiraganaMode { input = "abc", henkanMode = Skk.KakuteiInputMode "" }) (Skk.update skk key)
                ]
            , describe "hiragana mode"
                [ test "BSキーを入力すると、確定済み文字列の末尾文字が削除されること" <|
                    \_ ->
                        let
                            skk =
                                Skk.HiraganaMode { input = "あいう", henkanMode = Skk.KakuteiInputMode "" }

                            key =
                                { key = "BackSpace", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { input = "あい", henkanMode = Skk.KakuteiInputMode "" }) (Skk.update skk key)
                , test "Spaceキーを入力すると、確定済み文字列の末尾にスペースが追加されること" <|
                    \_ ->
                        let
                            skk =
                                Skk.HiraganaMode { input = "あいう", henkanMode = Skk.KakuteiInputMode "" }

                            key =
                                { key = " ", shift = False, ctrl = False }
                        in
                        Expect.equal (Skk.HiraganaMode { input = "あいう ", henkanMode = Skk.KakuteiInputMode "" }) (Skk.update skk key)
                ]
            ]
        ]

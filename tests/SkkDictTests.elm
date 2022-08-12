module SkkDictTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import SkkDict
import Test exposing (..)


suite : Test
suite =
    describe "The SkkDict module"
        [ describe "SkkDict.fromDictString"
            [ test "文字列から辞書を作成できること" <|
                \_ ->
                    let
                        dictStr =
                            """
                            ねこ /猫/
                            だいすk /大好/
                            きょう /今日/京/強/
                            """
                    in
                    Expect.equal
                        (Dict.fromList
                            [ ( "ねこ", [ "猫" ] )
                            , ( "だいすk", [ "大好" ] )
                            , ( "きょう", [ "今日", "京", "強" ] )
                            ]
                        )
                        (SkkDict.fromDictString dictStr)
            , test "コメント行が無視されること" <|
                \_ ->
                    let
                        dictStr =
                            """
                            a /b/c/
                            #comment
                            """
                    in
                    Expect.equal (Dict.fromList [ ( "a", [ "b", "c" ] ) ]) (SkkDict.fromDictString dictStr)
            , test "アノテーションが無視されること" <|
                \_ ->
                    let
                        dictStr =
                            """
                            a /b;annotation/c/
                            """
                    in
                    Expect.equal (Dict.fromList [ ( "a", [ "b", "c" ] ) ]) (SkkDict.fromDictString dictStr)
            ]
        ]

module SkkDictTests exposing (..)

import Dict
import Expect
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
        , describe "SkkDict.getHenkanList"
            [ test "存在するキーを指定すると対応する変換一覧を取得できること" <|
                \_ ->
                    let
                        dictStr =
                            """
                            ねこ /猫/
                            だいすk /大好/
                            きょう /今日/京/強/
                            """

                        dictStr2 =
                            """
                            きょう /卿/狂/
                            """

                        dict =
                            SkkDict.fromDictString dictStr

                        dict2 =
                            SkkDict.fromDictString dictStr2
                    in
                    Expect.equal
                        (Just [ "今日", "京", "強", "卿", "狂" ])
                        (SkkDict.getCandidateList "きょう" [ dict, dict2 ])
            , test "存在しないキーを指定するとNothingを返すこと" <|
                \_ ->
                    let
                        dictStr =
                            """
                            ねこ /猫/
                            だいすk /大好/
                            きょう /今日/京/強/
                            """

                        dictStr2 =
                            """
                            きょう /卿/狂/
                            """

                        dict =
                            SkkDict.fromDictString dictStr

                        dict2 =
                            SkkDict.fromDictString dictStr2
                    in
                    Expect.equal
                        Nothing
                        (SkkDict.getCandidateList "存在しないキー" [ dict, dict2 ])
            ]
        ]

module Skk exposing (AsciiInputModeValue, HiraganaInputModeValue, Skk, SkkContext, SkkInputKey, SkkMode(..), init, update)

import Regex
import SkkDict
import SkkKanaRule


type alias Skk =
    { mode : SkkMode
    , context : SkkContext
    }


type alias SkkContext =
    { kanaRules : SkkKanaRule.SkkKanaRules
    , dict : SkkDict.SkkDict
    }


type SkkMode
    = AsciiInputMode AsciiInputModeValue
    | HiraganaInputMode HiraganaInputModeValue -- ローマ字からひらがなに変換する確定入力モード
    | HiraganaMidashiInputMode HiraganaMidashiInputModeValue -- 辞書変換の対象となるひらがなの見出し語を入力するモード
    | HiraganaDictHenkanMode -- ひらがなの見出し語について辞書変換を行うモード


type alias AsciiInputModeValue =
    { input : String
    }


type alias HiraganaInputModeValue =
    { kakutei : String
    , mikakutei : String
    }


type alias HiraganaMidashiInputModeValue =
    { prevMode : HiraganaInputModeValue
    , kakutei : String -- 確定した見出し語
    , mikakutei : String -- 未確定の見出し語
    , okuri : String -- 送り仮名
    }


type alias HiraganaDictHenkanModeValue =
    { prevMode : HiraganaMidashiInputModeValue
    , candidateList : SkkDict.SkkDictCandidateList
    , pos : Int
    }


type alias SkkInputKey =
    { key : String
    , shift : Bool
    , ctrl : Bool
    }



-- factory


init : SkkContext -> Skk
init context =
    { mode = AsciiInputMode { input = "" }
    , context = context
    }



-- update


update : Skk -> SkkInputKey -> Skk
update skk key =
    case skk.mode of
        AsciiInputMode value ->
            { skk | mode = updateAsciiInputMode value key }

        HiraganaInputMode value ->
            { skk | mode = updateHiraganaInputMode value key }

        _ ->
            skk


updateAsciiInputMode : AsciiInputModeValue -> SkkInputKey -> SkkMode
updateAsciiInputMode value inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToKanaModeKey inputKey then
        HiraganaInputMode { kakutei = value.input, mikakutei = "" }

    else if Regex.contains asciiKey inputKey.key then
        AsciiInputMode { input = value.input ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiInputMode { input = applyBackSpace value.input }

    else
        -- ignore
        AsciiInputMode { input = value.input }


updateHiraganaInputMode : HiraganaInputModeValue -> SkkInputKey -> SkkMode
updateHiraganaInputMode value inputKey =
    if isSwitchToHenkanModeKey inputKey then
        -- TODO
        -- (a) 確定入力モード → 見出し語入力モード
        -- (b) 見出し語入力モードで送り仮名の位置を指定
        HiraganaInputMode value

    else if isHenkanAcceptedKey inputKey then
        -- TODO
        -- (a) 確定入力モード: ひらがな変換
        -- (b) 見出し語入力モード: 送り仮名なし
        -- (c) 見出し語入力モード: 送り仮名あり
        HiraganaInputMode value

    else if isBackSpaceKey inputKey then
        let
            ( kakutei, mikakutei ) =
                applyBackSpaceForKana value.kakutei value.mikakutei
        in
        HiraganaInputMode { value | kakutei = kakutei, mikakutei = mikakutei }

    else if isSpaceKey inputKey then
        HiraganaInputMode { value | kakutei = value.kakutei ++ " ", mikakutei = "" }

    else
        -- ignore
        HiraganaInputMode value



-- helper


isSwitchToKanaModeKey : SkkInputKey -> Bool
isSwitchToKanaModeKey { key, ctrl } =
    key == "j" && ctrl


isSwitchToHenkanModeKey : SkkInputKey -> Bool
isSwitchToHenkanModeKey { key } =
    let
        pattern =
            Regex.fromString "^[A-Z]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains pattern key


isHenkanAcceptedKey { key } =
    let
        pattern =
            Regex.fromString "^[a-z0-9+=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains pattern key


isSpaceKey : SkkInputKey -> Bool
isSpaceKey { key } =
    key == " "


isBackSpaceKey : SkkInputKey -> Bool
isBackSpaceKey { key } =
    key == "BackSpace"


applyBackSpace : String -> String
applyBackSpace input =
    String.dropRight 1 input


applyBackSpaceForKana : String -> String -> ( String, String )
applyBackSpaceForKana kakutei mikakutei =
    if String.isEmpty mikakutei then
        ( String.dropRight 1 kakutei, mikakutei )

    else
        ( kakutei, String.dropRight 1 mikakutei )

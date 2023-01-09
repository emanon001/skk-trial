module Skk exposing (AsciiModeValue, HiraganaModeValue, Skk, SkkContext, SkkHenkanMode(..), SkkInputKey, SkkInputMode(..), init, update)

import Regex
import SkkDict
import SkkKanaRule


type alias Skk =
    { inputMode : SkkInputMode
    , context : SkkContext
    }


type alias SkkContext =
    { kanaRules : SkkKanaRule.SkkKanaRules
    , dict : SkkDict.SkkDict
    }


type SkkInputMode
    = AsciiMode AsciiModeValue
    | HiraganaMode HiraganaModeValue


type alias AsciiModeValue =
    { input : String
    }


type alias HiraganaModeValue =
    { input : String
    , henkanMode : SkkHenkanMode
    }


type alias SkkInputKey =
    { key : String
    , shift : Bool
    , ctrl : Bool
    }


type SkkHenkanMode
    = -- 確定入力モード。ローマ字から『ひらがな』または『カタカナ』に変換するモード
      KakuteiInputMode String
      -- 辞書変換の対象となる見出し語を入力するモード
    | MidashiInputMode
        { midashi : String
        , okuri : String
        }
      -- 見出し語について辞書変換を行うモード
    | DictHenkanMode
        { candidateList : SkkDict.SkkDictCandidateList
        , pos : Int
        }



-- factory


init : SkkContext -> Skk
init context =
    { inputMode = AsciiMode { input = "" }
    , context = context
    }



-- update


update : Skk -> SkkInputKey -> Skk
update skk key =
    case skk.inputMode of
        AsciiMode value ->
            { skk | inputMode = updateAsciiMode value key }

        HiraganaMode value ->
            { skk | inputMode = updateHiraganaMode value key }


updateAsciiMode : AsciiModeValue -> SkkInputKey -> SkkInputMode
updateAsciiMode value inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToKanaModeKey inputKey then
        HiraganaMode { input = value.input, henkanMode = KakuteiInputMode "" }

    else if Regex.contains asciiKey inputKey.key then
        AsciiMode { input = value.input ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { input = applyBackSpace value.input }

    else
        -- ignore
        AsciiMode { input = value.input }


updateHiraganaMode : HiraganaModeValue -> SkkInputKey -> SkkInputMode
updateHiraganaMode value inputKey =
    if isSwitchToHenkanModeKey inputKey then
        -- TODO
        -- (a) 確定入力モード → 見出し語入力モード
        -- (b) 見出し語入力モードで送り仮名の位置を指定
        HiraganaMode value

    else if isHenkanAcceptedKey inputKey then
        -- TODO
        -- (a) 確定入力モード: ひらがな変換
        -- (b) 見出し語入力モード: 送り仮名なし
        -- (c) 見出し語入力モード: 送り仮名あり
        case value.henkanMode of
            KakuteiInputMode roma ->
                HiraganaMode value

            _ ->
                HiraganaMode value

    else if isBackSpaceKey inputKey then
        HiraganaMode { value | input = applyBackSpace value.input }

    else if isSpaceKey inputKey then
        HiraganaMode { value | input = value.input ++ " " }

    else
        -- ignore
        HiraganaMode value



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
applyBackSpace key =
    String.dropRight 1 key

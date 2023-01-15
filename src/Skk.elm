module Skk exposing (AsciiInputModeValue, HiraganaInputModeValue, Skk, SkkContext, SkkHenkanMode(..), SkkInputKey, SkkInputMode(..), init, update)

import Regex
import SkkDict
import SkkKanaRule


type alias Skk =
    { mode : SkkInputMode
    , context : SkkContext
    }


type alias SkkContext =
    { kanaRules : SkkKanaRule.SkkKanaRules
    , dict : SkkDict.SkkDict
    }


type SkkInputMode
    = AsciiInputMode AsciiInputModeValue -- Ascii文字を入力するモード
    | HiraganaInputMode HiraganaInputModeValue -- ひらがなを入力するモード


type alias AsciiInputModeValue =
    { kakutei : String
    }


type alias HiraganaInputModeValue =
    { kakutei : String
    , henkanMode : SkkHenkanMode
    }


type SkkHenkanMode
    = KakuteiInputMode KakuteiInputModeValue -- ■モード(確定入力モード)。ルールに従って、ローマ字から『ひらがな』『カタカタ』に変換するモード
    | MidashiInputMode MidashiInputModeValue -- ▽モード。辞書変換の対象となる『ひらがな』『カタカナ』の見出し語を入力するモード
    | DictHenkanMode DictHenkanModeValue -- ▼モード。見出し語について辞書変換を行うモード


type alias KakuteiInputModeValue =
    { mikakutei : String
    }


type alias MidashiInputModeValue =
    { kakutei : String -- 確定した見出し語
    , mikakutei : String -- 未確定の見出し語
    , okuri : String -- 送り仮名
    }


type alias DictHenkanModeValue =
    { prevMode : MidashiInputModeValue
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
    { mode = AsciiInputMode { kakutei = "" }
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


updateAsciiInputMode : AsciiInputModeValue -> SkkInputKey -> SkkInputMode
updateAsciiInputMode value inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToKanaModeKey inputKey then
        HiraganaInputMode { kakutei = value.kakutei, henkanMode = KakuteiInputMode { mikakutei = "" } }

    else if Regex.contains asciiKey inputKey.key then
        AsciiInputMode { kakutei = value.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiInputMode { kakutei = applyBackSpace value.kakutei }

    else
        -- ignore
        AsciiInputMode { kakutei = value.kakutei }


updateHiraganaInputMode : HiraganaInputModeValue -> SkkInputKey -> SkkInputMode
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
            ( kakutei, henkanMode ) =
                applyBackSpaceForKana value.kakutei value.henkanMode
        in
        HiraganaInputMode { value | kakutei = kakutei, henkanMode = henkanMode }

    else if isSpaceKey inputKey then
        let
            ( kakutei, henkanMode ) =
                applySpaceForKana value.kakutei value.henkanMode
        in
        HiraganaInputMode { value | kakutei = kakutei, henkanMode = henkanMode }

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


isHenkanAcceptedKey : SkkInputKey -> Bool
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


applyBackSpaceForKana : String -> SkkHenkanMode -> ( String, SkkHenkanMode )
applyBackSpaceForKana kakutei henkanMode =
    case henkanMode of
        KakuteiInputMode value ->
            if String.isEmpty value.mikakutei then
                ( String.dropRight 1 kakutei, henkanMode )

            else
                ( kakutei, KakuteiInputMode { mikakutei = String.dropRight 1 value.mikakutei } )

        _ ->
            ( kakutei, henkanMode )


applySpaceForKana : String -> SkkHenkanMode -> ( String, SkkHenkanMode )
applySpaceForKana kakutei henkanMode =
    case henkanMode of
        KakuteiInputMode value ->
            ( kakutei ++ " ", KakuteiInputMode { mikakutei = String.dropRight 1 value.mikakutei } )

        _ ->
            ( kakutei, henkanMode )

module Skk exposing (AsciiInputModeValue, HiraganaInputModeValue, Skk, SkkContext, SkkConvertMode(..), SkkInputKey, SkkInputMode(..), init, update)

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
    , convertMode : SkkConvertMode
    }


type SkkConvertMode
    = KakuteiInputMode KakuteiInputModeValue -- ■モード(確定入力モード)。ルールに従って、ローマ字から『ひらがな』『カタカタ』に変換するモード
    | MidashiInputMode MidashiInputModeValue -- ▽モード。辞書変換の対象となる『ひらがな』『カタカナ』の見出し語を入力するモード
    | DictConvertMode DictconvertModeValue -- ▼モード。見出し語について辞書変換を行うモード


type alias KakuteiInputModeValue =
    { mikakutei : String
    }


type alias MidashiInputModeValue =
    { kakutei : String -- 確定した見出し語
    , mikakutei : String -- 未確定の見出し語
    , okuri : String -- 送り仮名
    }


type alias DictconvertModeValue =
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
            { skk | mode = updateAsciiInputMode value skk.context key }

        HiraganaInputMode value ->
            { skk | mode = updateHiraganaInputMode value skk.context key }


updateAsciiInputMode : AsciiInputModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateAsciiInputMode value _ inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToKanaModeKey inputKey then
        HiraganaInputMode { kakutei = value.kakutei, convertMode = KakuteiInputMode { mikakutei = "" } }

    else if Regex.contains asciiKey inputKey.key then
        AsciiInputMode { kakutei = value.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiInputMode { kakutei = String.dropRight 1 value.kakutei }

    else
        -- ignore
        AsciiInputMode { kakutei = value.kakutei }


updateHiraganaInputMode : HiraganaInputModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateHiraganaInputMode value context inputKey =
    if isSwitchToconvertModeKey inputKey then
        -- TODO
        -- (a) 確定入力モード → 見出し語入力モード
        -- (b) 見出し語入力モードで送り仮名の位置を指定
        HiraganaInputMode value

    else if isConvertAcceptedKey inputKey then
        convertForHiragana value context inputKey.key

    else if isBackSpaceKey inputKey then
        let
            ( kakutei, convertMode ) =
                applyBackSpaceForKana value.kakutei value.convertMode
        in
        HiraganaInputMode { value | kakutei = kakutei, convertMode = convertMode }

    else if isSpaceKey inputKey then
        let
            ( kakutei, convertMode ) =
                applySpaceForKana value.kakutei value.convertMode
        in
        HiraganaInputMode { value | kakutei = kakutei, convertMode = convertMode }

    else
        -- ignore
        HiraganaInputMode value



-- key check functions


isSwitchToKanaModeKey : SkkInputKey -> Bool
isSwitchToKanaModeKey { key, ctrl } =
    key == "j" && ctrl


isSwitchToconvertModeKey : SkkInputKey -> Bool
isSwitchToconvertModeKey { key } =
    let
        pattern =
            Regex.fromString "^[A-Z]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains pattern key


isConvertAcceptedKey : SkkInputKey -> Bool
isConvertAcceptedKey { key } =
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



-- apply key functions


applyBackSpaceForKana : String -> SkkConvertMode -> ( String, SkkConvertMode )
applyBackSpaceForKana kakutei convertMode =
    case convertMode of
        KakuteiInputMode value ->
            if String.isEmpty value.mikakutei then
                ( String.dropRight 1 kakutei, convertMode )

            else
                ( kakutei, KakuteiInputMode { mikakutei = String.dropRight 1 value.mikakutei } )

        _ ->
            ( kakutei, convertMode )


applySpaceForKana : String -> SkkConvertMode -> ( String, SkkConvertMode )
applySpaceForKana kakutei convertMode =
    case convertMode of
        KakuteiInputMode value ->
            ( kakutei ++ " ", KakuteiInputMode { mikakutei = String.dropRight 1 value.mikakutei } )

        _ ->
            ( kakutei, convertMode )


convertForHiragana : HiraganaInputModeValue -> SkkContext -> String -> SkkInputMode
convertForHiragana value context key =
    let
        { kakutei, convertMode } =
            value
    in
    case convertMode of
        -- (a) 確定入力モード: ひらがな変換
        KakuteiInputMode { mikakutei } ->
            let
                searchKey =
                    mikakutei ++ key
            in
            case SkkKanaRule.search searchKey context.kanaRules of
                SkkKanaRule.PartialMatch ->
                    HiraganaInputMode { value | convertMode = KakuteiInputMode { mikakutei = searchKey } }

                SkkKanaRule.PerfectMatch { hiragana, next } ->
                    HiraganaInputMode { kakutei = kakutei ++ hiragana, convertMode = KakuteiInputMode { mikakutei = Maybe.withDefault "" next } }

                SkkKanaRule.NoMatch ->
                    HiraganaInputMode { value | convertMode = KakuteiInputMode { mikakutei = key } }

        _ ->
            -- TODO
            -- (b) 見出し語入力モード: 送り仮名なし
            -- (c) 見出し語入力モード: 送り仮名あり
            HiraganaInputMode value

module Skk exposing (AsciiModeValue, HiraganaModeValue, Skk, SkkContext, SkkConvertMode(..), SkkInputKey, SkkInputMode(..), init, update)

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
    = AsciiMode AsciiModeValue -- Ascii文字を入力するモード
    | HiraganaMode HiraganaModeValue -- ひらがなを入力するモード


type alias AsciiModeValue =
    { kakutei : String
    }


type alias HiraganaModeValue =
    { kakutei : String
    , convertMode : SkkConvertMode
    }


type SkkConvertMode
    = KakuteiInputMode KakuteiInputModeValue -- ■モード(確定入力モード)。ルールに従って、ローマ字から『ひらがな』『カタカタ』に変換するモード
    | MidashiInputMode MidashiInputModeValue -- ▽モード(見出し語入力モード)。辞書変換の対象となる『ひらがな』『カタカナ』の見出し語を入力するモード
    | DictConvertMode DictconvertModeValue -- ▼モード(辞書変換モード)。見出し語について辞書変換を行うモード


type alias KakuteiInputModeValue =
    { mikakutei : String
    }


type alias MidashiInputModeValue =
    { midashi :
        { kakutei : String -- 確定した見出し語
        , mikakutei : String -- 未確定の見出し語
        }
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
    { mode = AsciiMode { kakutei = "" }
    , context = context
    }



-- update


update : Skk -> SkkInputKey -> Skk
update skk key =
    case skk.mode of
        AsciiMode value ->
            { skk | mode = updateAsciiMode value skk.context key }

        HiraganaMode value ->
            { skk | mode = updateHiraganaMode value skk.context key }


updateAsciiMode : AsciiModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateAsciiMode value _ inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToKanaModeKey inputKey then
        HiraganaMode { kakutei = value.kakutei, convertMode = KakuteiInputMode { mikakutei = "" } }

    else if Regex.contains asciiKey inputKey.key then
        AsciiMode { kakutei = value.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { kakutei = String.dropRight 1 value.kakutei }

    else
        -- ignore
        AsciiMode { kakutei = value.kakutei }


updateHiraganaMode : HiraganaModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateHiraganaMode value context inputKey =
    case value.convertMode of
        KakuteiInputMode convertValue ->
            updateKanaKakuteiInputMode True value.kakutei convertValue context inputKey

        _ ->
            HiraganaMode value


updateKanaKakuteiInputMode : Bool -> String -> KakuteiInputModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateKanaKakuteiInputMode isHiragana kakutei convertValue context inputKey =
    let
        mkKanaMode : String -> SkkConvertMode -> SkkInputMode
        mkKanaMode s convertMode =
            if isHiragana then
                HiraganaMode { kakutei = s, convertMode = convertMode }

            else
                -- TODO: カタカナモード
                HiraganaMode { kakutei = s, convertMode = convertMode }

        mkKakuteiMode : String -> SkkConvertMode
        mkKakuteiMode s =
            KakuteiInputMode { mikakutei = s }

        default : SkkInputMode
        default =
            mkKanaMode kakutei (mkKakuteiMode convertValue.mikakutei)
    in
    if isSwitchToconvertModeKey inputKey then
        -- TODO
        -- (a) 確定入力モード → 見出し語入力モード
        default

    else if isConvertAcceptedKey inputKey then
        let
            searchKey =
                convertValue.mikakutei ++ inputKey.key
        in
        case SkkKanaRule.search searchKey context.kanaRules of
            SkkKanaRule.PartialMatch ->
                mkKanaMode kakutei (mkKakuteiMode searchKey)

            SkkKanaRule.PerfectMatch { hiragana, next } ->
                mkKanaMode (kakutei ++ hiragana) (mkKakuteiMode (Maybe.withDefault "" next))

            SkkKanaRule.NoMatch ->
                mkKanaMode kakutei (mkKakuteiMode inputKey.key)

    else if isBackSpaceKey inputKey then
        if String.isEmpty convertValue.mikakutei then
            mkKanaMode (String.dropRight 1 kakutei) (mkKakuteiMode convertValue.mikakutei)

        else
            mkKanaMode kakutei (mkKakuteiMode (String.dropRight 1 convertValue.mikakutei))

    else if isSpaceKey inputKey then
        mkKanaMode (kakutei ++ " ") (mkKakuteiMode "")

    else
        -- ignore
        default



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

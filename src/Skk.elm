module Skk exposing (AsciiModeValue, HiraganaModeValue, Skk, SkkContext, SkkConvertMode(..), SkkInputKey, SkkInputMode(..), init, update)

import Html.Attributes exposing (default)
import Regex
import SkkDict
import SkkKanaRule
import String exposing (dropRight)


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
    | KatakanaMode KatakanaModeValue -- カタカナを入力するモード


type alias AsciiModeValue =
    { kakutei : String
    }


type alias HiraganaModeValue =
    { kakutei : String
    , convertMode : SkkConvertMode
    }


type alias KatakanaModeValue =
    { kakutei : String
    , convertMode : SkkConvertMode
    }


type SkkConvertMode
    = KakuteiInputMode KakuteiInputModeValue -- ■モード(確定入力モード)。ルールに従って、ローマ字から『ひらがな』『カタカタ』に変換するモード
    | MidashiInputMode MidashiInputModeValue -- ▽モード(見出し語入力モード)。辞書変換の対象となる『ひらがな』『カタカナ』の見出し語を入力するモード
    | DictConvertMode DictConvertModeValue -- ▼モード(辞書変換モード)。見出し語について辞書変換を行うモード


type alias KakuteiInputModeValue =
    { mikakutei : String
    }


type alias MidashiInputModeValue =
    { midashi :
        { kakutei : String -- 確定した見出し語
        , mikakutei : String -- 未確定の見出し語
        }
    , okuri :
        { kakutei : String -- 確定した送り仮名
        , mikakutei : String -- 未確定の送り仮名
        }
    }


type alias DictConvertModeValue =
    { prevMode : MidashiInputModeValue --
    , candidateList : SkkDict.SkkDictCandidateList
    , pos : Int
    }


{-| KeyboardEventのwrapper
<https://developer.mozilla.org/ja/docs/Web/API/KeyboardEvent>
-}
type alias SkkInputKey =
    { key : String -- 入力したキーを表す文字列
    , shift : Bool -- Shiftキーを入力しているか
    , ctrl : Bool -- Ctrlキーを入力しているか
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

        KatakanaMode value ->
            { skk | mode = updateKatakanaMode value skk.context key }



-- update 入力モード


updateAsciiMode : AsciiModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateAsciiMode value _ inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToHiraganaModeKey inputKey then
        HiraganaMode { kakutei = value.kakutei, convertMode = initKakuteiInputMode }

    else if Regex.contains asciiKey inputKey.key then
        AsciiMode { kakutei = value.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { kakutei = String.dropRight 1 value.kakutei }

    else
        -- ignore
        AsciiMode value


updateHiraganaMode : HiraganaModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateHiraganaMode value context inputKey =
    case value.convertMode of
        KakuteiInputMode convertValue ->
            updateKanaKakuteiInputMode True value.kakutei convertValue context inputKey

        MidashiInputMode convertValue ->
            updateMidashiInputMode True value.kakutei convertValue context inputKey

        _ ->
            HiraganaMode value


updateKatakanaMode : KatakanaModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateKatakanaMode value context inputKey =
    case value.convertMode of
        KakuteiInputMode convertValue ->
            updateKanaKakuteiInputMode False value.kakutei convertValue context inputKey

        MidashiInputMode convertValue ->
            updateMidashiInputMode True value.kakutei convertValue context inputKey

        _ ->
            KatakanaMode value



-- update 変換モード


updateKanaKakuteiInputMode : Bool -> String -> KakuteiInputModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateKanaKakuteiInputMode isHiragana kakutei convertValue context inputKey =
    let
        -- ひらがな・カタカナモードのファクトリ
        buildKanaMode : String -> SkkConvertMode -> SkkInputMode
        buildKanaMode s convertMode =
            if isHiragana then
                HiraganaMode { kakutei = s, convertMode = convertMode }

            else
                KatakanaMode { kakutei = s, convertMode = convertMode }

        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode kakutei (buildKakuteiInputMode convertValue.mikakutei)
    in
    if isSwitchToMidashiInputModeKey inputKey then
        -- 確定入力モード → 見出し語入力モード
        -- あいう + S → あいう▽s
        let
            key =
                String.toLower inputKey.key

            ( midashiKakutei, midashiMikakutei ) =
                convertToKana isHiragana convertValue.mikakutei key context.kanaRules
        in
        buildKanaMode kakutei
            (MidashiInputMode
                { midashi = { kakutei = midashiKakutei, mikakutei = midashiMikakutei }
                , okuri = { kakutei = "", mikakutei = "" }
                }
            )

    else if isSwitchToKanaModeKey inputKey then
        -- ひらがなモードとカタカナモードの切り替え
        if isHiragana then
            -- ひらがな → カタカナ
            KatakanaMode { kakutei = kakutei, convertMode = buildKakuteiInputMode "" }

        else
            -- カタカナ → ひらがな
            HiraganaMode { kakutei = kakutei, convertMode = buildKakuteiInputMode "" }

    else if isConvertAcceptedKey inputKey then
        -- かな変換を試みる
        let
            ( kakutei2, mikakutei ) =
                convertToKana isHiragana convertValue.mikakutei inputKey.key context.kanaRules
        in
        buildKanaMode (kakutei ++ kakutei2) (buildKakuteiInputMode mikakutei)

    else if isBackSpaceKey inputKey then
        -- 削除
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar kakutei convertValue.mikakutei
        in
        buildKanaMode newKakutei (buildKakuteiInputMode newMikakutei)

    else
        -- ignore
        default


updateMidashiInputMode : Bool -> String -> MidashiInputModeValue -> SkkContext -> SkkInputKey -> SkkInputMode
updateMidashiInputMode isHiragana kakutei convertValue context inputKey =
    let
        -- ひらがな・カタカナモードのファクトリ
        buildKanaMode : String -> SkkConvertMode -> SkkInputMode
        buildKanaMode s convertMode =
            if isHiragana then
                HiraganaMode { kakutei = s, convertMode = convertMode }

            else
                KatakanaMode { kakutei = s, convertMode = convertMode }

        isConvertingMidashi : Bool
        isConvertingMidashi =
            let
                okuri =
                    convertValue.okuri
            in
            String.isEmpty okuri.kakutei && String.isEmpty okuri.mikakutei

        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode kakutei (MidashiInputMode convertValue)
    in
    if isSwitchToOkuriInputModeKey inputKey then
        -- TODO: 送り仮名の入力に切り替え
        default

    else if isCancelKey inputKey then
        -- あいう▽ねこ + Ctrl-g → あいう
        -- あいう▽はし*r + Ctrl-g → あいう▽はし
        if isConvertingMidashi then
            buildKanaMode kakutei initKakuteiInputMode

        else
            buildKanaMode kakutei (MidashiInputMode { convertValue | okuri = { kakutei = "", mikakutei = "" } })

    else if isConvertAcceptedKey inputKey then
        -- かな変換を試みる
        -- ▽sy + a → ▽しゃ
        let
            midashi =
                convertValue.midashi
        in
        if isConvertingMidashi then
            -- 見出しの変換
            let
                ( midashiKakutei2, midashiMikakutei ) =
                    convertToKana isHiragana midashi.mikakutei inputKey.key context.kanaRules
            in
            buildKanaMode kakutei
                (MidashiInputMode
                    { convertValue
                        | midashi = { kakutei = midashi.kakutei ++ midashiKakutei2, mikakutei = midashiMikakutei }
                    }
                )

        else
            -- 送りの変換
            -- TODO: かな変換
            -- TODO: 辞書変換モードに遷移
            default

    else if isEnterKey inputKey then
        -- 確定
        -- あいう▽ねこ + Enter → あいうねこ
        if isConvertingMidashi then
            buildKanaMode (kakutei ++ convertValue.midashi.kakutei) initKakuteiInputMode

        else
            default

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽ねこ + BS → ▽ね
        -- ▽はし*r + BS → ▽はし
        -- ▽はし*っt + BS → ▽はし*っ
        if isConvertingMidashi then
            let
                ( newKakutei, newMikakutei ) =
                    deleteInputChar convertValue.midashi.kakutei convertValue.midashi.mikakutei
            in
            buildKanaMode kakutei (MidashiInputMode { convertValue | midashi = { kakutei = newKakutei, mikakutei = newMikakutei } })

        else
            let
                ( newKakutei, newMikakutei ) =
                    deleteInputChar convertValue.okuri.kakutei convertValue.okuri.mikakutei
            in
            buildKanaMode kakutei (MidashiInputMode { convertValue | okuri = { kakutei = newKakutei, mikakutei = newMikakutei } })

    else
        -- ignore
        default



-- convert kana


convertToKana : Bool -> String -> String -> SkkKanaRule.SkkKanaRules -> ( String, String )
convertToKana isHiragana currentKey addtionalKey rules =
    let
        searchKey =
            currentKey ++ addtionalKey
    in
    case SkkKanaRule.search searchKey rules of
        SkkKanaRule.PartialMatch ->
            ( "", searchKey )

        SkkKanaRule.PerfectMatch { hiragana, katakana, next } ->
            if isHiragana then
                ( hiragana, Maybe.withDefault "" next )

            else
                ( katakana, Maybe.withDefault "" next )

        SkkKanaRule.NoMatch ->
            if String.isEmpty currentKey then
                ( addtionalKey, "" )

            else
                -- 未確定文字列を初期化して再度変換を行う
                convertToKana isHiragana "" addtionalKey rules



-- delete char


deleteInputChar : String -> String -> ( String, String )
deleteInputChar kakutei mikakutei =
    if String.isEmpty mikakutei then
        ( dropRight 1 kakutei, mikakutei )

    else
        ( kakutei, dropRight 1 mikakutei )



-- factory(internal)


initKakuteiInputMode : SkkConvertMode
initKakuteiInputMode =
    buildKakuteiInputMode ""


buildKakuteiInputMode : String -> SkkConvertMode
buildKakuteiInputMode mikakutei =
    KakuteiInputMode { mikakutei = mikakutei }



-- key check functions
-- ref. https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values


isSwitchToHiraganaModeKey : SkkInputKey -> Bool
isSwitchToHiraganaModeKey { key, ctrl } =
    key == "j" && ctrl


isSwitchToMidashiInputModeKey : SkkInputKey -> Bool
isSwitchToMidashiInputModeKey { key } =
    let
        pattern =
            Regex.fromString "^[A-Z]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains pattern key


isSwitchToOkuriInputModeKey : SkkInputKey -> Bool
isSwitchToOkuriInputModeKey =
    isSwitchToMidashiInputModeKey


isSwitchToKanaModeKey : SkkInputKey -> Bool
isSwitchToKanaModeKey { key } =
    key == "q"


isConvertAcceptedKey : SkkInputKey -> Bool
isConvertAcceptedKey { key } =
    let
        pattern =
            Regex.fromString "^[a-z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    Regex.contains pattern key


isBackSpaceKey : SkkInputKey -> Bool
isBackSpaceKey { key } =
    key == "BackSpace"


isEnterKey : SkkInputKey -> Bool
isEnterKey { key } =
    key == "Enter"


isCancelKey : SkkInputKey -> Bool
isCancelKey { key, ctrl } =
    key == "g" && ctrl

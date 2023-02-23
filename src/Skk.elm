module Skk exposing (Skk, SkkContext, SkkConvertMode(..), SkkInputKey, SkkInputMode(..), SkkPreDictConvertMode(..), init, update)

import Array
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
    | MidashiOkuriInputMode MidashiOkuriInputModeValue -- ▽モード(見出し語入力モード/送り仮名)。辞書変換の対象となる『ひらがな』『カタカナ』の送り仮名を入力するモード
    | DictConvertMode DictConvertModeValue -- ▼モード(辞書変換モード)。見出し語について辞書変換を行うモード


type alias KakuteiInputModeValue =
    { mikakutei : String
    }


type alias MidashiInputModeValue =
    { kakutei : String -- 確定した見出し語
    , mikakutei : String -- 未確定の見出し語
    }


type alias MidashiOkuriInputModeValue =
    { midashi : MidashiInputModeValue -- 見出し語。"▽たたか*っt" の場合は "たたか"
    , head : String -- 送り仮名の最初のローマ字。"▽たたか*っt" の場合は "っ" を構成する "t"
    , kakutei : String -- 確定した送り仮名。"▽たたか*っt" の場合は "っ"
    , mikakutei : String -- 未確定の送り仮名。"▽たたか*っt" の場合は "t"
    }


type SkkPreDictConvertMode
    = PreDictConvertMidashiInputMode MidashiInputModeValue
    | PreDictConvertMidashiOkuriInputMode MidashiOkuriInputModeValue


type alias DictConvertModeValue =
    { prevMode : SkkPreDictConvertMode -- 辞書変換直前のモード
    , candidateList : SkkDict.SkkDictCandidateList -- 変換候補の一覧
    , pos : Int -- 変換候補の位置
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
            { skk | mode = updateAsciiMode { inputModeValue = value, context = skk.context, inputKey = key } }

        HiraganaMode value ->
            { skk | mode = updateHiraganaMode { inputModeValue = value, context = skk.context, inputKey = key } }

        KatakanaMode value ->
            { skk | mode = updateKatakanaMode { inputModeValue = value, context = skk.context, inputKey = key } }



-- update 入力モード


updateAsciiMode : { inputModeValue : AsciiModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateAsciiMode { inputModeValue, inputKey } =
    let
        asciiRegex =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToHiraganaModeKey inputKey then
        HiraganaMode { kakutei = inputModeValue.kakutei, convertMode = initKakuteiInputMode }

    else if Regex.contains asciiRegex inputKey.key then
        AsciiMode { kakutei = inputModeValue.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { kakutei = String.dropRight 1 inputModeValue.kakutei }

    else
        -- ignore
        AsciiMode inputModeValue


updateHiraganaMode : { inputModeValue : HiraganaModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateHiraganaMode { inputModeValue, context, inputKey } =
    let
        isHiragana =
            True
    in
    case inputModeValue.convertMode of
        KakuteiInputMode convertValue ->
            updateKanaKakuteiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        MidashiInputMode convertValue ->
            updateMidashiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        MidashiOkuriInputMode convertValue ->
            updateMidashiOkuriInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        DictConvertMode convertValue ->
            updateDictConvertMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }


updateKatakanaMode : { inputModeValue : KatakanaModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateKatakanaMode { inputModeValue, context, inputKey } =
    let
        isHiragana =
            False
    in
    case inputModeValue.convertMode of
        KakuteiInputMode convertValue ->
            updateKanaKakuteiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        MidashiInputMode convertValue ->
            updateMidashiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        MidashiOkuriInputMode convertValue ->
            updateMidashiOkuriInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        DictConvertMode convertValue ->
            updateDictConvertMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , convertModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }



-- update 変換モード


updateKanaKakuteiInputMode : { isHiragana : Bool, kakutei : String, convertModeValue : KakuteiInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateKanaKakuteiInputMode { isHiragana, kakutei, convertModeValue, context, inputKey } =
    let
        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (buildKakuteiInputMode convertModeValue.mikakutei)
    in
    if isSwitchToMidashiInputModeKey inputKey then
        -- 確定入力モード → 見出し語入力モード
        -- あいう + S → あいう▽s
        let
            key =
                String.toLower inputKey.key

            ( midashiKakutei, midashiMikakutei ) =
                convertToKana isHiragana convertModeValue.mikakutei key context.kanaRules
        in
        buildKanaMode isHiragana
            kakutei
            (MidashiInputMode
                { kakutei = midashiKakutei, mikakutei = midashiMikakutei }
            )

    else if isSwitchToAsciiModeKey inputKey then
        -- TODO: Asciiモードに切り替え
        default

    else if isSwitchToKanaModeKey inputKey then
        -- ひらがなモードとカタカナモードの切り替え
        if isHiragana then
            -- ひらがな → カタカナ
            KatakanaMode { kakutei = kakutei, convertMode = buildKakuteiInputMode "" }

        else
            -- カタカナ → ひらがな
            HiraganaMode { kakutei = kakutei, convertMode = buildKakuteiInputMode "" }

    else if isKanaConvertAcceptedKey inputKey then
        -- かな変換を試みる
        let
            ( kakutei2, mikakutei ) =
                convertToKana isHiragana convertModeValue.mikakutei inputKey.key context.kanaRules
        in
        buildKanaMode isHiragana (kakutei ++ kakutei2) (buildKakuteiInputMode mikakutei)

    else if isBackSpaceKey inputKey then
        -- 削除
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar kakutei convertModeValue.mikakutei
        in
        buildKanaMode isHiragana newKakutei (buildKakuteiInputMode newMikakutei)

    else
        -- ignore
        default


updateMidashiInputMode : { isHiragana : Bool, kakutei : String, convertModeValue : MidashiInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateMidashiInputMode { isHiragana, kakutei, convertModeValue, context, inputKey } =
    let
        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (MidashiInputMode convertModeValue)
    in
    if isSwitchToOkuriInputModeKey inputKey then
        -- TODO: 送り仮名の入力に切り替え
        default

    else if isCancelKey inputKey then
        -- キャンセル
        -- あいう▽ねこ + Ctrl-g → あいう
        buildKanaMode isHiragana kakutei initKakuteiInputMode

    else if isKanaConvertAcceptedKey inputKey then
        -- かな変換を試みる
        -- ▽sy + a → ▽しゃ
        let
            ( kakutei2, mikakutei ) =
                convertToKana isHiragana convertModeValue.mikakutei inputKey.key context.kanaRules
        in
        buildKanaMode isHiragana
            kakutei
            (MidashiInputMode { kakutei = convertModeValue.kakutei ++ kakutei2, mikakutei = mikakutei })

    else if isConvertKey inputKey then
        -- 変換開始
        -- TODO: 変換候補がない場合に、辞書登録モードに移行
        -- ▽ねこ + Space → ▼猫
        let
            prevMode =
                PreDictConvertMidashiInputMode { convertModeValue | mikakutei = "" }

            canditateList =
                SkkDict.getCandidateList convertModeValue.kakutei context.dict
        in
        case canditateList of
            Just candidateList ->
                buildKanaMode isHiragana
                    kakutei
                    (DictConvertMode { prevMode = prevMode, candidateList = candidateList, pos = 0 })

            Nothing ->
                default

    else if isEnterKey inputKey then
        -- 確定
        -- あいう▽ねこ + Enter → あいうねこ
        buildKanaMode isHiragana (kakutei ++ convertModeValue.kakutei) initKakuteiInputMode

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽ねこ + BS → ▽ね
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar convertModeValue.kakutei convertModeValue.mikakutei
        in
        buildKanaMode isHiragana kakutei (MidashiInputMode { kakutei = newKakutei, mikakutei = newMikakutei })

    else
        -- ignore
        default


updateMidashiOkuriInputMode : { isHiragana : Bool, kakutei : String, convertModeValue : MidashiOkuriInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateMidashiOkuriInputMode { isHiragana, kakutei, convertModeValue, context, inputKey } =
    let
        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (MidashiOkuriInputMode convertModeValue)
    in
    if isCancelKey inputKey then
        -- キャンセル
        -- あいう▽はし*r + Ctrl-g → あいう▽はし
        buildKanaMode isHiragana kakutei (MidashiInputMode convertModeValue.midashi)

    else if isKanaConvertAcceptedKey inputKey then
        -- TODO: かな変換を試みる
        -- TODO: 辞書変換モードに遷移
        default

    else if isConvertKey inputKey then
        -- TODO: 変換開始
        default

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽はし*r + BS → ▽はし
        -- ▽はし*っt + BS → ▽はし*っ
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar convertModeValue.kakutei convertModeValue.mikakutei

            newHead =
                if newKakutei == "" && newMikakutei == "" then
                    ""

                else
                    convertModeValue.head
        in
        buildKanaMode isHiragana kakutei (MidashiOkuriInputMode { convertModeValue | head = newHead, kakutei = newKakutei, mikakutei = newMikakutei })

    else
        -- ignore
        default


updateDictConvertMode : { isHiragana : Bool, kakutei : String, convertModeValue : DictConvertModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateDictConvertMode { isHiragana, kakutei, convertModeValue, context, inputKey } =
    let
        -- 直前の変換モード
        previousConvertMode : SkkInputMode
        previousConvertMode =
            let
                { prevMode } =
                    convertModeValue
            in
            buildKanaMode isHiragana
                kakutei
                (case prevMode of
                    PreDictConvertMidashiInputMode v ->
                        MidashiInputMode v

                    PreDictConvertMidashiOkuriInputMode v ->
                        MidashiOkuriInputMode v
                )

        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (DictConvertMode convertModeValue)
    in
    if isCancelKey inputKey then
        -- キャンセル
        previousConvertMode

    else if isNextCandidateKey inputKey then
        -- 次候補
        let
            { candidateList, pos } =
                convertModeValue
        in
        if pos + 1 == List.length candidateList then
            -- TODO: 辞書登録モード
            default

        else
            buildKanaMode isHiragana kakutei (DictConvertMode { convertModeValue | pos = pos + 1 })

    else if isPreviousCandidateKey inputKey then
        -- 前候補
        let
            { pos } =
                convertModeValue
        in
        if pos == 0 then
            -- 直前の変換モードに戻る
            previousConvertMode

        else
            buildKanaMode isHiragana kakutei (DictConvertMode { convertModeValue | pos = pos - 1 })

    else if isEnterKey inputKey then
        -- TODO: 確定
        -- あいう▼猫 → あいう猫
        let
            { pos, candidateList, prevMode } =
                convertModeValue

            converted =
                Array.fromList candidateList |> Array.get pos |> Maybe.withDefault ""
        in
        case prevMode of
            PreDictConvertMidashiInputMode _ ->
                buildKanaMode isHiragana (kakutei ++ converted) (KakuteiInputMode { mikakutei = "" })

            PreDictConvertMidashiOkuriInputMode _ ->
                -- TODO: 送り仮名に対応
                default

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


buildKanaMode : Bool -> String -> SkkConvertMode -> SkkInputMode
buildKanaMode isHiragana kakutei convertMode =
    if isHiragana then
        HiraganaMode { kakutei = kakutei, convertMode = convertMode }

    else
        KatakanaMode { kakutei = kakutei, convertMode = convertMode }



-- key check functions
-- ref. https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values


isSwitchToHiraganaModeKey : SkkInputKey -> Bool
isSwitchToHiraganaModeKey { key, ctrl } =
    key == "j" && ctrl


isSwitchToAsciiModeKey : SkkInputKey -> Bool
isSwitchToAsciiModeKey { key } =
    key == "l"


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


isKanaConvertAcceptedKey : SkkInputKey -> Bool
isKanaConvertAcceptedKey { key } =
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


isConvertKey : SkkInputKey -> Bool
isConvertKey { key } =
    key == "Space"


isNextCandidateKey : SkkInputKey -> Bool
isNextCandidateKey { key } =
    key == "Space"


isPreviousCandidateKey : SkkInputKey -> Bool
isPreviousCandidateKey { key } =
    key == "x"

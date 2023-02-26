module Skk exposing (Skk, SkkContext, SkkConversionMode(..), SkkInputKey, SkkInputMode(..), SkkPrevDictConversionMode(..), init, update)

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
    , conversionMode : SkkConversionMode
    }


type alias KatakanaModeValue =
    { kakutei : String
    , conversionMode : SkkConversionMode
    }


type SkkConversionMode
    = KakuteiInputMode KakuteiInputModeValue -- ■モード(確定入力モード)。ルールに従って、ローマ字から『ひらがな』『カタカタ』に変換するモード
    | MidashiInputMode MidashiInputModeValue -- ▽モード(見出し語入力モード)。辞書変換の対象となる『ひらがな』『カタカナ』の見出し語を入力するモード
    | MidashiOkuriInputMode MidashiOkuriInputModeValue -- ▽モード(見出し語入力モード/送り仮名)。辞書変換の対象となる『ひらがな』『カタカナ』の送り仮名を入力するモード
    | DictConversionMode DictConversionModeValue -- ▼モード(辞書変換モード)。見出し語について辞書変換を行うモード


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


type SkkPrevDictConversionMode
    = PreDictConversionMidashiInputMode MidashiInputModeValue


type alias DictConversionModeValue =
    { prevMode : SkkPrevDictConversionMode -- 辞書変換前のモード
    , candidateList : SkkDict.SkkDictCandidateList -- 変換候補の一覧
    , pos : Int -- 変換候補の位置
    , okuri : Maybe String -- 送り仮名。"走る" を変換している場合は "る"
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


{-| Asciiモードを更新
-}
updateAsciiMode : { inputModeValue : AsciiModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateAsciiMode { inputModeValue, inputKey } =
    let
        asciiRegex =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToHiraganaModeKey inputKey then
        HiraganaMode { kakutei = inputModeValue.kakutei, conversionMode = initKakuteiInputMode }

    else if Regex.contains asciiRegex inputKey.key then
        AsciiMode { kakutei = inputModeValue.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { kakutei = String.dropRight 1 inputModeValue.kakutei }

    else
        -- ignore
        AsciiMode inputModeValue


{-| ひらがなモードを更新
-}
updateHiraganaMode : { inputModeValue : HiraganaModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateHiraganaMode { inputModeValue, context, inputKey } =
    updateKanaMode { isHiragana = True, inputModeValue = inputModeValue, context = context, inputKey = inputKey }


{-| カタカナモードを更新
-}
updateKatakanaMode : { inputModeValue : KatakanaModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateKatakanaMode { inputModeValue, context, inputKey } =
    updateKanaMode { isHiragana = False, inputModeValue = inputModeValue, context = context, inputKey = inputKey }


updateKanaMode : { isHiragana : Bool, inputModeValue : KatakanaModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateKanaMode { isHiragana, inputModeValue, context, inputKey } =
    case inputModeValue.conversionMode of
        KakuteiInputMode convertValue ->
            updateKanaKakuteiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        MidashiInputMode convertValue ->
            updateMidashiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        MidashiOkuriInputMode convertValue ->
            updateMidashiOkuriInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }

        DictConversionMode convertValue ->
            updateDictConversionMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = convertValue
                , context = context
                , inputKey = inputKey
                }



-- update 変換モード


updateKanaKakuteiInputMode : { isHiragana : Bool, kakutei : String, conversionModeValue : KakuteiInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateKanaKakuteiInputMode { isHiragana, kakutei, conversionModeValue, context, inputKey } =
    let
        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (buildKakuteiInputMode conversionModeValue.mikakutei)
    in
    if isSwitchToMidashiInputModeKey inputKey then
        -- 確定入力モード → 見出し語入力モード
        -- あいう + S → あいう▽s
        let
            key =
                String.toLower inputKey.key

            ( midashiKakutei, midashiMikakutei ) =
                convertToKana isHiragana conversionModeValue.mikakutei key context.kanaRules
        in
        buildKanaMode isHiragana
            kakutei
            (MidashiInputMode
                { kakutei = midashiKakutei, mikakutei = midashiMikakutei }
            )

    else if isSwitchToAsciiModeKey inputKey then
        -- Asciiモードに切り替え
        AsciiMode { kakutei = kakutei }

    else if isSwitchToKanaModeKey inputKey then
        -- ひらがなモードとカタカナモードの切り替え
        if isHiragana then
            -- ひらがな → カタカナ
            KatakanaMode { kakutei = kakutei, conversionMode = buildKakuteiInputMode "" }

        else
            -- カタカナ → ひらがな
            HiraganaMode { kakutei = kakutei, conversionMode = buildKakuteiInputMode "" }

    else if isKanaConversionAcceptedKey inputKey then
        -- かな変換を試みる
        let
            ( kakutei2, mikakutei ) =
                convertToKana isHiragana conversionModeValue.mikakutei inputKey.key context.kanaRules
        in
        buildKanaMode isHiragana (kakutei ++ kakutei2) (buildKakuteiInputMode mikakutei)

    else if isBackSpaceKey inputKey then
        -- 削除
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar kakutei conversionModeValue.mikakutei
        in
        buildKanaMode isHiragana newKakutei (buildKakuteiInputMode newMikakutei)

    else
        -- ignore
        default


updateMidashiInputMode : { isHiragana : Bool, kakutei : String, conversionModeValue : MidashiInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateMidashiInputMode { isHiragana, kakutei, conversionModeValue, context, inputKey } =
    let
        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (MidashiInputMode conversionModeValue)
    in
    if isSwitchToOkuriInputModeKey inputKey then
        -- 送り仮名の入力に切り替え
        -- あいう▽はし + R → あいう▽はし*r
        let
            head =
                String.toLower inputKey.key
        in
        if not (String.isEmpty conversionModeValue.kakutei) && String.isEmpty conversionModeValue.mikakutei then
            -- 見出し語が確定している
            -- NOTE: 入力したキー(母音以外のアルファベット)によって送り仮名が確定する可能性は考慮しない
            buildKanaMode isHiragana
                kakutei
                (MidashiOkuriInputMode
                    { midashi = conversionModeValue
                    , head = head
                    , mikakutei = head
                    , kakutei = ""
                    }
                )

        else
            default

    else if isCancelKey inputKey then
        -- キャンセル
        -- あいう▽ねこ + Ctrl-g → あいう
        buildKanaMode isHiragana kakutei initKakuteiInputMode

    else if isKanaConversionAcceptedKey inputKey then
        -- かな変換を試みる
        -- ▽sy + a → ▽しゃ
        let
            ( kakutei2, mikakutei ) =
                convertToKana isHiragana conversionModeValue.mikakutei inputKey.key context.kanaRules
        in
        buildKanaMode isHiragana
            kakutei
            (MidashiInputMode { kakutei = conversionModeValue.kakutei ++ kakutei2, mikakutei = mikakutei })

    else if isConversionKey inputKey then
        -- 変換開始
        -- ▽ねこ + Space → ▼猫
        let
            prevMode =
                PreDictConversionMidashiInputMode { conversionModeValue | mikakutei = "" }

            canditateList =
                SkkDict.getCandidateList conversionModeValue.kakutei context.dict
        in
        case canditateList of
            Just candidateList ->
                buildKanaMode isHiragana
                    kakutei
                    (DictConversionMode { prevMode = prevMode, candidateList = candidateList, pos = 0, okuri = Nothing })

            Nothing ->
                -- TODO: 変換候補がない場合に、辞書登録モードに移行
                default

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽ねこ + BS → ▽ね
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar conversionModeValue.kakutei conversionModeValue.mikakutei
        in
        buildKanaMode isHiragana kakutei (MidashiInputMode { kakutei = newKakutei, mikakutei = newMikakutei })

    else
        -- ignore
        default


updateMidashiOkuriInputMode : { isHiragana : Bool, kakutei : String, conversionModeValue : MidashiOkuriInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateMidashiOkuriInputMode { isHiragana, kakutei, conversionModeValue, context, inputKey } =
    let
        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (MidashiOkuriInputMode conversionModeValue)
    in
    if isCancelKey inputKey then
        -- キャンセル
        -- あいう▽はし*r + Ctrl-g → あいう▽はし
        buildKanaMode isHiragana kakutei (MidashiInputMode conversionModeValue.midashi)

    else if isKanaConversionAcceptedKey inputKey then
        -- TODO: かな変換を試みる
        -- TODO: 辞書変換モードに遷移
        let
            newOkuriHead =
                if String.isEmpty conversionModeValue.head then
                    inputKey.key

                else
                    conversionModeValue.head

            ( okuriKakutei2, newOkuriMikakutei ) =
                convertToKana isHiragana conversionModeValue.mikakutei inputKey.key context.kanaRules

            newOkuriKakutei =
                conversionModeValue.kakutei ++ okuriKakutei2

            isOkuriKakutei =
                not (String.isEmpty newOkuriKakutei) && String.isEmpty newOkuriMikakutei
        in
        if isOkuriKakutei then
            -- 送り仮名が確定した
            let
                prevMode =
                    PreDictConversionMidashiInputMode conversionModeValue.midashi

                searchKey =
                    conversionModeValue.midashi.kakutei ++ newOkuriHead

                canditateList =
                    SkkDict.getCandidateList searchKey context.dict
            in
            case canditateList of
                Just candidateList ->
                    buildKanaMode isHiragana
                        kakutei
                        (DictConversionMode { prevMode = prevMode, candidateList = candidateList, pos = 0, okuri = Just newOkuriKakutei })

                Nothing ->
                    -- TODO: 変換候補がない場合に、辞書登録モードに移行
                    default

        else
            -- 送り仮名が確定していない
            buildKanaMode isHiragana
                kakutei
                (MidashiOkuriInputMode { conversionModeValue | head = newOkuriHead, kakutei = newOkuriKakutei, mikakutei = newOkuriMikakutei })

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽はし*r + BS → ▽はし
        -- ▽はし*っt + BS → ▽はし*っ
        let
            ( newKakutei, newMikakutei ) =
                deleteInputChar conversionModeValue.kakutei conversionModeValue.mikakutei

            newHead =
                if newKakutei == "" && newMikakutei == "" then
                    ""

                else
                    conversionModeValue.head
        in
        buildKanaMode isHiragana kakutei (MidashiOkuriInputMode { conversionModeValue | head = newHead, kakutei = newKakutei, mikakutei = newMikakutei })

    else
        -- ignore
        default


updateDictConversionMode : { isHiragana : Bool, kakutei : String, conversionModeValue : DictConversionModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateDictConversionMode { isHiragana, kakutei, conversionModeValue, inputKey } =
    let
        -- 直前の変換モード
        previousConversionMode : SkkInputMode
        previousConversionMode =
            let
                { prevMode } =
                    conversionModeValue
            in
            buildKanaMode isHiragana
                kakutei
                (case prevMode of
                    PreDictConversionMidashiInputMode v ->
                        MidashiInputMode v
                )

        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (DictConversionMode conversionModeValue)
    in
    if isCancelKey inputKey then
        -- キャンセル
        previousConversionMode

    else if isNextCandidateKey inputKey then
        -- 次候補
        let
            { candidateList, pos } =
                conversionModeValue
        in
        if pos + 1 == List.length candidateList then
            -- TODO: 辞書登録モード
            default

        else
            buildKanaMode isHiragana kakutei (DictConversionMode { conversionModeValue | pos = pos + 1 })

    else if isPreviousCandidateKey inputKey then
        -- 前候補
        let
            { pos } =
                conversionModeValue
        in
        if pos == 0 then
            -- 直前の変換モードに戻る
            previousConversionMode

        else
            buildKanaMode isHiragana kakutei (DictConversionMode { conversionModeValue | pos = pos - 1 })

    else if isEnterKey inputKey then
        -- 確定
        -- あいう▼猫 → あいう猫
        let
            { pos, candidateList, prevMode } =
                conversionModeValue

            converted =
                Array.fromList candidateList |> Array.get pos |> Maybe.withDefault ""
        in
        case prevMode of
            PreDictConversionMidashiInputMode _ ->
                buildKanaMode isHiragana (kakutei ++ converted) (KakuteiInputMode { mikakutei = "" })

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
                -- ルールに存在しない検索キーは変換後の文字列として扱う
                -- e.g. 数字、クォート
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


initKakuteiInputMode : SkkConversionMode
initKakuteiInputMode =
    buildKakuteiInputMode ""


buildKakuteiInputMode : String -> SkkConversionMode
buildKakuteiInputMode mikakutei =
    KakuteiInputMode { mikakutei = mikakutei }


buildKanaMode : Bool -> String -> SkkConversionMode -> SkkInputMode
buildKanaMode isHiragana kakutei conversionMode =
    if isHiragana then
        HiraganaMode { kakutei = kakutei, conversionMode = conversionMode }

    else
        KatakanaMode { kakutei = kakutei, conversionMode = conversionMode }



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
isSwitchToOkuriInputModeKey { key } =
    let
        alphaPattern =
            Regex.fromString "^[A-Z]$" |> Maybe.withDefault Regex.never

        vowelPattern =
            Regex.fromString "^[AIUEO]$" |> Maybe.withDefault Regex.never
    in
    -- 母音は除く
    Regex.contains alphaPattern key && not (Regex.contains vowelPattern key)


isSwitchToKanaModeKey : SkkInputKey -> Bool
isSwitchToKanaModeKey { key } =
    key == "q"


isKanaConversionAcceptedKey : SkkInputKey -> Bool
isKanaConversionAcceptedKey { key } =
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


isConversionKey : SkkInputKey -> Bool
isConversionKey { key } =
    key == "Space"


isNextCandidateKey : SkkInputKey -> Bool
isNextCandidateKey { key } =
    key == "Space"


isPreviousCandidateKey : SkkInputKey -> Bool
isPreviousCandidateKey { key } =
    key == "x"

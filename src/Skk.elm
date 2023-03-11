module Skk exposing (Skk, SkkContext, SkkConversionMode(..), SkkInputKey, SkkInputMode(..), SkkPrevDictConversionMode(..), SkkPrevDictRegistrationModeValue(..), init, update)

import Array
import Maybe.Extra exposing (isJust, isNothing)
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



---- 入力モード


type SkkInputMode
    = AsciiMode AsciiModeValue -- Ascii文字を入力するモード
    | HiraganaMode HiraganaModeValue -- ひらがなを入力するモード
    | KatakanaMode KatakanaModeValue -- カタカナを入力するモード


{-| Asciiモード
-}
type alias AsciiModeValue =
    { kakutei : Maybe String
    }


{-| ひらがなモード
-}
type alias HiraganaModeValue =
    { kakutei : Maybe String
    , conversionMode : SkkConversionMode
    }


{-| カタカナモード
-}
type alias KatakanaModeValue =
    { kakutei : Maybe String
    , conversionMode : SkkConversionMode
    }



---- 変換モード


type SkkConversionMode
    = KakuteiInputMode KakuteiInputModeValue -- ■モード(確定入力モード)。ルールに従って、ローマ字から『ひらがな』『カタカタ』に変換するモード
    | MidashiInputMode MidashiInputModeValue -- ▽モード(見出し語入力モード)。辞書変換の対象となる『ひらがな』『カタカナ』の見出し語を入力するモード
    | MidashiOkuriInputMode MidashiOkuriInputModeValue -- ▽モード(見出し語入力モード/送り仮名)。辞書変換の対象となる『ひらがな』『カタカナ』の送り仮名を入力するモード
    | DictConversionMode DictConversionModeValue -- ▼モード(辞書変換モード)。見出し語について辞書変換を行うモード
    | DictRegistrationMode DictRegistrationModeValue -- 辞書登録モード。辞書登録を行うモード


{-| 確定入力モードの値
-}
type alias KakuteiInputModeValue =
    { mikakutei : Maybe String
    }


{-| 見出し語入力モードの値
-}
type alias MidashiInputModeValue =
    { kakuteiMidashi : Maybe String -- 確定した見出し語
    , mikakuteiMidashi : Maybe String -- 未確定の見出し語
    }


{-| 送り仮名入力モードの値
-}
type alias MidashiOkuriInputModeValue =
    { midashi : MidashiInputModeValue -- 見出し語。"▽たたか*っt" の場合は "たたか"
    , headOkuri : Maybe String -- 送り仮名の最初のローマ字。"▽たたか*っt" の場合は "っ" を構成する "t"
    , kakuteiOkuri : Maybe String -- 確定した送り仮名。"▽たたか*っt" の場合は "っ"
    , mikakuteiOkuri : Maybe String -- 未確定の送り仮名。"▽たたか*っt" の場合は "t"
    }


{-| 辞書変換前のモード
-}
type SkkPrevDictConversionMode
    = PrevDictConversionMidashiInputMode MidashiInputModeValue


{-| 辞書変換モードの値
-}
type alias DictConversionModeValue =
    { prevMode : SkkPrevDictConversionMode -- 辞書変換前のモード
    , candidateList : SkkDict.SkkDictCandidateList -- 変換候補の一覧
    , pos : Int -- 変換候補の位置
    , okuri : Maybe String -- 送り仮名。"走る" を変換している場合は "る"
    }


{-| 辞書登録前のモード
-}
type SkkPrevDictRegistrationModeValue
    = PrevDictRegistrationMidashiInputMode MidashiInputModeValue
    | PrevDictRegistrationDictConversionMode DictConversionModeValue


{-| 辞書登録モードの値
-}
type alias DictRegistrationModeValue =
    { prevMode : SkkPrevDictRegistrationModeValue -- 辞書登録前のモード
    , inputMode : SkkInputMode -- 辞書登録に使用する入力モード
    }



---- 入力キー


{-| KeyboardEventのwrapper
<https://developer.mozilla.org/ja/docs/Web/API/KeyboardEvent>
-}
type alias SkkInputKey =
    { key : String -- 入力したキーを表す文字列
    , shift : Bool -- Shiftキーを入力しているか
    , ctrl : Bool -- Ctrlキーを入力しているか
    }



---- factory


init : SkkContext -> Skk
init context =
    { mode = AsciiMode { kakutei = Nothing }
    , context = context
    }



---- update


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
        buildKanaMode True inputModeValue.kakutei initKakuteiInputMode

    else if Regex.contains asciiRegex inputKey.key then
        AsciiMode { kakutei = concatInputString inputModeValue.kakutei (Just inputKey.key) }

    else if isBackSpaceKey inputKey then
        AsciiMode { kakutei = deleteMaybeInputString inputModeValue.kakutei }

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
        KakuteiInputMode conversionValue ->
            updateKanaKakuteiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = conversionValue
                , context = context
                , inputKey = inputKey
                }

        MidashiInputMode conversionValue ->
            updateMidashiInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = conversionValue
                , context = context
                , inputKey = inputKey
                }

        MidashiOkuriInputMode conversionValue ->
            updateMidashiOkuriInputMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = conversionValue
                , context = context
                , inputKey = inputKey
                }

        DictConversionMode conversionValue ->
            updateDictConversionMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = conversionValue
                , context = context
                , inputKey = inputKey
                }

        DictRegistrationMode conversionValue ->
            updateDictRegistrationMode
                { isHiragana = isHiragana
                , kakutei = inputModeValue.kakutei
                , conversionModeValue = conversionValue
                , context = context
                , inputKey = inputKey
                }



-- update 変換モード


updateKanaKakuteiInputMode : { isHiragana : Bool, kakutei : Maybe String, conversionModeValue : KakuteiInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
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
                { kakuteiMidashi = midashiKakutei, mikakuteiMidashi = midashiMikakutei }
            )

    else if isSwitchToAsciiModeKey inputKey then
        -- Asciiモードに切り替え
        AsciiMode { kakutei = kakutei }

    else if isSwitchToKanaModeKey inputKey then
        -- ひらがなモードとカタカナモードの切り替え
        if isHiragana then
            -- ひらがな → カタカナ
            KatakanaMode { kakutei = kakutei, conversionMode = buildKakuteiInputMode Nothing }

        else
            -- カタカナ → ひらがな
            HiraganaMode { kakutei = kakutei, conversionMode = buildKakuteiInputMode Nothing }

    else if isKanaConversionAcceptedKey inputKey then
        -- かな変換を試みる
        let
            ( kakutei2, mikakutei ) =
                convertToKana isHiragana conversionModeValue.mikakutei inputKey.key context.kanaRules
        in
        buildKanaMode isHiragana (concatInputString kakutei kakutei2) (buildKakuteiInputMode mikakutei)

    else if isBackSpaceKey inputKey then
        -- 削除
        let
            ( newKakutei, newMikakutei ) =
                deleteInputConversionString kakutei conversionModeValue.mikakutei
        in
        buildKanaMode isHiragana newKakutei (buildKakuteiInputMode newMikakutei)

    else
        -- ignore
        default


updateMidashiInputMode : { isHiragana : Bool, kakutei : Maybe String, conversionModeValue : MidashiInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
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
        if isJust conversionModeValue.kakuteiMidashi && isNothing conversionModeValue.mikakuteiMidashi then
            -- 見出し語が確定している
            -- NOTE: 入力したキー(母音以外のアルファベット)によって送り仮名が確定する可能性は考慮しない
            buildKanaMode isHiragana
                kakutei
                (MidashiOkuriInputMode
                    { midashi = conversionModeValue
                    , headOkuri = Just head
                    , mikakuteiOkuri = Just head
                    , kakuteiOkuri = Nothing
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
                convertToKana isHiragana conversionModeValue.mikakuteiMidashi inputKey.key context.kanaRules
        in
        buildKanaMode isHiragana
            kakutei
            (MidashiInputMode { kakuteiMidashi = concatInputString conversionModeValue.kakuteiMidashi kakutei2, mikakuteiMidashi = mikakutei })

    else if isConversionKey inputKey then
        -- 変換開始
        -- ▽ねこ + Space → ▼猫
        let
            prevMode =
                PrevDictConversionMidashiInputMode { conversionModeValue | mikakuteiMidashi = Nothing }

            canditateList =
                SkkDict.getCandidateList (Maybe.withDefault "" conversionModeValue.kakuteiMidashi) [ context.dict ]
        in
        case canditateList of
            Just candidateList ->
                buildKanaMode isHiragana
                    kakutei
                    (DictConversionMode { prevMode = prevMode, candidateList = candidateList, pos = 0, okuri = Nothing })

            Nothing ->
                -- 変換候補がない場合は辞書登録モードに遷移
                buildKanaMode isHiragana
                    kakutei
                    (DictRegistrationMode
                        { prevMode = PrevDictRegistrationMidashiInputMode conversionModeValue
                        , inputMode = initKanaMode isHiragana
                        }
                    )

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽ねこ + BS → ▽ね
        let
            ( newKakutei, newMikakutei ) =
                deleteInputConversionString conversionModeValue.kakuteiMidashi conversionModeValue.mikakuteiMidashi
        in
        buildKanaMode isHiragana kakutei (MidashiInputMode { kakuteiMidashi = newKakutei, mikakuteiMidashi = newMikakutei })

    else
        -- ignore
        default


updateMidashiOkuriInputMode : { isHiragana : Bool, kakutei : Maybe String, conversionModeValue : MidashiOkuriInputModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
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
        -- かな変換を試みる
        let
            newOkuriHead =
                Just (Maybe.withDefault inputKey.key conversionModeValue.headOkuri)

            ( okuriKakutei2, newOkuriMikakutei ) =
                convertToKana isHiragana conversionModeValue.mikakuteiOkuri inputKey.key context.kanaRules

            newOkuriKakutei =
                concatInputString conversionModeValue.kakuteiOkuri okuriKakutei2

            isOkuriKakutei =
                isJust newOkuriKakutei && isNothing newOkuriMikakutei
        in
        if isOkuriKakutei then
            -- 送り仮名が確定した
            let
                prevMode =
                    PrevDictConversionMidashiInputMode conversionModeValue.midashi

                searchKey =
                    Maybe.withDefault "" (concatInputString conversionModeValue.midashi.kakuteiMidashi newOkuriHead)

                canditateList =
                    SkkDict.getCandidateList searchKey [ context.dict ]
            in
            case canditateList of
                Just candidateList ->
                    buildKanaMode isHiragana
                        kakutei
                        (DictConversionMode { prevMode = prevMode, candidateList = candidateList, pos = 0, okuri = newOkuriKakutei })

                Nothing ->
                    -- 変換候補がない場合は辞書登録モードに遷移する
                    buildKanaMode isHiragana
                        kakutei
                        (DictRegistrationMode
                            { prevMode = PrevDictRegistrationMidashiInputMode conversionModeValue.midashi
                            , inputMode = initKanaMode isHiragana
                            }
                        )

        else
            -- 送り仮名が確定していない
            buildKanaMode isHiragana
                kakutei
                (MidashiOkuriInputMode { conversionModeValue | headOkuri = newOkuriHead, kakuteiOkuri = newOkuriKakutei, mikakuteiOkuri = newOkuriMikakutei })

    else if isBackSpaceKey inputKey then
        -- 削除
        -- ▽はし*r + BS → ▽はし
        -- ▽はし*っt + BS → ▽はし*っ
        let
            ( newKakutei, newMikakutei ) =
                deleteInputConversionString conversionModeValue.kakuteiOkuri conversionModeValue.mikakuteiOkuri

            newHead =
                if isNothing newKakutei && isNothing newMikakutei then
                    Nothing

                else
                    conversionModeValue.headOkuri
        in
        buildKanaMode isHiragana kakutei (MidashiOkuriInputMode { conversionModeValue | headOkuri = newHead, kakuteiOkuri = newKakutei, mikakuteiOkuri = newMikakutei })

    else
        -- ignore
        default


updateDictConversionMode : { isHiragana : Bool, kakutei : Maybe String, conversionModeValue : DictConversionModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
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
                    PrevDictConversionMidashiInputMode v ->
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
            buildKanaMode isHiragana
                kakutei
                (DictRegistrationMode
                    { prevMode = PrevDictRegistrationDictConversionMode conversionModeValue
                    , inputMode = initKanaMode isHiragana
                    }
                )

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
                Array.fromList candidateList |> Array.get pos
        in
        case prevMode of
            PrevDictConversionMidashiInputMode _ ->
                buildKanaMode isHiragana (concatInputString kakutei converted) (KakuteiInputMode { mikakutei = Nothing })

    else
        -- ignore
        default


updateDictRegistrationMode : { isHiragana : Bool, kakutei : Maybe String, conversionModeValue : DictRegistrationModeValue, context : SkkContext, inputKey : SkkInputKey } -> SkkInputMode
updateDictRegistrationMode { isHiragana, kakutei, conversionModeValue, context, inputKey } =
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
                    PrevDictRegistrationMidashiInputMode v ->
                        MidashiInputMode v

                    PrevDictRegistrationDictConversionMode v ->
                        DictConversionMode v
                )

        -- TODO: キャンセル可能か
        canCancel : Bool
        canCancel =
            False

        -- TODO: 確定可能か
        canEnter : Bool
        canEnter =
            False

        -- デフォルト値
        default : SkkInputMode
        default =
            buildKanaMode isHiragana kakutei (DictRegistrationMode conversionModeValue)
    in
    if isCancelKey inputKey && canCancel then
        -- キャンセル
        previousConversionMode

    else if isEnterKey inputKey && canEnter then
        -- TODO: 確定
        default

    else
        -- 辞書登録用の入力モードに処理を移譲する
        let
            newSkk =
                update { mode = conversionModeValue.inputMode, context = context } inputKey
        in
        buildKanaMode isHiragana kakutei (DictRegistrationMode { conversionModeValue | inputMode = newSkk.mode })



---- convert


convertToKana : Bool -> Maybe String -> String -> SkkKanaRule.SkkKanaRules -> ( Maybe String, Maybe String )
convertToKana isHiragana currentKey addtionalKey rules =
    let
        searchKey =
            concatInputString currentKey (Just addtionalKey) |> Maybe.withDefault ""
    in
    case SkkKanaRule.search searchKey rules of
        SkkKanaRule.PartialMatch ->
            ( Nothing, Just searchKey )

        SkkKanaRule.PerfectMatch { hiragana, katakana, next } ->
            if isHiragana then
                ( Just hiragana, next )

            else
                ( Just katakana, next )

        SkkKanaRule.NoMatch ->
            if isNothing currentKey then
                -- ルールに存在しない検索キーは変換後の文字列として扱う
                -- e.g. 数字、クォート
                ( Just addtionalKey, Nothing )

            else
                -- 未確定文字列を初期化して再度変換を行う
                convertToKana isHiragana Nothing addtionalKey rules



---- input string functions


toInputString : String -> Maybe String
toInputString s =
    if String.isEmpty s then
        Nothing

    else
        Just s


deleteInputString : String -> Maybe String
deleteInputString str =
    dropRight 1 str |> toInputString


deleteMaybeInputString : Maybe String -> Maybe String
deleteMaybeInputString =
    Maybe.andThen deleteInputString


deleteInputConversionString : Maybe String -> Maybe String -> ( Maybe String, Maybe String )
deleteInputConversionString kakutei mikakutei =
    case ( kakutei, mikakutei ) of
        ( k, Just s ) ->
            ( k, deleteInputString s )

        ( Just s, m ) ->
            ( deleteInputString s, m )

        _ ->
            ( kakutei, mikakutei )


concatInputString : Maybe String -> Maybe String -> Maybe String
concatInputString s1 s2 =
    Maybe.withDefault "" s1 ++ Maybe.withDefault "" s2 |> toInputString



---- factory(internal)


initKakuteiInputMode : SkkConversionMode
initKakuteiInputMode =
    buildKakuteiInputMode Nothing


buildKakuteiInputMode : Maybe String -> SkkConversionMode
buildKakuteiInputMode mikakutei =
    KakuteiInputMode { mikakutei = mikakutei }


initKanaMode : Bool -> SkkInputMode
initKanaMode isHiragana =
    buildKanaMode isHiragana Nothing initKakuteiInputMode


buildKanaMode : Bool -> Maybe String -> SkkConversionMode -> SkkInputMode
buildKanaMode isHiragana kakutei conversionMode =
    if isHiragana then
        HiraganaMode { kakutei = kakutei, conversionMode = conversionMode }

    else
        KatakanaMode { kakutei = kakutei, conversionMode = conversionMode }



---- key check functions
---- ref. https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values


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

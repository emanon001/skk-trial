module Skk exposing (AsciiModeValue, HiraganaModeValue, Skk(..), SkkInputKey, init, update)

import Regex


type Skk
    = AsciiMode AsciiModeValue
    | HiraganaMode HiraganaModeValue


type alias AsciiModeValue =
    { kakutei : String
    }


type alias HiraganaModeValue =
    { kakutei : String
    , mikakutei : String
    }


type alias SkkInputKey =
    { key : String
    , shift : Bool
    , ctrl : Bool
    }



-- factory


init : Skk
init =
    AsciiMode { kakutei = "" }



-- update


update : Skk -> SkkInputKey -> Skk
update skk key =
    case skk of
        AsciiMode value ->
            updateAsciiMode value key

        HiraganaMode value ->
            updateHiraganaMode value key


updateAsciiMode : AsciiModeValue -> SkkInputKey -> Skk
updateAsciiMode value inputKey =
    let
        asciiKey =
            Regex.fromString "^[a-zA-Z0-9 +=!@#$%^&*()\\-_`~\\|'\":;[\\]{}?/.,<>]$" |> Maybe.withDefault Regex.never
    in
    if isSwitchToKanaModeKey inputKey then
        HiraganaMode { kakutei = value.kakutei, mikakutei = "" }

    else if Regex.contains asciiKey inputKey.key then
        AsciiMode { kakutei = value.kakutei ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { kakutei = applyBackSpace value.kakutei }

    else
        -- ignore
        AsciiMode { kakutei = value.kakutei }


updateHiraganaMode : HiraganaModeValue -> SkkInputKey -> Skk
updateHiraganaMode value inputKey =
    if isSwitchToHenkanModeKey inputKey then
        -- TODO
        HiraganaMode value

    else if isHenkanAcceptedKey inputKey then
        -- TODO
        HiraganaMode value

    else if isBackSpaceKey inputKey then
        HiraganaMode { value | kakutei = applyBackSpace value.kakutei }

    else if isSpaceKey inputKey then
        HiraganaMode { value | kakutei = value.kakutei ++ " " }

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

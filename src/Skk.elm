module Skk exposing (AsciiModeValue, HiraganaModeValue, Skk(..), SkkHenkanMode(..), SkkInputKey, init, update)

import Regex
import SkkDict


type Skk
    = AsciiMode AsciiModeValue
    | HiraganaMode HiraganaModeValue


type SkkHenkanMode
    = KakuteiInputMode String
    | MidashiInputMode
        { midashi : String
        , okuri : String
        }
    | DictHenkanMode
        { candidateList : SkkDict.SkkDictCandidateList
        , pos : Int
        }


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



-- factory


init : Skk
init =
    AsciiMode { input = "" }



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
        HiraganaMode { input = value.input, henkanMode = KakuteiInputMode "" }

    else if Regex.contains asciiKey inputKey.key then
        AsciiMode { input = value.input ++ inputKey.key }

    else if isBackSpaceKey inputKey then
        AsciiMode { input = applyBackSpace value.input }

    else
        -- ignore
        AsciiMode { input = value.input }


updateHiraganaMode : HiraganaModeValue -> SkkInputKey -> Skk
updateHiraganaMode value inputKey =
    if isSwitchToHenkanModeKey inputKey then
        -- TODO
        HiraganaMode value

    else if isHenkanAcceptedKey inputKey then
        -- TODO
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

module SkkTests.TestHelper exposing (..)
import Skk exposing (SkkConversionMode(..))
import SkkDict
import SkkKanaRule
import Test exposing (..)



initSkk : Skk.SkkInputMode -> Skk.Skk
initSkk mode =
    let
        dictString =
            """
            ねこ /猫/
            だいすk /大好/
            きょう /今日/京/強/
            はしr /走/奔/
            """
    in
    { mode = mode
    , context =
        { kanaRules = SkkKanaRule.getDefaultRules
        , dict = SkkDict.fromDictString dictString
        }
    }


buildPlainInputKey : String -> Skk.SkkInputKey
buildPlainInputKey key =
    { key = key
    , shift = False
    , ctrl = False
    }


buildMidashiInputKey : String -> Skk.SkkInputKey
buildMidashiInputKey key =
    { key = key
    , shift = True
    , ctrl = False
    }

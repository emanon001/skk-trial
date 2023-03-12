module SkkDict exposing (SkkDict, SkkDictCandidateList, SkkDictKey, fromDictString, getCandidateList)

import Dict exposing (Dict)
import List
import Maybe.Extra



---- types


type alias SkkDict =
    Dict SkkDictKey SkkDictCandidateList


{-| ユーザー辞書
-}
type alias SkkUserDict =
    { dict : SkkDict
    , get : SkkDictKey -> SkkDict -> Maybe SkkDictCandidateList
    , insert : SkkDictKey -> String -> SkkDict -> SkkDict
    , remove : SkkDictKey -> String -> SkkDict -> SkkDict
    }


type alias SkkDictKey =
    String


type alias SkkDictCandidateList =
    List String



---- create


fromDictString : String -> SkkDict
fromDictString dictStr =
    let
        toLines : String -> List String
        toLines =
            String.lines >> List.map String.trim

        filterLines : List String -> List String
        filterLines =
            List.filter (\l -> not (String.startsWith ";" l) && not (String.isEmpty l))

        toEntry : String -> Maybe ( String, List String )
        toEntry l =
            case String.split "/" l of
                _ :: [] ->
                    Nothing

                key :: conversionList ->
                    Just
                        ( String.trim key
                        , List.map removeAnnotation conversionList
                            |> Maybe.Extra.values
                            |> List.filter (not << String.isEmpty)
                        )

                _ ->
                    Nothing

        removeAnnotation : String -> Maybe String
        removeAnnotation s =
            (String.split ";" >> List.head) s

        buildDict : List String -> SkkDict
        buildDict lines =
            List.filterMap toEntry lines |> Dict.fromList
    in
    toLines dictStr |> filterLines |> buildDict


initUserDict : String -> SkkUserDict
initUserDict dictStr =
    let
        dict =
            fromDictString dictStr

        get : SkkDictKey -> SkkDict -> Maybe SkkDictCandidateList
        get key d =
            -- TODO: 実装
            Nothing

        insert : SkkDictKey -> String -> SkkDict -> SkkDict
        insert key v d =
            -- TODO: 実装
            d

        remove : SkkDictKey -> String -> SkkDict -> SkkDict
        remove k v d =
            -- TODO: 実装
            d
    in
    { dict = dict
    , get = get
    , insert = insert
    , remove = remove
    }



---- query


getCandidateList : SkkDictKey -> List SkkDict -> Maybe SkkDictCandidateList
getCandidateList key dictList =
    case dictList of
        [] ->
            Nothing

        dict :: [] ->
            Dict.get key dict

        dict :: rest ->
            let
                list1 =
                    Dict.get key dict

                list2 =
                    getCandidateList key rest
            in
            Maybe.Extra.combine [ list1, list2 ] |> Maybe.map List.concat

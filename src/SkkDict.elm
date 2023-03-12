module SkkDict exposing (SkkDict, SkkDictCandidateList, SkkDictKey, fromDictString, getCandidateList)

import Dict exposing (Dict)
import List
import Maybe.Extra



-- types


type alias SkkDict =
    Dict SkkDictKey SkkDictCandidateList


type alias SkkDictKey =
    String


type alias SkkDictCandidateList =
    List String



-- create


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



-- get


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

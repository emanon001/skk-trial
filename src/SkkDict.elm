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

                key :: henkanList ->
                    Just
                        ( String.trim key
                        , List.map (String.split ";" >> List.head) henkanList
                            |> List.map (Maybe.withDefault "")
                            |> List.filter (not << String.isEmpty)
                        )

                _ ->
                    Nothing

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

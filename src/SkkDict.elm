module SkkDict exposing (..)

import Dict exposing (Dict)



-- types


type alias SkkDict =
    Dict SkkDictKey SkkDictValues


type alias SkkDictKey =
    String


type alias SkkDictValues =
    List String



-- create


fromDictString : String -> SkkDict
fromDictString dictStr =
    let
        toList : String -> List String
        toList =
            String.lines >> List.map String.trim

        filterList : List String -> List String
        filterList list =
            List.filter (\row -> not (String.startsWith ";" row) && not (String.isEmpty row)) list

        toEntry : String -> Maybe ( String, List String )
        toEntry s =
            case String.split "/" s of
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
        buildDict list =
            List.filterMap toEntry list |> Dict.fromList
    in
    toList dictStr |> filterList |> buildDict

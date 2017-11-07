module Grid exposing
  ( Grid
  , empty
  , insert
  , remove
  , member
  , memberRow
  , sizeRow
  , toIndexedList)

import Dict exposing (Dict)

type Grid a
  = Grid (Dict Int (Dict Int a))

empty : Int -> Grid a
empty rows =
  let
    range =
      List.range 0 (rows - 1)

    create range dict =
      case range of
        [] ->
          dict

        row :: rest ->
          Dict.insert row Dict.empty dict
            |> create rest
  in
    Grid (create range Dict.empty)

insert : Int -> Int -> a -> Grid a -> Grid a
insert row column a grid =
  case grid of
    Grid dict ->
      let
        rowDict =
          case Dict.get row dict of
            Just dict ->
              Dict.insert column a dict

            Nothing ->
              Dict.empty
                |> Dict.insert column a
      in
        Grid (Dict.insert row rowDict dict)

remove : Int -> Int -> Grid a -> Grid a
remove row column grid =
  case grid of
    Grid dict ->
      let
        remove dict =
          Dict.remove column dict
      in
        Grid (Dict.update row (Maybe.map remove) dict)

member : Int -> Int -> Grid a -> Bool
member row column grid =
  case grid of
    Grid dict ->
      case Dict.get row dict of
        Just dict ->
          Dict.member column dict

        Nothing ->
          False

memberRow : Int -> Grid a -> Bool
memberRow row grid =
  case grid of
    Grid dict ->
      Dict.member row dict

sizeRow : Int -> Grid a -> Int
sizeRow row grid =
  case grid of
    Grid dict ->
      case Dict.get row dict of
        Just dict ->
          Dict.size dict

        Nothing ->
          0

toIndexedList : Grid a -> List ((Int, Int), a)
toIndexedList grid =
  case grid of
    Grid dict ->
      let
        rowToList (row, dict) =
          Dict.toList dict
            |> List.map (\(column, a) -> ((row, column), a))
      in
        Dict.toList dict
          |> List.map rowToList
          |> List.concat

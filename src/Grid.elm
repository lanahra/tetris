module Grid exposing (Grid, empty, set, toIndexedList)

import Dict exposing (Dict)

type Grid a
  = Grid (Dict Int (Dict Int a))

empty : Grid a
empty =
  Grid Dict.empty

set : Int -> Int -> a -> Grid a -> Grid a
set row column a grid =
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

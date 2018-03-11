module Field exposing
  ( Field
  , rows
  , columns
  , empty
  , insert
  , take
  , occupied
  , get
  , set
  , toIndexedList
  )


import Array exposing (Array)
import Tetromino exposing (Tetromino, Shape)


type Field =
  Field (Array (Array (Maybe Shape)))


(rows, columns) = (22, 10)


empty : Field
empty =
  Field (Nothing |> Array.repeat columns |> Array.repeat rows)


insert : Tetromino -> Field -> Field
insert tetromino field =
  let
    shape =
      Just (Tetromino.shape tetromino)

    ins positions field =
      case positions of
        [] ->
          field

        (x, y) :: rest ->
          ins rest (set x y shape field)
  in
    ins (Tetromino.position tetromino) field


take : Tetromino -> Field -> Field
take tetromino field =
  let
    remove positions field =
      case positions of
        [] ->
          field

        (x, y) :: rest ->
          remove rest (set x y Nothing field)
  in
    remove (Tetromino.position tetromino) field


occupied : Int -> Int -> Field -> Bool
occupied x y field =
  if x < 0 || x >= columns || y < 0 || y >= rows then
    True
  else
    case get x y field of
      Just s ->
        True

      Nothing ->
        False


get : Int -> Int -> Field -> Maybe Shape
get x y field =
  case field of
    Field a ->
      case Array.get y a of
        Just a ->
          case Array.get x a of
            Just s ->
              s

            Nothing ->
              Nothing

        Nothing ->
          Nothing


set : Int -> Int -> Maybe Shape -> Field -> Field
set x y shape field =
  case field of
    Field rows ->
      case Array.get y rows of
        Just columns ->
          let
            row =
              Array.set x shape columns
          in
            Field (Array.set y row rows)

        Nothing ->
          field


toIndexedList : Field -> List ((Int, Int), Shape)
toIndexedList field =
  let
    columns y (x, s) =
      case s of
        Just s ->
          [ ((x, y), s) ]

        Nothing ->
          []

    rows (y, a) =
      Array.toIndexedList a
        |> List.map (columns y)
        |> List.concat
  in
    case field of
      Field a ->
        Array.toIndexedList a
          |> List.map rows
          |> List.concat

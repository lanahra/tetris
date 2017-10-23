import Array exposing (Array)
import Collage exposing (..)
import Color
import Element exposing (..)
import Html exposing (..)
import Matrix exposing (Matrix, Location)
import Task
import Time exposing (Time)
import Window

main =
  Html.program
    { init = (init model, initialSizeCmd)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform WindowResize Window.size


-- MODEL

type alias Model =
  { windowSize : (Int, Int)
  , matrix : Matrix Cell
  , blocks : List Block
  }

type Cell
  = Empty
  | Square

type alias Block =
  { row : Int
  , column : Int
  }

(columns, rows) = (10, 22)

model : Model
model =
  { windowSize = (0, 0)
  , matrix = Matrix.matrix rows columns (\_ -> Empty)
  , blocks = { row = 3, column = 5} :: { row = 2, column = 5} :: []
  }

init : Model -> Model
init model =
  { model | matrix = initMatrix model.blocks model.matrix }

initMatrix : List Block -> Matrix Cell -> Matrix Cell
initMatrix blocks matrix =
  case blocks of
    [] ->
      matrix

    [block] ->
      Matrix.set (block.row, block.column) Square matrix

    (block::rest) ->
      let
        newMatrix =
          Matrix.set (block.row, block.column) Square matrix
      in
        initMatrix rest newMatrix


-- UPDATE

type Msg
  = WindowResize Window.Size
  | Step Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowResize size ->
      ({ model | windowSize = (size.width, size.height) }, Cmd.none)
    Step _ ->
      (step model, Cmd.none)

step : Model -> Model
step model =
  { model | matrix = stepMatrix model.matrix }

stepMatrix : Matrix Cell -> Matrix Cell
stepMatrix matrix =
  recMatrix (rows - 1) 0 matrix

recMatrix : Int -> Int -> Matrix Cell -> Matrix Cell
recMatrix row column matrix =
  if column == columns then
    recMatrix (row - 1) 0 matrix
  else
    if row < 0 then
      matrix
    else
      let
        newMatrix =
          stepCell row column matrix
      in
        recMatrix row (column + 1) newMatrix

stepCell : Int -> Int -> Matrix Cell -> Matrix Cell
stepCell row column matrix =
  let
    bottomCell =
      Matrix.get (row + 1, column) matrix
  in
    case bottomCell of
      Just Empty ->
        let
          cell =
            Matrix.get (row, column) matrix
        in
          case cell of
            Just Square ->
              Matrix.set (row, column) Empty matrix
                |> Matrix.set (row + 1, column) Square

            Just Empty ->
              matrix

            Nothing ->
              matrix

      Just Square ->
        matrix

      Nothing ->
        matrix


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\size -> WindowResize size)
    , Time.every Time.second Step
    ]


-- VIEW

blockSize = 40

view : Model -> Html Msg
view model =
  toHtml <|
  uncurry container model.windowSize middle <|
  layers <|
  (::) makeField <|
  List.singleton <|
  flow down <|
  List.map makeRow <|
  (++) (List.repeat 2 (List.repeat columns Empty)) <|
  List.drop 2 (Matrix.toList model.matrix)

makeField : Element
makeField =
  let
    (width, height) =
      (blockSize * columns, blockSize * rows)
  in
    collage width height <|
      [ outlined defaultLine (rect width height)
      ]

makeRow : List (Cell) -> Element
makeRow row =
  flow right <|
  List.map makeCell row

makeCell : Cell -> Element
makeCell cell =
  case cell of
    Square ->
      collage blockSize blockSize <|
        [ filled Color.red (square blockSize)
        , outlined defaultLine (square blockSize)
        ]

    Empty ->
      spacer blockSize blockSize

import Array exposing (Array)
import Collage exposing (..)
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Grid exposing (Grid, set)
import Html exposing (..)
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

(gridRows, gridColumns) = (22, 10)

type alias Model =
  { windowSize : (Int, Int)
  , grid : Grid Tetromino
  , activeTetromino : Tetromino
  , activeSquares : List (Int, Int)
  }

type Tetromino
  = I
  | O
  | T
  | J
  | L
  | S
  | Z

model : Model
model =
  { windowSize = (0, 0)
  , grid = Grid.empty
  , activeTetromino = I
  , activeSquares = []
  }

init : Model -> Model
init model =
  initSquares model
    |> initGrid

initSquares : Model -> Model
initSquares model =
  let
    updateSquares squares =
      { model | activeSquares = squares }
  in
    case model.activeTetromino of
      I ->
        updateSquares [(0, 4), (1, 4), (2, 4), (3, 4)]

      O ->
        updateSquares [(0, 4), (0, 5), (1, 4), (1, 5)]

      T ->
        updateSquares [(0, 4), (1, 4), (1, 3), (1, 5)]

      J ->
        updateSquares [(0, 4), (1, 4), (2, 4), (2, 3)]

      L ->
        updateSquares [(0, 4), (1, 4), (2, 4), (2, 5)]

      S ->
        updateSquares [(0, 4), (0, 5), (1, 3), (1, 4)]

      Z ->
        updateSquares [(0, 4), (0, 5), (1, 5), (1, 6)]

initGrid : Model -> Model
initGrid model =
  let
    setGrid squares grid =
      case squares of
        [] ->
          grid

        square :: rest ->
          uncurry Grid.set square model.activeTetromino grid
            |> setGrid rest
  in
    { model | grid = setGrid model.activeSquares model.grid }


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
  model


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
  layers (makeField :: [makeGrid model.grid])

makeField : Element
makeField =
  let
    (width, height) =
      (blockSize * gridColumns, blockSize * gridRows)
  in
    collage width height <|
      [ outlined defaultLine (rect width height)
      ]

makeGrid : Grid Tetromino -> Element
makeGrid grid =
  let
    makeElement ((row, column), square) =
      makeSquare square
        |> moveSquare row column
  in
    Grid.toIndexedList grid
      |> List.map makeElement
      |> layers

moveSquare : Int -> Int -> Element -> Element
moveSquare row column square =
  let
    (width, height) =
      (blockSize * gridColumns, blockSize * gridRows)

    position =
      topLeftAt (absolute (column * blockSize)) (absolute (row * blockSize))
  in
    container width height position square

makeSquare : Tetromino -> Element
makeSquare square =
  let
    squareCollage color =
      collage blockSize blockSize <|
        [ filled color (Collage.square blockSize)
        , outlined defaultLine (Collage.square blockSize)
        ]
  in
    case square of
      I ->
        squareCollage Color.blue

      O ->
        squareCollage Color.red

      T ->
        squareCollage Color.green

      J ->
        squareCollage Color.yellow

      L ->
        squareCollage Color.orange

      S ->
        squareCollage Color.purple

      Z ->
        squareCollage Color.brown


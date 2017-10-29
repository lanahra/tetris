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
  , grid : Grid Square
  }

type Square
  = ISquare
  | OSquare
  | TSquare
  | JSquare
  | LSquare
  | SSquare
  | ZSquare

model : Model
model =
  { windowSize = (0, 0)
  , grid = Grid.empty
  }

init : Model -> Model
init model =
  model


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

makeGrid : Grid Square -> Element
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

makeSquare : Square -> Element
makeSquare square =
  let
    squareCollage color =
      collage blockSize blockSize <|
        [ filled color (Collage.square blockSize)
        , outlined defaultLine (Collage.square blockSize)
        ]
  in
    case square of
      ISquare ->
        squareCollage Color.blue

      OSquare ->
        squareCollage Color.red

      TSquare ->
        squareCollage Color.green

      JSquare ->
        squareCollage Color.yellow

      LSquare ->
        squareCollage Color.orange

      SSquare ->
        squareCollage Color.purple

      ZSquare ->
        squareCollage Color.brown


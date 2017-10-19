import Array exposing (Array)
import Collage exposing (..)
import Color
import Element exposing (..)
import Html exposing (..)
import Task
import Window

main =
  Html.program
    { init = (model, initialSizeCmd)
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
  , grid : Grid
  }

type alias Grid =
  Array (Array Cell)

type Cell
  = Empty
  | Block

(columns, rows) = (10, 22)

model : Model
model =
  { windowSize = (0, 0)
  , grid = Array.repeat rows (Array.repeat columns Empty)
  }


-- UPDATE

type Msg
  = WindowResize Window.Size

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowResize size ->
      ({ model | windowSize = (size.width, size.height) }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Window.resizes (\size -> WindowResize size)


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
  List.drop 2 (gridToList model.grid)

makeField : Element
makeField =
  let
    (width, height) =
      (blockSize * columns, blockSize * rows)
  in
    collage width height <|
      [ outlined defaultLine (rect width height)
      ]

gridToList : Array (Array Cell) -> List (List Cell)
gridToList grid =
  Array.map (\row -> Array.toList row) grid
    |> Array.toList

makeRow : List (Cell) -> Element
makeRow row =
  flow right <|
  List.map makeCell row

makeCell : Cell -> Element
makeCell cell =
  case cell of
    Block ->
      collage blockSize blockSize <|
        [ filled Color.red (square blockSize)
        , outlined defaultLine (square blockSize)
        ]

    Empty ->
      spacer blockSize blockSize

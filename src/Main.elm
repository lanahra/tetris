import Array exposing (Array)
import Collage exposing (..)
import Color
import Element exposing (..)
import Grid exposing (Grid)
import Html exposing (..)
import Random exposing (Generator)
import Set
import Task
import Time exposing (Time)
import Window


main =
  Html.program
    { init = (init model, initCmd)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


initCmd : Cmd Msg
initCmd =
  Cmd.batch
    [ Task.perform WindowResize Window.size
    , initRollTetromino
    ]


-- MODEL

(gridRows, gridColumns) = (22, 10)


type alias Model =
  { windowSize : (Int, Int)
  , gameState : GameState
  , grid : Grid Tetromino
  , activeTetromino : Tetromino
  , nextTetromino : Tetromino
  , activeSquares : List (Int, Int)
  }


type GameState
  = Running
  | Over


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
  , gameState = Running
  , grid = Grid.empty gridRows
  , activeTetromino = I
  , nextTetromino = I
  , activeSquares = []
  }


init : Model -> Model
init model =
  model


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
          uncurry Grid.insert square model.activeTetromino grid
            |> setGrid rest
  in
    { model | grid = setGrid model.activeSquares model.grid }


-- UPDATE

type Msg
  = WindowResize Window.Size
  | Step Time
  | RollTetromino
  | NewTetromino Tetromino
  | InitTetromino (Tetromino, Tetromino)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowResize size ->
      ({ model | windowSize = (size.width, size.height) }, Cmd.none)

    Step _ ->
      step model

    RollTetromino ->
      (model, rollTetromino)

    NewTetromino tetromino ->
      (newTetromino tetromino model, Cmd.none)

    InitTetromino tetrominos ->
      (initTetromino tetrominos model, Cmd.none)


step : Model -> (Model, Cmd Msg)
step model =
  case model.gameState of
    Running ->
      if tetrominoCanDrop model.grid model.activeSquares then
        (dropTetromino model, Cmd.none)
      else
        setGameState model

    Over ->
      (model, Cmd.none)


dropTetromino : Model -> Model
dropTetromino model =
  let
    remove squares grid =
      case squares of
        [] ->
          grid

        square :: rest ->
          uncurry Grid.remove square grid
            |> remove rest

    insert squares grid =
      case squares of
        [] ->
          grid

        square :: rest ->
          uncurry Grid.insert square model.activeTetromino grid
            |> insert rest

    dropSquares =
      List.map (\(row, column) -> (row + 1, column)) model.activeSquares

    drop =
      remove model.activeSquares model.grid
        |> insert dropSquares
  in
    { model
        | grid = drop
        , activeSquares = dropSquares
    }


tetrominoCanDrop : Grid Tetromino -> List (Int, Int) -> Bool
tetrominoCanDrop grid squares =
  let
    canDrop drops =
      case drops of
        [] ->
          True

        (row, column) :: rest ->
          let
            squareCanDrop =
              Grid.memberRow row grid
              && (Grid.member row column >> not) grid
          in
            if squareCanDrop then
              canDrop rest
            else
              False
  in
    List.map (\(row, column) -> (row + 1, column)) squares
      |> Set.fromList
      |> flip Set.diff (Set.fromList squares)
      |> Set.toList
      |> canDrop


setGameState : Model -> (Model, Cmd Msg)
setGameState model =
  if gameIsOver model.grid then
    ({ model | gameState = Over }, Cmd.none)
  else
    (model, rollTetromino)


gameIsOver : Grid Tetromino -> Bool
gameIsOver grid =
  Grid.sizeRow 0 grid + Grid.sizeRow 1 grid > 0


newTetromino : Tetromino -> Model -> Model
newTetromino tetromino model =
  { model
      | activeTetromino = model.nextTetromino
      , nextTetromino = tetromino
  }
    |> initSquares
    |> initGrid


initTetromino : (Tetromino, Tetromino) -> Model -> Model
initTetromino init model =
  case init of
    (active, next) ->
      { model
          | activeTetromino = active
          , nextTetromino = next
      }
        |> initSquares
        |> initGrid


-- COMMANDS

randomTetromino : Generator Tetromino
randomTetromino =
  let
    intToTetromino i =
      case i of
        0 ->
          I

        1 ->
          O

        2 ->
          T

        3 ->
          J

        4 ->
          L

        5 ->
          S

        _ ->
          Z
  in
    Random.map intToTetromino (Random.int 0 6)


rollTetromino : Cmd Msg
rollTetromino =
  Random.generate NewTetromino randomTetromino


initRollTetromino : Cmd Msg
initRollTetromino =
  Random.generate InitTetromino <|
    Random.pair randomTetromino randomTetromino


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

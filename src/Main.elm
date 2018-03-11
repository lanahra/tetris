import Array exposing (Array)
import Collage exposing (..)
import Color
import Element exposing (..)
import Field exposing (Field)
import Html exposing (..)
import Keyboard exposing (KeyCode)
import Random exposing (Generator)
import Set exposing (Set)
import Task
import Tetromino exposing (Tetromino, Shape(..))
import Time exposing (Time)
import Window


main =
  Html.program
    { init = model ! [ cmds ]
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- COMMANDS

cmds : Cmd Msg
cmds =
  Cmd.batch
    [ Task.perform WindowResize Window.size
    , rollBag
    ]


rollBag : Cmd Msg
rollBag =
  Random.generate Bag Tetromino.bag


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\size -> WindowResize size)
    , Time.every (200 * Time.millisecond) Step
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]


-- MODEL


type alias Model =
  { windowSize : (Int, Int)
  , pressed : Set KeyCode
  , state : State
  , field : Field
  , tetromino : Tetromino
  , bag : List Tetromino.Shape
  }


type State
  = Running
  | Over


model : Model
model =
  { windowSize = (0, 0)
  , pressed = Set.empty
  , state = Running
  , field = Field.empty
  , tetromino = Tetromino.init I
  , bag = []
  }


-- UPDATE

type Msg
  = WindowResize Window.Size
  | Step Time
  | RollBag
  | Bag (List Tetromino.Shape)
  | KeyDown KeyCode
  | KeyUp KeyCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowResize size ->
      { model | windowSize = (size.width, size.height) }
        ! []

    Step _ ->
      let
        drop =
          Tetromino.drop model.tetromino

        field =
          Field.take model.tetromino model.field
            |> Field.insert drop

        over positions =
          case positions of
            [] ->
              False

            ((x, y), s) :: rest ->
              if y >= 20 then
                True
              else
                over rest
      in
        if occupied (Tetromino.diff drop model.tetromino) model.field then
          if over (Field.toIndexedList model.field) then
            { model | state = Over }
              ! []
          else
            case model.bag of
              [] ->
                model
                  ! [ rollBag ]

              shape :: rest ->
                let
                  tetromino = Tetromino.init shape
                in
                  { model
                      | field = Field.insert tetromino model.field
                      , tetromino = tetromino
                      , bag = rest
                  }
                    ! []
        else
          { model
              | field = field
              , tetromino = drop
          }
            ! []

    RollBag ->
      model
        ! [ rollBag ]

    Bag bag ->
      case bag of
        [] ->
          model ! []

        shape :: rest ->
          let
            tetromino = Tetromino.init shape
          in
            { model
                | field = Field.insert tetromino model.field
                , tetromino = tetromino
                , bag = rest
            }
              ! []

    KeyDown code ->
      input code model
        ! []

    KeyUp code ->
      { model | pressed = Set.remove code model.pressed }
        ! []


occupied : List Tetromino.Position -> Field -> Bool
occupied positions field =
  let
    f positions =
      case positions of
        [] ->
          False

        (x, y) :: rest ->
          if Field.occupied x y field then
            True
          else
            f rest
  in
    f positions


input : KeyCode -> Model -> Model
input code model =
  if Set.member code model.pressed then
    model
  else
    case model.state of
      Running ->
        let
          move =
            case code of
              68 -> -- a
                Tetromino.rotateLeft model.tetromino

              65 -> -- d
                Tetromino.rotateRight model.tetromino

              39 -> -- right arrow
                Tetromino.moveRight model.tetromino

              37 -> -- left arrow
                Tetromino.moveLeft model.tetromino

              _ ->
                model.tetromino

          field =
            Field.take model.tetromino model.field
              |> Field.insert move

        in
          if not <| occupied (Tetromino.diff move model.tetromino) model.field then
            { model
                | pressed = Set.insert code model.pressed
                , field = field
                , tetromino = move
            }
          else
            { model | pressed = Set.insert code model.pressed }

      Over ->
        model


-- VIEW

blockSize = 40


view : Model -> Html Msg
view model =
  toHtml <|
  uncurry container model.windowSize middle <|
  layers (drawOutline :: [ drawField model.field ])


drawOutline : Element
drawOutline =
  let
    (width, height) =
      (blockSize * Field.columns, blockSize * Field.rows)
  in
    collage width height <|
      [ outlined defaultLine (rect width height)
      ]


drawField : Field -> Element
drawField field =
  let
    drawElement ((x, y), shape) =
      drawShape shape
        |> transformShape x y
  in
    Field.toIndexedList field
      |> List.map drawElement
      |> layers


transformShape : Int -> Int -> Element -> Element
transformShape x y shape =
  let
    (width, height) =
      (blockSize * Field.columns, blockSize * Field.rows)

    position =
      bottomLeftAt (absolute (x * blockSize)) (absolute (y * blockSize))
  in
    container width height position shape


drawShape : Tetromino.Shape -> Element
drawShape shape =
  let
    square color =
      collage blockSize blockSize <|
        [ filled color (Collage.square blockSize)
        , outlined defaultLine (Collage.square blockSize)
        ]
  in
    case shape of
      I ->
        square Color.blue

      O ->
        square Color.red

      T ->
        square Color.green

      J ->
        square Color.yellow

      L ->
        square Color.orange

      S ->
        square Color.purple

      Z ->
        square Color.brown

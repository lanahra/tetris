module Tetromino exposing
  ( Tetromino
  , Shape(..)
  , Position
  , init
  , shape
  , position
  , diff
  , bag
  , transform
  , drop
  , moveLeft
  , moveRight
  , rotateLeft
  , rotateRight
  )


import Random exposing (Generator)
import Random.List
import Set


type Tetromino =
  Tetromino
    { shape : Shape
    , blocks : List Position
    , position : Position
    }


type Shape
  = I
  | O
  | T
  | J
  | L
  | S
  | Z


type alias Position =
  (Int, Int)


init : Shape -> Tetromino
init shape =
  case shape of
    I ->
      Tetromino
        { shape = I
        , blocks = [ (-1, 0), (0, 0), (1, 0), (2, 0) ]
        , position = (4, 20)
        }

    O ->
      Tetromino
        { shape = O
        , blocks = [ (0, 1), (1, 1), (0, 0), (1, 0) ]
        , position = (4, 20)
        }

    T ->
      Tetromino
        { shape = T
        , blocks = [ (0, 1), (-1, 0), (0, 0), (1, 0) ]
        , position = (4, 20)
        }

    J ->
      Tetromino
        { shape = J
        , blocks = [ (-1, 1), (-1, 0), (0, 0), (1, 0) ]
        , position = (4, 20)
        }

    L ->
      Tetromino
        { shape = L
        , blocks = [ (1, 1), (-1, 0), (0, 0), (1, 0) ]
        , position = (4, 20)
        }

    S ->
      Tetromino
        { shape = S
        , blocks = [ (0, 1), (1, 1), (-1, 0), (0, 0) ]
        , position = (4, 20)
        }

    Z ->
      Tetromino
        { shape = Z
        , blocks = [ (-1, 1), (0, 1), (0, 0), (1, 0) ]
        , position = (4, 20)
        }


shape : Tetromino -> Shape
shape tetromino =
  case tetromino of
    Tetromino t ->
      t.shape


position : Tetromino -> List Position
position tetromino =
  let
    position (x, y) (z, w) =
      (x + z, y + w)
  in
    case tetromino of
      Tetromino t ->
        List.map (position t.position) t.blocks


diff : Tetromino -> Tetromino -> List Position
diff a b =
  let
    positions tetromino =
      position tetromino
        |> Set.fromList
  in
    Set.diff (positions a) (positions b)
      |> Set.toList


bag : Generator (List Shape)
bag =
  let
    shapes =
      [ I, O, T, J, L, S, Z ]
  in
    Random.List.shuffle shapes


transform : (Position -> Position) -> (Position -> Position) -> Tetromino -> Tetromino
transform f g tetromino =
  case tetromino of
    Tetromino t ->
      Tetromino
        { t
            | blocks = List.map f t.blocks
            , position = g t.position
        }


drop : Tetromino -> Tetromino
drop tetromino =
  transform (\x -> x) (\(x, y) -> (x, y - 1)) tetromino


moveLeft : Tetromino -> Tetromino
moveLeft tetromino =
  transform (\x -> x) (\(x, y) -> (x - 1, y)) tetromino


moveRight : Tetromino -> Tetromino
moveRight tetromino =
  transform (\x -> x) (\(x, y) -> (x + 1, y)) tetromino


type Rotation
  = Clockwise
  | CounterClockwise


rotate : Rotation -> Tetromino -> Tetromino
rotate rotation tetromino =
  let
    f (x, y) =
      case rotation of
        Clockwise ->
          (negate y, x)

        CounterClockwise ->
          (y, negate x)

    rotate (px, py) (x, y) =
      (toFloat x, toFloat y)
        |> \(x, y) -> ((x - px), (y - py))
        |> f
        |> \(x, y) -> (truncate (x + px), truncate (y + py))
  in
    case tetromino of
      Tetromino t ->
        case t.shape of
          O ->
            tetromino

          I ->
            transform (rotate (0.5, 0.5)) (\x -> x) tetromino

          _ ->
            transform (rotate (0.0, 0.0)) (\x -> x) tetromino


rotateLeft : Tetromino -> Tetromino
rotateLeft tetromino =
  rotate CounterClockwise tetromino


rotateRight : Tetromino -> Tetromino
rotateRight tetromino =
  rotate Clockwise tetromino

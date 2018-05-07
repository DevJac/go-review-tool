module Board (
    Board
  , PlayerColor(Black, White)
  , newBoard
  , update
  , get
  , boardSize
) where
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

data PlayerColor = Black | White deriving (Show, Eq)

data Board = Board Int (V.Vector (Maybe PlayerColor)) deriving (Eq)

newBoard :: Int -> Board
newBoard size = Board size (V.replicate (size * size) Nothing)

update :: Int -> Int -> Maybe PlayerColor -> Board -> Board
update x y color b@(Board size v)
    | x >= size || y >= size = b
    | x < 0 || y < 0         = b
    | otherwise              =
        Board size (v V.// [(x * size + y, color)])


get :: Int -> Int -> Board -> Maybe PlayerColor
get x y (Board size v) = fromMaybe Nothing $ v V.!? (x * size + y)

boardSize :: Board -> Int
boardSize (Board size _) = size

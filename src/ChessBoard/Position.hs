-- | These positions restrict moves and queries to only valid positions, which
-- helps to keep the interface cleaner. Coordinates can be used but it is up to
-- the client to create a position out of them via coordToPos before they can
-- modify the board.
module ChessBoard.Position (
  Position(Position)
  , Coord
  , X(X1 , X2 , X3 , X4 , X5 , X6 , X7 , X8)
  , Y(YA , YB , YC , YD , YE , YF , YG , YH)
  , posToCoord
  , coordToPos
) where  

import Control.Applicative ((<$>),(<*>))

-- | Combines an X coordinate and a Y coordinate.
data Position  = Position X Y deriving (Eq,Show)

-- | Horizontal (X) Coordinates are labelled 1 to 8
data X = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 deriving (Eq,Show)

-- | Vertical (Y) Coordinates are labelled A to H
data Y = YA | YB | YC | YD | YE | YF | YG | YH deriving (Eq,Show)

-- | Coordinates are 0 based. (e.g. (X1,YA) -> (0,0) and (X8,YH) -> (7,7) )
type Coord = (Int,Int)

-- | Converts a position to a Coordinate for easier manipulation. 
posToCoord :: Position -> Coord
posToCoord (Position x y) = ( (xCoord x), (yCoord y) )
  where
    xCoord X1 = 0
    xCoord X2 = 1
    xCoord X3 = 2
    xCoord X4 = 3
    xCoord X5 = 4
    xCoord X6 = 5
    xCoord X7 = 6
    xCoord X8 = 7
    yCoord YA = 0 
    yCoord YB = 1
    yCoord YC = 2
    yCoord YD = 3
    yCoord YE = 4
    yCoord YF = 5
    yCoord YG = 6
    yCoord YH = 7    

-- | Converts a coordinate to a position. This will return nothing unless x and
-- y are both between 0 and 7.    
coordToPos :: Coord -> Maybe Position
coordToPos (x,y) = Position <$> (xPos x) <*> (yPos y)
  where
    xPos 0 = Just X1
    xPos 1 = Just X2
    xPos 2 = Just X3
    xPos 3 = Just X4
    xPos 4 = Just X5
    xPos 5 = Just X6
    xPos 6 = Just X7
    xPos 7 = Just X8
    xPos _ = Nothing
    yPos 0 = Just YA
    yPos 1 = Just YB
    yPos 2 = Just YC
    yPos 3 = Just YD
    yPos 4 = Just YE
    yPos 5 = Just YF
    yPos 6 = Just YG
    yPos 7 = Just YH
    yPos _ = Nothing

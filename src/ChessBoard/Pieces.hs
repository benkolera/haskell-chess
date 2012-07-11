module ChessBoard.Pieces (
  Player (White,Black)
  , Piece (King,Queen,Bishop,Knight,Rook,Pawn)
  , PlayerPiece (PlayerPiece)
) where

-- | The types of player on the chess board.
data Player = White | Black deriving (Eq)

-- | All of the pieces that are on the board
data Piece = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq)

-- | Pieces on the chess board have both a player and a type
data PlayerPiece = PlayerPiece Player Piece deriving (Eq)

-- | Prints either "w" or "b"
instance Show Player where
  show White  = "w"
  show Black  = "b"

-- | Prints "k" , "q" , "b" , "n" , "r" or "p"
instance Show Piece where
  show King   = "k"
  show Queen  = "q"
  show Bishop = "b"
  show Knight = "n"
  show Rook   = "r"
  show Pawn   = "p"

-- | Combines piece ++ player. (e.g. "pb" for pawn black)
instance Show PlayerPiece where
  show (PlayerPiece player piece) = (show piece) ++ (show player)

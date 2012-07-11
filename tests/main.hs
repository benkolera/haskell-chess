module Main where

import ChessBoard
import ChessBoard.Position
import ChessBoard.Pieces

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe,isNothing,isJust)

import Test.HUnit
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Data.Either.Unwrap (fromRight)


{-
-------------------------------------------------------------------------------
-- CORE TEST GOALS
--------------------------------------------------------------------------------
Test this required functionality:
 * Move a piece from one position to another. If a piece already exists at the
   destination position it is removed first.
 * Remove a piece that is at a specified position on the board.
 * Reset the position of pieces. The board is reset ready for the start of a
   new chess match.
 * Return the type of piece that’s at a specified position on the board. Note
   that there needs to be a way of reporting that the specified position is
   empty.
--------------------------------------------------------------------------------
-}

main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Position" [
     testProperty "Position2Coord Inverse" prop_position_inverse
     , testProperty "CoordToPosition Partial" prop_position_coord_to_pos
     ]
  , testGroup "Board" [
     testCase "BoardInitShow" test_board_init_show
     , testCase "BoardPieceAtLightRook" test_board_init_pieceAt_light_rook
     , testCase "BoardPieceAtNothing" test_board_init_pieceAt_nothing
     , testCase "BoardPieceAtDarkKing" test_board_init_pieceAt_dark_king
     , testCase "BoardMoveNothing" test_board_move_nothing
     , testCase "BoardMoveSamePosition" test_board_move_start_end_the_same
     , testCase "BoardMoveCaptureOwnPiece" test_board_move_capture_own_piece
     , testCase "BoardMoveNoCapture" test_board_move_no_capture
     , testCase "BoardMoveCapture" test_board_move_capture
     , testCase "BoardMoveRemoveNothing" test_board_remove_nothing
     , testCase "BoardMoveRemoveSomething" test_board_remove_something
     , testCase "BoardMoveSameRow" test_board_move_same_row
    ]
  ]

{-
--------------------------------------------------------------------------------
-- Position Tests
--------------------------------------------------------------------------------
Test that posToCoord and coordToPos are inverses of each other and that the
coordToPos handles erroneous coords fine.
--------------------------------------------------------------------------------
-}

-- Make sure going from a coordToPos is an inverse of posToCoord
prop_position_inverse s = (coordToPos (posToCoord s)) == (Just s)

-- Make sure any coord that is not valid comes out as nothing.
prop_position_coord_to_pos c@(x,y) = p
  where
    p | (validC x) && (validC y) = fromMaybe False $ fmap validP $ coordToPos c
      | otherwise                = isNothing $ coordToPos c
    validP   = (c ==) . posToCoord
    validC x = x >= 0 && x < 8

{-
--------------------------------------------------------------------------------
-- initBoard / show Board tests
--------------------------------------------------------------------------------
Test that our board initialised properly to a standard chess staring setup
and that in prints out to string properly    

Initial Board definition as per :
  http://en.wikipedia.org/wiki/Template_talk:Chess_position
Main difference is that wiki calls the players Light and Dark. Requirements
for this problem calls them Black and White, so l -> w and d -> b
   
8 |rd|nd|bd|qd|kd|bd|nd|rd|= 8
7 |pd|pd|pd|pd|pd|pd|pd|pd|= 7
6 |  |  |  |  |  |  |  |  |= 6
5 |  |  |  |  |  |  |  |  |= 5
4 |  |  |  |  |  |  |  |  |= 4
3 |  |  |  |  |  |  |  |  |= 3
2 |pl|pl|pl|pl|pl|pl|pl|pl|= 2
1 |rl|nl|bl|ql|kl|bl|nl|rl|= 1
   a  b  c  d  e  f  g  h
-}
test_board_init_show = (show initBoard) @?= expectedStr
  where
    expectedStr = unlines [
      "  -----------------------------------------",
      "8 | rb | nb | bb | qb | kb | bb | nb | rb |",
      "7 | pb | pb | pb | pb | pb | pb | pb | pb |",
      "6 |    |    |    |    |    |    |    |    |",
      "5 |    |    |    |    |    |    |    |    |",
      "4 |    |    |    |    |    |    |    |    |",
      "3 |    |    |    |    |    |    |    |    |",
      "2 | pw | pw | pw | pw | pw | pw | pw | pw |",
      "1 | rw | nw | bw | qw | kw | bw | nw | rw |",
      "  -----------------------------------------",
      "  |  A |  B |  C |  D |  E |  F |  G |  H |"
      ]

{-
--------------------------------------------------------------------------------
-- PieceAt Tests
--------------------------------------------------------------------------------
Test that we can get pieces from the known starting points, including testing
the case where there is no piece at that point.    
--------------------------------------------------------------------------------    
-}

testInitPieceAt pos expectedPP = pieceAt initBoard pos  @?= expectedPP

test_board_init_pieceAt_light_rook = testInitPieceAt (Position X1 YA) rw
test_board_init_pieceAt_nothing    = testInitPieceAt (Position X4 YC) Nothing
test_board_init_pieceAt_dark_king  = testInitPieceAt (Position X8 YE) kb

{-
--------------------------------------------------------------------------------
-- moveTests
--------------------------------------------------------------------------------
Test our error conditions on our moves:
 * Moving from a position that doesn't have a piece is an error
 * Moving to the same spot as the start is an error
 * Moving to a spot that would capture a piece of the same team is an error
Otherwise, test the following success cases:
 * Capturing no piece
 * Capturing a piece
 * Moving on the same horizonal line (white box test. Moving on the same line
   only updates one inner vector rather than two )
--------------------------------------------------------------------------------
-}

testMoveErr board p1 p2 expectedMsg = move board p1 p2 @?= (Left expectedMsg)
test_board_move_nothing =
  testMoveErr board (Position X4 YC) (Position X5 YC) expectedMsg
  where
    board       = initBoard
    expectedMsg = "No piece at starting position!"

test_board_move_start_end_the_same =
  testMoveErr board (Position X1 YA) (Position X1 YA) expectedMsg
  where
    board       = initBoard
    expectedMsg = "Must move to a different position!"

test_board_move_capture_own_piece =
  testMoveErr board (Position X1 YA) (Position X2 YA) expectedMsg
  where
    board       = initBoard
    expectedMsg = "You probably don't want to capture your own piece!"

testMoveGood board p1 p2 expectedBoardList expectedCapture = 
  case move board p1 p2 of
    (Left msg)              -> assertFailure ("Got error: " ++ msg)
    (Right (board,capture)) -> do
      (boardToList board) @?= expectedBoardList
  
test_board_move_no_capture =
  testMoveGood board (Position X2 YE) (Position X4 YE) expectedBoard Nothing
  where
    board         = initBoard
    expectedBoard = reverse [
      [rb,nb,bb,qb,kb,bb,nb,rb], -- 8
      [pb,pb,pb,pb,pb,pb,pb,pb], -- 7
      [__,__,__,__,__,__,__,__], -- 6
      [__,__,__,__,__,__,__,__], -- 5
      [__,__,__,__,pw,__,__,__], -- 4
      [__,__,__,__,__,__,__,__], -- 3
      [pw,pw,pw,pw,__,pw,pw,pw], -- 2
      [rw,nw,bw,qw,kw,bw,nw,rw]  -- 1
   --  A |B |C |D |E |F |G |H      
      ]

test_board_move_capture =
  testMoveGood board (Position X2 YE) (Position X8 YE) expectedBoard kb
  where
    board         = initBoard
    expectedBoard = reverse [
      [rb,nb,bb,qb,pw,bb,nb,rb], -- 8
      [pb,pb,pb,pb,pb,pb,pb,pb], -- 7
      [__,__,__,__,__,__,__,__], -- 6
      [__,__,__,__,__,__,__,__], -- 5
      [__,__,__,__,__,__,__,__], -- 4
      [__,__,__,__,__,__,__,__], -- 3
      [pw,pw,pw,pw,__,pw,pw,pw], -- 2
      [rw,nw,bw,qw,kw,bw,nw,rw]  -- 1
   --  A |B |C |D |E |F |G |H      
      ]

test_board_move_same_row =
  testMoveGood board (Position X4 YE) (Position X4 YH) expectedBoard Nothing
  where
    board         = fst $ fromRight $ move initBoard (Position X2 YE) (Position X4 YE)
    expectedBoard = reverse [
      [rb,nb,bb,qb,kb,bb,nb,rb], -- 8
      [pb,pb,pb,pb,pb,pb,pb,pb], -- 7
      [__,__,__,__,__,__,__,__], -- 6
      [__,__,__,__,__,__,__,__], -- 5
      [__,__,__,__,__,__,__,pw], -- 4
      [__,__,__,__,__,__,__,__], -- 3
      [pw,pw,pw,pw,__,pw,pw,pw], -- 2
      [rw,nw,bw,qw,kw,bw,nw,rw]  -- 1
   --  A |B |C |D |E |F |G |H      
      ]

{-
--------------------------------------------------------------------------------
-- remove Tests
--------------------------------------------------------------------------------
Test that removing a piece has the desired effect on the board.
Also, test that removing nothing does not error or break the board.
--------------------------------------------------------------------------------
-}

test_board_remove_nothing =
  (boardToList $ removePieceAt initBoard (Position X4 YE)) @?= expectedBoard
  where
    expectedBoard = reverse [
      [rb,nb,bb,qb,kb,bb,nb,rb], -- 8
      [pb,pb,pb,pb,pb,pb,pb,pb], -- 7
      [__,__,__,__,__,__,__,__], -- 6
      [__,__,__,__,__,__,__,__], -- 5
      [__,__,__,__,__,__,__,__], -- 4
      [__,__,__,__,__,__,__,__], -- 3
      [pw,pw,pw,pw,pw,pw,pw,pw], -- 2
      [rw,nw,bw,qw,kw,bw,nw,rw]  -- 1
   --  A |B |C |D |E |F |G |H      
      ]

test_board_remove_something =
  (boardToList $ removePieceAt initBoard (Position X8 YH)) @?= expectedBoard
  where
    expectedBoard = reverse [
      [rb,nb,bb,qb,kb,bb,nb,__], -- 8
      [pb,pb,pb,pb,pb,pb,pb,pb], -- 7
      [__,__,__,__,__,__,__,__], -- 6
      [__,__,__,__,__,__,__,__], -- 5
      [__,__,__,__,__,__,__,__], -- 4
      [__,__,__,__,__,__,__,__], -- 3
      [pw,pw,pw,pw,pw,pw,pw,pw], -- 2
      [rw,nw,bw,qw,kw,bw,nw,rw]  -- 1
   --  A |B |C |D |E |F |G |H      
      ]

{-
--------------------------------------------------------------------------------
-- Helper Stuff
--------------------------------------------------------------------------------
-}

-- Generator of arbitrary positions used for position testing.
instance Arbitrary Position where
  arbitrary = liftM2 Position xs ys
    where
      xs = elements $ concat $ replicate 8 [X1,X2,X3,X4,X5,X6,X7,X8]
      ys = elements $ concatMap (replicate 8) [YA,YB,YC,YD,YE,YF,YG,YH]

-- Easy shortcuts for making pieces
__ = Nothing
rw = Just $ PlayerPiece White Rook
nw = Just $ PlayerPiece White Knight
bw = Just $ PlayerPiece White Bishop
kw = Just $ PlayerPiece White King
qw = Just $ PlayerPiece White Queen
pw = Just $ PlayerPiece White Pawn

rb = Just $ PlayerPiece Black Rook
nb = Just $ PlayerPiece Black Knight
bb = Just $ PlayerPiece Black Bishop
kb = Just $ PlayerPiece Black King
qb = Just $ PlayerPiece Black Queen
pb = Just $ PlayerPiece Black Pawn



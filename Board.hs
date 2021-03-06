module Board where

import Data.Char (ord)
import Utils

-- **************** data types *******************

data Piece = Piece {pieceType::PieceType, pieceColor::PieceColor} deriving Eq
data PieceType = Rook | Knight | Bishop | King | Queen | Pawn deriving (Eq, Show)
data PieceColor = Black | White deriving (Eq, Show)

type Square = Maybe Piece

type Board = [[Square]]

type Pos = (Int, Int)

instance Show Piece where
    show (Piece t c) = show t ++ " " ++ show c

readSquare :: Char -> Maybe Piece
readSquare c | c=='k' = Just (Piece King Black)
            | c=='q' = Just (Piece Queen Black)
            | c=='b' = Just (Piece Bishop Black)
            | c=='r' = Just (Piece Rook Black)
            | c=='n' = Just (Piece Knight Black)
            | c=='p' = Just (Piece Pawn Black)
            | c=='K' = Just (Piece King White)
            | c=='Q' = Just (Piece Queen White)
            | c=='B' = Just (Piece Bishop White)
            | c=='R' = Just (Piece Rook White)
            | c=='N' = Just (Piece Knight White)
            | c=='P' = Just (Piece Pawn White)
            | otherwise = Nothing



-- **************** output functions *******************
prettyPrintBoard :: Board -> IO ()
prettyPrintBoard = putStr . prettyBoardIndent 10

prettyBoard :: Board -> [String]
prettyBoard  b = l ++ [interline,cordX]
    where   
        l = prependToAll interline (cordY b) 
        prependToAll            :: a -> [a] -> [a]
        prependToAll _   []     = []
        prependToAll sep (x:xs) = sep : x : prependToAll sep xs

prettyBoardIndent :: Int -> Board -> String
prettyBoardIndent x b =  unlines $ [indent x ++ l | l <- prettyBoard b]
    where   
        indent x = take x (repeat ' ')


prettySquare::Square->String
prettySquare Nothing = ".|"
prettySquare (Just (Piece King Black))      = "k|"
prettySquare (Just (Piece Queen Black))     = "q|"
prettySquare (Just (Piece Knight Black))    = "n|"
prettySquare (Just (Piece Bishop Black))    = "b|"
prettySquare (Just (Piece Rook Black))      = "r|"
prettySquare (Just (Piece Pawn Black))      = "p|"
prettySquare (Just (Piece King White))      = "K|"
prettySquare (Just (Piece Queen White))     = "Q|"
prettySquare (Just (Piece Knight White))    = "N|"
prettySquare (Just (Piece Bishop White))    = "B|"
prettySquare (Just (Piece Rook White))      = "R|"
prettySquare (Just (Piece Pawn White))      = "P|"
interline = " +-+-+-+-+-+-+-+-+"
cordX     = "  a b c d e f g h "
cordY b = zipWith (++) (map show [8,7..1]) ligne
    where 
        ligne = map prettyLine b 
        prettyLine l = "|" ++ (concatMap prettySquare l)
 
-- **************** auxiliary board functions *******************

oppositeColor::PieceColor->PieceColor
oppositeColor White = Black
oppositeColor Black = White

isEmpty::Board->Pos->Bool
isEmpty board pos = Nothing == getSquare board pos

emptySquare::Square
emptySquare = Nothing

getSquare::Board->Pos->Square
getSquare board (a, b) = board!!a!!b

updateBoard::Pos->Square->Board->Board
updateBoard = updateMatrix

deleteSquare::Pos->Board->Board
deleteSquare p = updateBoard p emptySquare

-- moves the piece at p1 to p2
movePos::Pos->Pos->Board->Board
movePos p1 p2 b = updateBoard p2 (getSquare b p1) (deleteSquare p1 b)

move::String->String->Board->Board
move p1 p2 = movePos (toPos p1) (toPos p2)

-- computes the internal representation of "a1:h8"
toPos::String->Pos
toPos [x, y] = (7 - (ord y - ord '1'), ord x - ord 'a')

outside,inside::Pos->Bool
outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7

inside = not . outside

colorPos::PieceColor->Board->[Pos]
colorPos f board = [(a, b)|a<-[0..7],b<-[0..7], hasColor f (getSquare board (a,b))]

hasColor::PieceColor->Square->Bool
hasColor _ Nothing = False
hasColor f1 (Just (Piece a f2)) = f1 == f2

-- **************** some boards *******************

initialBoard, emptyBoard::Board
initialBoard = [[Just (Piece Rook Black), Just (Piece Knight Black), Just (Piece Bishop Black), Just (Piece Queen Black), Just (Piece King Black), Just (Piece Bishop Black), Just (Piece Knight Black), Just (Piece Rook Black)],
                [Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black), Just (Piece Pawn Black)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White), Just (Piece Pawn White)],
                [Just (Piece Rook White), Just (Piece Knight White), Just (Piece Bishop White), Just (Piece Queen White), Just (Piece King White), Just (Piece Bishop White), Just (Piece Knight White), Just (Piece Rook White)]]

emptyBoard = [[Nothing|_<-[1..8]]|_<-[1..8]]


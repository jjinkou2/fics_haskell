import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Board

data Observer =   WATCHING_ISOLATED_POSITION
                | WATCHING_EXAMINE
                | PLAYING_WITH_OPP_TO_MOVE
                | WATCHING_GAME
                | PLAYING_AND_TO_MOVE
                | EXAMINING
    deriving (Show,Eq)

data BoardFlip = WhiteDown | BlackDown deriving (Show,Enum)

data GameState = 
    GS { board :: Board
       , side_to_move :: String
       , double_push :: Int
       , can_white_castle_short :: Bool
       , can_white_castle_long  :: Bool
       , can_black_castle_short :: Bool
       , can_black_castle_long :: Bool
       , reversible_plies_count :: Int
       , game_no :: Int
       , white :: String
       , black :: String
       , observer_role :: Observer
       , clock :: GameClock
       , white_material :: Int
       , black_material :: Int
       , white_remaining_time :: Int
       , black_remaining_time :: Int
       , next_move_no :: Int
       , last_move_coord_text :: String
       , last_move_time_spent :: GameClock
       , last_move_text :: String
       , board_orientation :: BoardFlip
       , is_clock_ticking :: Bool
       , last_move_lag :: Int
    } deriving (Show)

toGameState style12Str = 
    GS { board = toBoard 
        , side_to_move = fields !! 8
        , double_push = read.(!!9)$fields
        , can_white_castle_short = toEnum.read.(!!10)$fields
        , can_white_castle_long = toEnum.read.(!!11)$fields
        , can_black_castle_short = toEnum.read.(!!12)$fields
        , can_black_castle_long = toEnum.read.(!!13)$fields
        , reversible_plies_count = read.(!!14)$fields
        , game_no = read.(!!15)$fields
        , white = fields!!16
        , black = fields!!17
        , observer_role = toEnum.read.(!!18)$fields
        , clock = GameClock 0 (read(fields!!19)) (read (fields!!20))
        , white_material = read.(!!21)$fields
        , black_material = read.(!!22)$fields
        , white_remaining_time = read.(!!23)$fields
        , black_remaining_time = read.(!!24)$fields
        , next_move_no = read.(!!25)$fields
        , last_move_coord_text = fields!!26
        , last_move_time_spent = toGameClock $ fields!!27
        , last_move_text = fields!!28
        , board_orientation = toEnum.read.(!!29)$fields
        , is_clock_ticking = toEnum.read.(!!30)$fields
        , last_move_lag = read (fields!!31) 
        }
        where
            fields = drop 1.splitOn " " $ style12Str
            toBoard = map (map readSquare). take 8 $ fields
            toGameClock str =  case (splitOn ":".trimParenthesis$str ) of
                [x,y,z] -> GameClock (read x) (read y) (read z)
                [x,y]   -> GameClock 0 (read x) (read y)
            trimParenthesis str = drop 1.take (length str -1) $ str

swap (x,y) = (y,x)

instance Enum Observer where
    fromEnum = fromJust . flip lookup table
    toEnum   = fromJust . flip lookup (map swap table)

table = [(WATCHING_ISOLATED_POSITION, -3),(WATCHING_EXAMINE,-2)
        ,(PLAYING_WITH_OPP_TO_MOVE,-1),(WATCHING_GAME,0)
        ,(PLAYING_AND_TO_MOVE,1),(EXAMINING, 2)]


data GameClock = GameClock { gameHr :: Int, gameMin :: Int , gameSec :: Int} deriving (Show)


------------------
--Test
--
------------------
ficsstr="<12> -----r-- --r-p-kp ----Qnp- ----p--- -------- -----PP- P-q---BP ---R-R-K B -1 0 1 0 0 0 164 CamyC android 2 3 0 26 26 112 107 24 Q/a6-e6 (1:12:55) Qxe6 0 1 215"

gs = toGameState ficsstr

main = prettyPrintBoard . board $ gs

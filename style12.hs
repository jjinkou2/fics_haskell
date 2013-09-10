import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Observer =   WATCHING_ISOLATED_POSITION
                | WATCHING_EXAMINE
                | PLAYING_WITH_OPP_TO_MOVE
                | WATCHING_GAME
                | PLAYING_AND_TO_MOVE
                | EXAMINING
    deriving (Show,Eq)

data BoardFlip = WhiteDown | BlackDown deriving (Show,Enum)

swap (x,y) = (y,x)

instance Enum Observer where
    fromEnum = fromJust . flip lookup table
    toEnum   = fromJust . flip lookup (map swap table)

table = [(WATCHING_ISOLATED_POSITION, -3),(WATCHING_EXAMINE,-2)
        ,(PLAYING_WITH_OPP_TO_MOVE,-1),(WATCHING_GAME,0)
        ,(PLAYING_AND_TO_MOVE,1),(EXAMINING, 2)]


data GameClock = GameClock { gameMin :: Int , gameSec :: Int} deriving (Show)

ficsstr="<12> -----r-- --r-p-kp ----Qnp- ----p--- -------- -----PP- P-q---BP ---R-R-K B -1 0 1 0 0 0 164 CamyC android 2 3 0 26 26 112 107 24 Q/a6-e6 (0:01) Qxe6 0 1 215"

fields = drop 1.splitOn " " $ ficsstr
row1 = fields!!0
row2 = fields!!1
row3 = fields!!2
row4 = fields!!3
row5 = fields!!4
row6 = fields!!5
row7 = fields!!6
row8 = fields!!7
side_to_move = fields!!8
double_push = read.(!!9)$fields::Int
can_white_castle_short = toEnum.read.(!!10)$fields::Bool
can_white_castle_long = toEnum.read.(!!11)$fields::Bool
can_black_castle_short = toEnum.read.(!!12)$fields::Bool
can_black_castle_long = toEnum.read.(!!13)$fields::Bool
reversible_plies_count = read.(!!14)$fields::Int
game_no = read.(!!15)$fields::Int
white = fields!!16
black = fields!!17
observer_role = toEnum.read.(!!18)$fields::Observer
clock = GameClock (read(fields!!19)) (read (fields!!20))
white_material = read.(!!21)$fields::Int
black_material = read.(!!22)$fields::Int
white_remaining_time = read.(!!23)$fields::Int
black_remaining_time = read.(!!24)$fields::Int
next_move_no = read.(!!25)$fields::Int
last_move_coord_text = fields!!26
last_move_time_spent_text = fields!!27
--        self.last_move_time_spent = _unpack_time_spent(self.last_move_time_spent_text)
last_move_text = fields!!28
board_orientation = toEnum.read.(!!29)$fields::BoardFlip
is_clock_ticking = toEnum.read.(!!30)$fields::Bool
last_move_lag = read (fields!!31) :: Int


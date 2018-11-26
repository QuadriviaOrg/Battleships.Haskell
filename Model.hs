main = print "OK"
data Orient = Horiz | Vert deriving Eq
data SqVal = Empty|Miss|Hit
data Loc = Loc {col::Int, row:: Int} deriving (Show)
data Ship = Ship {loc::Loc, orient::Orient, name::String, shipSize::Int, hits::[Loc]}
data Board = Board {boardSize::Int, misses::[Loc], ships::[Ship], messages::String}

isSunk:: Ship -> Bool
isSunk s = length (hits s) > shipSize s

addHit:: Ship -> Loc -> Ship
addHit s l = Ship (loc s) (orient s) (name s) (shipSize s) (l : hits s)

fireAt:: Ship -> Loc -> (Ship, Bool, String)
fireAt s l
  | occupies s l = (addHit s l, True, hitMessage s l)
  | otherwise = (s, False, "")
  
occupies:: Ship -> Loc -> Bool
occupies s l
  | orient s == Horiz = row (loc s) == row l && col l >= col (loc s) && col l < col (loc s) + shipSize s
  | otherwise = col (loc s) == col l && row l >= row (loc s) && row l < row (loc s) + shipSize s
  
hitMessage:: Ship -> Loc -> String
hitMessage s l
  | isSunk sh = (name sh) ++ " sunk!"
  | otherwise = "Hit a "++(name sh)++" at ("++show(col l)++","++show(row l)++")."
  where sh = addHit s l
  
allSunk:: [Ship] -> Bool
allSunk ships = all isSunk ships

trainingGame:: [Ship]
trainingGame = [
  (Ship (Loc 1 8) Horiz "Aircraft Carrier" 5 []), 
  (Ship (Loc 8 1) Vert "Battleship" 4 []), 
  (Ship (Loc 7 6) Vert "Submarine" 3 []),  
  (Ship (Loc 5 9) Horiz "Destroyer" 3 []),   
  (Ship (Loc 1 4) Vert "Patrol Boat" 2 [])]
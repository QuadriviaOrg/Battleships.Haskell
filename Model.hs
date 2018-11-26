data Orient = Horiz | Vert
data SqVal = Empty|Miss|Hit
data Loc = Loc {row::Int, col:: Int} deriving (Show)
data Ship = Ship {loc::Loc, orient::Orient, name::String, shipSize::Int, hits::[Loc]}
data Board = Board {boardSize::Int, misses::[Loc], ships::[Ship], messages::String}

isSunk:: Ship -> Bool
isSunk s = length (hits s) > shipSize s

addHit:: Ship -> Loc -> Ship
addHit s l = Ship (loc s) (orient s) (name s) (shipSize s) (l : hits s)

--fireAt:: Ship -> Loc -> (Ship, Bool, String)
--TODO

occupies:: Ship -> Loc -> Bool
occupies s l
  | orient s == Horiz = row (loc s) == row l && col l >= col (loc s) && col l < col (loc s) + shipSize s
  | otherwise = False
  
allSunk:: [Ship] -> Bool
allSunk ships = all isSunk ships

trainingGame:: [Ship]
trainingGame = [
  (Ship (Loc 1 8) Horiz "Aircraft Carrier" 5 []),  
  (Ship (Loc 8 1) Vert "Battleship" 4 []), 
  (Ship (Loc 7 6) Vert "Submarine" 3 []),  
  (Ship (Loc 5 9) Horiz "Destroyer" 3 []),   
  (Ship (Loc 1 4) Vert "Patrol Boat" 2 [])]





 


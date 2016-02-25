import Data.List 
import Data.Array


data Direction = Horisontal | Vertical 
data Ship = Ship5 Cord Cord | Ship4 Cord Cord | Ship3 Cord Cord  | Ship2 Cord Cord
data Square = Empty | Alive | Dead deriving (Eq, Show)
type Board = [(Cord, Square)]
type Cord = (Int,Int)




main :: IO ()
main = do 
		menu
		
		

play :: IO ()
play = do
		  inputName1



inputName1 :: IO String
inputName1 = do 
		  		putStrLn "What is the name of player 1?"
				name <- getLine
				return ("Hello " ++ name)

inputName2 :: IO String
inputName2 = do
				putStrLn "What is the name of player 2?"
				name <- getLine
				return name 


boardSize :: IO Board
boardSize = do 
				putStrLn "How big do you want your board?"
				cord <- getLine
				return (makeBoard (read cord))

menu :: IO ()
menu = do
		putStrLn "Welcome to our game!\n"
		putStrLn "1. Play game\n2. Rules"
		input <- getLine
		if (input == "1") 
			then play --här ska play vara
			else if (input == "2") 
				then rules -- här ska rules vara
				else menu

rules :: IO ()
rules = do 
		putStrLn "Dont play yourself!"
		
	
placeShip2:: IO ()
placeShip2 = do
		     name <- inputName1
		     putStrLn (name ++ ", Place your Ships!")

victory :: Board -> Bool
victory b = if (length (filter (== Alive) (sortAlive b))) == 0 then True else False

findCord :: Board -> Cord -> Board ->Board
findCord (x:xs) (a,b) aux 
	| (fst x) == (a,b) = xs
	| otherwise = (x:aux) ++ (findCord xs (a,b) aux)

attack :: Board -> Cord -> Board
attack (x:xs) (a,b)
	| (snd x) == Empty = (x:xs)
	| otherwise = ((a,b), Dead) : (findCord (x:xs) (a,b) [])

sortAlive :: Board -> [Square]
sortAlive [] = []
sortAlive (b:bs) = ((snd b) : sortAlive bs)


makeBoardAux :: Cord -> Board
makeBoardAux (0,0) = []
makeBoardAux (_,0) = []
makeBoardAux (0,_) = []
makeBoardAux (x,y) = ((x,y), Empty) : makeBoardAux (x, (y-1)) {-++ makeBoardAux (x, (y-1)) -}

makeBoard :: Cord -> Board 
makeBoard (_,0)  = []
makeBoard (0,_)  = []
makeBoard (x,y)  = makeBoard ((x-1),y) ++ reverse (makeBoardAux (x,y)) 

changedBoard :: Board -> [Cord] -> Board -> Square -> Board
changedBoard (b:bs) [] aux _ = (b:bs)
changedBoard (b:bs) (x:xs) aux square
	| getCord x (b:bs) == False = (b:bs)
	| x == fst b = changedBoard ((x, square) : bs) xs aux Alive 
	| otherwise = changedBoard ((b:aux) ++ changedBoard bs [x] aux square) xs aux Alive

placeShip :: Ship -> Board -> Board

placeShip (Ship2 (x,y) (x1,y1)) (b:bs) = changedBoard (b:bs) [(x,y),(x1,y1)] [] Alive
placeShip (Ship3 (x,y) (x1,y1)) (b:bs) 
	| x < x1 || y < y1 = shipPos (Ship3 (x,y) (x1,y1)) (b:bs)
	| x > x1 || y > y1 = shipNeg (Ship3 (x,y) (x1,y1)) (b:bs)
placeShip (Ship4 (x,y) (x1,y1)) (b:bs) 
	| x < x1 || y < y1 = shipPos (Ship4 (x,y) (x1,y1)) (b:bs)
	| x > x1 || y > y1 = shipNeg (Ship4 (x,y) (x1,y1)) (b:bs)

placeShip (Ship5 (x,y) (x1,y1)) (b:bs) 
	| x < x1 || y < y1 = shipPos (Ship5 (x,y) (x1,y1)) (b:bs)
	| x > x1 || y > y1 = shipNeg (Ship5 (x,y) (x1,y1)) (b:bs)
	 		
shipPos (Ship3 (x,y) (x1,y1)) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipPos (Ship4 (x,y) (x1,y1)) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipPos (Ship5 (x,y) (x1,y1)) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2)),(x,(y+3)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y),((x+3),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipNeg (Ship3 (x,y) (x1,y1)) (b:bs)
 	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)

shipNeg (Ship4 (x,y) (x1,y1)) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipNeg (Ship5 (x,y) (x1,y1)) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2)),(x,(y-3)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y),((x-3),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)

getCord :: Cord -> Board -> Bool
getCord (x,y) [] = False
getCord (x,y) (b:bs) 
	| (x,y) == fst b = True 
	| otherwise = getCord (x,y) bs

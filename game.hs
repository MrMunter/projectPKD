import Data.List 
import Data.Array


data Direction = Horisontal | Vertical 
data Ship = Ship5 (Cord,Cord) | Ship4 (Cord,Cord) | Ship3 (Cord,Cord)  | Ship2 (Cord,Cord)
data Square = Empty | Alive | Dead deriving (Eq, Show)
--data Names = Name1 | Name2
type Board = [(Cord, Square)]
type Cord = (Int,Int)
type BoardSize = (Int, Int)
--type Name1 = String
--type Name2 = String





main :: IO ()
main = do 
		menu
		
		

play :: IO ()
play = do

		name1 <- inputName1
		name2 <- inputName2
		board1 <- boardSize1
		board2 <- boardSize2
		print board1
		--go board1 board2
		putStrLn ""


{-placeShip2 :: IO Board
placeShip2 = do
				putStrLn "first cord?"
				firstCord <- getLine 
				putStrLn "second cord"
				secondCord <- getLine
				return (placeShip (Ship2 (read firstCord) (read secondCord)) (makeBoard (3,3)))-}


inputName1 :: IO String
inputName1 = do 
		  		putStrLn "What is the name of player 1?"
				name1 <- getLine
				return name1
				

inputName2 :: IO String
inputName2 = do
				putStrLn "What is the name of player 2?"
				name2 <- getLine
				return name2

boardSize1 :: IO Board
boardSize1 = do 
				putStrLn "How big do you want your board?\tWrite in the format: (x,y)"
				cord <- getLine
				putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
				cords <- getLine
				putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
				cords2 <- getLine
				putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
				cords3 <- getLine
				putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
				cords4 <- getLine
				return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (read cord)))))))

boardSize2 :: IO Board
boardSize2 = do 
				putStrLn "How big do you want your board?\tWrite in the format: (x,y)"
				cord <- getLine
				putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
				cords <- getLine
				putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
				cords2 <- getLine
				putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
				cords3 <- getLine
				putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
				cords4 <- getLine
				return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (read cord)))))))
				{-putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
					cords2 <- getLine
				newBoard2 <- (placeShip (Ship3 (read cords2)) newBoard)-}				

{-go b1 b2 = do
						putStrLn "Player 1 attack"
					  	attackIO <- getLine
					  	newboard1 <- attack b2 (read attackIO)
					  	if victory newboard1 then do
					    	putStrLn "Player won!"
					    	putStrLn ""
					    	main
					   	  else do
					   		putStrLn "Player 2 attack"
					   		attackIO2 <- getLine
					    	newboard2 <- attack b1 (read attackIO2)  
					    	if victory newboard2 then do
					      		putStrLn "Computer won!"
					      		putStrLn ""
					      		main
					           else
					      		go newboard1 newboard2 -}
							

menu :: IO ()
menu = do
		putStrLn "Welcome to our game!\n"
		putStrLn "1. Play game\n2. Rules\n3. Quit"
		input <- getLine
		if (input == "1") 
			then play    --putStrLn "lets play" --play --här ska play vara
			else if (input == "2") 
				then rules -- här ska rules vara
				else if (input == "3")
					then return ()
					else menu
 
rules :: IO ()
rules = do 
		putStrLn "Dont play yourself!\n"
		menu
		
	
victory :: Board -> Bool
victory b = if (length (filter (== Alive) (sortAlive b))) == 0 then True else False

-- Removes a element from the board
{-findCord :: Board -> Cord -> Board ->Board
findCord (x:xs) (a,b) aux 
	| (fst x) == (a,b) = xs
	| otherwise = (x:aux) ++ (findCord xs (a,b) aux)  -}

attack :: Board -> Cord -> Board
attack (x:xs) (a,b)
	| getCord (a,b) (x:xs) == False = (x:xs)
	| (a,b) == (fst x) && (snd x) == Empty = (x:xs)
	| (a,b) == (fst x) && (snd x) == Alive = ((a,b), Dead) : xs
	| otherwise = (x : attack xs (a,b))

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

placeShip (Ship2 ((x,y),(x1,y1))) (b:bs) = changedBoard (b:bs) [(x,y),(x1,y1)] [] Alive
placeShip (Ship3 ((x,y),(x1,y1))) (b:bs) 
	| x < x1 || y < y1 = shipPos (Ship3 ((x,y),(x1,y1))) (b:bs)
	| x > x1 || y > y1 = shipNeg (Ship3 ((x,y),(x1,y1))) (b:bs)
placeShip (Ship4 ((x,y),(x1,y1))) (b:bs) 
	| x < x1 || y < y1 = shipPos (Ship4 ((x,y),(x1,y1))) (b:bs)
	| x > x1 || y > y1 = shipNeg (Ship4 ((x,y),(x1,y1))) (b:bs)

placeShip (Ship5 ((x,y),(x1,y1))) (b:bs) 
	| x < x1 || y < y1 = shipPos (Ship5 ((x,y),(x1,y1))) (b:bs)
	| x > x1 || y > y1 = shipNeg (Ship5 ((x,y),(x1,y1))) (b:bs)
	 		
shipPos (Ship3 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipPos (Ship4 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipPos (Ship5 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2)),(x,(y+3)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y),((x+3),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipNeg (Ship3 ((x,y),(x1,y1))) (b:bs)
 	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)

shipNeg (Ship4 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)
shipNeg (Ship5 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2)),(x,(y-3)),(x1,y1)] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y),((x-3),y),(x1,y1)] [] Alive
	| otherwise = (b:bs)

getCord :: Cord -> Board -> Bool
getCord (x,y) [] = False
getCord (x,y) (b:bs) 
	| (x,y) == fst b = True 
	| otherwise = getCord (x,y) bs
	
validShip :: Ship -> Board -> Bool
validShip (Ship2 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 
validShip (Ship3 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 
validShip (Ship4 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 
validShip (Ship5 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 

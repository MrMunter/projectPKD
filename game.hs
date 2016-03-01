import Data.List 
import Data.Array
import Test.HUnit
import System.Random


{- REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
   REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
-}
 
data Direction = Horisontal | Vertical

{- REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
   REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
-}
 
data Ship = Ship5 (Cord,Cord) | Ship4 (Cord,Cord) | Ship3 (Cord,Cord)  | Ship2 (Cord,Cord)

{- REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
   REPRESENTATION INVARIANT:  ... requirements on elements of the datatype that the code preserves at all times ...
-}
 
data Square = Empty | Alive | Dead | Miss deriving (Eq, Show)

type Board = [(Cord, Square)]
type Cord = (Int,Int)
type BoardSize = (Int, Int)





{- main
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

main :: IO ()
main = do 
		menu
		
{- menu
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}	


menu :: IO ()
menu = do
		putStrLn "Welcome to our game!\n"
		putStrLn "1. Play game\n2. Rules\n3. Quit\n4. Multiplayer"
		input <- getLine
		if (input == "1") 
			then play    --putStrLn "lets play" --play --här ska play vara
			else if (input == "2") 
				then rules -- här ska rules vara
				else if (input == "3")
					then return ()
						else if (input == "4")
							then playAI
							else menu
                    
{- play
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

play :: IO ()
play = do

		name1 <- inputName1
		name2 <- inputName2
		board1 <- boardSize1
		board2 <- boardSize2
		game board1 board2 name1 name2
			where 
				game b1 b2 n1 n2 = do 
						putStrLn (n1 ++ " attack 1 coordinate. (x,y)")
						cord <- getLine
						printBoard (attack b2 (read cord)) 
						if victory (attack b2 (read cord))
							then do putStrLn (n1 ++ " win\n") >> main						 
						else do
						putStrLn (n2 ++ " attack 1 coordinate. (x,y)") 
						cord2 <- getLine
						printBoard (attack b1 (read cord2)) 
						if victory (attack b1 (read cord2)) 
							then do putStrLn (n2 ++ " Win\n") >> main
						else game (attack b1 (read cord2)) (attack b2 (read cord)) n1 n2 


playAI :: IO ()
playAI = do 

      name1  <- inputName1
      board1 <- boardSize1
      board2 <- randomBoardGen


      game board1 board2 name1
        where 
          game b1 b2 n1 = do 
              putStrLn (n1 ++ " Attack")
              cord <- getLine
              printBoard (attack b2 (read cord)) 
              if victory (attack b2 (read cord))
                then do putStrLn (n1 ++ " win\n") >> main            
              else do
              putStrLn ("Computer Attack") 
              computerCordX <- randomRIO (1,10)
              computerCordY <- randomRIO (1,10)
              printBoard (attack b1 (computerCordX, computerCordY)) 
              if victory (attack b1 (computerCordX, computerCordY)) 
                then do putStrLn ("Computer Wins\n") >> main
              else game (attack b1 (computerCordX, computerCordY)) (attack b2 (read cord)) n1
        
rand :: IO ()
rand = do
              a <- randomRIO (1,9)  :: IO Int
              b <- randomRIO (1,10) :: IO Int

              c <- randomRIO (1,8)  :: IO Int
              d <- randomRIO (1,10) :: IO Int

              e <- randomRIO (1,7)  :: IO Int
              f <- randomRIO (1,10) :: IO Int

              g <- randomRIO (1,6)  :: IO Int
              h <- randomRIO (1,10) :: IO Int

              print (a,b,c,d,e,f,g,h)

randomBoardGen :: IO Board
randomBoardGen = do
              
              -- FOR HORIZONTAL SHIPS -- 
              randomCord2X <- randomRIO (1,9)    :: IO Int
              randomCord2Y <- randomRIO (1,10)   :: IO Int

              randomCord3X <- randomRIO (1,8)    :: IO Int
              randomCord3Y <- randomRIO (1,10)   :: IO Int

              randomCord4X <- randomRIO (1,7)    :: IO Int
              randomCord4Y <- randomRIO (1,10)   :: IO Int

              randomCord5X <- randomRIO (1,6)    :: IO Int
              randomCord5Y <- randomRIO (1,10)   :: IO Int

              -- FOR VERTICAL SHIPS -- 
              vrandomCord2X <- randomRIO (1,10)  :: IO Int
              vrandomCord2Y <- randomRIO (1,9)   :: IO Int

              vrandomCord3X <- randomRIO (1,10)  :: IO Int
              vrandomCord3Y <- randomRIO (1,8)   :: IO Int

              vrandomCord4X <- randomRIO (1,10)  :: IO Int
              vrandomCord4Y <- randomRIO (1,7)   :: IO Int

              vrandomCord5X <- randomRIO (1,10)  :: IO Int
              vrandomCord5Y <- randomRIO (1,6)   :: IO Int


              caseGen       <- randomRIO (1,2)  :: IO Int
              
              if caseGen == 1 then do              
                    if ((length (filter (\x -> snd x == Alive) (placeShip (Ship5 ((randomCord5X, randomCord5Y), (randomCord5X,(randomCord5Y+4)))) (placeShip (Ship4 ((randomCord4X, randomCord4Y), (randomCord4X,(randomCord4Y+3)))) (placeShip (Ship3 ((randomCord3X, randomCord3Y), (randomCord3X,(randomCord3Y+2)))) (placeShip (Ship2 ((randomCord2X, randomCord2Y), (randomCord2X,(randomCord2Y+1)))) (makeBoard(10,10))))))))) == 14 then return (placeShip (Ship5 ((randomCord5X, randomCord5Y), (randomCord5X,(randomCord5Y+4)))) (placeShip (Ship4 ((randomCord4X, randomCord4Y), (randomCord4X,(randomCord4Y+3)))) (placeShip (Ship3 ((randomCord3X, randomCord3Y), (randomCord3X,(randomCord3Y+2)))) (placeShip (Ship2 ((randomCord2X, randomCord2Y), (randomCord2X,(randomCord2Y+1)))) (makeBoard(10,10)))))) else randomBoardGen
              else 
                     if ((length (filter (\x -> snd x == Alive) (placeShip (Ship5 ((vrandomCord5X, vrandomCord5Y), ((vrandomCord5X+4),vrandomCord5Y))) (placeShip (Ship4 ((vrandomCord4X, vrandomCord4Y), ((vrandomCord4X+3),vrandomCord4Y))) (placeShip (Ship3 ((vrandomCord3X, vrandomCord3Y), ((vrandomCord3X+2),vrandomCord3Y))) (placeShip (Ship2 ((vrandomCord2X, vrandomCord2Y), ((vrandomCord2X+1),vrandomCord2Y))) (makeBoard(10,10))))))))) == 14 then return (placeShip (Ship5 ((vrandomCord5X, vrandomCord5Y), ((vrandomCord5X+4),vrandomCord5Y))) (placeShip (Ship4 ((vrandomCord4X, vrandomCord4Y), ((vrandomCord4X+3),vrandomCord4Y))) (placeShip (Ship3 ((vrandomCord3X, vrandomCord3Y), ((vrandomCord3X+2),vrandomCord3Y))) (placeShip (Ship2 ((vrandomCord2X, vrandomCord2Y), ((vrandomCord2X+1),vrandomCord2Y))) (makeBoard(10,10)))))) else randomBoardGen

{- rules
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

rules :: IO ()
rules = do 
		putStrLn "Dont play yourself!\n"
		menu 


{- inputName1
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

inputName1 :: IO String
inputName1 = do 
		  		putStrLn "What is the name of player 1?"
				name1 <- getLine
				return name1
                
{- inputName2
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}				

inputName2 :: IO String
inputName2 = do
				putStrLn "What is the name of player 2?"
				name2 <- getLine
				return name2
                
{- boardSize1
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

boardSize1 :: IO Board
boardSize1 = do 
				putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
				cords <- getLine
				putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
				cords2 <- getLine
				putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
				cords3 <- getLine
				putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
				cords4 <- getLine
				return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (10,10)))))))

{- boardSize2
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
boardSize2 :: IO Board
boardSize2 = do 
				putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
				cords <- getLine
				putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
				cords2 <- getLine
				putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
				cords3 <- getLine
				putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
				cords4 <- getLine
				return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (10,10)))))))


	
{- victory b
   PURPOSE:  See if any player have won
   PRE:  True
   POST: True if no Alive statement. Else False
   SIDE EFFECTS: None
   EXAMPLES: victory [((1,1),Alive),((1,2),Alive),((2,1),Empty),((2,2),Empty)] = False
             victory [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)] = True
-}				

victory :: Board -> Bool
victory b = if (length (filter (== Alive) (sortAlive b))) == 0 then True else False
    where 
        sortAlive :: Board -> [Square]
        sortAlive [] = []
        sortAlive (b:bs) = ((snd b) : sortAlive bs)

{- attack (x:xs) (a,b) 
   PURPOSE:  Performing a attack on the board.
   PRE:  True 
   POST: (x:xs) with the element ((a,b), Empty) to ((a,b), Miss) || ((a,b), Alive) to ((a,b), Dead)
   SIDE EFFECTS: If (a,b) is not a Cord in (x:xs) we get (x:xs) back.
   EXAMPLES: attack [((1,1), Alive)] (1,1) = [((1,1), Dead)]
             attack [((1,1), Empty)] (1,1) = [((1,1), Miss)]
-}

attack :: Board -> Cord -> Board
attack (x:xs) (a,b)
	| getCord (a,b) (x:xs) == False = (x:xs)
	| (a,b) == (fst x) && (snd x) == Empty = ((a,b), Miss) : xs
	| (a,b) == (fst x) && (snd x) == Alive = ((a,b), Dead) : xs
	| otherwise = (x : attack xs (a,b))


{- makeBoard (x,y)
   PURPOSE:  Makes a board
   PRE:  x & y is positive Integers 
   POST: List of tuples with all combinations 1 -> x and 1 -> y and Empty as second argument.          [((x,y), Empty)] 
   EXAMPLES: makeBoard (3,3) = [((1,1),Empty),((1,2),Empty),((1,3),Empty),((2,1),Empty),
   ((2,2),Empty),((2,3),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty)]
             makeBoard (1,1) = [((1,1),Empty)]
-}



makeBoard :: Cord -> Board
makeBoard (_,0)  = []
makeBoard (0,_)  = []
makeBoard (x,y)  = makeBoard (x,(y-1)) ++ reverse (makeBoardAux (x,y)) 
		where 
			makeBoardAux :: Cord -> Board
			makeBoardAux (0,0) = []
			makeBoardAux (_,0) = []
			makeBoardAux (0,_) = []
			makeBoardAux (x,y) = ((x,y), Empty) : makeBoardAux ((x-1), y)

{- changedBoard (b:bs) (x:xs) aux square
   PURPOSE: change statement from Empty to Alive
   PRE: (b:bs) is not an empty list.
   POST: -
   EXAMPLES: changedBoard ([((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]) [(1,1),(1,2)] [] Alive = [((1,1),Alive),((1,2),Alive),((2,1),Empty),((2,2),Empty)]
-}
changedBoard :: Board -> [Cord] -> Board -> Square -> Board
changedBoard (b:bs) [] aux _ = (b:bs)
changedBoard (b:bs) (x:xs) aux square
	| getCord x (b:bs) == False = (b:bs)
	| x == fst b = changedBoard ((x, square) : bs) xs aux Alive 
	| otherwise = changedBoard ((b:aux) ++ changedBoard bs [x] aux square) xs aux Alive

{- placeShip (Ship (x,y) (x1,y1)) (b:bs)
   PURPOSE:  Put out a ship on a board
   PRE:  (b:bs) is not empty
   POST: (b:bs) with the Squares of Cords (x->x1, y->y1) changed to Alive  
   SIDE EFFECTS: The function does not check if the ship is inside the board. 
   EXAMPLES: placeShip (Ship3 ((1,1),(1,3))) [((1,1),Empty),((1,2),Empty),((1,3),Empty),              ((2,1),Empty),((2,2),Empty),((2,3),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty)] =
   =[((1,1),Alive),((1,2),Alive),((1,3),Alive),((2,1),Empty),((2,2),Empty),((2,3),Empty),              ((3,1),Empty),((3,2),Empty),((3,3),Empty)]
-}

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
	 		
{- shipPos
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
shipPos :: Ship -> Board -> Board 
shipPos (Ship2 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y)] [] Alive
	| otherwise = (b:bs)	 		

shipPos (Ship3 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y)] [] Alive
	| otherwise = (b:bs)

shipPos (Ship4 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2)),(x,(y+3))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y),((x+3),y)] [] Alive
	| otherwise = (b:bs)

shipPos (Ship5 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y+1)),(x,(y+2)),(x,(y+3)),(x,(y+4))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x+1),y),((x+2),y),((x+3),y),((x+4),y)] [] Alive
	| otherwise = (b:bs)

{- shipNeg
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

shipNeg :: Ship -> Board -> Board 
shipNeg (Ship2 ((x,y),(x1,y1))) (b:bs)
 	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y)] [] Alive
	| otherwise = (b:bs)

shipNeg (Ship3 ((x,y),(x1,y1))) (b:bs)
 	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y)] [] Alive
	| otherwise = (b:bs)

shipNeg (Ship4 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2)),(x,(y-3))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y),((x-3),y)] [] Alive
	| otherwise = (b:bs)

shipNeg (Ship5 ((x,y),(x1,y1))) (b:bs)
	| x == x1 = changedBoard (b:bs) [(x,y),(x,(y-1)),(x,(y-2)),(x,(y-3)),(x,(y-4))] [] Alive 
	| y == y1 = changedBoard (b:bs) [(x,y),((x-1),y),((x-2),y),((x-3),y),((x-4),y)] [] Alive
	| otherwise = (b:bs)

{- getCord (x,y) (b:bs)
   PURPOSE:  Check if coordinate is in Board
   PRE: True
   POST: if coord (x,y) is in (b:bs) then True. Else False
   EXAMPLES: getCord (1,1) (makeBoard (2,2)) = True
             getCord (1,3) (makeBoard (2,2)) = False
-}
getCord :: Cord -> Board -> Bool
getCord (x,y) [] = False
getCord (x,y) (b:bs) 
	| (x,y) == fst b = True
	| otherwise = getCord (x,y) bs

{- validShip Ship 
   PURPOSE:  ... high level description of purpose of function
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
validShip :: Ship -> Board -> Bool
validShip (Ship2 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 
validShip (Ship3 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 
validShip (Ship4 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 
validShip (Ship5 (x,c))  b = if (getCord x b) == True && (getCord c b) == True then True else False 

{- printBoard (b:bs)
   PURPOSE:  print the board
   PRE: Boardsize 10x10
   POST: A 10x10 size board printed in ghci
   EXAMPLES: printBoard (makeBoard (10,10)) = 
 _  _  _  _  _  _  _  _  _  _
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
-}

printBoard :: Board -> IO()
printBoard boardsize  = putStr" _  _  _  _  _  _  _  _  _  _\n" >> (printBattleground boardsize 1 10)

{- printBattleground  (a:xs) x y 
   PURPOSE:  här skriver max
   PRE:  ... pre-condition on the arguments ...
   POST: ... post-condition on the result, in terms of the arguments ...
   SIDE EFFECTS: ... if any, including exceptions ...
   EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

	where
		printBattleground :: Board -> Int -> Int -> IO()
		printBattleground (a:xs) x y 
			|x == 10 && y == 1 = printAttack (snd a) >> putStr "\n"
			|x == 10 = printAttack (snd a) >> putStr "\n" >>printBattleground (xs) 1 (y-1)
			|x /= 10 = printAttack (snd a) >> printBattleground (xs) (x+1) y
            
{- printAttack square
   PURPOSE:  print out attacks
   PRE: True
   POST: print out attacked coordinates on the Board depending on statement of that cord.
   EXAMPLES: 
-}
		printAttack :: Square -> IO()
		printAttack square
			|square == Dead  = putStr "|x|"
			|square == Miss  = putStr "|#|"
			|square == Alive = putStr "|O|"
			|otherwise       = putStr "|_|"

{- validShot (x:xs) (a,b) 
   PURPOSE:  Checks if the cordinates for the shot is valid.
   PRE:  True
   POST: True if (a,b) exists in (x:xs) with the Square Empty or Alive. Else False
   EXAMPLES: validShot [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)] (2,2) = True
             validShot [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)] (3,2) = False
             validShot [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Dead)] (2,2) = False
-}
validShot :: Board -> Cord -> Bool
validShot (x:xs) (a,b) 
	| (getCord (a,b) (x:xs)) == False = False
	| (snd (pickCord (x:xs) (a,b))) == Dead = False
	| (snd (pickCord (x:xs) (a,b))) == Miss = False  
	| otherwise = True 
		where 
			pickCord (x:xs) (a,b) 
				| (fst x) == (a,b) = x 
				| otherwise = pickCord xs (a,b)





--------------------------------------------------------------
----TEST CASES


---- play
--test1 = TestCase $ assertEqual "play"
--            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

---- codeTable
---- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
--test2 = TestCase $ assertEqual "codeTable"
--            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

---- compress
---- while the precise code for the given example string may vary, its length should always be 135 bits
--test3 = TestCase $ assertEqual "compress"
--            135 (length (snd (compress "this is an example of a huffman tree")))

---- decompress
--test4 =
--    let s = "this is an example of a huffman tree"
--    in
--      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
--        s (let (h, bits) = compress s in decompress h bits)

--test5 =
--    let s = "xxx"
--    in
--      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
--        s (let (h, bits) = compress s in decompress h bits)

--test6 =
--    let s = ""
--    in
--      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
--        s (let (h, bits) = compress s in decompress h bits)

---- for running all the tests
--runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]



















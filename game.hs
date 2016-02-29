import Data.List 
import Data.Array
import Test.HUnit


data Direction = Horisontal | Vertical 
data Ship = Ship5 (Cord,Cord) | Ship4 (Cord,Cord) | Ship3 (Cord,Cord)  | Ship2 (Cord,Cord)
data Square = Empty | Alive | Dead | Miss deriving (Eq, Show)

type Board = [(Cord, Square)]
type Cord = (Int,Int)
type BoardSize = (Int, Int)


boardSize = (10,10)




main :: IO ()
main = do 
		menu
		
		

play :: IO ()
play = do

		name1 <- inputName1
		name2 <- inputName2
		board1 <- boardSize1
		board2 <- boardSize2
		game board1 board2 name1 name2
			where 
				game b1 b2 n1 n2 = do 
						putStrLn (n1 ++ " Attack")
						cord <- getLine
						cpbord (attack b2 (read cord)) 
						if victory (attack b2 (read cord))
							then do putStrLn (n1 ++ " win\n") >> main						 
						else do
						putStrLn (n2 ++ " Attack") 
						cord2 <- getLine
						cpbord (attack b1 (read cord2)) 
						if victory (attack b1 (read cord2)) 
							then do putStrLn (n2 ++ " Win\n") >> main
						else game (attack b1 (read cord2)) (attack b2 (read cord)) n1 n2 


 
attackIO :: Board -> IO Board 
attackIO board = return board 




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
				putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
				cords <- getLine
				putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
				cords2 <- getLine
				putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
				cords3 <- getLine
				putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
				cords4 <- getLine
				return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (10,10)))))))

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



attack :: Board -> Cord -> Board
attack (x:xs) (a,b)
	| getCord (a,b) (x:xs) == False = (x:xs)
	| (a,b) == (fst x) && (snd x) == Empty = ((a,b), Miss) : xs
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



cpbord :: Board -> IO()
cpbord boardsize  = putStr" _  _  _  _  _  _  _  _  _  _\n" >> (printBattleground boardsize 1 10)

	where
		printBattleground :: Board -> Int -> Int -> IO()
		printBattleground (a:xs) x y
			|x == 10 && y == 1 = printShip (snd a) >> putStr "\n"
			|x == 10 = printShip (snd a) >> putStr "\n" >>printBattleground (xs) 1 (y-1)
			|x /= 10 = printShip (snd a) >> printBattleground (xs) (x+1) y


		printShip :: Square -> IO()
		printShip square 
			|square == Dead  = putStr "|x|"
			|square == Miss  = putStr "|#|"
			|otherwise       = putStr "|_|"

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










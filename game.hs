import Data.List 
import Data.Array
import Test.HUnit
import System.Random
import Control.Monad


{- REPRESENTATION CONVENTION: Represent the coordinates the ship is positioned at
   REPRESENTATION INVARIANT:  A valid board
-}
data Ship = Ship5 (Cord,Cord) | Ship4 (Cord,Cord) | Ship3 (Cord,Cord)  | Ship2 (Cord,Cord)

{- REPRESENTATION CONVENTION: Represent the statement of a square 
   REPRESENTATION INVARIANT:  True
-}
data Square = Empty | Alive | Dead | Miss deriving (Eq, Show)

{- REPRESENTATION CONVENTION: Represent a board with all the cordinates 
   REPRESENTATION INVARIANT:  True
 -}
type Board = [(Cord, Square)]

{- REPRESENTATION CONVENTION: Represent the positions of a square
   REPRESENTATION INVARIANT:  each element of the tuple >= 1 
 -}
type Cord = (Int,Int)





{- main
   PURPOSE:  run the game
   PRE:      True
   POST:     menu
-}
main :: IO ()
main = do 
    menu
    
{- menu
   PURPOSE:  To choose what you want to do in the game
   PRE:      None
   EXAMPLES: ~~~~~~~~~~~BATTLESHIPS~~~~~~~~~~~

             1. Play with a friend
             2. Play against the computer
             3. Read rules
             4. Quit
-}  
menu :: IO ()
menu = do
    putStrLn "\n~~~~~~~~~~~BATTLESHIPS~~~~~~~~~~~\n"
    putStrLn "1. Play with a friend\n2. Play against the computer\n3. Read rules\n4. Quit\n\n"
    input <- getLine
    if (input == "1") 
      then play   
      else if (input == "2") 
        then  playAI 
        else if (input == "3")
          then rules
          else if (input == "4")
          then return()
            else menu
                    
{- play
   PURPOSE:      start a multiplayer game for 2 persons
   PRE:          None
   POST:         returns you to the menu by calling main
   SIDE EFFECTS: None
-}
play :: IO ()
play = do

    name1 <- inputName1
    name2 <- inputName2
    putStrLn (name1 ++ " it's your turn to place your ships")
    board1 <- boardSize1
    putStrLn (name2 ++ " it's your turn to place your ships")
    board2 <- boardSize2
    game board1 board2 name1 name2
      where 
{- game
PURPOSE:      runs the multiplayermode
PRE:          None
POST:         return you to the menu by calling main
SIDE EFFECTS: None
-}        
        game b1 b2 n1 n2 = do 
            putStrLn (n1 ++ " , it's your turn to attack!")
            cord <- getLine
            if cord == "Quit" || cord == "quit" then do main else if elem cord (map show (range ((1,1),(10,10)))) /= True then putStrLn "Invalid move, please try again!" >> game b1 b2 n1 n2
              else do
                putStrLn ("\n        " ++ n2 ++ "'s board") >> printBoard (attack b2 (read cord)) 
                if victory (attack b2 (read cord))
                  then do putStrLn (n1 ++ " win\n") >> main            
                else game2 b1 n2 cord
                  where
                    game2 b1 n2 cord = do
                      putStrLn (n2 ++ ", it's your turn to attack!") 
                      cord2 <- getLine
                      if cord2 == "Quit" || cord2 == "quit" then do main else if elem cord2 (map show (range ((1,1),(10,10)))) /= True then putStrLn "Invalid move, please try again!" >> game2 b1 n2 cord
                        else do
                          putStrLn ("\n        " ++ n1 ++ "'s board") >> printBoard (attack b1 (read cord2)) 
                          if victory (attack b1 (read cord2)) 
                            then do putStrLn (n2 ++ " Win\n") >> main
                          else game (attack b1 (read cord2)) (attack b2 (read cord)) n1 n2 

{- playAI
   PURPOSE:      start a game versus an AI
   PRE:          None
   POST:         returns you to the menu by calling main
   SIDE EFFECTS: None
-}
playAI :: IO ()
playAI = do 

      name1  <- inputName1
      board1 <- boardSize1
      board2 <- randomBoardGen


      game board1 board2 name1
        where 
{- game b1 b2 n1
PURPOSE:      Runs a single player game against the computer
PRE:          True
POST:         returns you to the menu by calling main
SIDE EFFECTS: None
-}  
          
          game b1 b2 n1 = do 
              putStrLn (n1 ++ ", it's your turn to attack!\nIf you want to see your board before making your move, type Show")
              cord <- getLine
              if cord == "Show" || cord == "show" then do printBoard b1 >> game b1 b2 n1 else 
                if cord == "Quit" || cord == "quit" then do main else if elem cord (map show (range ((1,1),(10,10)))) /= True then putStrLn "Invalid move, please try again!" >> game b1 b2 n1
                  else do
                    putStrLn ("Computers Board")
                    printBoard (attack b2 (read cord)) 
                    if victory (attack b2 (read cord))
                      then do putStrLn (n1 ++ " win\n") >> main            
                    else do
                      putStrLn ("Computer Attack") 
                      computerCordX <- randomRIO (1,10)
                      computerCordY <- randomRIO (1,10)
                      putStrLn "Computer's Board"
                      if victory (attack b1 (computerCordX, computerCordY)) 
                        then do putStrLn ("Computer Wins\n") >> printBoard (attack b1 (computerCordX, computerCordY)) >> main
                        else game (attack b1 (computerCordX, computerCordY)) (attack b2 (read cord)) n1


              
{- randomBoardGen
   PURPOSE:  generates the board used by the AI
   PRE:      True
   POST:     the AI board with ships placed at random coordinates
   EXAMPLES: randomBoardGen = [((1,1),Empty),((2,1),Empty),((3,1),Empty),((4,1),Empty),((5,1),Empty),((6,1),Empty),((7,1),Empty),((8,1),Empty),((9,1),Empty),((10,1),Empty),((1,2),Empty),((2,2),Empty),((3,2),Alive),((4,2),Alive),((5,2),Alive),((6,2),Empty),((7,2),Empty),((8,2),Empty),((9,2),Empty),((10,2),Empty),((1,3),Empty),((2,3),Empty),((3,3),Empty),((4,3),Empty),((5,3),Empty),((6,3),Empty),((7,3),Alive),((8,3),Alive),((9,3),Alive),((10,3),Alive),((1,4),Empty),((2,4),Empty),((3,4),Empty),((4,4),Empty),((5,4),Empty),((6,4),Empty),((7,4),Empty),((8,4),Empty),((9,4),Empty),((10,4),Empty),((1,5),Empty),((2,5),Empty),((3,5),Empty),((4,5),Empty),((5,5),Alive),((6,5),Alive),((7,5),Alive),((8,5),Alive),((9,5),Alive),((10,5),Empty),((1,6),Empty),((2,6),Empty),((3,6),Empty),((4,6),Empty),((5,6),Empty),((6,6),Empty),((7,6),Alive),((8,6),Alive),((9,6),Empty),((10,6),Empty),((1,7),Empty),((2,7),Empty),((3,7),Empty),((4,7),Empty),((5,7),Empty),((6,7),Empty),((7,7),Empty),((8,7),Empty),((9,7),Empty),((10,7),Empty),((1,8),Empty),((2,8),Empty),((3,8),Empty),((4,8),Empty),((5,8),Empty),((6,8),Empty),((7,8),Empty),((8,8),Empty),((9,8),Empty),((10,8),Empty),((1,9),Empty),((2,9),Empty),((3,9),Empty),((4,9),Empty),((5,9),Empty),((6,9),Empty),((7,9),Empty),((8,9),Empty),((9,9),Empty),((10,9),Empty),((1,10),Empty),((2,10),Empty),((3,10),Empty),((4,10),Empty),((5,10),Empty),((6,10),Empty),((7,10),Empty),((8,10),Empty),((9,10),Empty),((10,10),Empty)]
-}
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
   PURPOSE:  reads a .txt-file and prints it to the terminal.
   PRE:      a .txt-file
   POST:     an imported text-file. 
   EXAMPLES: contents <- readFile "gameRules.txt"
             print contents = "TEST-TEXT"
-}
rules :: IO ()
rules = do 
         contents <- readFile "gameRules.txt"
         print contents 
         menu 


{- inputName1
   PURPOSE:  to get the first persons name
   PRE:      True
   POST:     a string saved in name1 
-}

inputName1 :: IO String
inputName1 = do 
                putStrLn "What is the name of player 1?"
                name1 <- getLine
                return name1
                
{- inputName2
   PURPOSE:  to get the second persons name
   PRE:      True
   POST:     a string saved in name2 
-}

inputName2 :: IO String
inputName2 = do
                putStrLn "What is the name of player 2?"
                name2 <- getLine
                return name2
                
{- boardSize1
   PURPOSE:  place out ships from input
   PRE:      correct format of the coordinates
   POST:     board1 with ships at locations from typed coordinates. (cords,cords2,cords3,cords4)
   EXAMPLES: Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))
             ((1,2),(1,3))
             Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))
             ((2,3),(2,5))
             Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))
             ((5,3),(5,6))
             Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))
             ((6,3),(6,7)) saves a board that looks like this in boardSize1
             
 _  _  _  _  _  _  _  _  _  _
|_||_||_||_||_||_||_||_||_||_|
|O||_||_||_||_||_||_||_||_||_|
|O||O||_||_||O||O||_||_||_||_|
|_||O||_||_||O||O||_||_||_||_|
|_||O||_||_||O||O||_||_||_||_|
|_||_||_||_||O||O||_||_||_||_|
|_||_||_||_||_||O||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
-}

boardSize1 :: IO Board
boardSize1 = do 
                putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
                cords <- getLine
                if (length cords) < 13 || (length cords) > 16 || (take 2 cords) /= "((" || (drop (length (cords)-2) cords) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize1 else do
                putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
                cords2 <- getLine
                if (length cords2) < 13 || (length cords2) > 16 || (take 2 cords2) /= "((" || (drop (length (cords2)-2) cords2) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize1 else do                
                putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
                cords3 <- getLine
                if (length cords3) < 13 || (length cords3) > 16 || (take 2 cords3) /= "((" || (drop (length (cords3)-2) cords3) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize1 else do                
                putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
                cords4 <- getLine
                if (length cords4) < 13 || (length cords4) > 16 || (take 2 cords4) /= "((" || (drop (length (cords4)-2) cords4) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize1 else do                
                if (length (filter (\x -> snd x == Alive) (placeShip (Ship5 (read cords4))     (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2))  (placeShip (Ship2 (read cords)) (makeBoard (10,10))))))))) == 14 then return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (10,10))))))) else putStrLn "Something went wrong" >> boardSize1
{- boardSize2
   PURPOSE:  place out ships from input
   PRE:      correct format of the coordinates
   POST:     board2 with ships at locations from typed coordinates. (cords,cords2,cords3,cords4) 
   EXAMPLES: Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))
             ((1,2),(1,3))
             Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))
             ((2,3),(2,5))
             Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))
             ((5,3),(5,6))
             Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))
             ((6,3),(6,7)) saves a board that looks like this in boardSize2
             
 _  _  _  _  _  _  _  _  _  _
|_||_||_||_||_||_||_||_||_||_|
|O||_||_||_||_||_||_||_||_||_|
|O||O||_||_||O||O||_||_||_||_|
|_||O||_||_||O||O||_||_||_||_|
|_||O||_||_||O||O||_||_||_||_|
|_||_||_||_||O||O||_||_||_||_|
|_||_||_||_||_||O||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
|_||_||_||_||_||_||_||_||_||_|
-}

boardSize2 :: IO Board
boardSize2 = do 
                putStrLn "Place your ship, 2 tiles. Format ((x1,y1),(x2,y2))"
                cords <- getLine
                if (length cords) < 13 || (length cords) > 16 || (take 2 cords) /= "((" || (drop (length (cords)-2) cords) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize2 else do
                putStrLn "Place your ship, 3 tiles. Format ((x1,y1),(x2,y2))"
                cords2 <- getLine
                if (length cords2) < 13 || (length cords2) > 16 || (take 2 cords2) /= "((" || (drop (length (cords2)-2) cords2) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize2 else do                
                putStrLn "Place your ship, 4 tiles. Format ((x1,y1),(x2,y2))"
                cords3 <- getLine
                if (length cords3) < 13 || (length cords3) > 16 || (take 2 cords3) /= "((" || (drop (length (cords3)-2) cords3) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize2 else do                
                putStrLn "Place your ship, 5 tiles. Format ((x1,y1),(x2,y2))"
                cords4 <- getLine
                if (length cords4) < 13 || (length cords4) > 16 || (take 2 cords4) /= "((" || (drop (length (cords4)-2) cords4) /= "))" then putStrLn "Invalid ship position, please try again!" >> boardSize2 else do                
                if (length (filter (\x -> snd x == Alive) (placeShip (Ship5 (read cords4))     (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2))  (placeShip (Ship2 (read cords)) (makeBoard (10,10))))))))) == 14 then return (placeShip (Ship5 (read cords4)) (placeShip (Ship4 (read cords3)) ((placeShip (Ship3 (read cords2)) (placeShip (Ship2 (read cords)) (makeBoard (10,10))))))) else putStrLn "Invalid ship positions, please try again!" >> boardSize2
{- victory b
   PURPOSE:  See if any player have won
   PRE:      True
   POST:     True if no Alive statement. Else False
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
   PURPOSE:      Performing a attack on the board.
   PRE:          True 
   POST:         (x:xs) with the element ((a,b), Empty) to ((a,b), Miss) || ((a,b), Alive) to ((a,b), Dead)
   SIDE EFFECTS: If (a,b) is not a Cord in (x:xs) we get (x:xs) back.
   EXAMPLES:     attack [((1,1), Alive)] (1,1) = [((1,1), Dead)]
                 attack [((1,1), Empty)] (1,1) = [((1,1), Miss)]
-}
attack :: Board -> Cord -> Board
attack (x:xs) (a,b)
  | getCord (a,b) (x:xs)        == False = (x:xs)
  | (a,b) == (fst x) && (snd x) == Empty = ((a,b), Miss) : xs
  | (a,b) == (fst x) && (snd x) == Alive = ((a,b), Dead) : xs
  | otherwise = (x : attack xs (a,b))


{- makeBoard (x,y)
   PURPOSE:  Makes a board
   PRE:      x & y is positive Integers 
   POST:     List of tuples with all combinations 1 -> x and 1 -> y and Empty as second argument.          [((x,y), Empty)] 
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
   PURPOSE:  change statement from Empty to Alive
   PRE:      (b:bs) is not an empty list.
   POST:     -
   EXAMPLES: changedBoard ([((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]) [(1,1),(1,2)] [] Alive =
                           [((1,1),Alive),((1,2),Alive),((2,1),Empty),((2,2),Empty)]
-}
changedBoard :: Board -> [Cord] -> Board -> Square -> Board
changedBoard (b:bs) [] aux _ = (b:bs)
changedBoard (b:bs) (x:xs) aux square
  | getCord x (b:bs) == False = (b:bs)
  | x == fst b = changedBoard ((x, square) : bs) xs aux Alive 
  | otherwise = changedBoard ((b:aux) ++ changedBoard bs [x] aux square) xs aux Alive

{- placeShip (Ship (x,y) (x1,y1)) (b:bs)
   PURPOSE:      Put out a ship on a board
   PRE:          (b:bs) is not empty
   POST:         (b:bs) with the Squares of Cords (x->x1, y->y1) changed to Alive  
   SIDE EFFECTS: The function does not check if the ship is inside the board. 
   EXAMPLES:     placeShip (Ship3 ((1,1),(1,3))) [((1,1),Empty),((1,2),Empty),((1,3),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty)] =
   						   		                 [((1,1),Alive),((1,2),Alive),((1,3),Alive),((2,1),Empty),((2,2),Empty),((2,3),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty)]
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
      
{- shipPos Ship (b:bs)
   PURPOSE:  Check if the ship is horistantal or vertical and place it on a board.
   PRE:  x1 > x || y1 > y && x == x1 || y == y1
   POST: Ship placed Vertical if y1 > y, Ship placed horisontal if x1 > x. In (b:bs)
   EXAMPLES: shipPos (Ship2 ((1,1),(1,2))) [((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)] = [((1,1),Alive),((2,1),Empty),((1,2),Alive),((2,2),Empty)]
             shipPos (Ship2 ((1,2),(2,2))) [((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)] = [((1,1),Empty),((2,1),Empty),((1,2),Alive),((2,2),Alive)]
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

{- shipNeg Ship (b:bs)
   PURPOSE: placeShip calls this function when you place a ship with the first cord "larger" then the second, then Check if the ship is horistantal or vertical and place it on a board.
   PRE:  x1 < x || y1 < y && x == x1 || y == y1
   POST: Ship placed Vertical if y1 < y, Ship placed horisontal if x1 < x. In (b:bs)
   EXAMPLES: shipNeg (Ship2 ((1,2),(1,1))) [((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)] = [((1,1),Alive),((2,1),Empty),((1,2),Alive),((2,2),Empty)]
             shipNeg (Ship2 ((2,2),(1,2))) [((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)] = [((1,1),Empty),((2,1),Empty),((1,2),Alive),((2,2),Alive)]
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
   PURPOSE:  check if coordinate is in board
   PRE:      True
   POST:     if coord (x,y) is in (b:bs) then True. Else False
   EXAMPLES: getCord (1,1) ([((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)]) = True
             getCord (1,3) ([((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)]) = False
-}
getCord :: Cord -> Board -> Bool
getCord (x,y) [] = False
getCord (x,y) (b:bs) 
  | (x,y) == fst b = True
  | otherwise = getCord (x,y) bs


{- printBoard (b:bs)
   PURPOSE:  print the board
   PRE:      boardsize 10x10
   POST:     a 10x10 size board printed in ghci
   EXAMPLES: printBoard ([((1,1),Empty),((2,1),Empty),((3,1),Empty),((4,1),Empty),((5,1),Empty),((6,1),Empty),((7,1),Empty),((8,1),Empty),((9,1),Empty),((10,1),Empty),
   						((1,2),Empty),((2,2),Empty),((3,2),Empty),((4,2),Empty),((5,2),Empty),((6,2),Empty),((7,2),Empty),((8,2),Empty),((9,2),Empty),((10,2),Empty),
   						((1,3),Empty),((2,3),Empty),((3,3),Empty),((4,3),Empty),((5,3),Empty),((6,3),Empty),((7,3),Empty),((8,3),Empty),((9,3),Empty),((10,3),Empty),
   						((1,4),Empty),((2,4),Empty),((3,4),Empty),((4,4),Empty),((5,4),Empty),((6,4),Empty),((7,4),Empty),((8,4),Empty),((9,4),Empty),((10,4),Empty),
   						((1,5),Empty),((2,5),Empty),((3,5),Empty),((4,5),Empty),((5,5),Empty),((6,5),Empty),((7,5),Empty),((8,5),Empty),((9,5),Empty),((10,5),Empty),
   						((1,6),Empty),((2,6),Empty),((3,6),Empty),((4,6),Empty),((5,6),Empty),((6,6),Empty),((7,6),Empty),((8,6),Empty),((9,6),Empty),((10,6),Empty),
   						((1,7),Empty),((2,7),Empty),((3,7),Empty),((4,7),Empty),((5,7),Empty),((6,7),Empty),((7,7),Empty),((8,7),Empty),((9,7),Empty),((10,7),Empty),
   						((1,8),Empty),((2,8),Empty),((3,8),Empty),((4,8),Empty),((5,8),Empty),((6,8),Empty),((7,8),Empty),((8,8),Empty),((9,8),Empty),((10,8),Empty),
   						((1,9),Empty),((2,9),Empty),((3,9),Empty),((4,9),Empty),((5,9),Empty),((6,9),Empty),((7,9),Empty),((8,9),Empty),((9,9),Empty),((10,9),Empty),
   						((1,10),Empty),((2,10),Empty),((3,10),Empty),((4,10),Empty),((5,10),Empty),((6,10),Empty),((7,10),Empty),((8,10),Empty),((9,10),Empty),((10,10),Empty)]) = 
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
   PURPOSE:  That you type in right coordinates (här skriver max)
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
   PRE:      True
   POST:     print out attacked coordinates on the Board depending on statement of that cord.
   EXAMPLES: 
-}
    printAttack :: Square -> IO()
    printAttack square
      |square == Dead  = putStr "|x|"
      |square == Miss  = putStr "|#|"
      |square == Alive = putStr "|O|"
      |otherwise       = putStr "|_|"


--------------------------------------------------------------
----TEST CASES

---- makeBoard
test1 = TestCase $ assertEqual "makeBoard"
            ([]) ( (makeBoard(0,0)))

---- getCord
test2 = TestCase $ assertEqual "getCord"
           (True) ( (getCord (1,1) [((1,1), Alive)])) 

---- victory
test3 = TestCase $ assertEqual "victory"
            (False) ( (victory [((1,1),Alive),((1,2),Alive),((2,1),Empty),((2,2),Empty)]))
---- shipNeg
test4 = TestCase $ assertEqual "shipNeg"
            ([((1,1),Alive),((2,1),Empty),((3,1),Empty),((4,1),Empty),((5,1),Empty),((6,1),Empty),((7,1),Empty),((8,1),Empty),((9,1),Empty),((10,1),Empty),((1,2),Empty),((2,
            2),Empty),((3,2),Empty),((4,2),Empty),((5,2),Empty),((6,2),Empty),((7,2),Empty),
            ((8,2),Empty),((9,2),Empty),((10,2),Empty),((1,3),Empty),((2,3),Empty),((3,3),Empty),((4,3),Empty),((5,3),Empty),((6,3),Empty),((7,3),Empty),((8,3),Empty),((9,3
            ),Empty),((10,3),Empty),((1,4),Empty),((2,4),Empty),((3,4),Empty),((4,4),Empty),
            ((5,4),Empty),((6,4),Empty),((7,4),Empty),((8,4),Empty),((9,4),Empty),((10,4),Empty),((1,5),Empty),((2,5),Empty),((3,5),Empty),((4,5),Empty),((5,5),Empty),((6,5
            ),Empty),((7,5),Empty),((8,5),Empty),((9,5),Empty),((10,5),Empty),((1,6),Empty),
            ((2,6),Empty),((3,6),Empty),((4,6),Empty),((5,6),Empty),((6,6),Empty),((7,6),Empty),((8,6),Empty),((9,6),Empty),((10,6),Empty),((1,7),Empty),((2,7),Empty),((3,7
            ),Empty),((4,7),Empty),((5,7),Empty),((6,7),Empty),((7,7),Empty),((8,7),Empty),(
            (9,7),Empty),((10,7),Empty),((1,8),Empty),((2,8),Empty),((3,8),Empty),((4,8),Empty),((5,8),Empty),((6,8),Empty),((7,8),Empty),((8,8),Empty),((9,8),Empty),((10,8
            ),Empty),((1,9),Empty),((2,9),Empty),((3,9),Empty),((4,9),Empty),((5,9),Empty),(
            (6,9),Empty),((7,9),Empty),((8,9),Empty),((9,9),Empty),((10,9),Empty),((1,10),Empty),((2,10),Empty),((3,10),Empty),((4,10),Empty),((5,10),Empty),((6,10),Empty),
            ((7,10),Empty),((8,10),Empty),((9,10),Empty),((10,10),Empty)]) 
            (shipNeg (Ship2 ((1,1),(1,2))) (makeBoard(10,10)))

---- attack
test5 = TestCase $ assertEqual "attack"
			([((1,1), Dead)])  ( (attack [((1,1), Alive)] (1,1))) 

---- changedBoard
test6 = TestCase $ assertEqual "changedBoard"
			([((1,1),Alive),((1,2),Alive),((2,1),Empty),((2,2),Empty)]) ( (changedBoard ([((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]) [(1,1),(1,2)] [] Alive))

---- placeShip
test7 = TestCase $ assertEqual "placeShip"
			([((1,1),Alive),((1,2),Alive),((1,3),Alive),((2,1),Empty),((2,2),Empty),((2,3),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty)])
			( (placeShip (Ship3 ((1,1),(1,3))) [((1,1),Empty),((1,2),Empty),((1,3),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty)]))

---- shipPos
test8 = TestCase $ assertEqual "shipPos"
			([((1,1),Alive),((2,1),Empty),((1,2),Alive),((2,2),Empty)]) ( (shipPos (Ship2 ((1,1),(1,2))) [((1,1),Empty),((2,1),Empty),((1,2),Empty),((2,2),Empty)]))

---- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8]

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell
            {
                img :: Char
            } deriving (Eq, Ord, Show)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level
            {
                cells :: A.Array Position Cell
            } deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level
    where show lvl@(Level cellsArr) = foldl concatCell [endl] (assocs cellsArr)
            where
                maxY = snd $ snd $ A.bounds cellsArr
                concatCell remains cell
                    | maxY == (snd $ fst cell)    = remains ++ [img $ snd cell] ++ [endl]
                    | otherwise                   = remains ++ [img $ snd cell]

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel (rX, rY) = Level $ A.listArray ((0, 0), (rX, rY)) (take ((rX + 1) * (rY + 1)) (repeat $ Cell emptySpace))

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

addCell :: (Char, Position) -> Level -> Level
addCell (cell, pos@(x, y)) lvl@(Level cellsArr)
    | x >= 0 && y >= 0 && x <= maxX && y <= maxY && crtCell == emptySpace    = Level $ cellsArr A.// [(pos, (Cell cell))]
    | otherwise                                                              = lvl
        where
            crtCell = (img $ cellsArr A.! pos)
            maxX = fst $ snd $ A.bounds cellsArr
            maxY = snd $ snd $ A.bounds cellsArr

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celulele din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos cps = foldr addCell emptyStage cps
    where emptyStage = emptyLevel pos

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos@(x, y) dir lvl@(Level cellsArr)
    | x < 0 || y < 0 || x > maxX || y > maxY || isImmutable cellImg                      = lvl
    | dir == North && x - 1 >= 0 && (img $ cellsArr A.! (x - 1, y)) == emptySpace        = swapCells (x - 1, y) pos
    | dir == South && x + 1 <= maxX && (img $ cellsArr A.! (x + 1, y)) == emptySpace     = swapCells (x + 1, y) pos
    | dir == East && y + 1 <= maxY && (img $ cellsArr A.! (x, y + 1)) == emptySpace      = swapCells (x, y + 1) pos
    | dir == West && y - 1 >= 0 && (img $ cellsArr A.! (x, y - 1)) == emptySpace         = swapCells (x, y - 1) pos
    | otherwise                                                                          = lvl
        where 
            cellImg                 = img $ cellsArr A.! pos
            maxX                    = fst $ snd $ A.bounds cellsArr
            maxY                    = snd $ snd $ A.bounds cellsArr
            swapCells newPos oldPos = addCell (cellImg, newPos) (Level $ cellsArr A.// [(oldPos, (Cell emptySpace))])
            isImmutable cellRep     = elem cellRep startCells || elem cellRep winningCells

{-
    *** TODO ***

    Verifică dacă două celule se pot conecta.
    Atenție: Această funcție se aplică de la stânga la 
    dreapta(nu este comutativă).

    ex: connection botLeft horPipe = True (╚═)
        connection horPipe botLeft = False (═╚)
-}

connection :: Cell -> Cell -> Directions -> Bool
connection cell1@(Cell imgC1) cell2@(Cell imgC2) dir
    | imgC1 == emptySpace || imgC1 == emptyCell                                                                  = False    
    | imgC1 == startUp && dir == North && (elem imgC2 connUp)                                                    = True
    | imgC1 == startDown && dir == South && (elem imgC2 connDown)                                                = True
    | imgC1 == startRight && dir == East && (elem imgC2 connRight)                                               = True
    | imgC1 == startLeft && dir == West && (elem imgC2 connLeft)                                                 = True
    | imgC1 == horPipe && ((dir == East && (elem imgC2 connRight)) || (dir == West && (elem imgC2 connLeft)))    = True
    | imgC1 == verPipe && ((dir == North && (elem imgC2 connUp)) || (dir == South && (elem imgC2 connDown)))     = True
    | imgC1 == topRight && ((dir == South && (elem imgC2 connDown)) || (dir == West && (elem imgC2 connLeft)))   = True
    | imgC1 == topLeft && ((dir == South && (elem imgC2 connDown)) || (dir == East && (elem imgC2 connRight)))   = True
    | imgC1 == botRight && ((dir == North && (elem imgC2 connUp)) || (dir == West && (elem imgC2 connLeft)))     = True
    | imgC1 == botLeft && ((dir == North && (elem imgC2 connUp)) || (dir == East && (elem imgC2 connRight)))     = True
    | otherwise                                                                                         = False
        where
            connRight = [startLeft, winLeft, horPipe, botRight, topRight]
            connLeft  = [startRight, winRight, horPipe, botLeft, topLeft]
            connUp    = [startDown, winDown, topRight, topLeft, verPipe]
            connDown  = [startUp, winUp, botRight, botLeft, verPipe]

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

wonLevel :: Level -> Bool
wonLevel lvl@(Level cellsArr)
    | startingCellImg == startUp       = won lvl startingCellPos South startingCell
    | startingCellImg == startDown     = won lvl startingCellPos North startingCell
    | startingCellImg == startRight    = won lvl startingCellPos West startingCell
    | startingCellImg == startLeft     = won lvl startingCellPos East startingCell
        where
            maxX = fst $ snd $ A.bounds cellsArr
            maxY = snd $ snd $ A.bounds cellsArr
            findStartCell lvl@(Level cellsArr) pos@(x, y)
                | elem (img $ cellsArr A.! pos) startCells  = pos
                | y == maxY && x /= maxX                    = findStartCell lvl (x + 1, 0)
                | y == maxY && x == maxX                    = (-1, -1)
                | otherwise                                 = findStartCell lvl (x, y + 1)
            startingCellPos = findStartCell lvl (0, 0)
            startingCell    = cellsArr A.! startingCellPos
            startingCellImg = img $ startingCell
            won lvl@(Level cellsArrTmp) pos@(x, y) dir crtCell
                | elem (img crtCell) winningCells                                = True
                | dir == North && (check cellsArrTmp (x + 1, y) crtCell South)   = won lvl (x + 1, y) North (cellsArrTmp A.! (x + 1, y))
                | dir == North && (check cellsArrTmp (x, y - 1) crtCell West)    = won lvl (x, y - 1) East (cellsArrTmp A.! (x, y - 1))
                | dir == North && (check cellsArrTmp (x, y + 1) crtCell East)    = won lvl (x, y + 1) West (cellsArrTmp A.! (x, y + 1))
                | dir == South && (check cellsArrTmp (x - 1, y) crtCell North)   = won lvl (x - 1, y) South (cellsArrTmp A.! (x - 1, y))
                | dir == South && (check cellsArrTmp (x, y - 1) crtCell West)    = won lvl (x, y - 1) East (cellsArrTmp A.! (x, y - 1))
                | dir == South && (check cellsArrTmp (x, y + 1) crtCell East)    = won lvl (x, y + 1) West (cellsArrTmp A.! (x, y + 1))
                | dir == East && (check cellsArrTmp (x - 1, y) crtCell North)    = won lvl (x - 1, y) South (cellsArrTmp A.! (x - 1, y))
                | dir == East && (check cellsArrTmp (x + 1, y) crtCell South)    = won lvl (x + 1, y) North (cellsArrTmp A.! (x + 1, y))
                | dir == East && (check cellsArrTmp (x, y - 1) crtCell West)     = won lvl (x, y - 1) East (cellsArrTmp A.! (x, y - 1))
                | dir == West && (check cellsArrTmp (x - 1, y) crtCell North)    = won lvl (x - 1, y) South (cellsArrTmp A.! (x - 1, y))
                | dir == West && (check cellsArrTmp (x + 1, y) crtCell South)    = won lvl (x + 1, y) North (cellsArrTmp A.! (x + 1, y))
                | dir == West && (check cellsArrTmp (x, y + 1) crtCell East)     = won lvl (x, y + 1) West (cellsArrTmp A.! (x, y + 1))
                | otherwise                                                      = False
                where
                    check cellsArrTmp newPos@(newX, newY) thisCell thisDir = 
                        newX >= 0 && newX <= maxX && newY >= 0 && newY <= maxY 
                        && (connection crtCell (cellsArrTmp A.! newPos) thisDir)    
                    
instance ProblemState Level (Position, Directions) where
    successors :: Level -> [((Position, Directions), Level)]
    successors lvl@(Level cellsArr) = let maxX = fst $ snd $ A.bounds cellsArr
                                          maxY = snd $ snd $ A.bounds cellsArr
                                          in 
                                            [(((x, y), dir), (moveCell (x,y) dir lvl))
                                                | x <- [0 .. maxX], y <- [0 .. maxY], dir <- [North, South, West, East],
                                                (moveCell (x, y) dir lvl) /= lvl]

    isGoal :: Level -> Bool
    isGoal lvl = wonLevel lvl

    reverseAction :: ((Position, Directions), Level) -> ((Position, Directions), Level)
    reverseAction ((pos@(x, y), dir), lvl@(Level cellsArr))
        | (dir == North) = (((x - 1, y), South), lvl)
        | (dir == South) = (((x + 1, y), North), lvl)
        | (dir == East) = (((x, y + 1), West), lvl)
        | (dir == West) = (((x, y - 1), East), lvl)
        | otherwise = (((x, y), dir), lvl)

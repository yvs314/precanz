module ParseSOP
(
  getDsc
, readMx
, getDimT
, getName
, mkLytT
, prepPrcMx
, mkPcMx
, mkPairsList
, mkPrecPairOut
, mkIdxP
) where

import System.IO
import Data.List(intersperse,intercalate,transpose)
--import Data.Char(toUpper)
import Data.Array
--import System.Environment
--import GHC.IO.Handle

--no argument means eat stdin, 
--argument list interpreted as file names (no whitespace please)

-- getLine = hGetLine stdin
-- putStrLn = hPutStrLn stdout
-- print = hPrint stdout







--process :: String -> String
--process = map toUpper


-- %%%%%%%%%%DESCRIPTION%%%%%%%%%%%%%%%

-- | Get the TSPLIB-SOP description (all up to but not EDGE_WEIGHT_SECTION)
getDsc :: [String] -> [String]
getDsc = takeWhile (\s -> s/= "EDGE_WEIGHT_SECTION")

-- first string is NAME: $name
-- thus, just drop first six symbols
getName :: [String] -> String
getName ss = drop 6 $ head ss

-- %%%%%%%%%%%%%%%%%%%%%%COST%%%%%%%%%%%%%%%%%%%%%%%%%%%

-- | Carve out the lines representing the weight/prec matrix
-- | that is, skip everything up to EDGE_WEIGHT_SECTION,
-- | then drop two lines ("EDGE_WEIGHT_SECTION" and "$dim")
-- | then take all until "EOF"
 -- takeWhile (\s /="EOF") drop 2 . dropWhile (\s /="EDGE_WEIGHT_SECTION")
getMxStr :: [String] -> [String]
getMxStr = takeWhile (\s -> s/="EOF") . drop 2 . dropWhile (\s -> s/= "EDGE_WEIGHT_SECTION")


-- | read  the trimmed 'literal' matrix (see trimMxStr) as [[Int]];
-- | that's now ready to be output as costs,
-- | still, note that I find it improper to stick INF=1000000
-- | for the cost of going base to terminal; wouldn't precedence
-- | description "-1" suffice?
-- | trimMxStr tidies up the matrix' representation,
-- | that is, culls the useless whitespace
-- | and splits it into lines; done through wording 
readMx :: [String] -> [[Int]]
readMx = map (map read) . trimMxStr 
--trimMxStr :: [String] -> [[String]]
  where trimMxStr = map words . getMxStr

-- TODO: add cost output (intercalate "\n")
-- %%%%%%%%%%%END--COST%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-- %%%%%%%%%%%%%%%%%%%%%%%%LAYOUT%%%%%%%%%%%%%%

-- I think I'd mandate the terminal in my cost definition, it's simple to 
-- make it all zeroes
-- | T for TermCost (like TSPLIB-SOP), where a terminal is specified in addition to the base
-- | carve out the DIMENSION: $dim::Int line,
-- | read $dim as Int, 
-- | output $dim - 2 because neither the base nor the
-- | terminal function influence the dimension that much;
-- | make a note to write TERMCST in layout
getDimT :: [String] -> Int
getDimT =  (subtract 2). read . last . words . concat . filter (\s -> take 10 s == "DIMENSION:") 
--filter (\s -> (head $ words s) == "DIMENSION:")

-- | makes a TermCST layout; gets dimension from getDimT;
-- | all clusters have pop 1 because non-generalized TSP
-- | complete with "TERMCST\n"
--mkLytT :: [String] -> [String]
mkLytT ss = intercalate "\n" $ (show dim):(intersperse ' ' $ replicate dim '1'):["TERMCST"]
  where dim = getDimT ss


{-
The LAYOUT format is as follows:
$N\n --- the number of proper clusters (not counting either base or terminal)
$M_1 ... $M_N$\n --- the number of cities per cluster
OPEN|CLOSED|TERMCST 
--- OPEN start from base, free termination; 
--- CLOSED: back to base;
--- TERMCST finish at terminal; the last one is more for visualisation than sln 
-}


-- %%%%%%%%END--LAYOUT%%%%%%%%%

-- %%%%%%PRECEDENCE CONSTRAINTS%%%%%%%%%%
{-
Look, all readMx's edces are redundant precedence constraints-wise:
first row --- we know that base comes first, why stick it in
last row --- we know terminal is the last city we visit anyway
first column --- just prohibited (going from wherever to base)
last columnt --- going from wherever to the terminal is OK (except base-->trm)
thus, we'll first cut the first and last rows (init . tail)
then, cut the first and last out of the remaining rows 
(map init . tail)

now, everything else than (-1) is not about precedence 
(the INF==1000000 base to term is already cut away), so 0 it goes;
to make a convential order-like relation prec[i][j]==1  <=> i < j
from existing cost[i][j]==-1 meaning j goes before i (j is a predecessor of i,
we transpose the matrix (Data.List to the rescue), flip -1 to 1,
and nullify all other values (that is, we have successors)

-}
prepPrcMx :: [[Int]] -> [[Int]]
--prepPrcMx :: (Eq a, Num b, Num a) => [[a]] -> [[b]] --from GHCi
prepPrcMx rdmx =  transpose $ map (map nullify) (map (init . tail) $  (init . tail) rdmx)
  where nullify x 
          | x == -1 = 1
          | otherwise = 0


-- | prepare indices (i,j) to be zipped with precedence constraints'
-- | matrix (obtained through prepPrcMx. readMx).
-- | they go (1,1)-->(1,dim)
-- |          --------------
-- |         (dim,1)-->(dim,dim)
-- | where 'dim' is the number of the last proper
-- | city/cluster
mkIdxP dim = [(i,j) | i<-[1..dim], j<-[1..dim]]

-- | make an array from the Precedence Matrix
mkPcMx pp =  array ((1,1),(dim,dim)) (zip (mkIdxP dim) (concat pp))
  where dim = length pp

-- | from a precedence matrix, get the number of nontrivial 
-- | cities (square root of elements' count)

getPcMxDim :: (Integral c, Ix i) => Array i a -> c
getPcMxDim pss = floor.sqrt.fromIntegral $ length $ elems pss

--makes a pairs list out of a precedence matrix--array
mkPairsList pss = 
  let asc = assocs pss in 
  fst.unzip $ filter (\p -> snd p == 1) asc

mkPrecPairOut pss = intercalate "\n" $ (show pn ) : map show ps
  where 
    ps = mkPairsList pss
    pn = length ps



-- %%%%%%END--PRECEDENCE CONSTRAINTS%%%%%%%%%%


{-%%%%%%%%BIT%%%BUCKET%%%%%%%%%%%%-}

--main :: IO ()
--this stdin/out hookup code is due to ErikR from stackOverflow
--main = do
--  args <- getArgs
--  let (reader, writer) =
--        case args of
--          []         -> (getContents, putStr)
--          (path : _) -> let outpath = path ++ ".out"
--                        in (readFile path, writeFile outpath)
--  contents <- reader
--  writer (process contents)

--linp for "lined input"
--test routine; read a sample file, line it and
--output
--makes it easier to test
imp = do
  let t = readFile "ESC07.sop"
  inp <- t
  --return $ (mkPcMx (getDimT (lines inp)) (prepPrcMx $ readMx $ lines inp))
  return $ (mkPcMx (prepPrcMx $ readMx $ lines inp))
--limp <- imp




--main = do
--       args <- getArgs
--       if(not $ null args) then
--        do
--           print $ "working with "++ (head args)
--           finHandle <- openFile (head args) ReadMode --open the supplied input file
--           hDuplicateTo finHandle stdin --bind stdin to finName's handle
--           foutHandle <- openFile ((head args) ++ ".out") WriteMode --open the output file for writing
--           hDuplicateTo foutHandle stdout --bind stdout to the outgoing file
--           --inpStr <- readFile finName
--           --writeFile (finName ++ ".out") (map toUpper inpStr)
--           --return
--         --_ -> do interact ((++) "Here you go---\n" . map toUpper)--else just go for stdin/out
--       else print "working through stdin/redirect" --get to know 
--       interact ((++) "Here you go---\n" . map toUpper)
----I can finally shove everything into interact

--  Put the TSPLIB-SOP description (everything up to the weights/prec matrix),
--  i.e., NAME: ...\n...\n EDGE_WEIGHT_FORMAT: FULL_MATRIX\n 
--  in the output tuple's fst; put the remaining EDGE_WEIGHT_SECTION  into snd
--cutDsc :: [String] -> ([String],[String])
--cutDsc ss = span (\s -> s/="EDGE_WEIGHT_SECTION") ss

--older prepPrcMx, without transpose, hence, retaining the -1s
--prepPrcMx :: (Eq a, Num b, Num a) => [[a]] -> [[b]] --from GHCi
--prepPrcMx rdmx =   map (map nullify) (map (init . tail) $  (init . tail) rdmx)
--  where nullify x 
--          | x == -1 = -1
--          | otherwise = 0
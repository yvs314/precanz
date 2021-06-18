import ParseSOP
import PrecAnz

import System.Directory (listDirectory)

import Data.List(intersperse,intercalate)

import Text.Printf

{-
Vx :: Int --- for numbering and counting vertices,
i.e., the elements ordered

Output format discription: $Type denotes a String that expresses 
(--> show) the Type

NAME: $String
SUPPORT_SIZE: $Vx -- the number of vertices: also "n" below
PAIRS_COUNT: $Vx -- the number of pairs in the initial ordering
TRANSITIVE_REDUCTION_SIZE: $Vx -- the number of pairs in the pairs' transitive REDUCTION
TRANSITIVE_CLOSURE_SIZE: $Vx -- the number of pairs in the pairs' transitive CLOSURE
DENSITY: $Fractional -- TRS_CLOSURE_SIZE / (n)(n-1)/2: how does it relate to a total order (pairwise)
WIDTH: $Vx -- the maximum cardinality of antichains contained in SUPPORT

TRANSITIVE_REDUCTION_PAIRS: ($Vx, $Vx) ... ($Vx, $Vx)
-- pairs making up the TRS_RED: space-separated: terminated by NEWLINE


TRANSITIVE_CLOSURE_PAIRS:

MAX_BIPARTITE_PAIRS: ($Vx, $Vx) ... ($Vx, $Vx)
-- maximum cardinality matching for an auxiliary bipartite graph: for debugging width

-- to be continued
-}

imp = do
  let t = readFile "ESC07.sop"
  inp <- t
  --return $ (mkPcMx (getDimT (lines inp)) (prepPrcMx $ readMx $ lines inp))
  return $ (prepPrcMx $ readMx $ lines inp ::Rll)
--limp <- imp

pimp = do
  let t = readFile "ESC07.sop"
  inp <- t
  --return $ (mkPcMx (getDimT (lines inp)) (prepPrcMx $ readMx $ lines inp))
  return $ (mkPcMx (prepPrcMx $ readMx $ lines inp) ::Rmx)

  -- listDirectory "."

{-
listDirectory "."
["wat2","wat","processSOP.hs","precAnz.hs.bak","precAnz.hs","parseSOP.o","parseSOP.hs.bak","parseSOP.hs","parseSOP.hi","parse1.o","parse1.hs","parse1.hi","ESC11.sop","ESC07.sop.out","ESC07.sop"]

--> filter (\s -> if (inverse.(take 4).inverse s)==".sop" then True else False)
-}

-- test if the extension is .sop
sopTest = (\s -> if ((reverse.(take 4).reverse $ s)==".sop") then True else False)

{-
process :: FilePath -> IO String -> [String]
(actually, FilePath -> [String])
process path =
  do wat <- readFile path
     sopAnz wat
-}

-- | read a .sop file and analyze (light output) it
processLt:: FilePath -> IO [String]
processLt ifName = do
  wat <- readFile ifName
  return (sopAnzLt wat)

-- | read a .sop file and analyze it (full output)
process:: FilePath -> IO [String]
process ifName = do
  wat <- readFile ifName
  return (sopAnz wat)

-- | write analysis' results supplied as filename::String, outss::[String]
-- | slightly reworked writeFile
dumpResults :: (FilePath, [String]) -> IO ()
dumpResults (name, outss) = writeFile (name ++ ".anz") (unlines outss)


{-
aggregated output format:
NAME:SUPPORT_SIZE:PAIRS_COUNT:TRANSITIVE_REDUCTION_SIZE:TRANSITIVE_CLOSURE_SIZE:DENSITY:WIDTH

-}

-- | dump results into $name.sop.anz
-- | compile a single .csv-like dir.out.csv 

collectOutLt ::[String] -> String
collectOutLt ss = intercalate ":"  (map wat ss)  
  where wat = tail . (dropWhile (\c -> c/=' '))

mkCsvOut outs = unlines $ header : (map collectOutLt outs)
  where header = "NAME:SUPPORT_SIZE:PAIRS_COUNT:TRANSITIVE_REDUCTION_SIZE:TRANSITIVE_CLOSURE_SIZE:DENSITY:WIDTH"

main = do
    files <- listDirectory "."
    let todo = filter sopTest files
    putStrLn "Found the following .sop files: "
    print todo
    putStrLn "Processing started"
    --out <- (processLt.head $ todo)
    outLt <- mapM processLt todo
    out <- mapM process todo
    let namedOutLt = zip todo outLt
    let namedOut = zip todo out
    writeFile "precAnz.csv" (mkCsvOut outLt)
    mapM dumpResults namedOut
    --dumpResults 
    print $ namedOutLt
    return namedOutLt

--todo=["ry48p.4.sop","ry48p.3.sop","ry48p.2.sop","ry48p.1.sop","rbg378a.sop","rbg358a.sop","rbg341a.sop","rbg323a.sop","rbg253a.sop","rbg174a.sop","rbg150a.sop","rbg109a.sop","rbg050c.sop","rbg048a.sop","prob.42.sop","prob.100.sop","p43.4.sop","p43.3.sop","p43.2.sop","p43.1.sop","kro124p.4.sop","kro124p.3.sop","kro124p.2.sop","kro124p.1.sop","ft70.4.sop","ft70.3.sop","ft70.2.sop","ft70.1.sop","ft53.4.sop","ft53.3.sop","ft53.2.sop","ft53.1.sop","ESC78.sop","ESC63.sop","ESC47.sop","ESC25.sop","ESC12.sop","ESC11.sop","ESC07.sop","br17.12.sop","br17.10.sop"]

-- | without "heavy" information --- no pairs' listings, etc.
sopAnzLt:: String -> [String]
sopAnzLt = (take 7).sopAnz 

-- from raw file input to output as described: doesn't know or care about the ifName
sopAnz:: String -> [String]
sopAnz inp = 
-- "s" before a name denotes a String (sx is ~showed x)
    let linp = lines inp
        rmx = extractRmx inp
        dim = getDimT linp -- or 
        sdim = show dim
        
        rsl = rmx2rslist rmx
        rplist = rmx2rplist rmx
        
        spct = show $ length rplist
        ti = trsInfo rsl
        sRedCt = show $ length $ fst ti
        clsCt = length $ snd ti
        sClsCt = show $ clsCt
        density::Float -- for resolving the printf
        density = (fromIntegral clsCt) / (0.5* (fromIntegral dim)*(fromIntegral (dim-1)))
        ds = printf "%.2f" density  -- show density
        wi = widthInfo dim $ rslist2rplist rsl
        sw = show $ snd.fst $ wi
        
--(getName linp):("SUPPORT_SIZE: " ++ sdim):("TRANSITIVE_REDUCTION_SIZE"):("WIDTH: "++ sw):[]
    in ["NAME: "++ getName linp
        ,"SUPPORT_SIZE: " ++ sdim
        ,"PAIRS_COUNT: "  ++ spct
        ,"TRANSITIVE_REDUCTION_SIZE: " ++ sRedCt 
        ,"TRANSITIVE_CLOSURE_SIZE: " ++ sClsCt
        ,"DENSITY: "++ ds
        ,"WIDTH: "++ sw
        ,"TRANSITIVE_REDUCTION_PAIRS: " ++ show (fst ti)
        ,"TRANSITIVE_CLOSURE_PAIRS: " ++ show (snd ti)]

--process ifName = do
--    let reader = readFile ifName
--    inp <- reader
--process is


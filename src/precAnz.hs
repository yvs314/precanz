
{-# LANGUAGE ConstrainedClassMethods #-}
module PrecAnz
(
  widthInfo
, trsInfo
, rslTcl
, rslRed
, extractRmx
, rmx2rslist
, rmx2rll
, rmx2rplist
, rslist2rplist
, rslist2rmx
, rslist2rll
, rplist2rslist
, rplist2rmx
, Vx
, Rll
, Rmx
, Rplist
, Rslist 

) where 

import ParseSOP
-- System.IO not really required
-- nod really import System.IO 
import Data.List
import Data.Array
-- nod really import System.Environment
import Data.Graph.MaxBipartiteMatching
-- last two for MaxBiparitteMatching
import qualified Data.Map as M
import qualified Data.Set as S



{-
Precedence Constraints Analyzer mod. 2
2017-Spring.

I. rewrite: Poset typeclass, to unify
Rmx, Rslist, etc. (its instances)

The class should
a. report its support's cardinality 
aa. 
b. provide prec Vx -> [Vx] and succ Vx -> [Vx]
NB: might replace [Vx] with a bitset or something --- if ordinary lists choke on SOPLIB (600)
c. provide min [Vx] -> [Vx] and max (in view of this Poset)
d. generators?

II.bis: width and ideal count inequalities
II.bis.anc: Dilworth chain decomposition

III. Various generators (bruteforce counting)
IV. Schrage--Baker "hash"
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

{- THE TYPES -}
{-
look, cities are indexed by natural numbers,
1..n; 0 is always the base, there may be a terminal n+1;
n is the dimension of the problem (the number of PROPER cities)

just in case we want to report something strange,
let the city (vertex or point) be numbered by Int;

actually, any Bounded Eq should suffice, though default
order would help too

-}

-- | unification for Rplist (ordered pairs list), Rslist (succ list), Rmx (matrix) types
class PosetRep a where
    -- | support set cardinality; how many (proper) cities are there
    suppSize :: Integral a => a
    -- | predecessors list
    predList :: a -> Vx -> [Vx] -- aka principal ideal
    -- | successors list
    succList :: a -> Vx -> [Vx] -- aka principal filter
    
    


type Vx = Int -- Vx for vertex, e.g., a point or a city
{- We deal partial order relations here, 
implicitly assumed to be <-relations,
and use three their representations:-}

-- | 'address pairs list'; (a,b) means a < b, or a 'sends' to b
type Rplist = [(Vx,Vx)] -- list of 'address pairs'



-- | "(a,b)" to (a,b)::(Vx,Vx), nothing else expected
readRpair :: String -> (Vx,Vx)
readRpair = read

-- | from "(a,b) (c,d) ..." read all address pairs
readRplist :: String -> Rplist
readRplist ps = map readRpair (words ps)

-- | increase by 1 the city numbers; Aleksei Grigor'ev uses indices 0..n-1
-- | I use 1..n, thus the need for this operation
incRplist :: Rplist -> Rplist
incRplist = map inc1
  where inc1::(Vx,Vx) -> (Vx,Vx)
        inc1 (a,b) = (a+1,b+1)

-- | read A.Gr's address pairs, 
-- | return trans red as the first component, trans cl as second
--process dim iap = [wat, rslRed wat, rslTcl wat]
--  where wat = rplist2rslist dim (incRplist.readRplist $ iap)

--countLen dim iap = map length $ map rslist2rplist (process dim iap)

---- | process' is for my pairs format, without inc/dec (I don't use city 0)
--process' dim iap = [wat, rslRed wat, rslTcl wat]
--  where wat = rplist2rslist dim (readRplist $ iap)

--countLen' dim iap = map length $ map rslist2rplist (process' dim iap)



--let wwat = [p | p<- (wat !! 0), not $ elem p (wat !! 1)]
--let wat = map decRplist (map rslist2rplist $ process 45 pcs1)
-- | density is the trans.closure's cardinality divided by 
-- | the comparability relation's cardinality
--let den = (length $ wat !! 2 ) / (dim*(dim-1)*0.5)


-- | companion function to liftRplist; return indices to Aleksei's 0..n-1
-- | from my 1..n
decRplist :: Rplist -> Rplist
decRplist  = map dec1
  where dec1 :: (Vx,Vx) -> (Vx,Vx)
        dec1 (a,b) = (a-1,b-1)

--sortRplist 

-- | make a successor list from pairs list; dimension must be specified
--rplist2rslist :: Rplist -> Vx -> Rslist

-- | Rll!!(a-1)!!(b-1)==1 means a < b
-- | this is precedence matrix in the list of lists form;
-- | mostly indended for IO, not always convenient
type Rll=[[Int]] 

-- | 'successor list' form; for a vertex, lists its successors
-- | that is, x --> [y | x<y]
type Rslist = Array Vx [Vx]

-- | 'successor matrix', i.e., <-matrix; 
-- | Rmx !(a,b)==1 means a<b; I might adopt Bool or add
-- | toEnum etc 
type Rmx = Array (Vx,Vx) Int

-- extracts precedence constraints in Rmx form from raw .sop input
-- with functions from parseSOP
extractRmx:: String -> Rmx
extractRmx s = mkPcMx (prepPrcMx $ readMx $ lines s):: Rmx

{----RELATION--FORMAT---CONVERSION----}

-- | strips the indices while 'unflattening' the elems list
rmx2rll :: Rmx -> Rll
rmx2rll rmx = aux $ elems rmx
  where aux [] = []
        aux es = take dim es: aux(drop dim es)
          where dim = snd . snd $ bounds rmx

rll2rmx :: Rll -> Rmx
rll2rmx = mkPcMx 

-- | imported from parseSOP
rmx2rplist :: Rmx -> Rplist
rmx2rplist  = mkPairsList

rmx2rslist :: Rmx -> Rslist
rmx2rslist rmx = array (1,dim) (zip [1..] (map binLst2num (rmx2rll rmx)) )
  where dim = snd . snd $ bounds rmx

rslist2rll :: Rslist -> Rll
rslist2rll rel = map (num2binLst dim) (snd . unzip $ assocs rel)
  where dim = snd $ bounds rel

rslist2rmx :: Rslist -> Rmx
rslist2rmx = rll2rmx . rslist2rll

rslist2rplist :: Rslist -> Rplist
rslist2rplist = rmx2rplist.rslist2rmx

-- | no inherent dimension parameter in the pairs' list
-- | therefore, must be supplied externally
-- | uses dumb filtering to test if a matrix' entry should be 1 or 0
rplist2rmx :: Vx -> Rplist -> Rmx
rplist2rmx dim ps = array ((1,1),(dim,dim)) [ ((i,j), test (i,j) ) | (i,j)<- mkIdxP dim]
  where test :: (Vx,Vx) -> Vx
        test (i,j) = case elem (i,j) ps of True -> 1
                                           False -> 0

rplist2rslist :: Vx -> Rplist -> Rslist
rplist2rslist dim ps = rmx2rslist (rplist2rmx dim ps)

-- | from a 'binary' input list like [0,1,0,1,0]
-- | make a list of numbers --> [2,4]
-- | first entry is understood as 1 (universum = [1..length input])
binLst2num :: [Int] -> [Vx]
binLst2num xs = [i | (1,i) <- (zip xs [1..])]

-- | premature optimization: keep the vs' state, remove those added
num2binLst :: Vx -> [Vx] -> [Int]
num2binLst dim vs = map ( fromEnum . (flip $ elem) vs) [1..dim]

-- | complement relation:: flip all 1 to 0;
-- | set-theoretic complement for the corresponding Cartesian product
rmxC :: Rmx -> Rmx
rmxC mx = array (bounds mx) [(fst e, flp $ snd e) | e<- assocs mx]
  where flp 0 = 1
        flp 1 = 0
        flp _ = error "not 0-1 entry"

-- | rmx-derived Rslist complement
rslC :: Rslist -> Rslist
rslC rel = rmx2rslist ( rmxC (rslist2rmx rel))

-- | Rslist union; note that operands are assumed to be of the same length
rslUnion :: Rslist -> Rslist -> Rslist
rslUnion rl rr = array (bounds rl) ( zipWith lcup (assocs rl) (assocs rr))
  where lcup :: (Vx,[Vx]) -> (Vx, [Vx]) -> (Vx, [Vx])
        lcup (v,vs) (u,us) = (v, cup vs us)

-- | Rslist intersection; note that operands are assumed to be of the same length
rslIsect :: Rslist -> Rslist -> Rslist
rslIsect rl rr = array (bounds rl) ( zipWith lcap (assocs rl) (assocs rr))
  where lcap :: (Vx,[Vx]) -> (Vx, [Vx]) -> (Vx, [Vx])
        lcap (v,vs) (u,us) = (v, isect vs us)

-- | sortedList difference; code stolen from Data.List.Ordered
mnus [] _ys = []
mnus xs [] = xs
mnus (x:xs) (y:ys)
       = case compare x y of
          LT -> x : mnus xs (y:ys)
          EQ ->     mnus xs (y:ys)
          GT ->     mnus (x:xs) ys

-- |sortedList intersection; code stolen from Data.List.Ordered
isect [] _ =  []
isect _ [] = []
isect (x:xs) (y:ys) = 
  case compare x y of  LT ->     isect xs (y:ys)
                       EQ -> x : isect xs ys
                       GT ->     isect (x:xs) ys

-- | function (relation) composition
-- | F \subsetof X\times X, G \subsetof X\times X
-- | --> F . G = \{(x,z) | xFy \wedge yFz \}
rslComp :: Rslist -> Rslist -> Rslist
rslComp lss rss = array (bounds lss) (zip idx [foldr cup [] [rss!i | i <- lss!x ]| x <- idx ])
  where idx = range $ bounds lss

-- | shorthand for squaring a relation (compose \w itself)
rslSqr :: Rslist -> Rslist
rslSqr rel = rslComp rel rel

-- | finally, transitive reduction
rslRed :: Rslist -> Rslist
rslRed rel = rslIsect trel (rslC.rslSqr $ trel)
  where trel = rslTcl rel

-- | list successors of x in the relation rel
--scc :: Int -> Array Int [Int] -> [Int]
scc :: Vx -> Rslist -> [Vx]
scc x rel = rel ! x

-- | list predecessors of x in the relation rel
prd :: Vx -> Rslist -> [Vx]--Int -> Array Int [Int] -> [Int]
prd x rel = [y | y<-[1..dim] , elem x (scc y rel)]
  where dim = snd $ bounds rel
--prd x rel = filter (\y -> elem x (scc y rel)) [1..7]
  --where dim = length rel

-- %%%%SAMPLES%%%%%%%

zap :: Rslist--Array Int [Int]
zap = array (1,4) ([(1,[2]),(2,[3]),(3,[4]),(4,[])])

wap :: Rslist
wap = array (1,4) ([(1,[]),(2,[1]),(3,[]),(4,[1,2])])

pcs1="(5,22) (39,23) (11,27) (6,44) (9,10) (30,13) (25,11) (8,42) (39,33) (41,44) (33,6) (44,4) (30,14) (21,44) (40,2) (29,41) (15,27) (25,41) (19,22) (22,0) (22,30) (41,19) (16,6) (13,23) (9,20) (41,30) (20,5) (25,26) (41,18) (36,3) (7,16) (21,19) (29,16) (31,17) (13,16)"
pcs2="(4,39) (17,15) (7,8) (14,6) (32,12) (42,6) (22,34) (26,13) (13,42) (23,2) (36,44) (34,3) (16,44) (9,12) (34,29) (38,26) (15,21) (44,18) (27,11) (33,30) (8,35) (22,31) (6,19) (6,20) (9,39) (35,5) (43,33) (37,32) (15,1) (3,28) (15,27) (7,14) (5,27) (24,17) (20,39)"

pcs0="(1,10) (12,2) (2,13) (13,15) (6,16) (15,16) (18,27) (9,27) (10,9) (11,19) (20,19) (25,26) (23,22) (21,20) (24,22) (14,16) (7,10) (8,2) (1,9) (14,26) (2,27) (3,6) (3,19) (18,17) (14,25)"


pcs00="(1,2) (1,3)"
pcs01="(1,2) (1,3) (2,3)"

pl00 = readRplist pcs01
pl07:: Rplist
pl07 = [(1,4),(1,5),(1,6),(1,7),(4,5),(6,5),(7,5)]

meh0::Rplist
meh0=[(1,10),(12,2),(2,13),(13,15),(6,16),(15,16),(18,27),(9,27),(10,9),(11,19),(20,19),(25,26),(23,22),(21,20),(24,22),(14,16),(7,10),(8,2),(1,9),(14,26),(2,27),(3,6),(3,19),(18,17),(14,25)]
pl31::Rplist
pl31=[(1,10),(12,2),(2,13),(13,15),(6,16),(15,16),(18,27),(9,27),(10,9),(11,19),(20,19),(25,26),(23,22),(21,20),(24,22),(14,16),(7,10),(8,2),(1,9),(14,26),(2,27),(3,6),(3,19),(18,17),(14,25),(28,3),(29,5),(30,21),(7,27),(18,9),(31,9),(5,18),(5,9),(5,31)]
pl30::Rplist
pl30=[(1,10),(12,2),(2,13),(13,15),(6,16),(15,16),(18,27),(9,27),(10,9),(11,19),(20,19),(25,26),(23,22),(21,20),(24,22),(14,16),(7,10),(8,2),(1,9),(14,26),(2,27),(3,6),(3,19),(18,17),(14,25),(28,3),(29,5),(30,21),(7,27),(18,9)]
-- %%%%END--SAMPLES%%%

-- %%%WIDTH%%%%%%%%
{-
DESCRIPTION:

we find the width of an order P=(1..n,<)
 as in (Steiner 1990):
through max bipartite matching in 
an auxiliary bipartite graph (L,R,E),
where 
L=1..n are the vertices
R=1'..n' are  their duplicates, in fact, numbered n+1..2n
E=\{e=(i,j')\in L\times R \big| i<j  \}
i.e., a prime is affixed to the second component of 
every pair in the relation (<)

then, the width (so sayeth (Steiner 1990)) is 
$n - |\operatorname{maxBipartiteMatching}(L,R,E)|$


-}

-- | sort of duplicate the pairs: 
-- | prime the second component; made by adding 
-- | the dim, so (1,2') will look like (1, 2+n)
primeRplist :: Vx -> Rplist -> Rplist
primeRplist n ps = map prmPair ps
  where prmPair (a,b) = (a,b+n)

-- | the reverse, to have a look at the result later
unprimeRplist :: Vx -> Rplist -> Rplist
unprimeRplist n ps = map uprmPair ps
  where uprmPair (a,b) = (a,b-n)

-- | unprime while reversing the components
-- | that's because the matching returns them inversed
iunprimeRplist :: Vx -> Rplist -> Rplist
iunprimeRplist n ps = map iuprmPair ps
  where iuprmPair (b, a) = (a, b-n)

-- | take the dimension and the pairs
-- | return the pairs forming the maximum cardinality
-- | bipartite matching (Rplist: processed, unprimed; M.Map Vx Vx --- 'raw' matching output)
-- | the second coordinate of the triple is the width
-- | $ width = n - |MaxCardinalityMatching|$
widthInfo :: Vx -> Rplist -> ((Rplist, Int), M.Map Vx Vx )
widthInfo n ps = ((out, width),raw)
  where raw = matching (S.fromList (primeRplist n ps)  )
        out = iunprimeRplist n (M.toAscList $ raw )
        width = n - (length out)

-- return input's (transRed, transCls)
trsInfo :: Rslist -> (Rplist, Rplist)
trsInfo rel = (rslist2rplist $ rslRed rel, rslist2rplist $ rslTcl rel)

-- %%%%END--WIDTH%%%%%%

-- 


-- | Roy--Warshall operation:: x > y ==> z > x --> z > y
-- | i.e., for all y <- prd x rel ADD scc x rel TO scc y rel
--rW :: Array Int [Int] -> Int -> Array Int [Int]
rW :: Rslist -> Vx -> Rslist
rW rel x = rel // [ (p,  cup (scc p rel) (scc x rel) ) | p<- prd x rel]
  
-- |transitive closure through Roy--Warshall operation
-- |that is, consecutively apply it to the input relation for all points
--rslTcl :: Array Int [Int] -> Array Int [Int]
rslTcl :: Rslist -> Rslist
rslTcl rel = foldl' rW rel [1..dim]
  where dim = snd $ bounds rel

-- |sortedList union with duplicate removal on merging
-- |note that duplicates in input would remain
cup [] ys = ys
cup xs [] = xs
cup (x:xs) (y:ys) = 
  case compare x y of EQ -> x : cup xs ys
                      LT -> x : cup xs (y:ys)
                      GT -> y : cup (x:xs) ys

{-%%%%%%%%BIT%%%BUCKET%%%%%%%%%%%%-}

{- %%%%DUMB%%%COPY%%%% -}
{-

vertices :: Relation -> VertexSet
vertices rs = [0..length rs - 1]

transcl :: Relation -> Relation
--transcl rs = warsh rs (vertices rs)
transcl rs = warsh (map (nub.sort) rs) (vertices rs)

warsh :: Relation -> VertexSet -> Relation
warsh rs [] = rs
warsh rs (e:vs) =
  let ss = warsh rs vs
  in [if elem e ms then cup ms (ss!!e) else ms | ms <- ss]

cup :: VertexSet -> VertexSet -> VertexSet
cup [] ys = ys
cup xs [] = xs
cup (x:xs) (y:ys) = 
  case compare x y of EQ -> x : cup xs ys
                      LT -> x : cup xs (y:ys)
                      GT -> y : cup (x:xs) ys
-}




-- | plain list of successors; head lists successors of 1, 
-- | then etc;
-- | note : we take prepPrcMx output, it is a <-matrix ('successor list'),
-- | thus, no need to transpose (original *.sop features a 'predecessor list')
--prepSuccLst :: [[Int]] -> [[Int]]
--prepSuccLst input = map binLst2num (transpose $ prepPrcMx input)
--prepSuccLst prcMx = map binLst2num  prcMx

-- | make an array of successor list; [(v,[v])]
--mkSuccList :: [[Int]] -> Array Int [Int]
--mkSuccList prcMx = array (1, dim) (zip [1..] (prepSuccLst prcMx))
--  where dim = length prcMx
--        prepSuccLst = map binLst2num


--no argument means eat stdin, 
--argument list interpreted as file names (no whitespace please)

-- getLine = hGetLine stdin
-- putStrLn = hPutStrLn stdout
-- print = hPrint stdout

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


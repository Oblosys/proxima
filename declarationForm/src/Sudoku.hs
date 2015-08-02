{-# OPTIONS_GHC -fglasgow-exts -XMonomorphismRestriction #-}
{- By Chris Kuklewicz <haskell@list.mightyreason.com> -}
module Sudoku where

{- This has 3 methods that perform deductions:

ruleSubsetQ looks at the ways constraints can be satisfied along a
row/column/block and looks for subsets of length 1,2,3.. which are
"self-contained" and removes those possibilites from the other
locations.  For length 1, this the typical deductive propagation.

ruleBlockP looks for single "locked" rows/cols in single blocks and looks for pairs
of locked rows/cols in a pair of blocks.

inference considers the subsets of contraints which are "strong links"
with exactly two ways to be satisfied.  These interact in simple ways
and first a contradition among them is looked for so that one or more
"strong links" can be decided.  Then other locations which are implied
off by both options of a "strong link" are looked for and these weak
hints are returned.

The evolve method applies the above looking for the first group of new
hints to apply for recursing.  The solve method calls evolve and when
stuck will employ guessing/branching (depth-first).  A count of
guesses is kept, and a trail of the correct path.

The exported deduce method is an interface to solve that hides the
data representation of this module.  And the exported oneStep is hack
for only evolving once.  The only details the calling module
uses/needs are the values of "lo" and "hi" which are also exported.

Where in the gray scale of logic to brute force do the methods lie?
The length 1 inference is obviously just recognizing when a location
in the puzzle has been solved.  The ruleBlockP for single blocks is a
similar propagation.  The higher length variants of these (pairs of
blocks and subset of 2 to 8 elements) are straightforward to compute
within the same methods. [ The separate code for killOne is a
performance tweak, and used to be folded into the general code. ] The
inference method(s) came last, from looking at the solver at
http://www.stolaf.edu/people/hansonr/sudoku/index.htm and comparing to
a friend, since both could see additional an additional move without
the full guess/check effort.  The actual implementation of "inference"
was done without seeing any code from that website, just by reading
some of the explanations and looking at the "chain table" it produced.
The methods itself essentially identifies the easy to reason about
2-valued "strong links" and looks for possibilities that lead to
contradictions among them and for possibilities implied by either
value of a "strong link".  These are computed in a straightforward way
(literally meaning it is not a backtracking search).  This lack of
branching  leads me classify the method as logic and not brute-force.

That same web site also constructs more elaborate moves via
"hypothesis" methods.  These seem, at a quick glance, to be "guess and
check with only some of the deductive tools" approaches which try to
elimitate possibilities more efficiently than depth-first guessing.
As such they seem to be more educated instead of brute force, and more
breadth-first instead of depth-first.  I have read posts describing
how limited-depth breadth-first approaches are extremely powerful, and
they seem related.  They all derive information from speculative
look-ahead, and hold a middle-ground between straightforward logic and
brute-force.  None of these have been added to this program.


-}

import Control.Monad.ST(runST, ST)
import Data.Array.Base(MArray(unsafeWrite, unsafeRead))
import Data.Array.IArray(ixmap, accumArray, assocs, (!), listArray)
import Data.Array.MArray(thaw, getAssocs,MArray(newArray))
import Data.Array.Unsafe(unsafeThaw, unsafeFreeze)
import Data.Array.IO(IOUArray)
import Data.Array.ST(STUArray)
import Data.Array.Unboxed(UArray)
import Data.Char(digitToInt, intToDigit)
import Data.Either(Either)
import Data.Ix(Ix(inRange, range))
import Data.List((\\), sortBy, sort, groupBy, transpose, delete)
import qualified Data.Map as Map (fromAscList,fromListWith, findWithDefault, Map)
import Data.Maybe(Maybe)
import Data.STRef(modifySTRef, writeSTRef, readSTRef, newSTRef)
import Control.Monad(liftM, guard)
import System.IO.Unsafe(unsafePerformIO)
import Control.Exception

default ()

{- Avoid bounds checking, prefer uncurry'd write -}



sudokuHint :: String -> Int -> Int -> Maybe Int
sudokuHint s r c = unsafePerformIO $
 do let board = parseBoard s
    (p',guesses,solStr) <- deduce board
    print s
    print solStr
    let sol = parseBoard solStr -- row & col start at 1 in hints
        solMap = [ ((r1-1,c1-1),v) | (r1,c1,v) <- sol ]
    return $ lookup (r,c) solMap
 `Control.Exception.catch` \(exc :: SomeException) -> return Nothing
                 
loC = intToDigit lo
hiC = intToDigit hi

parseBoard s = map toHint justSet
  where rcs = [ (r,c) | r <- range (lo,hi), c <- range (lo,hi) ]
        isHint vC = inRange (loC,hiC) vC
        justSet = filter (isHint . snd) (zip rcs s)
        toHint ((r,c),vC) = (r,c,digitToInt vC)



ircv :: Index -> Int
{-# INLINE ircv #-}
ircv (R r, C c, V v) = (v+9*(c+9*r)-91)

writeArray' :: MArray a e m => a Index e ->  (Index,e) -> m ()
{-# INLINE writeArray' #-}
writeArray' a (rcv,e) = unsafeWrite a (ircv rcv) e >> return ()

readArray' :: MArray a e m => a Index e -> Index -> m e
{-# INLINE readArray' #-}
readArray' a rcv = unsafeRead a (ircv rcv)

{- Typesafe values for indices -}

type E = Int
newtype R = R E deriving (Eq,Ord,Ix,Enum,Show) -- Row index
newtype C = C E deriving (Eq,Ord,Ix,Enum,Show) -- Column index
newtype V = V E deriving (Eq,Ord,Ix,Enum,Show) -- Value index
newtype B = B E deriving (Eq,Ord,Ix,Enum,Show) -- 3x3 Block index
newtype D = D E deriving (Eq,Ord,Ix,Enum,Show) -- Inside 3x3 Block index

rcToBD :: R -> C -> (B,D)
{-# INLINE rcToBD #-}
rcToBD (R r) (C c) = let (rq,rm) = quotRem (r-lo) 3;
                         (cq,cm) = quotRem (c-lo) 3
                         b = lo + ( 3*rq + cq )
                         d = lo + ( 3*rm + cm )
                     in (B b, D d)

bdToRC :: B -> D -> (R,C)
{-# INLINE bdToRC #-}
bdToRC (B b) (D d) = let (bq,bm) = quotRem (b-lo) 3
                         (dq,dm) = quotRem (d-lo) 3
                         r = lo + ( 3*bq + dq )
                         c = lo + ( 3*bm + dm )
                     in (R r, C c)

{- Polymorphic sources of indices -}

lo,hi :: (Enum a) => a
{-# INLINE lo #-}
{-# INLINE hi #-}
lo = toEnum 1
hi = toEnum 9

fullRange :: (Enum a,Ix a) => [a]
{-# INLINE fullRange #-}
fullRange = range (lo,hi)

{- Typeclasses and Data for "shuffle" and "unshuffle" -}

class (Show x, Ix x, Enum x, Ord x) => IE x
instance IE R; instance IE C; instance IE V; instance IE B; instance IE D

type ABC a b c =  [[ (Int,(a,b,[c])) ]] -- assert Int == length [c]

-- Reify the types "a b c" to a value of type Perms
class (IE a, IE b, IE c) => Perm a b c where
  shuffle :: (R,C,V) -> (a,b,c)
  unshuffle :: (a,b,c) -> (R,C,V)
  sAsBgC ::  Cube -> ABC a b c
  {-# INLINE sAsBgC #-}
  sAsBgC cube = do
    a <- fullRange :: [a]
    return ( sortWith fst ( do
      b <- fullRange :: [b]
      let cs = [ c | c <- fullRange :: [c], on==cube!(unshuffle (a,b,c)) ]
      return (length cs,(a,b,cs)) ) )

instance Perm R C V where
  shuffle = id
  unshuffle = id

instance Perm R V C where
  shuffle (r,c,v) = (r,v,c)
  unshuffle (r,v,c) = (r,c,v)

instance Perm C V R where
  shuffle (r,c,v) = (c,v,r)
  unshuffle (c,v,r) = (r,c,v)

instance Perm C R V where
  shuffle (r,c,v) = (c,r,v)
  unshuffle (c,r,v) = (r,c,v)

instance Perm V R C where
  shuffle (r,c,v) = (v,r,c)
  unshuffle (v,r,c) = (r,c,v)

instance Perm V C R where
  shuffle (r,c,v) = (v,c,r)
  unshuffle (v,c,r) = (r,c,v)

-- Special case
instance Perm B D V where
  shuffle (r,c,v) = let (b,d) = rcToBD r c in (b,d,v)
  unshuffle (b,d,v) = let (r,c) = bdToRC b d in (r,c,v)

instance Perm V B D where
  shuffle (r,c,v) = let (b,d) = rcToBD r c in (v,b,d)
  unshuffle (v,b,d) = let (r,c) = bdToRC b d in (r,c,v)

{- Main data structure, Array types, values and functions -}

type Index = (R,C,V)
type Cell = Bool
type Hints = [(Index,Cell)]

on,off :: Cell
on = True        -- Means this might be part of the solution
off = False      -- Means this cannot be part of the solution

boundsCube :: (Perm a b c) => ((a,b,c),(a,b,c))
{-# INLINE boundsCube #-}
boundsCube = ((lo,lo,lo),(hi,hi,hi))

type Cube = UArray Index Cell
type MCube = IOUArray Index Cell
makeEmptyCube :: IO MCube
makeEmptyCube = newArray boundsCube on
badCube :: Cube
badCube = accumArray const off boundsCube []
toConst :: MCube -> IO Cube  -- inplace
toConst = unsafeFreeze
toMut :: Cube -> IO MCube  -- inplace
toMut = unsafeThaw
toMutCopy :: Cube -> IO MCube -- makes a copy
toMutCopy = thaw

type View a b c = UArray (a,b,c) Cell
{-# INLINE view #-}
view :: (Perm a b c) => Cube -> View a b c
view cube = ixmap boundsCube unshuffle cube

{- Utility functions -}

fst3 (x,_,_) = x; snd3 (_,x,_) = x; thd3 (_,_,x) = x
fst4 (x,_,_,_) = x; snd4 (_,x,_,_) = x; thd4 (_,_,x,_) = x; fth4 (_,_,_,x) = x

{-# INLINE by #-}
by un bi = (\ left right -> (un left) `bi` (un right))
{-# INLINE sortWith #-}
sortWith un = sortBy (by un compare)
{-# INLINE groupWith #-}
groupWith un = groupBy (by un (==))
{-# INLINE groupSort #-}
groupSort un = groupWith un . sortWith un

atLeastOne = not . null
atLeastTwo (_:_:_) = True;     atLeastTwo _ = False
atLeastThree (_:_:_:_) = True; atLeastThree _ = False
exactlyOne [_] = True;         exactlyOne _ = False
exactlyTwo [_,_] = True;       exactlyTwo _ = False
oneOrTwo [_] = True;           oneOrTwo [_,_] = True; oneOrTwo _ = False

-- All subsets of length 'k', order is stable
subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets k (x:xs) = (fmap (x:) (subsets (pred k) xs)) ++ subsets k (xs)

-- Hopefully efficient merge a (list of (sorted lists)), unique values only
combine :: (Ord a) => [[a]] -> [a]
combine [] = []
combine [x] = x
combine xs = let (a,b) = split xs
             in merge (combine a) (combine b)
  where split [] = ([],[])
        split [a] = ([a],[])
        split (x:y:cs) = let (a,b) = split cs in (x:a,y:b)

        merge a [] = a
        merge [] b = b
        merge a@(x:a') b@(y:b') = case compare x y of
          EQ -> x : merge a' b'
          LT -> x : merge a' b
          GT -> y : merge a  b'


{- List operations that assume sorted input -}

sElem e [] = False
sElem e (x:xs) = case compare e x of
                   LT -> False
                   EQ -> True
                   GT -> sElem e xs

sNotElem e [] = True
sNotElem e (x:xs) = case compare e x of
                      LT -> True
                      EQ -> False
                      GT -> sNotElem e xs

sInter allX@(x:xs) allY@(y:ys) = -- sorted intersection
  case compare x y of
    EQ -> x : (sInter xs ys)
    LT -> sInter xs allY
    GT -> sInter allX ys
sInter [] _ = []
sInter _ [] = []

sNub (x:xs@(y:rest)) | x==y = sNub xs
                     | otherwise = x:sNub xs
sNub xs = xs

{- ruleBlock1 : When operating on Perm V R C:

   Given a value V, look along each row and see which blocks that value
   may occupy.  Find a row R1 for which the value is allowed in exactly
   one block B1 (and no other blocks).  This occupies [(B1,d)] in
   B1. Eliminate V from the other locations in B1.

   Given a value V, look along each row and see which blocks that value
   may occupy. Find two rows [R1,R2] for which the value only is allowed
   in exactly the same two blocks [B1,B2] (and no others).  These occupy
   [(B1,d11s)] in R1, [(B2,d12s)] in R1, [(B1,d21s)] in R2, and
   [(B2,d22s)] in R2. Eliminate V from the other locations in B1 and the
   other locations in B2.

   Works for R and C reversed, of course.
-}
{- ruleBlock2 : When operating on Perm V R C:

   Given a value V, look inside each block and see which rows that value
   may occupy. Find a block B1 for which only one row R1 is occupied
   (and no other rows).  This occupies [(R1,c11s)] in B1.  Eliminate V
   from all the other c's in row R1.

   Given a value V, look inside each block and see which rows that value
   may occupy. Find two blocks [B1,B2] for which the value only is allowed
   in exactly the same two rows [R1,R2] (and no others).  These occupy
   [(R1,c11s)] in B1, [(R1,c12s)] in B2, [(R2,c21s)] in B1, and [(R2,c22s)] in
   B2 for some C's. Eliminate V from the other rows in R1 and the
   other columns in R2.

   Works for R and C reversed, of course.
-}
{- ruleBlockP

   There is enough similarity between ruleBlock1 and ruleBlock2 to parameterize
   over expand.
-}
ruleBlockP :: forall a b c. (Ord a, Perm V b c) 
          => ( Index -> (V,a,b,c) )
         -> Cube -> Hints
{-# INLINE ruleBlockP #-}
ruleBlockP expand view =
  let allOn :: [(V,a,b,c)]
      allOn = map expand . isOn $ view
        where isOn = map fst . filter ((on==).snd) . assocs

      sV :: [ [(V,a,b,c)] ]
      sV = groupSort fst4 $ allOn      -- group by V

      sVsA :: [[ [(V,a,b,c)] ]]
      sVsA = map (groupSort snd4) $ sV -- group by a

      sVsAsB,sVsAsB2 :: [[[ [(V,a,b,c)] ]]]
      sVsAsB = map (map (groupSort thd4)) $ sVsA -- group by b
      -- These filters are to remove empty and redundantly full possibilities
      sVsAsB2 = filter atLeastOne . map (filter oneOrTwo) $ sVsAsB

      sVsAsBgB :: [[ ([b],[[(V,a,b,c)]]) ]]
      -- The filter is to remove solved parts of the puzzle (punt to ruleSubsetP)
      sVsAsBgB = map (map getAllB . filter (atLeastTwo . concat)) $ sVsAsB2
        where getAllB :: [[(V,a,b,c)]] -> ([b],[[(V,a,b,c)]])
              getAllB vabcss = (map (thd4 . head) vabcss, vabcss) -- length (concat vabcss) >= 2

      useful :: [ [([b],[[(V,a,b,c)]])] ]
      useful = concatMap (filter exactlySame . groupSort fst) $ sVsAsBgB
        where exactlySame :: [([b],[[(V,a,b,c)]])] -> Bool
              exactlySame sas@((sbs,_):_) = length sas == length sbs

      assemble :: [ ([b],[[(V,a,b,c)]]) ] -> Hints
      assemble stuff = [ (rcv,off) | rcv <- ixs, (view ! shuffle rcv) /= off ]
        where byVB :: [[(V,a,b,c)]]
              byVB = map concat . transpose . map snd $ stuff -- Regroup by identical 'b'
--            byVB = groupSort thd4 . concat . concat . map snd $ stuff -- equivalent
              act :: [(V,a,b,c)] -> [Index]
              act allVB@((v,_,b,_):_) = map (\c -> unshuffle (v,b,c)) (fullRange \\ map fth4 allVB)
              ixs :: [Index]
              ixs = concatMap act byVB

  in concatMap assemble useful

ruleBlocks :: [Cube -> Hints]
ruleBlocks = [ (ruleBlockP expand1 )
             , (ruleBlockP expand2 )
             , (ruleBlockP expand3 )
             , (ruleBlockP expand4 )
             ]

expand1 :: Index -> (V,R,B,D)
{-# INLINE expand1 #-}
expand1 (r,c,v) = let (b,d) = rcToBD r c in (v,r,b,d)
expand2 :: Index -> (V,C,B,D)
{-# INLINE expand2 #-}
expand2 (r,c,v) = let (b,d) = rcToBD r c in (v,c,b,d)
expand3 :: Index -> (V,B,R,C)
{-# INLINE expand3 #-}
expand3 (r,c,v) = let (b,d) = rcToBD r c in (v,b,r,c)
expand4 :: Index -> (V,B,C,R)
{-# INLINE expand4 #-}
expand4 (r,c,v) = let (b,d) = rcToBD r c in (v,b,c,r)

{- Given a list of locations, such as for the 9 columns of a row,
   look at the allowed values at each location.  Find a subset of k
   columns for which the union of their allowed values [V..] has
   length k.  Then eliminate [V..] from the (9-k) other columns.

   This clearly finds a list of N columns each with the same N values
   if such a thing exists, so it subsumes rule1P.

   This is fully symmetric in R C and V and depends on the constaints in
   R and V but not C. So the (View B D V) case also works.

   A useful property of this rule is that once there is only one way
   to place a value in a row or column or block then it will propagate
   that solution to the related contraints.  This is the case when
   minK is 1.
 -}
ruleSubsetQ :: forall a b c.(Perm a b c) => Cube -> ABC a b c -> [Hints]
{-# INLINE ruleSubsetQ #-}
ruleSubsetQ cube sAsBgC =
  let atA :: [(Int,(a,b,[c]))] -> [Hints]
      atA [] = []
      atA input@((_,(a,_,_)):_) =
        let killOne :: Hints
            killOne = opOne input
              where opOne ((1,(_,b1,[c1])):xs) = concatMap checkForC1 xs ++ opOne xs
                      where checkForC1 (_,(_,b2,cs2)) | c1 `sElem` cs2 && b2/=b1  = [(unshuffle (a,b2,c1),off)]
                                                      | otherwise = []
                    opOne (_:xs) = opOne xs
                    opOne [] = []
            --  Now assemble killK assuming killOne found nothing and is empty
            input2 :: [(Int,(a,b,[c]))]
            input2 = dropWhile ((1==).fst) input
            minK,maxK :: Int  -- The smallest and largest interesting number of locations in a subset
            minK = fst . head $ input2
            maxK = pred . length $ input2
            forK :: Int -> Hints
            forK k =
              let sets :: [ [(Int,(a,b,[c]))] ]
                  sets = subsets k . takeWhile ((k>=).fst) $ input2
                  chain :: [(xInt,(a,b,[c]))] -> (a,[b],[c])
                  chain set = (a,map (snd3.snd) set,combine (map (thd3.snd) set))
                  chains :: [(a,[b],[c])]
                  chains = filter ((k==).length.thd3) . map chain $ sets
              in if inRange (minK,maxK) k then concatMap assemble2 chains
                   else []
            killK :: [Hints]
            killK = if not (atLeastThree input2) then replicate 7 []
                      else [ forK k | k <- [2..8] ]
            assemble2 :: (a,[b],[c]) -> Hints
            assemble2 (a,inBs,inCs) = do -- List Monad
               (_,(_,b,cs)) <- input2
               guard (b `notElem` inBs)
               let eraseCs = sInter cs inCs
               guard (not (null eraseCs))
               [ (unshuffle (a,b,c),off) | c <- eraseCs ]
        in killOne:killK

  in map concat . transpose . map atA $ sAsBgC

eachPerm :: (forall x y z. (Perm x y z) => Cube -> ABC x y z -> a) -> [ Cube -> a ]
{-# INLINE eachPerm #-}
eachPerm rule =
    [ (\ known -> rule known ( sAsBgC known :: ABC R C V ) )
    , (\ known -> rule known ( sAsBgC known :: ABC R V C ) )
    , (\ known -> rule known ( sAsBgC known :: ABC C V R ) )
    , (\ known -> rule known ( sAsBgC known :: ABC C R V ) )
    , (\ known -> rule known ( sAsBgC known :: ABC V R C ) )
    , (\ known -> rule known ( sAsBgC known :: ABC V C R ) )
    , (\ known -> rule known ( sAsBgC known :: ABC B D V ) ) -- Special Case
    ]

ruleSubsets :: Cube -> (Hints,Hints)
{-# INLINE ruleSubsets #-}
ruleSubsets cube =
  let (kOne:kMore) = map concat . transpose . map ($ cube) $ rules
      kNext = concat . take 1 . filter (not.null) $ kMore
  in (kOne,kNext)
  where rules = eachPerm ruleSubsetQ

{- inference

  This is actually a pair of slow methods.

  The first one ('strong') takes all the constaints that have only 2
  ways to be satisfied ('strongLinks') and decides what options are
  implied on by other options being on.  If an option being on would
  cause a contradiction by excluding both options of another such
  constraint, then that option must be turned off.

  The second one ('weak') checks all the locations that are implied
  off by each strongLink possibility being on.  If a location is
  implied off by both options in a strongLink, then that location must
  be turned off.
-}
-- Cannot assume a consistent input when a previous guess has been made
-- This means an inconsistent cycle of strong links might be detected
-- Detecting and reporting this is an awful mess at the moment
-- And this *does happen* for the #2184 input out of the 36628 in sudoku17.1
inference :: Cube -> Maybe (Hints,Hints)
inference known =
  let strongLinks = filter exactlyTwo . conPos $ known

      Just (linkArray,maxLink) = mightHaveFailed
      mightHaveFailed = runST mayFail
      mayFail :: forall s. ST s (Maybe (UArray Index Int,Int))
      mayFail = do
        old <- newSTRef (0::Int)
        err <- newSTRef False -- used in reporting inconsistent input
        let gen = modifySTRef old succ >> readSTRef old
        failed <- newSTRef False
        (array::STUArray s Index Int) <- newArray boundsCube (0::Int)
        let setLink :: [[Index]] -> ST s () -- could be map'd, but not with abort capability
            setLink [] = return ()
            setLink ([rcv1,rcv2]:rest) = do
              n1 <- readArray' array rcv1
              n2 <- readArray' array rcv2
              case (n1,n2) of
                (0,0) -> newLink rcv1 rcv2 >> setLink rest
                (x,0) -> writeArray' array (rcv2,(negate x)) >> setLink rest
                (0,y) -> writeArray' array (rcv1,(negate y)) >> setLink rest
                (x,y) | x == negate y -> setLink rest -- nothing to do, consistent
                      | x == y -> writeSTRef err True -- an inconsistency was detected, abort
                      | otherwise -> renumberLink x y >> setLink rest
            newLink rcv1 rcv2 = do n <- gen
                                   writeArray' array (rcv1,n)
                                   writeArray' array (rcv2,(negate n))
            renumberLink x y = mapM_ change . filter ((abs y==).abs.snd) =<< getAssocs array
              where change (rcv,e) | e==y = writeArray' array (rcv,(negate x))
                                   | e==(-y) = writeArray' array (rcv,x)
                                   | otherwise = error "change is broken"
        setLink strongLinks
        foundInconsistency <- readSTRef err
        if foundInconsistency then return Nothing
           else do (frozen :: UArray Index Int) <- unsafeFreeze array
                   max <- readSTRef old
                   return (Just (frozen,max))

      maxRange = [(-maxLink)..maxLink]
      maxBound2 = ((-maxLink,-maxLink),(maxLink,maxLink))
      -- elements of conSets are lists of indices such that if one of them is True then the rest are False
      conSets :: [[Int]]
      conSets = if maxLink == 0 then [] 
                  else filter atLeastTwo . map (filter (0/=) . map (linkArray!)) $ conInd
      -- If link 'i' is True then '-i' is False and the list [ a | a<-maxRange, initialImplications ! (i,a) ] is True
      initialImplications,finalImplications :: UArray (Int,Int) Bool
      initialImplications = accumArray (||) False maxBound2 (concatMap forIndex maxRange)
        where forIndex i = let implied = sNub. sort . map negate . concatMap (delete i) . filter (i `elem`) $ conSets
                           in [((i,a),True) | a <- implied]
      -- Matrix multiplication propagates the conSets constraints until a fixed point is reached
      finalImplications = stable (iterate (`mmult` initialImplications) initialImplications)
        where stable (x:xs@(y:_)) = if x/=y then stable xs else x
              mmult a b = listArray maxBound2 [ or [ a!(r,k) && b!(k,c) | k<-maxRange] | r<-maxRange,c<-maxRange ]
      -- List of (i,[a]) where if link index i is True then [a] are all True; Note that [a] is sorted
      implicationSets :: [ (Int,[Int]) ]
      implicationSets = filter (atLeastOne.snd) [ (r,[ c | c <- maxRange, finalImplications!(r,c)]) | r <- maxRange ]
      -- List [i] of indices that imply contradictions, e.g. (-i) in [a] or [-x,x] subset of [a]
      excludeSet :: [Int]
      excludeSet = concatMap withContradiction implicationSets
        where withContradiction (i,a) = if (negate i) `elem` a || hasContradiction a then [i] else []
              hasContradiction [] = False
              hasContradiction (x:xs) | 0 <= x = False
                                      | otherwise = (negate x) `elem` xs || hasContradiction xs
      strongHints :: Hints
      strongHints = if null excludeSet then []
                    else map (\(i,_) -> (i,off)) . filter ((`elem` excludeSet).snd) . assocs $ linkArray

      -- And now work out any "weak" implications
      -- We need a reverse mapping from a link index to a list of (R,C,V) with that index
      linkIndices :: Map.Map Int [Index]
      linkIndices = Map.fromListWith (++) . map (\(rcv,i)->(i,[rcv])) . filter ((0/=).snd) . assocs $ linkArray
      -- Re-use the implication sets to construct a mapping from a link index i to a list of positions implied off
      linkImplied :: Map.Map Int Hints
      linkImplied = Map.fromAscList . map (\(i,xs) -> (i,impliedOff (i:xs))) $ implicationSets
        where linksOn link = Map.findWithDefault [] link linkIndices
              impliedOff links = sortWith fst . filter ((on==).(known!).fst) 
                               . concatMap offLocs . concatMap linksOn $ links
      -- If both (i) and (negate i) imply something is off, then it has to actually be off.
      weakHints :: Hints
      weakHints = let x = sNub. combine . map checkBoth $ [1..maxLink]
                  in x -- if null x then x else (trace ("weakInference: "++show(length x))) x
        where checkBoth i = let a = Map.findWithDefault [] (negate i) linkImplied
                                b = Map.findWithDefault [] i linkImplied
                            in sInter a b

  in if null strongLinks then Just ([],[]) 
       else case mightHaveFailed of
              Nothing -> Nothing
              Just _ -> Just (strongHints,weakHints)

  where conInd :: [[Index]]
        conInd = concat [iset (undefined :: (R,C,V)),iset (undefined :: (R,V,C))
                        ,iset (undefined :: (C,V,R)),iset (undefined :: (V,B,D))]
            where iset :: forall a b c. Perm a b c => (a,b,c) -> [[ Index ]]
                  iset _ = [ [ unshuffle (a,b,c) | c <- fullRange::[c] ] 
                                 | a <- fullRange::[a], b <- fullRange::[b] ]
        conPos :: Cube -> [[Index]]
        conPos known = map (filter ((on==).(known!))) conInd

{- Functions to export, and their helpers -}

-- Main exported function
deduce :: (Show e, Enum e) => [(e,e,e)] -> IO ([Int],Int,String)
deduce locs = do
  cube <- toConst =<< toCube locs
  checkCube "Input was inconsistent" cube
  case solve cube of
    Left guessCount -> fail ("failed after # guesses = "++show guessCount)
    Right result -> return result

-- Assume input seems consistent, output may not be
-- If an inconsistent cube is detected, then "badCube" is returned
-- This tries the different methods in what I hope is an efficient order
evolve :: Cube -> Cube
evolve cube =
  let (kOne,kNext) = ruleSubsets cube
      byBlocks = map ($ cube) $ ruleBlocks
      all = (kOne:byBlocks)++[kNext]
  in case filter (not.null) all of
       (hints:_) -> evolve (update hints)
       [] -> case inference cube of
               Nothing -> badCube -- inconsistent input was detected, return inconsistent cube
               Just (inferred@(_:_),_) -> evolve (update inferred)
               Just ([],weakInferred@(_:_)) -> evolve (update weakInferred)
               Just ([],[]) -> cube -- give up
  where update :: Hints -> Cube
        update hints =  unsafePerformIO $ do
          mutArray <- toMut cube
          mapM_ (writeArray' mutArray) hints
          toConst mutArray

-- Convert a list of generic hints into an MCube, validating the input
toCube :: (Enum e) => [(e,e,e)] -> IO MCube
toCube locs = do hints <- liftM concat $ mapM setLoc locs
                 ma <- makeEmptyCube
                 mapM_ ( (writeArray' ma)) hints
                 return ma
  where setLoc:: Enum e => (e,e,e) -> IO Hints
        setLoc i@(re,ce,ve) = if check then return (offLocs (r,c,v))
                                       else fail "Input location is out of range"
          where r = toEnum $ fromEnum re; c = toEnum $ fromEnum ce; v = toEnum $ fromEnum ve
                check = and [inRange (lo,hi) r,inRange (lo,hi) c,inRange (lo,hi) v]

-- Generate list of locations which must be off if (r,c,v) is in the solution
offLocs :: Index -> Hints
offLocs (r,c,v) = rs ++ cs ++ vs ++ bs
  where rs = [((r',c,v),off) | r' <- delete r fullRange]
        cs = [((r,c',v),off) | c' <- delete c fullRange]
        vs = [((r,c,v'),off) | v' <- delete v fullRange]
        bs = do let (b,d) = rcToBD r c
                d' <- delete d fullRange
                let (r',c') = bdToRC b d'
                return ((r',c',v),off)

-- Reverse the toCube operation
fromCube :: (Enum e,Show e) => Cube -> [(e,e,e)]
fromCube cube = map head . filter exactlyOne $
  [ [ eee | v <- fullRange, off /= cube ! (r,c,v)
          , let eee = (toEnum $ fromEnum r
                      ,toEnum $ fromEnum c
                      ,toEnum $ fromEnum v) ]
    | r <- fullRange, c <- fullRange ]

-- Check for obvious inconsistency, meaning one of the constraints has 0 possible values
consistent :: Cube -> Bool
consistent known = not $ or [ inConsistentView ( view known :: View R C V )
                            , inConsistentView ( view known :: View C V R )
                            , inConsistentView ( view known :: View R V C )
                            , inConsistentView ( view known :: View V B D ) ]
  where inConsistentView :: (Perm a b c) => View a b c -> Bool
        inConsistentView view = or [ null [ () | c <- fullRange, off /= view ! (a,b,c) ] 
                                         | a <- fullRange, b <- fullRange ]

-- This is optimized for when the first set of constraints is known to be consistent
consistent2 :: Cube -> Bool
consistent2 known = not $ or [ inConsistentView ( view known :: View C V R )
                             , inConsistentView ( view known :: View R V C )
                             , inConsistentView ( view known :: View V B D ) ]
  where inConsistentView :: (Perm a b c) => View a b c -> Bool
        inConsistentView view = or [ null [ () | c <- fullRange, off /= view ! (a,b,c) ] 
                                         | a <- fullRange, b <- fullRange ]

checkCube :: (Monad m) => String -> Cube -> m Cube
checkCube msg cube = if consistent cube then return cube else fail msg

-- Assume input is consistent. Returns Just [] if cube was solved
-- Return Nothing if obviously inconsistent
pickConstraint :: Cube -> Maybe [Index]
pickConstraint cube = getMin (10,[]) list
  where list = concat [ constraints ( view cube :: View R C V )
                      , constraints ( view cube :: View V B D )
                      , constraints ( view cube :: View C V R )
                      , constraints ( view cube :: View R V C ) ]
        getMin (_,vals) [] = Just vals
        getMin old ((0,_):_) = Nothing         -- Inconsistent
        getMin old ((1,_):xs) = getMin old xs  -- ignore solved ones
        getMin old ((2,vals):_) = Just vals    -- length 2 is best possible value
        getMin old@(oldLen,_) (new@(newLen,_):xs) = 
          if newLen < oldLen then getMin new xs else getMin old xs
        constraints :: (Perm a b c) => View a b c -> [(Int,[Index])]
        constraints view = do
          a <- fullRange
          b <- fullRange
          let ixs = [ unshuffle ix | c <- fullRange, let ix=(a,b,c), on==view!ix ]
          return (length ixs,ixs)

data State = Inconsistent String | Solved | Guesses [Cube]

-- Assume input is consistent, outputs will be consistent, Nothing if solved.
-- The original cube may not be touched after this, as pending guesses need it.
guesses :: Cube -> State
guesses cube = case choicesM of
                 Nothing -> Inconsistent "evolve create inconsistent cube"
                 Just [] -> Solved
                 Just choices -> case (filter consistent . map set $ choices) of
                                   [] -> Inconsistent "no consistent guesses"
                                   gs -> Guesses gs
  -- TODO : check guessed (r/c/v/b) for consistency, not entire cube
  where choicesM = pickConstraint cube
        set :: Index -> Cube
        set ix = unsafePerformIO $ do 
          newMut <- toMutCopy cube
          mapM_ (writeArray' newMut) (offLocs ix)
          toConst newMut

solve :: Cube -> Either Int ([Int],Int,String)
solve = solve' 0

-- Assume input is consistent
-- More clever ways to order the guessing breadth-first made things worse
solve' :: Int -> Cube -> Either Int ([Int],Int,String)
solve' guessCount known =
  let cube = evolve known
      (done,count,solution) = isSolved cube
      tryAll :: Int -> [Cube] -> Either Int ([Int],Int,String)
      tryAll gC [] = Left gC -- "Nothing left to try!"
      tryAll gC (c:cs) = case solve' (succ gC) c of
                           Left gC' -> tryAll gC' cs
                           Right (ls,gC',sol) -> Right ((count:ls),gC',sol)
  in if done
     then if consistent2 cube
          then Right ([81],guessCount,solution)
          else Left guessCount -- "Finished in inconsistent state!"
     else case guesses cube of
              Inconsistent msg -> Left guessCount -- "Cube was inconsistent!"
              Solved -> Right ([81],guessCount,solution)
              Guesses cs -> tryAll guessCount cs

-- This assumes the input is acutally a fully consistent cube
oneStep :: String -> IO String
oneStep s = do cube <- toConst =<< toCube =<< return (map toHint justSet)
               return (thd3 . isSolved . evolve  $ cube)
  where rcs = [(r,c) | r <- fullRange, c <- fullRange]
        isHint = inRange (intToDigit lo,intToDigit hi)
        justSet = filter (isHint . snd) (zip rcs s)
        toHint ((r,c),vC) = (r,c,digitToInt vC)

-- isSolve Takes a cube to (Bool (true if solved), Int (number of solved locations),String (board position for output))
isSolved :: Cube -> (Bool,Int,String)
isSolved cube = (all exactlyOne list, length . filter exactlyOne $ list, map convert list)
  where list = [ [ v | v <- fullRange, off /= cube ! (r,c,v) ]
                     | r <- fullRange, c <- fullRange ]
        convert [v] = intToDigit . fromEnum $ v
        convert [] = error "inconsistent conversion"
        convert _ = '0'

{- on 366628 puzzles

real    84m27.998s
user    63m28.240s
sys     1m16.305s

 total) 36628 164 223.341 90 406.978
0 : 36464        36464   36538
1 : 69   138     77
2 : 68   20      5
3 : 11   5       5
4 : 11   1       1
5 : 1            1
6 : 3            1
7 :              
8 :              
9 : 1            
   314 197   117  total good and bad guessses

Reshuffled input is fairly similar, but took a few more guesses on the
same puzzles:

 0 : 36464 36464 36538
 1 :    69   124    68
 2 :    59    29     9
 3 :    10     9     4
 4 :     9     2     3
 5 :     8     0     2
 6 :     2     0     3
 7 :     3     0     0
 8 :     2     0     0
 9 :     1     0     0
10 :     0     0     0
11 :     0     0     0
12 :     0     0     1
tot) 36628   164 223.341    90 406.978
      367    217    150

x : guesses good bad

So of the 164 that required guessing, 74 of them had to do no
backtracking, and 90 needed to do backtacking.

So only 1 in about every 407 puzzles needed to do backtracking.
-}
{- A comment of the element of luck in guessing:

The 59th entry in the topn85 is the least lucky I've seen so far "6..1...8..53.............4....8...6..9....7....24.........7.3.9....2.5..1.......":

(59,(18,40),(7,[24,25,30,32,34,35,81]),("7.53............8...42.......3.....7.....92...6...1....1...8.6.......5.4.9.......","6..1...8..53.............4....8...6..9....7....24.........7.3.9....2.5..1........","679143285453268197281759643715832964894516732362497851528674319946321578137985426"))

That took 40 guesses, 34 bad and 6 good.  Finding the contradictions
from the bad guesses was obviously very hard. Taking the shuffled verions:

Reason:~/Documents/projects/haskell/sudoku/rewrite-deduce/further/out chrisk$ echo  7.53............8...42.......3.....7.....92...6...1....1...8.6.......5.4.9....... | ../chains-7
(1,(18,4),(3,[24,28,81]),(".....9.3...2.1......5.....7..67....5....84...........2...6.....1......4.3.....98.","7.53............8...42.......3.....7.....92...6...1....1...8.6.......5.4.9.......","785396142629147385134285679253864917841739256967521438412958763378612594596473821"))

Which needs 4 guesses, 2 bad and 2 good.  Taking the shuffled version:

Reason:~/Documents/projects/haskell/sudoku/rewrite-deduce/further/out chrisk$ echo .....9.3...2.1......5.....7..67....5....84...........2...6.....1......4.3.....98. | ../chains-7
(1,(18,2),(2,[24,81]),("........8.1.3.9....6...2........13..4...7....5......6.....5....8...4...7......29.",".....9.3...2.1......5.....7..67....5....84...........2...6.....1......4.3.....98.","674529138932817654815346297286793415751284369493165872549638721168972543327451986"))

Which needs 2 guesses, 1 bad and 1 good.  If it had found the 1 good
guess first then this would have been a 1 guess, 0 bad and 1 good
puzzle.  So the worst puzzle I have seen was a matter of bad luck.

Perhaps a better strategy for guessing from a stuck position would be
to queue continuing from guesses which lead to another stuck position
until the whole set of guesses has been tried.

The # of good guesses required was 138 with 1, 20 with 2, 5 with 3,
and 1 with 4. ( For the topn87 it was up to 7 good guesses required ).
So for the 164 stuck puzzles, there could only be 0 or 1 bad guesses
for 138 of them.  And if the guesses are all 2-way then this is 1-4
bad for 2 good, 4-11 bad for 3 good, and 11-26 bad for 4 good.

average of 0.5 for 1 good = 138/2 = 69 bad
average of 2.5 for 2 good = 20*5/2 = 50 bad
average of 7.5 for 3 good = 5*15/2 = 37.5 bad
average of 18.5 for 4 good = 1*37/2 = 18.5 bad
Average Total of 175 bad guesses (min of 51)

Total actually seen was 90 bad guesses.  The only real test is to run it in practice.

b g
bb bg
bb bb  bb bg
bbbb bbbb bbbb bbbg


-}


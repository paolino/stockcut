
{-# language ViewPatterns #-}
module Direct where -- (mainT, Solution,report) where

import Data.List
import Data.Maybe
import Control.Arrow
import Data.Ord
import Control.Monad
import Data.Function


-- power set
poset = ([]:) . powset where
    powset [x] = [[x]]
    powset (x:xs) = [x]:(map (x:) zs ++ zs) where 
        zs = powset xs

using :: (Ord a, Num a) => [a] -> [a]
using = tail . scanl (+) 0

sanity t [] = Just []
sanity t xs = let
    l = length xs
    xs' = take l $ dropWhile (> t) $ take (2*l) $ cycle xs
    in if null xs' then Nothing else Just xs'

consume :: (Ord a , Num a) => a -> [a] -> Maybe ([a],[a])
consume t xs = (map fst *** map fst) . break ((> t) . snd) . (zip <*> using) <$> sanity t xs

data Subset a = Subset {
    subset :: [a],
    subsetSum :: a -- cached sum
    } deriving Show

mkSubset :: Num a => [a] -> Subset a
mkSubset = Subset <*> sum

data Bar a = Bar {
    total :: a,
    complete :: Subset a,
    subsets :: [Subset a],
    locked :: Bool
    } deriving Show

barElements = subset . complete

data Solution a = Solution {
    worst :: Bar a,
    cuts :: [Bar a]
    } deriving Show

waste :: (Num a) => Bar a -> a
waste (Bar t c _ _) = t - subsetSum c

mkBar :: (Num a, Ord a) =>  (a,Bool) -> [a] -> Maybe ([a], Bar a)
mkBar  (t,l) xs = do 
    (ys,zs) <-  consume t xs 
    return  (zs, Bar t (mkSubset ys) (map mkSubset $ poset ys) l)

unsafeBar :: (Num a, Ord a) => (a,Bool) -> [a] -> Bar a
unsafeBar (t,l)  = snd . fromJust . mkBar (t,l)

mkSolution :: (Num a, Ord a) => [(a,Bool)] -> [a] -> Solution a
mkSolution bs xs = elect . takeWhile (not . null . barElements) $ mkSolution' bs xs where
    mkSolution' bs [] = []
    mkSolution' (b:bs) xs = case mkBar b xs of
        Nothing -> mkSolution' bs xs
        Just (rs,b) -> b : mkSolution' bs rs

checkLocked b1 b2 
    | locked b1 && not (locked b2) = LT
    | not (locked b1) && locked b2 = GT
    | otherwise = comparing waste b1 b2

elect xss = 
    let (n,xs) = maximumBy (checkLocked `on` snd) . zip [0..] $ xss
        (xss',_:xss'') = splitAt n xss
    in Solution xs (xss' ++ xss'')

inside :: (Num a, Ord a) => a -> a -> Subset a -> Bool
inside t z (Subset _ s) = z > s && z <= s + t

substitute  :: (Num a, Ord a) 
        => a -- substitute
        -> Bar a  -- present elements
        -> Maybe ([a], Bar a) -- if successful a list of exiting elements and the new present elements
substitute y b@(Bar t (subset -> xs) subs l) = 
                fmap (\(subset -> ys) -> (ys, unsafeBar (t,l) . (y :) $ xs \\ ys)) . -- substitute y in the elements
                listToMaybe . -- select most gain 
                sortBy (comparing subsetSum) . -- order by gain 
                filter (inside (waste b) y) $ -- keep valid sustitutions
                subs

-- try substitution on all bars until one succeed
substituting :: (Num a , Ord a) => a -> [Bar a] -> Maybe ([a], [Bar a])
substituting y = substituting' where
    substituting' [] = Nothing
    substituting' (x : xs) = case substitute y x of
        Nothing -> second (x:) <$> substituting' xs
        Just (z, x') -> Just  (z, x' : xs)
 
-- a substituting from a Solution pov
turn :: (Num a , Ord a) => Solution a -> Maybe (Solution a)
turn (Solution (Bar t (subset -> x:xs) _ l) bs) = (\(xs',bs') -> Solution (unsafeBar (t,l) $ xs ++ xs') bs') <$> substituting x bs

cycleSubset :: Subset a -> [Subset a]
cycleSubset (Subset xs s) = map (\y -> Subset (take n y) s) $ take n . tails $ cycle xs
    where n = length xs

-- equivalent solutions, with cycled final piece
cycleSolution :: Solution a -> [Solution a]
cycleSolution (Solution (Bar t x rs l) xs) = map (\y -> Solution (Bar t y rs l) xs) $ cycleSubset x

-- insist until no more substitutions for final piece are possible
turning :: (Ord a,Num a) => Solution a -> Solution a
turning (Solution (barElements -> []) xss) = turning $ elect xss -- worst piece consumed! one less
turning x | all isNothing $ map turn (cycleSolution x) = x -- can't substitute final pieces nowhere
turning x = -- one substitution happens
        let (x',x'') = head . catMaybes $ map (\y -> (,) y <$> turn y) $ cycleSolution x
        in turning x''        

value :: (Num a, Ord a) => Solution a -> (a,Int)
value (Solution x xs) = (waste x, length xs + 1)

-- classifing value for a solution is number of bars and, on ties, the waste of the final piece should be maximised
comp :: (Num a,Ord a) => Solution a -> Solution a -> Bool
comp (value -> (l,n)) (value -> (l',n')) =  n' < n || n' == n && l' > l 
    where



    
   

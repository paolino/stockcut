
{-# language ViewPatterns #-}
module Direct (mainT, Solution,report) where

import Data.List
import Data.Maybe
import Control.Arrow
import Data.Ord
import Control.Monad
import System.Random.Shuffle
import System.Random
import Control.Concurrent


-- power set
poset = ([]:) . powset where
    powset [] = [] 
    powset [x] = [[x]]
    powset (x:xs) = [x]:(map (x:) zs ++ zs) where 
        zs = powset xs

using :: (Ord a, Num a) => [a] -> [a]
using = tail . scanl (+) 0

consume :: (Ord a , Num a) => a -> [a] -> ([a],[a])
consume t = (map fst *** map fst) . break ((> t) . snd) . (zip <*> using)

fill :: (Num a, Ord a) => a -> [a] -> [[a]]
fill t = takeWhile (not . null) . f where
    f = uncurry (:) . second f . consume t 

data Solution a = Solution {
    final :: [a],
    cuts :: [[a]]
    } deriving Show

mkSolution :: (Num a, Ord a) => a -> [a] -> Solution a
mkSolution t xs = elect t $ fill t xs

-- elect _ [] = Solution [] []
elect t xss = 
    let (n,xs) = maximumBy (comparing $ (waste t) . snd) . zip [0..] $ xss
        (xss',_:xss'') = splitAt n xss
    in Solution xs (xss' ++ xss'')

waste :: Num a => a -> [a] -> a
waste t xs = t - sum xs

data Subset a = Subset {
    subset :: [a],
    subsetSum :: a -- cached sum
    }

inside :: (Num a, Ord a) => a -> a -> Subset a -> Bool
inside t z (Subset _ s) = z > s && z <= s + t

substitute  :: (Num a, Ord a) 
        => a -- bar length
        -> a -- element to insert
        -> [a]  -- present elements
        -> Maybe ([a], [a]) -- if successful a list of exiting elements and the new present elements
substitute t y xs = 
                fmap (\(Subset ys s) -> (ys, y : (xs \\ ys))) . -- substitute y in the elements
                listToMaybe . -- select most gain 
                sortBy (comparing subsetSum) . -- order by gain 
                filter (inside (waste t xs) y) . -- keep valid sustitutions
                map (Subset <*> sum) $ poset xs 

-- try substitution on all bars until one succeed
substituting :: (Num a , Ord a) => a -> a -> [[a]] -> Maybe ([a] , [[a]])
substituting t y = substituting' where
    substituting' [] = Nothing
    substituting' (xs : xss) = case substitute t y xs of
        Nothing -> second (xs:) <$> substituting' xss
        Just (ys, xs') -> Just  (ys, xs' : xss)
 
-- a subsituting from a Solution pov
turn :: (Num a , Ord a) => a -> Solution a -> Maybe (Solution a)
turn t (Solution (x : xs) yss) = (\(xs',yss') -> Solution (xs ++ xs') yss') <$>  substituting t x yss 
    
-- equivalent solutions, with cycled final piece
cycling :: Solution a -> [Solution a]
cycling (Solution x xs) = map (\y -> Solution (take n y) xs) $ take n . tails $ cycle x
    where n = length x

-- insist until no more substitutions for final piece are possible
turning :: (Ord a,Num a) => a -> Solution a -> Solution a
turning t (Solution [] xss) = turning t (elect t xss) -- final piece consumed! one less
turning t x | all isNothing $ map (turn t) (cycling x) = x -- can't substitute final pieces nowhere
turning t x = -- one substitution happens
        let (x',x'') = head . catMaybes $ map (\y -> (,) y <$> turn t y) $ cycling x
        in turning t x''        

value :: (Num a, Ord a) => a -> Solution a -> (a,Int)
value t (Solution x xs) = (waste t x, length xs + 1)

-- classifing value for a solution is number of bars and, on ties, the waste of the final piece should be maximised
comp :: (Num a,Ord a) => a -> Solution a -> Solution a -> Bool
comp t (value t -> (l,n)) (value t -> (l',n')) =  n' < n || n' == n && l' > l 
    where


-- solutions as they are found
run :: (Num a , Ord a, Show a) => a ->  IO Bool -> (Solution a -> IO ()) -> IO () -> Solution a -> IO ()
run t p f c best@(Solution b bs) = do
    pause <- p
    if pause then do
        threadDelay 100000
        run t p f c best
    else do 
        x <- mkSolution t <$> shuffleM (concat $ b : bs)
        let ts = turning t x
        best' <- if comp t best ts then f ts >> return ts
                 else return best
        c 
        run t p f c best'

-- report x = print (length (cuts x) + 1, map (waste 6000) $ final x : cuts x)

        
mainT :: (Solution Int -> IO ()) -> IO () -> IO [Int] -> IO Bool -> IO b
mainT report count restart pause =  do
    let turn mt = do 
            ns <- restart
            maybe (return ()) killThread mt
            if null ns then turn Nothing
            else do
                let s = mkSolution 6000 ns
                report s
                forkIO (run 6000 pause report count s) >>= turn . Just 
    turn Nothing
    
report (Solution x xs) =  map (sortBy (flip compare) &&& waste 6000) (xs ++ [x])


    
    

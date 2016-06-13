{-# language ScopedTypeVariables, Rank2Types, FlexibleContexts #-}

module Cut (mainT, ISolution, IBar, report) where

import Control.Arrow (second , (***))
import Data.Array (listArray, (!))
import System.Random.Shuffle (shuffle', shuffleM)
import System.Random (getStdGen,split, randomRIO)
import Data.List (unfoldr, sortBy)
import Control.Monad (replicateM)
import Data.Ord (comparing)
import Control.Concurrent
import Control.Monad.ST
import Data.Array.ST

type IBar a = [(Int,a)]
type ISolution a = [IBar a]

using :: (Ord a, Num a) => IBar a -> [a]
using = tail . scanl (\s (n,x) -> s + x) 0

consume :: (Ord a , Num a) => a -> IBar a -> (IBar a, IBar a)
consume t = (m *** m) . break ((> t) . snd) . (zip <*> using) where
    m = map fst

fill :: (Num a, Ord a) => a -> IBar a -> ISolution a
fill t = takeWhile (not . null) . f where
    f = uncurry (:) . second f . consume t 

newtype Mealy a = Mealy (ISolution a -> (Maybe (ISolution a), Mealy a))

best :: (Ord a, Num a) => a -> Mealy a
best t = let
    r m mn = Mealy $ \xs -> let 
        l = length xs
        s = maximum $ map ((t -) . sum . map snd) xs
        in case mn of
            Just n -> if l < n || l == n && s > m 
                then (Just xs, r s $ Just l) else (Nothing, r m mn)
            Nothing -> (Just xs, r s $ Just l)
    in r 0 Nothing
  
val :: (Fractional a) => a -> [a] -> a
val d xs = (fromIntegral $ length xs) * (d*1.1 - sum xs)

type HH b a  = [(b,IBar a)]

insertBar :: (Ord b, Fractional b) =>  (a -> b) -> a -> IBar a -> HH b a  -> HH b a
insertBar f d xs = (:) (val (f d) $ map (f . snd) xs ,xs)

limit :: Ord b => Int -> HH b a -> HH b a
limit n h 
    | length h > n = take (n `div` 2) $ sortBy (comparing fst) h
    | otherwise = h

data Fact a = Fact {
    barLength :: a,
    pool :: Int
    }


update :: (Ord b, Fractional b) => (a -> b) -> Fact a -> ISolution a -> HH b a -> HH b a
update f (Fact d n) xs h = limit n . foldr (insertBar f d) h $ xs

-- quadratic
cleanQ :: IBar a -> IBar a
cleanQ ((x,a):xs) = (x,a) : cleanQ (filter ((/= x) . fst) xs)
cleanQ [] = []

--linear
cleanL :: forall a . Int -> IBar a -> IBar a
cleanL n xs = runST f where
    f :: forall s. ST s (IBar a)
    f = do
        presence :: STUArray s Int Bool  <- newArray (0, n - 1) False
        let insert c ((x,a):xs) = do
                test <- readArray presence x
                (z,c') <- if test then return (id,c)
                    else do
                        writeArray presence x True
                        return (((x,a):), c + 1)
                if c' >= n then 
                    return $ z []
                    else z <$> insert c' xs
        insert 0 xs

correct :: Int -> IBar a -> IBar a
correct n = take n . cleanL n

pick :: IBar a -> Int -> HH b a -> IO (IBar a)
pick all n h = do 
    as <- shuffleM all
    xs <- shuffleM (as : map snd h)
    return . correct n $ concat xs

fact :: Fact Int
fact = Fact 6000 200

run pause cell count as h (Mealy b) = do
                c <- pause
                if c then do
                    threadDelay 10000
                    run pause cell count as h (Mealy b)
                else do
                    x :: ISolution Int <- fill 6000 <$> pick as 30 h
                    let (r,b') = b x
                    count ()
                    case r of 
                        Just y -> cell y
                        Nothing -> return ()
                    run pause cell count as (update fromIntegral fact x h) b' 

mainT :: (ISolution Int -> IO ()) -> (() -> IO ()) -> IO [Int] -> IO Bool -> IO b
mainT report count restart pause =  do
    let turn mt = do 
            ns <- zip [0..] <$> restart 
            maybe (return ()) killThread mt
            forkIO (run pause report count ns []  (best 6000)) >>= turn . Just 
    turn Nothing
    
report xs =  zipWith (\n -> (\xs -> (xs,6000 - sum xs)) . map snd) [1..] xs




{-# language ScopedTypeVariables #-}
import Control.Arrow (second , (***))
import Data.Array (listArray, (!))
import System.Random.Shuffle (shuffle', shuffleM)
import System.Random (getStdGen,split, randomRIO)
import Data.List (unfoldr)
import Control.Monad (replicateM)
import qualified Data.Heap as H

type IBar a = [(Int,a)]
type ISolution a = [IBar a]


shuffles xs = map (shuffle' <*> length $ xs) . unfoldr (Just . split ) <$> getStdGen

using :: (Ord a, Num a) => IBar a -> [a]
using = tail . scanl (\s (n,x) -> (s + x)) 0

consume :: (Ord a , Num a) => a -> IBar a -> (IBar a, IBar a)
consume t = (m *** m) . break ((> t) . snd) . (zip <*> using) where
    m = map fst

fill :: (Num a, Ord a) => a -> IBar a -> ISolution a
fill t = takeWhile (not . null) . f where
    f = uncurry (:) . second f . consume t 


data Result a = Result {
    bars :: Int,
    bigrest :: a,
    cuts :: ISolution a
    } deriving Show
newtype Mealy a = Mealy (ISolution a -> (Maybe (Result a), Mealy a))

best :: (Ord a, Num a) => a -> Mealy a
best t = let
    r m mn = Mealy $ \xs -> let 
        l = length xs
        s = maximum $ map ((t -) . sum . map snd) xs
        in case mn of
            Just n -> if l < n || l == n && s > m 
                then (Just $ Result l s xs, r s $ Just l) else (Nothing, r m mn)
            Nothing -> (Just $ Result l s xs, r s $ Just l)
    in r 0 Nothing
  
solve :: Mealy a -> [ISolution a] -> [Result a]
solve (Mealy b) (x:xs) = let (r,b') = b x 
            in  maybe id (:) r $ solve b' xs



val :: (Fractional a) => a -> a -> [a] -> a
val d t xs = 1/ (fromIntegral $ length xs) / (d + t - sum xs)

type HH b a  = H.Heap (H.Entry b (IBar a))

insertBar :: (Ord b, Fractional b) =>  (a -> b) -> a -> a -> IBar a -> HH b a  -> HH b a
insertBar f d t xs = H.insert (H.Entry (val (f d) (f t) $ map (f . snd) xs) xs)

limit :: Int -> HH b a -> HH b a
limit n h 
    | H.size h > n = H.take (n `div` 2) $ H.nub h
    | otherwise = h

data Fact a = Fact {
    barLength :: a,
    tol :: a,
    pool :: Int
    }


update :: (Ord b, Fractional b) => (a -> b) -> Fact a -> ISolution a -> HH b a -> HH b a
update f (Fact t d n) xs h = limit n . foldr (insertBar f t d) h $ xs


clean :: IBar a -> IBar a
clean ((x,a):xs) = (x,a) : clean (filter ((/= x) . fst) xs)
clean [] = []

correct :: Int -> IBar a -> IBar a
correct n = take n . clean

pick :: IBar a -> Int -> HH b a -> IO (IBar a)
pick all n h = do 
    xs <- shuffleM $ H.toUnsortedList h
    as <- shuffleM all
    return $ correct n $ (++ as) . concat $  map H.payload $  xs

fact :: Fact Int
fact = Fact 6000 10 500

run as h (Mealy b) = do
                x :: ISolution Int <- fill 6000 <$> pick as 50 h
                let (r,b') = b x
                case r of 
                    Just y -> do
                        putStrLn "**************"
                        mapM_ print $ zip [1..] $ map (\x -> (x, 6000 - sum x)) . map (map snd) $ cuts y
                    Nothing -> return ()
                run as (update fromIntegral fact x h) b'
main = do
    
    ns :: IBar Int <- fmap (zip [0..]) . replicateM 150 $ randomRIO (300,3000) 
    
    run ns H.empty  (best 6000)
    
    -- mapM print . map (\(Result x y _) -> (x,y)) $ solve (best 6000) x
     

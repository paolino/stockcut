module Computing where


import System.Random.Shuffle
import Control.Concurrent
import Reconf (Reconf (..))
import Direct (mkSolution, Solution, turning, comp)



-- handle pausing
pausing     :: IO Bool -- retrieve pausing state
            -> IO a -- pausing effect
            -> IO a -- running effect
            -> IO a
pausing pause f g = do
    t <- pause
    if t then threadDelay 100000 >> f else g


-- computing thread, from raw and request. Handle pausing, report new solution and counting
computeThread   :: (Num a , Ord a) 
                => [(a,Bool)] -- lengths of raw material (True tagged are to be used if possible)
                -> [a] -- lengths of cuts requested
                -> IO Bool -- check if pausing
                -> (Solution a -> IO ()) -- report a new champion
                -> IO ()  -- count one more
                -> IO ()
computeThread raw request pause report count = do
        let s = mkSolution raw request
        report s
        computeThread' s where
    computeThread' best = pausing pause (computeThread' best) $ do 
        new  <- turning <$> mkSolution raw <$> shuffleM request -- shuffle the request and compute new solution
        best' <- if comp best new 
                    then report new >> return new -- report the new gold medal and promote it
                    else return best -- keep old
        count 
        computeThread' best' 
        
-- wait for new configurations and spawn / stop computation thread on the event
controlThread   :: (Num a , Ord a) 
                => IO (Reconf a) -- reconfiguration event 
                -> IO Bool -- pausing state
                -> (Solution a -> IO ()) -- report action
                -> IO () -- counting action
                -> IO b
controlThread reset pause report count =  do
    let loop mt = do 
            Reconf store stock request <- reset -- wait for a reset
            let raw = zip store (repeat True) ++ repeat (stock,False) -- all bars, store + infinite stock
            maybe (return ()) killThread mt -- stop the computing thread
            if null request 
                then loop Nothing -- don't run for null request
                else do
                    t <- forkIO $ computeThread raw request pause report count -- start computing thread 
                    loop $ Just t 
    loop Nothing


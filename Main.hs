{-# language ScopedTypeVariables #-}
{-# language PackageImports, ConstraintKinds,TemplateHaskell #-}
{-# language RecursiveDo,NoMonomorphismRestriction,FlexibleContexts,TypeFamilies #-}


module Main where

import Reflex.Dom hiding (combineDyn)
import Reflex hiding (combineDyn)
import Reflex.Dom.Class
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Ref 
import Data.Dependent.Map as DMap hiding (null)
import Data.IORef
import System.Random
import Control.Lens
import Data.List
import System.Random
import Control.Concurrent
import Control.Arrow
import Text.Read
import Data.Maybe
import Data.Either

import Computing (controlThread)
import Reconf (Reconf (Reconf),store,stock,request)
import Report (mkReport, Report (Report))
import Direct (Solution)
import ReflexLib
 
nominalBar = 6000
reconf0 = Reconf [] nominalBar []

main :: IO ()
main = do
    report <- newTChanIO  -- new best  solution found from algo
    count <- newTChanIO -- new solution produced from algo
    reconf <- newTChanIO -- reconf population from interface
    pause <- newTVarIO False -- pause state
    -- relief Computing module from using STM
    let     control_report = atomically . writeTChan report 
            control_count = atomically $ writeTChan count ()
            control_reconf = atomically $ readTChan reconf
            control_pause = atomically $ readTVar pause
    -- start control thread
    forkIO $ controlThread control_reconf control_pause control_report control_count
    -- start interface
    mainWidget $ gui report count reconf pause


randompop :: MonadIO m => Reconf Int -> m [Int]
randompop rq = liftIO $ replicateM 50 $ randomRIO (l `div` 20, l `div` 2) where
    l = view stock rq

setpause :: MonadIO m => TVar Bool -> Bool -> m ()
setpause pause  = liftIO . atomically . writeTVar pause 

countingE :: TChan () -> TChan (Reconf Int) -> (Int -> IO ()) -> IO ()
countingE count reconf eat =  do
    reconfc <- atomically $ dupTChan reconf
    let turn i = do
            j <- atomically $ (i <$ readTChan count) `orElse` (0 <$ readTChan reconfc)
            eat j
            turn $ j + 1
    turn (0 :: Int)

gui     :: MS m 
        => TChan (Solution Int) -- incoming champion solutions
        -> TChan () -- signals of new solution computed
        -> TChan (Reconf Int) -- outgoing reconfiguration for computation
        -> TVar Bool -- outgoing paused state
        -> m ()
gui report count reconf pause = do  
    solutionE <- eventSource $ \e -> forever (atomically (readTChan report) >>= e) -- handle incoming new champion solutions
    counterE <- eventSource $ countingE count reconf -- handle incoming signal of new solution
    rec params <- foldDyn ($) reconf0 $ leftmost [changeAlgo,changeRandomClean]
        performEvent_ $ (liftIO . atomically . writeTChan reconf) <$> updated params
        changeRandomClean :: ES (Reconf Int -> Reconf Int) <- divClass "controls" $ do
            e <- button "random"
            r <- performEvent (randompop <$> tagDyn params e)
            e <- button "clear"
            let r0 = [] <$ e
            elClass "span" "counter" $  do
                rec pausing <- holdDyn False ep
                    ep <- domMorph pauseButton pausing
                performEvent_ (setpause pause <$> tagDyn pausing ep)
                holdDyn 0 counterE >>= display
            return $ (over request . const) <$> leftmost [r,r0]
        changeAlgo <- divClass "algo" $ do
            changeStock <- divClass "raw" $ do
                divClass "title" $ do
                    text "Stock: "
                    mapDyn (view stock) params >>= elClass "span" "stock_bar" . display 
                domMorph driveStock params
            changeStore <- divClass "raw" $ do
                divClass "title" $ text "Store"
                domMorph driveStore params
            changePopulation <- divClass "population" $ do
                rec     ch  <- combineDyn params state >>= domMorph population
                        state :: DS [Int] <- mapDyn (view request) params
                return ch
            divClass "bestdiv" $ holdDyn [] (leftmost [mkReport <$> solutionE, [] <$ changeRandomClean]) >>= domMorph result
            return $ leftmost [changeStock, changeStore,changePopulation]
    return ()


pauseButton :: MS m => Bool -> m (ES Bool)
pauseButton True = (False <$) <$> button "run"
pauseButton False = (True <$) <$> button "stop"

checkedInputCore f = divClass "input" $ do  
    rec error <- holdDyn (Right 0) $ leftmost [updated measure]
        domMorph morphError error
        input <- textInput def
        measure <- mapDyn f $ view textInput_value input 
    return (measure, input)

-- checkedIntInput :: MS m => ES (Int)
checkedInputSingle :: MS m => (String -> Either String Int) -> m (ES Int)
checkedInputSingle f = do
    rec let enter = fmap (\(Right x) -> x) . ffilter isRight $ fst <$> attach (current measure) (textInputGetEnter input)
        (measure,input) <- checkedInputCore f
    return enter 

----------------------------------------------
-------- stock -----------------------------
------------------------------------------

checkStock :: Int -> String -> Either String Int
checkStock st x = case readMaybe x of
        Nothing -> Left "Positive integer numbers only!"
        Just (x :: Int) -> 
            if x > 0 then 
                if x >= st then Right x 
                else  Left "Must be bigger than requests and store bars!"
            else Left "Positive integer numbers only!"

driveStock :: MS m => Reconf Int -> m (ES (Reconf Int -> Reconf Int))
driveStock rq = divClass "flat" $ 
        (set stock <$>) <$> checkedInputSingle (checkStock $ maximum (0 : (view store rq ++ view request rq)))
-----------------------------------------------

---------------------------------------------------
------ store -------------------------------------
------------------------------------------------
checkStore :: Int -> String -> Either String Int
checkStore st x = case readMaybe x of
        Nothing -> Left "Positive integer numbers only!"
        Just (x :: Int) -> 
            if x > 0 then 
                if x <= st then Right x 
                else  Left "Must be smaller than stock bar length!"
            else Left "Positive integer numbers only!"

driveStore :: MS m => Reconf Int -> m (ES (Reconf Int -> Reconf Int))
driveStore rq  = divClass "flat" $ do
    elClass "ol" "store" $ forM (view store rq) $ \x ->
        el "li" $ button (show x)
    (over store . (:) <$>) <$> checkedInputSingle  (checkStore $ view stock rq)
----------------------------------------------------------        

result :: MS m => [Report Int] -> m (ES ())
result xs = divClass "flat" $ do
    divClass "title" $ text "Response"
    elClass  "ol" "best" $ forM xs $ \(Report xs s t v) ->
        el "li" $ do 
            elClass "span" (if v then "locked" else "plus") $ text (show t)
            elClass  "ol" "bestcuts" $ do 
                forM xs $ \x ->
                    el "li" $ do 
                        text (show x)
                        elClass "span" "plus" $ text "+"
                elClass "li" "scarto" $ text (show s)
    return never

morphError :: MS m => Either String Int -> m (ES ())
morphError (Right _) = return never
morphError (Left x) = divClass "error" (text x) >> return never

checkMultiply "" = Right 1
checkMultiply x = case readMaybe x of
        Nothing -> Left "Positive integer numbers only!"
        Just (x :: Int) -> if x > 0 then Right x else  
            Left "Positive integer numbers only!"

checkMeasure st x = case readMaybe x of
        Nothing -> Left "Positive integer numbers only!"
        Just (x :: Int) -> 
            if x > 0 then 
                if x <= st then Right x 
                else  Left "Too big to fit in a stock bar!"
            else Left "Positive integer numbers only!"

bothRight (Right x, Right y) = True
bothRight _ = False

population :: MS m => (Reconf Int,[Int]) -> m (ES (Reconf Int -> Reconf Int))
population (rq,ns) = divClass "flat" $ do 
    divClass "title" $ text "Request"
    rec (measure, measure_input) <- checkedInputCore (checkMeasure $ view stock rq) 
        (multiply, multiply_input) <- checkedInputCore checkMultiply 
        let enter  = attach (current multiply) $ fst <$> attach (current measure) 
                        (leftmost [textInputGetEnter measure_input, textInputGetEnter multiply_input])
        elClass "ul" "pieces" $ forM (sort ns) $ \n -> 
            elClass "li" "select" $ button (show n)
    return ((\(Right n,Right x) -> over request $ (++) (replicate n x)) <$> ffilter bothRight enter)
















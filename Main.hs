{-# language ScopedTypeVariables #-}
{-# language PackageImports, ConstraintKinds #-}
{-# language RecursiveDo,NoMonomorphismRestriction,FlexibleContexts,TypeFamilies #-}


module Main where

import Reflex.Dom
import Reflex
import Reflex.Dom.Class
import Reflex.Host.Class -- (newEventWithTriggerRef)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Ref 
import Data.Dependent.Map as DMap 
import Data.IORef
import System.Random
import Control.Lens
import Data.List

import Direct

main :: IO ()
main = do
    cell <- newTChanIO  -- new best  solution found from algo
    count <- newTChanIO -- new solution produced from algo
    reset <- newTChanIO -- reset population from interface
    pause <- newTVarIO False -- pause flip flop
    let     ncell = atomically . writeTChan cell 
            ncount = atomically $ writeTChan count ()
            nreset = atomically $ readTChan reset
            npause = atomically $ readTVar pause
    -- start algo
    forkIO $ mainT ncell ncount nreset npause
    -- start interface
    mainWidget $ run cell count reset pause


run :: MS m => TChan (Solution Int) -> TChan () -> TChan [Int] -> TVar Bool ->  m ()
run c nc reset pause = do  
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    e <- newEventWithTrigger $ \et -> do
        unsubscribe <- forkIO . forever $  do
            x <- atomically (readTChan c) 
            postGui $ runWithActions [et :=> Identity x]
        return $ killThread unsubscribe
    ec <- newEventWithTrigger $ \et -> do
        resetc <- atomically $ dupTChan reset
        unsubscribe <- forkIO $ do  
            let turn i = do
                    j <- atomically ((i <$ readTChan nc) `orElse` (0 <$ readTChan resetc)) 
                    postGui $ runWithActions [et :=> Identity j]
                    turn $ j + 1
            turn (0 :: Int)
        return $ killThread unsubscribe
    
    
    rand <- divClass "controls" $ do
        e <- button "random"
        r <- performEvent (randompop reset <$ e)
        e <- button "pause"
        performEvent_ (setpause pause <$ e)
        e <- button "clean"
        r0 <- performEvent (liftIO (atomically (writeTChan reset []) >> return []) <$ e)
        elClass "span" "counter" $  holdDyn 0 ec >>= display
        return (const <$> leftmost [r,r0])
    divClass "algo" $ do
        divClass "population" $ do
            rec     ch :: ES ([Int] -> [Int]) <- domMorph population state
                    state :: DS [Int] <- foldDyn ($) [] $ leftmost [ch,rand]
                    performEvent_ (newpop reset <$> fst <$> attachDyn state ch)
            return ()
        divClass "bestdiv" $ 
            holdDyn [] (leftmost [report <$> e, [] <$ rand]) >>= domMorph result
    return ()

result :: MS m => [([Int],Int)] -> m (ES ())
result xs = do
    divClass "title" $ text "Response"
    elClass  "ol" "best" $ forM xs $ \(xs,s) ->
        el "li" $ do 
            elClass  "ol" "bestcuts" $ do 
                forM xs $ \x ->
                    el "li" $ do 
                        text (show x)
                        elClass "span" "plus" $ text "+"
                elClass "li" "scarto" $ text (show s)
    return never
population :: MS m => [Int] -> m (ES ([Int] -> [Int]))
population ns = do 
    divClass "title" $ text "Request"
    input <- textInput def
    ninput <- textInput def
    let 
        value :: BS String = current . view textInput_value $ input
        nvalue :: BS String = current . view textInput_value $ ninput
        enter :: ES (String,String) = attach nvalue $ fst <$> attach value (leftmost [textInputGetEnter input, textInputGetEnter ninput])
    elClass "ul" "pieces" $ forM (sort ns) $ \n -> 
        elClass "li" "select" $ button (show n)
    return ((\(n,x) -> (++ replicate (read n) (read x))) <$> enter)
   
newpop :: MonadIO m => TChan [Int] -> [Int] -> m ()
newpop reset = liftIO . atomically . writeTChan reset 

randompop :: MonadIO m => TChan [Int]  -> m [Int]
randompop reset = liftIO $ do
    ns <- replicateM 50 $ randomRIO (300,3000)
    atomically $ writeTChan reset ns
    return ns

setpause :: MonadIO m => TVar Bool -> m ()
setpause pause = liftIO $ do
    atomically $ modifyTVar pause not















-------  reflex missings --------------

type Morph t m a = Dynamic t (m a) -> m (Event t a)

mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn f d = mapDyn f d >>= dyn >>= joinE

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

pick :: (GCompare k, Reflex t) => k a -> Event t (DMap k Identity) -> Event t a
pick x r = select (fan r) x -- shouldn't we share fan r ?

gateWith f = attachWithMaybe $ \allow a -> if f allow then Just a else Nothing

pair x = leftmost . (: [x])
--------- buggy namings, wait for Dynamic functor instance ---------------

combineDynWith = Reflex.combineDyn
combineDyn = combineDynWith (,)

------------- Spider synonims

type ES = Event Spider
type DS = Dynamic Spider
type BS = Behavior Spider

-------------- Dom + spider synonyms

type MS = MonadWidget Spider
type Plug a = ES (DMap a Identity)

-- specialized mapMorph for the Dom host 
domMorph ::     MonadWidget t m 
                => (a -> m (Event t b))  -- widget rewriter
                -> Dynamic t a           -- driver for rewritings
                -> m (Event t b)         -- signal the happened rewriting
domMorph = mapMorph dyn

ifDomMorph b f = domMorph h b where
    h True = f
    h _ = return never


-------------- create a Plug ----------
mergeDSums :: GCompare a => [DSum a ES] -> Plug a
mergeDSums = merge . DMap.fromList

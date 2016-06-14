
{-# language ConstraintKinds, FlexibleContexts, NoMonomorphismRestriction #-}
module ReflexLib where

import Reflex hiding (combineDyn)
import qualified Reflex (combineDyn)
import Reflex.Dom
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Ref 
import Data.Dependent.Map as DMap hiding (null)
import Control.Concurrent
import Reflex.Host.Class -- (newEventWithTriggerRef)

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

eventSource :: MS m => ((a -> IO ()) -> IO ()) -> m (ES a)
eventSource f = do
    eGui <- (.) <$> askPostGui <*> askRunWithActions
    newEventWithTrigger $ \et -> forkIO (f $ \x -> eGui [et :=> Identity x]) >>= return . killThread



-------------- create a Plug ----------
mergeDSums :: GCompare a => [DSum a ES] -> Plug a
mergeDSums = merge . DMap.fromList

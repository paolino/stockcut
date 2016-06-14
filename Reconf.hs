{-# language ScopedTypeVariables #-}
{-# language PackageImports, ConstraintKinds,TemplateHaskell #-}
{-# language RecursiveDo,NoMonomorphismRestriction,FlexibleContexts,TypeFamilies #-}

module Reconf where

import Control.Lens

data Reconf a = Reconf {
    _store :: [a],
    _stock :: a,
    _request :: [a]
    }

makeLenses ''Reconf



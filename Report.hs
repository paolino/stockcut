module Report (Report (..), mkReport) where

import Data.List (sortBy)
import Direct

data Report a = Report {
    reportCuts :: [a],
    reportWaste :: a,
    reportTotal :: a,
    reportIsStored :: Bool
    } 

reportBar :: (Num a , Ord a) => Bar a -> Report a
reportBar = Report <$> sortBy (flip compare) . barElements <*> waste <*> total <*> locked

-- solution layout
mkReport :: (Num a , Ord a) => Solution a -> [Report a]
mkReport (Solution x xs) =  map reportBar (xs ++ [x])



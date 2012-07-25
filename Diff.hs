module Diff (
    diff,
    Diff(Del,Mod,Add)
) where

import Data.List
import Data.Function (on)

data Diff a = Del a | Mod a a | Add a deriving (Show)

getContent :: Diff a -> a
getContent (Del a)   = a
getContent (Add a)   = a
getContent (Mod _ a) = a

foldOnContent :: Eq b => (a -> b) -> [Diff a] -> [Diff a]
foldOnContent id' (Add a:Del b:xs)
    | (id' a) == (id' b) = Mod b a : foldOnContent id' xs
foldOnContent id' (Del a:Add b:xs)
    | (id' a) == (id' b) = Mod a b : foldOnContent id' xs
foldOnContent id' (x:xs) = x : foldOnContent id' xs
foldOnContent _ []       = []

diff :: (Eq a, Ord i) => (a -> i) -> [a] -> [a] -> [Diff a]
diff id' xs ys = foldOnContent id' $ sortBy compareOnContent (del ++ add)
    where
        del = map Del $ xs \\ ys
        add = map Add $ ys \\ xs
        compareOnContent = compare `on` (id' . getContent)


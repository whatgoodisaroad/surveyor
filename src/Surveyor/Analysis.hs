--------------------------------------------------------------------------------
-- Surveyor.Analysis
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Analysis (
        scan,
        guidedBy,
        searchingFor,

        collate,
        crosstab,
        dependent,

        Dist (..),
        Table (..)
    ) where

import Surveyor hiding (text)

import Data.Maybe
import Data.List
import Data.Generics

import Data.List
import Text.PrettyPrint.Boxes

scan :: (a -> Maybe b) -> [a] -> [b]
scan f = catMaybes . map f

-- Generically creates a query for the target value from the given survey.
-- The result is a function which returns a maybe.
-- The maybe is because the survey might not involve the target type at all,
-- or because the answer may be formed in such a way that the target 
-- type would not be present, for example if the type would only be present in the
-- untaken branch of an Either construction.
guidedBy :: Typeable b => Survey a -> a -> Maybe b
guidedBy (Respond _ fn) = cast
guidedBy (Choose _ c) = choiceGuidedBy c
guidedBy (Group _ sub)        = guidedBy sub
guidedBy (left :+: right)     = \a -> orElse 
    (guidedBy left $ fst a) 
    (guidedBy right $ snd a)

-- A choice version of the above function on surveys.
choiceGuidedBy :: Typeable b => Choice a -> a -> Maybe b
choiceGuidedBy   (Item _ a) = cast
choiceGuidedBy c@(_ :||: _) = fromJoin c
choiceGuidedBy   (l :|: _ ) = choiceGuidedBy l
choiceGuidedBy   (c :->: s) = \a -> orElse 
    (choiceGuidedBy c $ fst a) 
    (guidedBy s $ snd a)

fromJoin :: Typeable b => Choice (Either c d) -> Either c d -> Maybe b
fromJoin (l :||: _) (Left x)  = choiceGuidedBy l x
fromJoin (_ :||: r) (Right x) = choiceGuidedBy r x

searchingFor :: Typeable b => Prompt -> Survey a -> a -> Maybe b
searchingFor name pr@(Respond p fn) = if p == name then guidedBy pr else const Nothing
searchingFor name mc@(Choose p c) = if p == name then guidedBy mc else const Nothing
searchingFor name gp@(Group p sub) = if p == name then guidedBy gp else searchingFor name sub
searchingFor name (left :+: right) = \a -> orElse
    (searchingFor name left $ fst a)
    (searchingFor name right $ snd a)

-- Distributions

-- A distribution finds answers which bear given values.
-- In the case where the value is not present, for example, if it would
-- only be in the untaken branch of an Either, the key value can be 
-- Nothing, which corresponds to what would be printed as "N/A" on a table.
data Dist a b = Dist [(Maybe b, [a])]

instance Show b => Show (Dist a b) where
    show (Dist ds) = concat $ intersperse "\n" $ map showVal ds
        where
            total :: Float
            total = fromIntegral $ sum $ map (length . snd) ds

            showVal :: Show b => (Maybe b, [a]) -> String
            showVal (m, as) = (showMaybe m) ++ ":\t" ++ (show porp) ++ "%"
                where
                    porp :: Float
                    porp = 100.0 * (fromIntegral $ length as) / total
            
showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = "N/A"


-- Creates a distribution given an accessor function and a set of answers.
collate :: 
    Eq b =>
    (a -> Maybe b) ->
    [a] ->
    Dist a b
collate acc ans = Dist $ do
    let pairs = map (\a -> (acc a, a)) ans
    pivot <- nub $ map fst pairs
    let sub = map snd $ filter (\p -> pivot == fst p) pairs
    return (pivot, sub)

data Table p np = Table [Maybe p] [Maybe np] [[Int]]


instance (Show p, Show np) => Show (Table p np) where
    show (Table rows cols cells) = render $ rowHeaderBox <+> dataBox
        where
            maxWidthOf :: Show a => [a] -> Int
            maxWidthOf = (+1) . maximum . map (length . show)

            rowHeaderColumn :: [String]
            rowHeaderColumn = "" : map showMaybe rows

            dataColumns :: [[String]]
            dataColumns = zipWith (:) (map showMaybe cols) (transpose $ map (map show) cells)

            vert = vcat left
            horiz = hsep 1 left

            rowHeaderBox :: Box
            rowHeaderBox = vert $ map text rowHeaderColumn

            dataBox :: Box
            dataBox = horiz $ map (vert . map text) dataColumns

            
crosstab :: Eq a => Dist a b -> Dist a c -> Table b c
crosstab (Dist d1) (Dist d2) = Table (map fst d1) (map fst d2) cells
    where
        cells = do
            row <- d1
            return $ do
                col <- d2
                return $ 
                    length $ 
                    (snd row) `intersect` (snd col)

dependent :: Eq a => (Maybe b -> Bool) -> Dist a b -> Dist a c -> Table b c
dependent fn (Dist xs) rh = (Dist $ filter (fn . fst) xs) `crosstab` rh









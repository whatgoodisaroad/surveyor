--------------------------------------------------------------------------------
-- Surveyor.Analysis
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Analysis (
        genericAccessor,
        nameAccessor,

        collate,
        crosstab,

        Distribution (..),
        Table (..)
	) where

import Surveyor

import Data.Maybe
import Data.List
import Data.Generics --hiding ((:*:))

-- Generically creates a query for the target value from the given survey.
-- The result is a function which returns a maybe.
-- The maybe is because the survey might not involve the target type at all,
-- or because the answer may be formed in such a way that the target 
-- type would not be present, for example if the type would only be present in the
-- untaken branch of an Either construction.
genericAccessor :: Typeable b => Survey a -> a -> Maybe b
genericAccessor (ParsedResponse _ fn) = cast
genericAccessor (MultipleChoice _ c) = genericChoiceAccessor c
genericAccessor (Group _ sub)        = genericAccessor sub
genericAccessor (left :-: right)     = \a -> orElse 
    (genericAccessor left $ fst a) 
    (genericAccessor right $ snd a)

-- A choice version of the above function on surveys.
genericChoiceAccessor :: Typeable b => Choice a -> a -> Maybe b
genericChoiceAccessor c@(_ :*: _)        = fromJoin c
genericChoiceAccessor   (l :+: _)        = genericChoiceAccessor l
genericChoiceAccessor (Option _ a)       = cast
genericChoiceAccessor (OptionPlus _ a s) = res
    where 
        res = case cast a `asTypeOf` res undefined of
            Just _  -> cast . fst
            Nothing -> genericAccessor s . snd

fromJoin :: Typeable b => Choice (Either c d) -> Either c d -> Maybe b
fromJoin (l :*: _) (Left x)  = genericChoiceAccessor l x
fromJoin (_ :*: r) (Right x) = genericChoiceAccessor r x

nameAccessor :: Typeable b => Prompt -> Survey a -> a -> Maybe b
nameAccessor name pr@(ParsedResponse p fn) = if p == name then genericAccessor pr else const Nothing
nameAccessor name mc@(MultipleChoice p c) = if p == name then genericAccessor mc else const Nothing
nameAccessor name gp@(Group p sub) = if p == name then genericAccessor gp else const Nothing
nameAccessor name (left :-: right) = \a -> orElse
    (nameAccessor name left $ fst a)
    (nameAccessor name right $ snd a)



-- Distributions

-- A distribution finds answers which bear given values.
-- In the case where the value is not present, for example, if it would
-- only be in the untaken branch of an Either, the key value can be 
-- Nothing, which corresponds to what would be printed as "N/A" on a table.
data Distribution a b = Dist [(Maybe b, [a])]

instance Show b => Show (Distribution a b) where
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
    Distribution a b
collate acc ans = Dist $ do
    let pairs = map (\a -> (acc a, a)) ans
    pivot <- nub $ map fst pairs
    let sub = map snd $ filter (\p -> pivot == fst p) pairs
    return (pivot, sub)

data Table p np = Table [Maybe p] [Maybe np] [[Int]]

instance (Show p, Show np) => Show (Table p np) where
    show (Table rows cols cells) = concat $ intersperse "\n" $ topRow : rest
        where
            maxWidthOf :: Show a => [a] -> Int
            maxWidthOf = (+1) . maximum . map (length . show)

            widths :: [Int]
            widths = do
                n <- [0..(pred $ length cols)]
                return $ (succ $ length $ showMaybe $ cols !! n) `max` (maxWidthOf $ cells !! n)

            rowColumnWidth :: Int
            rowColumnWidth = maxWidthOf $ map showMaybe rows

            pad :: Int -> String -> String
            pad n s = s ++ (take (n - length s) $ repeat ' ')

            topRow :: String
            topRow = concat $ intersperse " " $ pad (pred rowColumnWidth) "" : map showMaybe cols

            rest :: [String]
            rest = do
                n <- [0..(pred $ length rows)]
                let header = pad rowColumnWidth $ showMaybe $ rows !! n
                return $ header ++ (concat $ zipWith pad widths $ map show $ cells !! n)
            
crosstab :: Eq a => Distribution a b -> Distribution a c -> Table b c
crosstab (Dist d1) (Dist d2) = Table (map fst d1) (map fst d2) cells
    where
        cells = do
            row <- d1
            return $ do
                col <- d2
                return $ 
                    length $ 
                    (snd row) `intersect` (snd col)

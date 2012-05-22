--------------------------------------------------------------------------------
-- Surveyor.Analysis
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Analysis (
		genericAccessor,

        collate,
        crossTab,

        Distribution (Dist),
        Table (..)
	) where

import Surveyor

import Maybe
import Data.List
import Data.Generics hiding ((:*:))

-- Generically creates a query for the target value from the given survey.
-- The result is a function which returns a maybe.
-- The maybe is because the survey might not involve the target type at all,
-- or because the answer may be formed in such a way that the target 
-- type would not be present, for example if the type would only be present in the
-- untaken branch of an Either construction.
genericAccessor :: Typeable b => Survey a -> a -> Maybe b
genericAccessor (ParsedResponse _ _) = cast
genericAccessor (MultipleChoice _ c) = genericChoiceAccessor c
genericAccessor (Group _ sub)        = genericAccessor sub
genericAccessor (left :-: right)     = \a -> orElse (genericAccessor left (fst a)) 
                                                    (genericAccessor right (snd a))

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
    

-- Distributions

-- A distribution finds answers which bear given values.
-- In the case where the value is not present, for example, if it would
-- only be in the untaken branch of an Either, the key value can be 
-- Nothing, which corresponds to what would be printed as "N/A" on a table.
data Distribution a b = Dist [(Maybe b, [a])]

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

-- An algebraic datatype for tables. Needs revision.
type CellData = String
data Table = Table [HeaderCell] [HeaderCell] [Row]
type HeaderCell = String
type Row = [CellData]

instance Show Table where
    show (Table rhs chs rows) = concat $ intersperse "\n" $ headerRow : dataRows
        where
            headerRow = concat $ intersperse "," $ "" : chs
            dataRows = map dataRow $ enumFromTo 0 $ pred $ length rows
            dataRow n = concat $ intersperse "," $ (rhs !! n) : (rows !! n)

-- Generate a table for a crosstab analysis of the distribution, given an additional
-- accessor function for the horizontal axis.
crossTab :: 
    (Eq b, 
        Show b, 
        Eq c, 
        Show c) => 
    (a -> Maybe c) -> 
    Distribution a b -> 
    Table
crossTab rangeAcc (Dist ds) = Table rowHead colHead rows
    where
        pivotVals = map fst ds
        rangeVals = nub $ do
            ofPivot <- map snd ds
            d <- ofPivot
            return $ rangeAcc d

        rowHead = map sj pivotVals
        colHead = map sj rangeVals

        rows = (flip map) ds $ \(_, ofPivot) -> 
            (flip map) rangeVals $ \rangeVal -> 
                show $ length $ (flip filter) ofPivot $ \ans -> 
                    rangeAcc ans == rangeVal

        sj :: (Show a) => Maybe a -> String
        sj (Just a) = show a
        sj (Nothing) = "N/A"






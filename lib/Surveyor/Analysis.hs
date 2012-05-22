--------------------------------------------------------------------------------
-- Surveyor.Analysis
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Analysis (
		surveyContains, 
		genericAccessor,

        collate,
        crossTab,

        Distribution (Dist),
        Table (..)
	) where

import Surveyor

import Maybe
import Data.List
import Data.Generics

-- A version of cast where a value of the target type can be passed, but isn't used.
cast' :: 
    (Typeable a, Typeable b) => 
    b ->
    a -> 
    Maybe b
cast' b = cast

-- Returns whether an answer to the survey contains or could contain the target type.
surveyContains :: 
    Data.Generics.Typeable b => 
    Survey a -> 
    b -> 
    Bool
surveyContains (ParsedResponse _ fn) b = isJust $ cast' b $ fn ""
surveyContains (MultipleChoice _ c) b = c `choiceContains` b
surveyContains (Group _ sub) b = sub `surveyContains` b
surveyContains (left :-: right) b = left `surveyContains` b || right `surveyContains` b

-- Returns whether an answer to the multiple choice question could be of the target type.
choiceContains :: 
    Data.Generics.Typeable b => 
    Choice a ->
    b -> 
    Bool
choiceContains (Option _ a) b = isJust $ cast' b a
choiceContains (OptionPlus _ a s) b = (isJust $ cast' b a) || s `surveyContains` b
choiceContains (left :+: right) b = left `choiceContains` b
choiceContains (left :*: right) b = left `choiceContains` b || right `choiceContains` b

-- Generically creates a query for the target value from the given survey.
-- The result is a maybe wrapped in a function which returns a maybe.
-- The first maybe is because the survey might not involve the target type at all,
-- and thus, no accessor could be given.
-- The inner maybe is because the answer may be formed in such a way that the target 
-- type would not be present, for example if the type would only be present in the
-- untaken branch of an Either construction.
genericAccessor :: 
    Data.Generics.Typeable b => 
    Survey a -> 
    b -> 
    Maybe (a -> Maybe b)
genericAccessor s b
    | s `surveyContains` b = case s of 
        (ParsedResponse _ _) -> Just cast
        (MultipleChoice _ c) -> c `genericChoiceAccessor` b
        (Group _ sub) -> sub `genericAccessor` b
        (left :-: right) -> orElse
            (fmap (.fst) $ left `genericAccessor` b) 
            (fmap (.snd) $ right `genericAccessor` b)
    | otherwise = Nothing

-- A choice version of the above function on surveys.
genericChoiceAccessor :: 
    Data.Generics.Typeable b =>
    Choice a ->
    b ->
    Maybe (a -> Maybe b)
genericChoiceAccessor c b
    | c `choiceContains` b = case c of
        (Option _ a) -> Just cast
        (OptionPlus _ a s) -> case (cast' b a) of 
            Just _ -> fmap (.fst) $ Just cast
            Nothing -> fmap (.snd) $ s `genericAccessor` b
        (left :+: right) -> left `genericChoiceAccessor` b
        (left :*: right) -> Just $ fromJoin c b
    | otherwise = Nothing
    where
        fromJoin :: 
            Data.Generics.Typeable b =>
            Choice (Either c d) -> 
            b ->
            (Either c d) -> 
            Maybe b
        fromJoin (left :*: right) b val
            | leftHas && rightHas = case val of 
                Left x -> lacc x
                Right x -> racc x
            | leftHas = maybeLeft val >>= lacc
            | rightHas = maybeRight val >>= racc
            where
                leftHas = left `choiceContains` b
                rightHas = right `choiceContains` b

                lacc = fromJust $ left `genericChoiceAccessor` b
                racc = fromJust $ right `genericChoiceAccessor` b

        maybeLeft :: Either a b -> Maybe a
        maybeLeft (Left a) = Just a
        maybeLeft _ = Nothing

        maybeRight :: Either a b -> Maybe b
        maybeRight (Right b) = Just b
        maybeRight _ = Nothing


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






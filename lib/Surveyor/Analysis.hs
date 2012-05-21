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


cast' :: 
    (Data.Generics.Typeable a,
        Data.Generics.Typeable b) => 
    b ->
    a -> 
    Maybe b
cast' b = cast

surveyContains :: 
    Data.Generics.Typeable b => 
    Survey a -> 
    b -> 
    Bool
surveyContains (ParsedResponse _ fn) b = isJust $ cast' b $ fn ""
surveyContains (MultipleChoice _ c) b = c `choiceContains` b
surveyContains (Group _ sub) b = sub `surveyContains` b
surveyContains (left :-: right) b = left `surveyContains` b || right `surveyContains` b

choiceContains :: 
    Data.Generics.Typeable b => 
    Choice a ->
    b -> 
    Bool
choiceContains (Option _ a) b = isJust $ cast' b a
choiceContains (OptionPlus _ a s) b = (isJust $ cast' b a) || s `surveyContains` b
choiceContains (left :+: right) b = left `choiceContains` b
choiceContains (left :*: right) b = left `choiceContains` b || right `choiceContains` b

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

data Distribution a b = Dist [(Maybe b, [a])]

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






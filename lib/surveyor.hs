
{-#LANGUAGE 
        GADTs,
        DeriveDataTypeable
    #-}

module Surveyor (
        Prompt,
        Survey (..),
        Choice (..),

        freeResponse,
        stringOption,
        stringOptionPlus,

        FirstName,
        LastName,
        FullName,
        askName,

        AgeRange,
        askAgeRange,

        LikertScale (..),
        likert,

        surveyCli
    ) where

import Maybe
import Control.Applicative

import qualified Data.Generics
import Data.Generics.Aliases
import Data.Typeable



type Prompt = String

data Survey a where
    Group :: 
        (Data.Generics.Data a, 
            Data.Generics.Typeable a) => 
        Prompt -> 
        Survey a -> 
        Survey a
    
    ParsedResponse :: 
        (Data.Generics.Data a, 
            Data.Generics.Typeable a) => 
        Prompt -> 
        (String -> a) -> 
        Survey a
    
    MultipleChoice :: 
        (Data.Generics.Data a, 
            Data.Generics.Typeable a) => 
        Prompt -> 
        Choice a -> 
        Survey a

    (:-:) ::
        Survey b -> 
        Survey c -> 
        Survey (b, c)

    Coll :: 
        (Data.Generics.Data a, 
            Data.Generics.Typeable a) => 
        [Survey a] -> 
        Survey [a]

data Choice a where
    Option :: 
        (Data.Generics.Data a, 
            Data.Generics.Typeable a) => 
        String -> 
        a -> 
        Choice a

    OptionPlus :: 
        (Data.Generics.Data b, 
            Data.Generics.Typeable b) => 
        String -> 
        b -> 
        Survey c -> 
        Choice (b, c)

    (:+:) :: 
        Choice a -> 
        Choice a -> 
        Choice a

    (:*:) :: 
        Choice b -> 
        Choice c -> 
        Choice (Either b c)












-- Helpers

maybeLeft :: Either a b -> Maybe a
maybeLeft (Left a) = Just a
maybeLeft _ = Nothing

maybeRight :: Either a b -> Maybe b
maybeRight (Right b) = Just b
maybeRight _ = Nothing






-- Generic analysis

cast'' :: 
    (Data.Generics.Typeable a,
        Data.Generics.Typeable b) => 
    b ->
    a -> 
    Maybe b
cast'' b = cast

surveyContains :: 
    Data.Generics.Typeable b => 
    Survey a -> 
    b -> 
    Bool
surveyContains (ParsedResponse _ fn) b = isJust $ cast'' b $ fn ""
surveyContains (MultipleChoice _ c) b = c `choiceContains` b
surveyContains (Group _ sub) b = sub `surveyContains` b
surveyContains (left :-: right) b = left `surveyContains` b || right `surveyContains` b

choiceContains :: 
    Data.Generics.Typeable b => 
    Choice a ->
    b -> 
    Bool
choiceContains (Option _ a) b = isJust $ cast'' b a
choiceContains (OptionPlus _ a s) b = (isJust $ cast'' b a) || s `surveyContains` b
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
        (OptionPlus _ a s) -> case (cast'' b a) of 
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

        




-- Specialize generic analysis code to the Gender type:

data Gender =
      Male 
    | Female 
    deriving (
        Eq, 
        Show, 
        Data.Generics.Data, 
        Data.Generics.Typeable
    )

askGender :: Survey Gender
askGender = MultipleChoice "Gender" $ Option "Male" Male :+: Option "Female" Female
--askGender = ParsedResponse "Gender" $ \r -> if r == "Male" then Male else Female

hasGender :: Survey a -> Bool
hasGender = (flip surveyContains) Male

createGenderAccessor :: Survey a -> Maybe (a -> Maybe Gender)
createGenderAccessor = (flip genericAccessor) Male

data GenderDemographics = Porportion Float
instance Show GenderDemographics where
    show (Porportion f) = 
            show malePercent 
        ++ "% Male, " 
        ++ show femalePercent 
        ++ "% female"
        where
            malePercent = f * 100.0
            femalePercent = (1.0 - f) * 100.0

calcGenderDemographics :: Survey a -> [a] -> Maybe GenderDemographics
calcGenderDemographics survey answers = do
    accessor <- createGenderAccessor survey
    let genders = map accessor answers
    let success = filter (/= Nothing) genders
    let males = filter (== Just Male) success
    return $ Porportion $ (fromIntegral $ length males) / (fromIntegral $ length answers)


test1 = calcGenderDemographics (askGender :-: askName) [
        (Male, ("Wyatt", "Allen")),
        (Female, ("Margaret", "Allen")),
        (Male, ("Hugo", "Gernsback")),
        (Female, ("Ursula", "LeGuin")),
        (Female, ("Ada", "Lovelace")),
        (Female, ("Catherine", "The Great"))
    ]

test2 = calcGenderDemographics (askName :-: askGender) [
        (("Wyatt", "Allen"),            Male),
        (("Margaret", "Allen"),         Female),
        (("Hugo", "Gernsback"),         Male),
        (("Ursula", "LeGuin"),          Female),
        (("Ada", "Lovelace"),           Female),
        (("Catherine", "The Great"),    Female)
    ]















-- Combinators

freeResponse :: String -> Survey String
freeResponse text = ParsedResponse text id

stringOption :: String -> Choice String
stringOption text = Option text text

stringOptionPlus :: String -> Survey a -> Choice (String, a)
stringOptionPlus text sub = OptionPlus text text sub






type FirstName = String
type LastName = String
type FullName = (FirstName, LastName)

askName :: Survey FullName
askName = 
        freeResponse "First name"
    :-: freeResponse "Last name"

type AgeRange = (Int, Int)

askAgeRange :: Survey AgeRange
askAgeRange = MultipleChoice "Age Group" (
            Option "< 20"       (0, 20)
        :+: Option "21 -- 40"   (21, 40)
        :+: Option "41 -- 60"   (41, 60)
        :+: Option "> 61"       (61, 200)
    )

data LikertScale = 
      StronglyAgree
    | Agree
    | NeitherNor
    | Disagree
    | StronglyDisagree
    deriving (Show, Data.Generics.Data, Data.Generics.Typeable)

likert :: Prompt -> Survey LikertScale
likert prompt = MultipleChoice prompt (
            Option "Strongly disagree"          StronglyDisagree
        :+: Option "Disagree"                   Disagree
        :+: Option "Neither agree nor disagree" NeitherNor
        :+: Option "Agree"                      Agree
        :+: Option "Strongly agree"             StronglyAgree
    )


{-queryFromPrompt :: Prompt -> Survey a -> Maybe (a -> Maybe b)
queryFromPrompt p sur = qfp sur id
    where
        qfp :: Survey a -> (a -> c) -> Maybe (a -> b)
        qfp (Group gp sub) part
            | gp == p = Just part
            | otherwise = Nothing
        qfp (ParsedResponse prp parser) part
            | prp == p = Just part
            | otherwise = Nothing
        qfp (MultipleChoice mcp cs) part
            | mcp == p = Just part
            | otherwise = Nothing

        qfp (l :-: r) part = case (qfp l part) of
            Nothing -> case (qfp r part) of
                Nothing -> Nothing
                Just f -> Just $ snd.f
            Just f -> Just $ fst.f-}

-- Run

selected :: Int -> Choice a -> IO a
selected 0 (Option _ val) = return val
selected 0 (OptionPlus _ val sub) = do
    subval <- surveyCli sub
    return (val, subval)
selected n (l :+: r) = do
    let llen = choiceLength l
    if n < llen
        then selected n l
        else selected (n - llen) r
selected n (l :*: r) = do
    let llen = choiceLength l
    if n < llen 
        then fmap Left $ selected n l
        else fmap Right $ selected (n - llen) r

choiceLength :: Choice a -> Int
choiceLength (Option _ _) = 1
choiceLength (OptionPlus _ _ _) = 1
choiceLength (l :+: r) = choiceLength l + choiceLength r
choiceLength (l :*: r) = choiceLength l + choiceLength r

surveyCli :: Survey a -> IO a

surveyCli (Group name sub) = do
    putStrLn ""
    putStrLn name
    putStrLn $ take (length name) $ repeat '='
    surveyCli sub
 
surveyCli (ParsedResponse prompt parser) = do
    putStr $ "\n" ++ prompt ++ ": "
    ans <- getLine
    return $ parser ans

surveyCli (MultipleChoice prompt choiceExp) = do
    putStr $ "\n" ++ prompt ++ ": \n"
    dispChoices 1 choiceExp
    ans <- readLn :: IO Int
    selected (ans - 1) choiceExp
        where
            dispChoices :: Int -> Choice a -> IO Int
            dispChoices num (Option text val) = do
                putStrLn $ "[" ++ show num ++ "] " ++ text
                return $ num + 1
            dispChoices num (OptionPlus text val _) = do
                putStrLn $ "[" ++ show num ++ "] " ++ text
                return $ num + 1
            dispChoices num (l :+: r) = do
                next <- dispChoices num l
                dispChoices next r
            dispChoices num (l :*: r) = do
                next <- dispChoices num l
                dispChoices next r

surveyCli (left :-: right) = do
    la <- surveyCli left
    ra <- surveyCli right
    return (la, ra)

surveyCli (Coll ss) = mapM surveyCli ss





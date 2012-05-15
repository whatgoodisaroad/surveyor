
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

import qualified Data.Generics
import Data.Generics.Aliases
import Data.Typeable
import Maybe

type Prompt = String

data Survey a where
    Group :: 
        (Data.Generics.Data a, Data.Generics.Typeable a) => 
        Prompt -> 
        Survey a -> 
        Survey a
    
    ParsedResponse :: 
        (Data.Generics.Data a, Data.Generics.Typeable a) => 
        Prompt -> 
        (String -> a) -> 
        Survey a
    
    MultipleChoice :: 
        (Data.Generics.Data a, Data.Generics.Typeable a) => 
        Prompt -> 
        Choice a -> 
        Survey a

    (:-:) ::
        Survey b -> 
        Survey c -> 
        Survey (b, c)

    Coll :: 
        (Data.Generics.Data a, Data.Generics.Typeable a) => 
        [Survey a] -> 
        Survey [a]

data Choice a where
    Option :: 
        (Data.Generics.Data a, Data.Generics.Typeable a) => 
            String -> a -> Choice a

    OptionPlus :: 
        (Data.Generics.Data b, Data.Generics.Typeable b) => 
            String -> b -> Survey c -> Choice (b, c)

    (:+:) :: Choice a -> Choice a -> Choice a
    (:*:) :: Choice b -> Choice c -> Choice (Either b c)




-- SYB Example

data Gender =
      Male 
    | Female 
    deriving (
        Eq, 
        Show, 
        Data.Generics.Data, 
        Data.Generics.Typeable
    )

cast' :: 
    (Data.Generics.Data a, Data.Generics.Typeable a) => 
    a -> 
    Maybe Gender
cast' = cast

askGender :: Survey Gender
askGender = MultipleChoice "Gender" $ Option "Male" Male :+: Option "Female" Female
--askGender = ParsedResponse "Gender" $ \r -> if r == "male" then Male else Female

hasGender :: Survey a -> Bool
hasGender (Group _ sub) =  hasGender sub
hasGender (ParsedResponse _ fn) = case (cast' $ fn "") of 
    Nothing -> False
    Just _ -> True

hasGender (MultipleChoice _ c) = choiceHasGender c
hasGender (left :-: right) = hasGender left || hasGender right
hasGender (Coll _) = False

choiceHasGender :: Choice a -> Bool
choiceHasGender (Option _ a) = case (cast' a) of
    Nothing -> False
    Just _ -> True
choiceHasGender (OptionPlus _ a s) = case (cast' a) of
    Nothing -> hasGender s
    Just _ -> True
choiceHasGender (left :+: right) = choiceHasGender left
choiceHasGender (left :*: right) = choiceHasGender left || choiceHasGender right

createGenderAccessor :: Survey a -> Maybe (a -> Maybe Gender)
createGenderAccessor s
    | hasGender s = case s of 
        (ParsedResponse _ fn) -> Just cast
        (MultipleChoice _ c) -> createGenderChoiceAccessor c

        (Group _ sub) -> createGenderAccessor sub
        (left :-: right) -> fromJoin s
    | otherwise = Nothing
    where
        fromJoin :: Survey (b, c) -> Maybe ((b, c) -> Maybe Gender)
        fromJoin (left :-: right) = orElse
            (fmap (.fst) $ createGenderAccessor left) 
            (fmap (.snd) $ createGenderAccessor right)

createGenderChoiceAccessor :: Choice a -> Maybe (a -> Maybe Gender)
createGenderChoiceAccessor c
    | choiceHasGender c = case c of
        (Option _ a) -> Just $ cast'
        (OptionPlus _ _ _) -> fromPlus c
        (left :+: right) -> createGenderChoiceAccessor left
        (left :*: right) -> fromJoin c
    | otherwise = Nothing
    where
        fromJoin :: Choice (Either b c) -> Maybe ((Either b c) -> Maybe Gender)
        fromJoin (left :*: right) = Nothing -- TODO


        fromPlus :: Choice (b, c) -> Maybe ((b, c) -> Maybe Gender)
        fromPlus (OptionPlus _ a s) = case (cast' a) of 
            Nothing -> fmap (.snd) $ createGenderAccessor s
            Just _ -> fmap (.fst) $ Just cast'

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





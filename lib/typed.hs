
{-#LANGUAGE GADTs #-}

type Prompt = String

data Survey a where
    FreeResponse :: Prompt -> Survey String
    MultipleChoice :: Prompt -> Choice a -> Survey a
    (:-:) :: Survey b -> Survey c -> Survey (b, c)

data Choice a where
    Option :: String -> a -> Choice a
    OptionPlus :: String -> a -> Survey b -> Choice (a, b)
    (:+:) :: Choice a -> Choice a -> Choice a
    (:*:) :: Choice b -> Choice c -> Choice (Either b c)

-- String option combinator

stringOption :: String -> Choice String
stringOption text = Option text text
    

-- Example:

simpleSurvey :: Survey (String, String)
simpleSurvey = 
        FreeResponse "First name"
    :-: FreeResponse "Last name"

mySurvey :: Survey 
    (
        (
            (String, String), 
            String
        ), 
        Either 
            String 
            (
                String, 
                (String, String)
            )
   ) 

mySurvey =
        simpleSurvey
    :-: MultipleChoice "Age group" (
                stringOption "< 20" 
            :+: stringOption "21 - 40"
            :+: stringOption "41 - 60"
            :+: stringOption "> 60"
        )
    :-: MultipleChoice "Occupation" (
                stringOption "Retail"
            :+: stringOption "Education"
            :*: OptionPlus "Government" "Government" govtQuestions
        )
    where
        govtQuestions :: Survey (String, String)
        govtQuestions =
                MultipleChoice "Department" (
                        stringOption "Federal"
                    :+: stringOption "Municipal"
                )
            :-: FreeResponse "Boss' last name"

-- Lickert question combinator

data LickertScale = 
      StronglyAgree
    | Agree
    | NeitherNor
    | Disagree
    | StronglyDisagree
    deriving (Show)

lickert :: Prompt -> Survey LickertScale
lickert prompt = MultipleChoice prompt (
            Option "Strongly disagree"          StronglyDisagree
        :+: Option "Disagree"                   Disagree
        :+: Option "Neither agree nor disagree" NeitherNor
        :+: Option "Agree"                      Agree
        :+: Option "Strongly agree"             StronglyAgree
    )

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
 
surveyCli (FreeResponse prompt) = do
    putStr $ "\n" ++ prompt ++ ": "
    ans <- getLine
    return ans

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






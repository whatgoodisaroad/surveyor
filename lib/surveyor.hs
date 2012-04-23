

-- DSL

type Prompt = String

data Question = 
      MultipleChoice Prompt [Choice] 
    | FreeResponse Prompt

data Choice = 
      Choice String
    | ChoicePlus String Survey
    
data Survey =
      Pose Question
    | Survey :-: Survey

type Answer = (Prompt, String)

-- Example
    
simpleSurvey :: Survey
simpleSurvey = 
        (Pose $ FreeResponse "First name")
    :-: (Pose $ FreeResponse "Last name")
    
    
    
mySurvey :: Survey
mySurvey = 
        (Pose $ FreeResponse "First name")
    :-: (Pose $ FreeResponse "Last name")
    :-: (Pose $ MultipleChoice "Age group" [
            Choice "< 20",
            Choice "21 - 40",
            Choice "41 - 60",
            Choice "> 60"
            ]
        )
    :-: (Pose $ MultipleChoice "Occupation" [
            Choice "Retail",
            Choice "Education",
            ChoicePlus "Government" govtQuestions
            ]
        )
    where
        govtQuestions :: Survey
        govtQuestions = 
                (Pose $ MultipleChoice "Department" [
                    Choice "Federal",
                    Choice "Municipal"
                ])
            :-: (Pose $ FreeResponse "Boss' last name")
        

    










-- Run

surveyCli :: Survey -> IO [Answer]
surveyCli (Pose q) = do
    ans <- questionToPrompt q
    return ans
surveyCli (q1 :-: q2) = do
    as1 <- surveyCli q1
    as2 <- surveyCli q2
    return $ as1 ++ as2
    
questionToPrompt :: Question -> IO [Answer]
questionToPrompt (FreeResponse prompt) = do
    putStr $ "\n" ++ prompt ++ ": "
    ans <- getLine
    return [(prompt, ans)]
questionToPrompt (MultipleChoice prompt choices) = do
    putStrLn $ "\n" ++ prompt ++ ":"
    dispChoices 1 choices
    ans <- readLn :: IO Int
    
    let selected = choices !! pred ans
    
    plus <- case selected of
        (Choice _) -> return []
        (ChoicePlus _ sub) -> surveyCli sub
    
    return $ [(prompt, promptOf selected)] ++ plus
    where
        dispChoices :: Int -> [Choice] -> IO ()
        dispChoices n [] = return ()
        dispChoices n ((Choice text):cs) = do
            putStrLn $ "[" ++ show n ++ "] " ++ text
            dispChoices (n + 1) cs
        dispChoices n ((ChoicePlus text _):cs) = do
            putStrLn $ "[" ++ show n ++ "] " ++ text
            dispChoices (n + 1) cs
        promptOf :: Choice -> String
        promptOf (Choice p) = p
        promptOf (ChoicePlus p _) = p




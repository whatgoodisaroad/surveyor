--------------------------------------------------------------------------------
-- Surveyor.Execute
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Execute (
		runSurvey,
	) where

import Surveyor
import Control.Applicative

selected :: Int -> Choice a -> IO a
selected 0 (Item _ val) = return val
selected n (choice :->: sub) = do
    choiceval <- selected n choice
    subval <- runSurvey sub
    return (choiceval, subval)
selected n (l :|: r)
    | n < llen  = selected n l
    | otherwise = selected (n - llen) r
    where llen = choiceLength l
selected n (l :||: r)
    | n < llen = Left <$> selected n l
    | otherwise = Right <$> selected (n-llen) r
    where llen = choiceLength l

runSurvey :: Survey a -> IO a

runSurvey (Group name sub) = do
    putStrLn $ "\n" ++ name
    putStrLn $ take (length name) $ repeat '='
    runSurvey sub
 
runSurvey (Respond prompt parser) = do
    putStr ""
    putStr $ prompt ++ " "
    ans <- getLine
    return $ parser ans

runSurvey (Choose prompt choiceExp) = do
    putStr $ "\n" ++ prompt ++ ": \n"
    dispChoices 1 choiceExp
    ans <- readLn :: IO Int
    selected (ans - 1) choiceExp
        where
            dispChoices :: Int -> Choice a -> IO Int
            dispChoices num (Item text _) = do
                putStrLn $ "[" ++ show num ++ "] " ++ text
                return $ succ num
            dispChoices num (choice :->: _) = dispChoices num choice
            dispChoices num (l :|: r) = disp2 num l r
            dispChoices num (l :||: r) = disp2 num l r

            disp2 :: Int -> Choice a -> Choice b -> IO Int
            disp2 num l r = do
                next <- dispChoices num l
                dispChoices next r

runSurvey (left :+: right) = do
    la <- runSurvey left
    ra <- runSurvey right
    return (la, ra)









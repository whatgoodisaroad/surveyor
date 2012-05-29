--------------------------------------------------------------------------------
-- Surveyor.Execute
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Execute (
		surveyCli
	) where

import Surveyor

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


enclose :: String -> [(String, String)] -> String -> String
enclose tag attrs "" = "<" ++ tag ++ (renderAttrs attrs) ++  "/>"
enclose tag attrs content = "<" ++ tag ++ (renderAttrs attrs) ++ ">" ++ content ++ "</" ++ tag ++ ">"

renderAttrs :: [(String, String)] -> String
renderAttrs [] = ""
renderAttrs ((n, v):as) = " " ++ n ++ "='" ++ v ++ "'" ++ renderAttrs as

toHtml :: Survey a -> String
toHtml (Group name sub) = enclose "fieldset" [] $ (enclose "legend" [] name) ++ (toHtml sub)
toHtml (left :-: right) = toHtml left ++ toHtml right
toHtml (Coll ss) = concat $ map toHtml ss

toHtml (ParsedResponse prompt _) = enclose "div" [] $ (enclose "label" [] prompt) ++ (enclose "input" [("type", "text")] "")
toHtml (MultipleChoice prompt choices) = enclose "div" [] $ (enclose "label" [] prompt) ++ (renderChoices choices)

renderChoices :: Choice a -> String
renderChoices (left :+: right) = renderChoices left ++ renderChoices right
renderChoices (left :*: right) = renderChoices left ++ renderChoices right

renderChoices (Option p v) = (enclose "input" [("type", "radio"), ("value", "")] "") ++ (enclose "label" [] p)
renderChoices (OptionPlus p v _) = (enclose "input" [("type", "radio"), ("value", "")] "") ++ (enclose "label" [] p) -- TODO: Handle sub


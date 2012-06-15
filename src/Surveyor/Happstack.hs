--------------------------------------------------------------------------------
-- Surveyor.Happstack
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Happstack (
        runServer
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Maybe

import Text.Html hiding (toHtml, sub, selected)
import Happstack.Server (
        nullConf, 
        simpleHTTP, 
        toResponse, 
        ok, 
        dir,
        methodM,
        Method(POST),
        look,
        ServerPart
    )

import Surveyor
import qualified Surveyor.Execute
import Surveyor.Analysis

-- Some static text.

frontMatter, backMatter, sub, css :: String
frontMatter = "<html><head><title>Surveyor</title><style>" ++ css ++"</style></head><body><form action='/ans' method='get'>"
backMatter = "</form></body></html>\n"
sub = "<div id='sub'><input type='submit' value='Submit'/></div>"
css = "\
    \html {\
    \    background:#444;\
    \    font-family:sans-serif;\
    \}\
    \body {\
    \    width:600px;\
    \    margin:4em auto;\
    \    background:#fff;\
    \    padding:2em;\
    \    border:1px solid #111;\
    \    -webkit-border-radius: 4px;\
    \    border-radius: 4px;\
    \}\
    \.respond-label { display:inline-block; width:120px; }\
    \div.q { margin:1em 0; }\
    \input[type=text] { width:400px; }\
    \.choices { padding:0.5em 2em; }\
    \#sub{ text-align:right; padding:0.5em 0; }\
    \pre {\
    \    background:#666;\
    \    color:#ddd;\
    \    padding:1em;\
    \    margin:1em;\
    \    border:1px solid #444;\
    \    -webkit-border-radius: 4px;\
    \    border-radius: 4px;\
    \    overflow:scroll;\
    \    font-size:8pt;\
    \}"

-- Enclose text in a document and lift as HTML, with or without a submit button.

doc, subdoc :: String -> Html
doc s = Html $ return $ HtmlString $ frontMatter ++ s ++ backMatter
subdoc s = doc $ s ++ sub

-- Given a survey, create a webserver.

runServer :: Show a => Survey a -> IO [a]
runServer survey = server >> return []
    where
        surveyHtml = subdoc $ toHtml "-" survey
        server = simpleHTTP nullConf $ msum [ 
                dir "ans" $ handleAnswer survey,
                ok surveyHtml 
            ]

handleAnswer :: Show a => Survey a -> ServerPart Html
handleAnswer survey = do
    ans <- decodeSurvey "-" survey
    let name = "Anonymous" `fromMaybe` (("First name" `searchingFor` survey) ans)
    let html = (enclose "h1" [] "Submitted") ++ (enclose "h2" [] $ "Thanks, " ++ name ++ "!") ++ (enclose "pre" [] $ show ans) ++ (enclose "a" [("href", "/")] "Again")
    ok $ doc html

decodeSurvey :: Path -> Survey a -> ServerPart a
decodeSurvey path (left :+: right) = do
    dl <- decodeSurvey (path ++ "l") left
    dr <- decodeSurvey (path ++ "r") right
    return (dl, dr)
decodeSurvey path (Group name subsurvey) = decodeSurvey (path ++ "g") subsurvey
decodeSurvey path (Respond _ p) = do
    val <- look path
    return $ p val
decodeSurvey path (Choose prompt choices) = do
    idx <- (read :: String -> Int) <$> look path
    return $ selected idx choices

enclose :: String -> [(String, String)] -> String -> String
enclose tag attrs "" = "<" ++ tag ++ (renderAttrs attrs) ++  "/>"
enclose tag attrs content = 
    "<" ++ tag ++ (renderAttrs attrs) ++ ">" ++ 
    content ++ 
    "</" ++ tag ++ ">"

renderAttrs :: [(String, String)] -> String
renderAttrs [] = ""
renderAttrs ((n, v):as) = " " ++ n ++ "='" ++ v ++ "'" ++ renderAttrs as

type Path = String

toHtml :: Path -> Survey a -> String

toHtml path (Group name sub) = enclose "fieldset" [] $ 
    (enclose "legend" [] name) ++ (toHtml (path ++ "g") sub)

toHtml path (left :+: right) = toHtml (path ++ "l") left ++ toHtml (path ++ "r") right

toHtml path (Respond prompt _) = enclose "div" [("class", "q")] $ 
    (enclose "label" [("class","respond-label"), ("for", path)] prompt) ++ (enclose "input" [("type", "text"), ("id", path), ("name", path)] "")
toHtml path (Choose prompt choice) = enclose "div" [("class", "q")] $ 
    (enclose "label" [] prompt) ++ (enclose "div" [("class", "choices")] $ renderChoices path 0 choice)

renderChoices :: Path -> Int -> Choice a -> String
renderChoices path idx (left :|: right) = renderChoices path idx left ++ renderChoices path (choiceLength left + idx) right
renderChoices path idx (left :||: right) = renderChoices path idx left ++ renderChoices path (choiceLength left + idx) right

renderChoices path idx (Item p v) = enclose "div" [("class", "c")] $
    (enclose "input" [("type", "radio"), ("value", show idx), ("name", path), ("id", path ++ show idx)] "") ++ 
    (enclose "label" [("for", path ++ show idx)] p)




selected :: Int -> Choice a -> a
selected 0 (Item _ val) = val
selected n (l :|: r)
    | n < llen  = selected n l
    | otherwise = selected (n - llen) r
    where llen = choiceLength l
selected n (l :||: r)
    | n < llen = Left $ selected n l
    | otherwise = Right $ selected (n-llen) r
    where llen = choiceLength l



















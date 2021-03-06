--------------------------------------------------------------------------------
-- Surveyor.Happstack
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE GADTs #-}

module Surveyor.Happstack (
        runServer
    ) where

import Control.Applicative
import Control.Monad.Writer

import Data.Maybe
import Data.Either
import Data.Char

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
        lookPairs,
        
        ServerPart,
        HasRqData
    )

import Surveyor
import qualified Surveyor.Execute
import Surveyor.Analysis

-- Some static text.

frontMatter, backMatter, sub, css :: String
frontMatter = 
        "<html><head><title>Surveyor</title><style>" 
    ++  css 
    ++  "</style></head><body onload='load'><form action='/ans' method='get'>"
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
    \div.q { margin:1em 0; position:relative; width:100%; }\
    \input[type=text] { width:400px; }\
    \.choices { padding:0.5em 2em; }\
    \#sub{ text-align:right; padding:0.5em 0; clear:both;: }\
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
    \}\
    \.sub { \
    \    border:1px solid #444; \
    \    -webkit-border-radius: 4px;\
    \    border-radius: 4px;\ 
    \    margin:1em 0; \
    \    display:none; \
    \}"


-- Enclose text in a document and lift as HTML, with or without a submit button.

doc, subdoc :: String -> Html
doc s = Html $ return $ HtmlString $ frontMatter ++ s ++ backMatter
subdoc s = doc $ s ++ sub

-- Given a survey, create a webserver.

runServer :: Show a => Survey a -> IO [a]
runServer survey = server >> return []
    where
        surveyHtml = subdoc $ toHtml' survey

        server = simpleHTTP nullConf $ msum [ 
                dir "ans" $ handleAnswer survey,
                ok surveyHtml 
            ]





handleAnswer :: Show a => Survey a -> ServerPart Html
handleAnswer survey = do
    allPairs <- getHeaderPairs

    let eans = decodeSurvey allPairs "-" survey

    case eans of
        Right ans -> sendSuccess ("Anonymous" `fromMaybe` (("First name" `searchingFor` survey) ans)) (show ans)
        Left next -> ok $ next

sendSuccess :: String -> String -> ServerPart Html
sendSuccess name ans = ok $ doc $ hwrite $ do
    hheader 1 "Submitted";
    hheader 2 $ "Thanks, " ++ name ++ "!";
    htag "pre" [] $ htext ans;
    htag "a" [("href", "/")] $ htext "Again";
    













getHeaderPairs :: (Monad m, HasRqData m) => m [(String, String)]
getHeaderPairs = do
    pairs <- lookPairs
    return $ map unRight $ filter isRight pairs
    where
        isRight :: (String, Either a String) -> Bool
        isRight (_, Right _) = True
        isRight _          = False

        unRight :: (String, Either a String) -> (String, String)
        unRight (l, Right r) = (l, r)



headerPairsToHiddens :: [(String, String)] -> String
headerPairsToHiddens [] = ""
headerPairsToHiddens ((k, v):ps) = shidden k v ++ headerPairsToHiddens ps


type Path = String






decodeSurvey :: 
    [(String, String)] -> Path -> Survey a -> 
    Either Html a

decodeSurvey pairs path (left :+: right) = do
    dl <- decodeSurvey pairs (path ++ "l") left
    dr <- decodeSurvey pairs (path ++ "r") right
    return (dl, dr)

decodeSurvey pairs path (Group name sub) = decodeSurvey pairs (path ++ "g") sub

decodeSurvey pairs path s@(Respond _ p) = case mval of 
    (Just val)  -> return $ p val
    Nothing     -> Left $ subdoc $ toHtmlP path s ++ headerPairsToHiddens pairs
    where
        mval = (path `lookup` pairs) >>= atLeastOneChar

        atLeastOneChar :: String -> Maybe String
        atLeastOneChar [] = Nothing
        atLeastOneChar s = Just s


decodeSurvey pairs path s@(Choose prompt choices) = case mval of
    Just idx    -> narrowSelection pairs path idx choices
    Nothing     -> dfail
    where
        dfail = Left $ subdoc $ toHtmlP path s ++ headerPairsToHiddens pairs

        mval = (path `lookup` pairs) >>= maybeReadInt >>= (intLessThan $ choiceLength choices)

        maybeReadInt :: String -> Maybe Int
        maybeReadInt s
            | willRead s    = Just $ (read :: String -> Int) s
            | otherwise     = Nothing
            where
                willRead :: String -> Bool
                willRead = all isDigit 

        intLessThan :: Int -> Int -> Maybe Int
        intLessThan m n
            | m > n     = Just n
            | otherwise = Nothing











narrowSelection :: 
    [(String, String)] -> Path -> Int -> Choice a -> 
    Either Html a

narrowSelection pairs path idx (l :|: r)
    | idx < llen    = narrowSelection pairs path idx l
    | otherwise     = narrowSelection pairs path (idx - llen) r
    where llen      = choiceLength l
narrowSelection pairs path idx (l :||: r)
    | idx < llen    = Left  <$> narrowSelection pairs path idx l
    | otherwise     = Right <$> narrowSelection pairs path (idx - llen) r
    where llen      = choiceLength l    
narrowSelection pairs path idx (choice :->: sub) = do
    sval            <- decodeSurvey pairs (path ++ "s") sub
    cval            <- narrowSelection pairs path idx choice
    return          (cval, sval)
narrowSelection pairs path 0 (Item _ val) = return val















type HyperAttr = (String, String)
type HyperGen = Writer String ()

hempty :: HyperGen
hempty = return ()

htext :: String -> HyperGen
htext = tell

renderAttributes :: [HyperAttr] -> String
renderAttributes [] = ""
renderAttributes ((n, v):as) = concat [ " ", n, "='", v, "'" ] ++ renderAttributes as

htag :: String -> [HyperAttr] -> HyperGen -> HyperGen
htag name as sub
    | null hsub = tell $ concat [ "<", name, has, "/>" ]
    | otherwise = tell $ concat [ "<", name, has, ">", hsub, "</", name, ">" ]
    where
        (_, hsub) = runWriter sub

        has = renderAttributes as

hfieldset :: String -> HyperGen -> HyperGen
hfieldset legend sub = htag "fieldset" [] $ do
    htag "legend" [] $ htext legend
    sub

hdiv :: String -> HyperGen -> HyperGen
hdiv cssClass sub = htag "div" [("class", cssClass)] sub

respondLabel :: String -> String -> HyperGen
respondLabel path prompt = htag "label" [("class","respond-label"), ("for", path)] $ htext prompt

hblank :: String -> HyperGen
hblank name = htag "input" [("type", "text"), ("id", name), ("name", name)] hempty

hradio :: String -> Int -> HyperGen
hradio name idx = htag "input" [("type", "radio"), ("value", show idx), ("name", name), ("id", name ++ show idx)] hempty

hheader :: Int -> String -> HyperGen
hheader n s = htag ('h' : show n) [] $ htext s

shidden :: String -> String -> String
shidden key value = 
        snd 
    $   runWriter 
    $   htag "input" [("name", key), ("value", value), ("type", "hidden")] hempty




toHtml' :: Survey a -> String
toHtml' = toHtmlP "-"

toHtmlP :: String -> Survey a -> String
toHtmlP path = snd . runWriter . writeHtml path

hwrite :: HyperGen -> String
hwrite = snd . runWriter


writeHtml :: Path -> Survey a -> HyperGen

writeHtml path (Group name sub) = hfieldset name $ writeHtml (path ++ "g") sub

writeHtml path (left :+: right) = do
    writeHtml (path ++ "l") left
    writeHtml (path ++ "r") right

writeHtml path (Respond prompt _) = hdiv "q" $ do
    respondLabel path prompt
    hblank path

writeHtml path (Choose prompt choice) = hdiv "q" $ do
    htag "label" [] $ htext prompt
    hdiv "choices" $
        hChoices path 0 choice


hChoices :: Path -> Int -> Choice a -> HyperGen

hChoices path idx (left :|: right) = do
    hChoices path idx left
    hChoices path (idx + choiceLength left) right

hChoices path idx (left :||: right) = do
    hChoices path idx left
    hChoices path (idx + choiceLength left) right

hChoices path idx (Item prompt value) = hdiv "choice" $ do
    hradio path idx
    htag "label" [("for", path ++ show idx)] $ htext prompt

hChoices path idx (c :->: s) = hChoices path idx c





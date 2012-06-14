--------------------------------------------------------------------------------
-- Surveyor
-- Wyatt Allen
--------------------------------------------------------------------------------

{-#LANGUAGE 
        GADTs,
        DeriveDataTypeable
    #-}

module Surveyor (
        
        -- DSL
        Prompt,
        Survey (..),
        Choice (..),

        -- Smart Constructors
        text,
        
        prompt, prompted,
        
        showItem, vdep, showItems,
        
        choose, (???),

        yes, no,

        (==>),

        -- Basic types

        ValueGen (..),

        Name, FullName, fullName,

        Gender (..), gender,

        AgeRange, askAgeRange,

        LikertScale (..), likert,

        Handedness (..), handedness,





        choiceLength

    ) where

import Data.List
import Data.Generics
import Data.Generics.Aliases
import Data.Typeable

-- DSL

type Prompt = String
data Survey a where
    Respond :: Typeable a => Prompt -> (String->a) -> Survey a
    Choose :: Typeable a => Prompt -> Choice a -> Survey a
    Group :: String -> Survey a -> Survey a
    (:+:) :: Survey b -> Survey c -> Survey (b,c)

data Choice a where
    Item :: Typeable a => Prompt -> a -> Choice a
    (:|:) :: Choice a -> Choice a -> Choice a
    (:||:) :: Choice b -> Choice c -> Choice (Either b c)
    (:->:) :: Typeable b => Choice b -> Survey c -> Choice (b,c)


-- Smart constructors

text :: Prompt -> Survey String
text p = Respond p id

prompt :: Prompt -> Choice Prompt
prompt p = Item p p

prompted :: Prompt -> Survey a -> Choice (Prompt,a)
prompted p = (prompt p :->:)

showItem :: (Show v, Typeable v) => v -> Choice v
showItem v = Item (show v) v

vdep :: (Show v, Typeable v) => v -> Survey b -> Choice (v,b)
vdep v = (showItem v :->:)

prompts :: [Prompt] -> Choice Prompt
prompts = foldr1 (:|:) . map prompt

showItems :: (Show a, Typeable a) => [a] -> Choice a
showItems = foldr1 (:|:) . map showItem

choose, (???) :: (Show a, Typeable a) => Prompt -> [a] -> Survey a
choose p = Choose p . showItems
(???) = choose

yes :: Choice Bool
yes = Item "Yes" True

no :: Choice Bool
no = Item "No" False

(==>) :: (Typeable a) => Prompt -> Survey a -> CondSurvey a
p ==> s = Choose p $ no :||: (yes :->: s)

type DepSurvey a b = Survey (Either a (a,b))
type CondSurvey b = DepSurvey Bool b

-- Basic types

type Name = String
type FullName = (Name, Name)

fullName :: Survey FullName
fullName = text "First name" :+: text "Last name"

data Gender = Male | Female
    deriving (Eq, Show, Typeable, Enum, Bounded)

instance ValueGen Gender

gender :: Survey Gender
gender = "Gender" ??? [Male, Female]

type AgeRange = (Int, Int)

askAgeRange :: Survey AgeRange
askAgeRange = Choose "Age Group" $
        Item "< 20"     (0, 20)
    :|: Item "21 -- 40" (21, 40)
    :|: Item "41 -- 60" (41, 60)
    :|: Item "> 61"     (61, 200)

class (Bounded a,Enum a) => ValueGen a where
    values :: [a]
    values = enumFrom minBound

data LikertScale = 
      StronglyAgree 
    | Agree 
    | NeitherNor
    | Disagree 
    | StronglyDisagree
    deriving (Eq, Typeable, Enum, Bounded)

instance Show LikertScale where
    show StronglyDisagree = "Strongly disagree"
    show Disagree         = "Disagree"
    show NeitherNor       = "Neither agree nor disagree"
    show Agree            = "Agree"
    show StronglyAgree    = "Strongly agree"

instance ValueGen LikertScale

likert :: Prompt -> Survey LikertScale
{-likert prompt = MultipleChoice prompt $
        showOption StronglyDisagree
    :+: showOption Disagree
    :+: showOption NeitherNor
    :+: showOption Agree
    :+: showOption StronglyAgree-}
likert = (??? values)

data Handedness = LeftHanded | RightHanded 
     deriving (Eq, Show, Typeable, Bounded, Enum)

instance ValueGen Handedness

handedness :: Survey Handedness
handedness = "Handedness" ??? values








choiceLength :: Choice a -> Int
choiceLength (Item _ _) = 1
choiceLength (c :->: s) = choiceLength c
choiceLength (l :|: r) = choiceLength l + choiceLength r
choiceLength (l :||: r) = choiceLength l + choiceLength r

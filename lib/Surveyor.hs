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

        -- Combinators
        freeResponse,
        stringOption,
        stringOptionPlus,

        -- Basic types

        FirstName,
        LastName,
        FullName,
        askName,

        Gender (..),
        askGender,

        AgeRange,
        askAgeRange,

        LikertScale (..),
        likert,

        Handedness (..),
        askHandedness

    ) where

import Data.List
import Data.Generics
import Data.Generics.Aliases
import Data.Typeable

-- DSL

type Prompt = String

data Survey a where
    Group :: Typeable a => Prompt -> Survey a -> Survey a
    ParsedResponse :: Typeable a => Prompt -> (String -> a) -> Survey a
    MultipleChoice :: Typeable a => Prompt -> Choice a -> Survey a
    (:-:) :: Survey b -> Survey c -> Survey (b, c)
    Coll :: Typeable a => [Survey a] -> Survey [a]

data Choice a where
    Option :: Typeable a => String -> a -> Choice a
    OptionPlus :: Typeable b => String -> b -> Survey c -> Choice (b, c)
    (:+:) :: Choice a -> Choice a -> Choice a
    (:*:) :: Choice b -> Choice c -> Choice (Either b c)

-- Combinators

freeResponse :: String -> Survey String
freeResponse text = ParsedResponse text id

stringOption :: String -> Choice String
stringOption text = Option text text

stringOptionPlus :: String -> Survey a -> Choice (String, a)
stringOptionPlus text sub = OptionPlus text text sub


showOption :: (Show a, Typeable a) => a -> Choice a
showOption a = Option (show a) a

showOptionPlus :: (Show b, Typeable b) => b -> Survey c -> Choice (b, c)
showOptionPlus a sub = OptionPlus (show a) a sub

-- Basic types

type FirstName = String
type LastName = String
type FullName = (FirstName, LastName)

askName :: Survey FullName
askName = 
        freeResponse "First name"
    :-: freeResponse "Last name"

data Gender = Male | Female 
    deriving (Eq, Show, Typeable)

askGender :: Survey Gender
askGender = MultipleChoice "Gender" $ 
        showOption Male 
    :+: showOption Female

type AgeRange = (Int, Int)

askAgeRange :: Survey AgeRange
askAgeRange = MultipleChoice "Age Group" $
        Option "< 20"       (0, 20)
    :+: Option "21 -- 40"   (21, 40)
    :+: Option "41 -- 60"   (41, 60)
    :+: Option "> 61"       (61, 200)

data LikertScale = 
      StronglyAgree
    | Agree
    | NeitherNor
    | Disagree
    | StronglyDisagree
    deriving (Eq, Typeable)

instance Show LikertScale where
    show StronglyDisagree = "Strongly disagree"
    show Disagree = "Disagree"
    show NeitherNor = "Neither agree nor disagree"
    show Agree = "Agree"
    show StronglyAgree = "Strongly agree"

likert :: Prompt -> Survey LikertScale
likert prompt = MultipleChoice prompt $
        showOption StronglyDisagree
    :+: showOption Disagree
    :+: showOption NeitherNor
    :+: showOption Agree
    :+: showOption StronglyAgree

data Handedness = 
      LeftHanded 
    | RightHanded
    deriving (
            Eq,
            Typeable
        )

instance Show Handedness where
    show LeftHanded = "Left handed"
    show RightHanded = "Right handed"

askHandedness :: Survey Handedness
askHandedness = MultipleChoice "Handedness" $ 
        showOption LeftHanded 
    :+: showOption RightHanded











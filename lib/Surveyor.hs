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
    Group :: (Data a, Typeable a) => Prompt -> Survey a -> Survey a
    ParsedResponse :: (Data a, Typeable a) => Prompt -> (String -> a) -> Survey a
    MultipleChoice :: (Data a, Typeable a) => Prompt -> Choice a -> Survey a
    (:-:) :: Survey b -> Survey c -> Survey (b, c)
    Coll :: (Data a, Typeable a) => [Survey a] -> Survey [a]

data Choice a where
    Option :: (Data a, Typeable a) => String -> a -> Choice a
    OptionPlus :: (Data b, Typeable b) => String -> b -> Survey c -> Choice (b, c)
    (:+:) :: Choice a -> Choice a -> Choice a
    (:*:) :: Choice b -> Choice c -> Choice (Either b c)

-- Combinators

freeResponse :: String -> Survey String
freeResponse text = ParsedResponse text id

stringOption :: String -> Choice String
stringOption text = Option text text

stringOptionPlus :: String -> Survey a -> Choice (String, a)
stringOptionPlus text sub = OptionPlus text text sub

-- Basic types

type FirstName = String
type LastName = String
type FullName = (FirstName, LastName)

askName :: Survey FullName
askName = 
        freeResponse "First name"
    :-: freeResponse "Last name"

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
askGender = MultipleChoice "Gender" $ 
        Option "Male" Male 
    :+: Option "Female" Female

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
    deriving (
            Eq,
            Show, 
            Data.Generics.Data, 
            Data.Generics.Typeable
        )

likert :: Prompt -> Survey LikertScale
likert prompt = MultipleChoice prompt $
        Option "Strongly disagree"          StronglyDisagree
    :+: Option "Disagree"                   Disagree
    :+: Option "Neither agree nor disagree" NeitherNor
    :+: Option "Agree"                      Agree
    :+: Option "Strongly agree"             StronglyAgree

data Handedness = 
      LeftHanded 
    | RightHanded
    deriving (
            Eq,
            Data.Generics.Data,
            Data.Generics.Typeable
        )

instance Show Handedness where
    show LeftHanded = "Left handed"
    show RightHanded = "Right handed"

askHandedness :: Survey Handedness
askHandedness = MultipleChoice "Handedness" $ 
        Option "Left handed" LeftHanded 
    :+: Option "Right handed" RightHanded











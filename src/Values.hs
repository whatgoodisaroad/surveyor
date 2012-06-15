{-# LANGUAGE DeriveDataTypeable #-}

module Values where

import Data.Typeable

class (Bounded a,Enum a) => Value a where
  values :: [a]
  values = enumFrom minBound

data Gender = Male | Female 
     deriving (Eq, Show, Typeable, Bounded, Enum)

data LikertScale = StronglyAgree | Agree | NeitherNor
                 | Disagree | StronglyDisagree
                 deriving (Eq, Show, Typeable, Bounded, Enum)

data Handedness = LeftHanded | RightHanded 
     deriving (Eq, Show, Typeable, Bounded, Enum)

instance Value Gender
instance Value LikertScale
instance Value Handedness

{-
To derivve Value automatically, we have to use Generics:

        http://www.haskell.org/ghc/docs/latest/html/users_guide/generic-programming.html

-}

genderValues :: [Gender]
genderValues = values

likertValues :: [LikertScale]
likertValues = values

handedValues :: [Handedness]
handedValues = values






import Surveyor
import Surveyor.Execute
import Surveyor.Analysis
import Surveyor.Happstack

import Data.Maybe

basicQuestions :: Survey ((FullName, Gender), Handedness)
basicQuestions = fullName :+: gender :+: handedness

sentimentQuestions :: Survey (Either Bool (Bool, LikertScale))
sentimentQuestions = Choose "Have you bought one of our notebooks?" $
             Item "No" False
        :||: Item "Yes" True :->: (likert "The notebook is good.")
    

type CustomerSurveyType = (
        ((FullName, Gender), Handedness), 
        (Either Bool (Bool, LikertScale))
    )

customerSurvey :: Survey CustomerSurveyType
customerSurvey = basicQuestions :+: sentimentQuestions

responses :: [CustomerSurveyType]
responses = [
        (((("John","Smith"),Male),RightHanded),
            Right (True,Agree)),
        (((("Jane","Doe"),Female),RightHanded),
            Right (True,Disagree)),
        (((("John","Smith"),Male),LeftHanded),
            Right (True,Agree)),
        (((("Jane","Doe"),Female),RightHanded),
            Right (True,StronglyAgree)),
        (((("Mary","Smith"),Female),LeftHanded),
            Right (True,StronglyDisagree)),
        (((("Jane","Doe"),Female),LeftHanded),
            Right (True,Disagree)),
        (((("John","Smith"),Male),RightHanded),
            Left False),
        (((("Jane","Doe"),Female),RightHanded),
            Right (True,NeitherNor)),
        (((("John","Smith"),Male),RightHanded),
            Right (True,NeitherNor)),
        (((("Jane","Doe"),Female),RightHanded),
            Right (True,Agree)),
        (((("John","Smith"),Male),RightHanded),
            Right (True,Agree)),
        (((("Jane","Doe"),Female),RightHanded),
            Right (True,StronglyAgree))
    ]

genders :: [Gender]
genders = guidedBy customerSurvey `scan` responses

handednesses :: [Handedness]
handednesses = guidedBy customerSurvey `scan` responses

genderDist :: Dist CustomerSurveyType Gender
genderDist = guidedBy customerSurvey `collate` responses


handednessDist :: Dist CustomerSurveyType Handedness
handednessDist = guidedBy customerSurvey `collate` responses

likertDist :: Dist CustomerSurveyType LikertScale
likertDist = guidedBy customerSurvey `collate` responses

genderAndHandedness :: Table Gender Handedness
genderAndHandedness = genderDist `crosstab` handednessDist

handednessAndLikert :: Table LikertScale Handedness
handednessAndLikert = likertDist `crosstab` handednessDist

main = runServer customerSurvey

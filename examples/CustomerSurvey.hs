import Surveyor
import Surveyor.Execute
import Surveyor.Analysis

import Data.Maybe

basicQuestions :: 
    Survey ((FullName, Gender), Handedness)
basicQuestions = 
    askName :-: askGender :-: askHandedness

sentimentQuestions :: 
    Survey (Either Bool (Bool, LikertScale))
sentimentQuestions = MultipleChoice
    "Have you bought one of our notebooks?" (
            Option "No" False
        :*: OptionPlus 
            "Yes" True 
            (likert "The notebook is good.")
    )

type CustomerSurveyType = (
        ((FullName, Gender), Handedness), 
        (Either Bool (Bool, LikertScale))
    )

customerSurvey :: Survey CustomerSurveyType
customerSurvey = 
    basicQuestions :-: sentimentQuestions

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
genders = catMaybes $ map 
    (genericAccessor customerSurvey)
    responses

handednesses :: [Handedness]
handednesses = catMaybes $ map 
    (nameAccessor "Handedness" customerSurvey)
    responses

genderDist :: 
    Distribution CustomerSurveyType Gender
genderDist = collate 
    (genericAccessor customerSurvey)
    responses


handednessDist :: 
    Distribution CustomerSurveyType Handedness
handednessDist = collate 
    (genericAccessor customerSurvey)
    responses

likertDist :: 
    Distribution CustomerSurveyType LikertScale
likertDist = collate 
    (genericAccessor customerSurvey)
    responses

genderAndHandedness :: Table Gender Handedness
genderAndHandedness = 
    genderDist `crosstab` handednessDist

handednessAndLikert :: Table LikertScale Handedness
handednessAndLikert = 
    likertDist `crosstab` handednessDist

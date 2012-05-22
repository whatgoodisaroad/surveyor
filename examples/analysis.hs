
import Surveyor
import Surveyor.Analysis

import Maybe

-- Example for tabulation

createGenderAccessor :: Survey a -> a -> Maybe Gender
createGenderAccessor = genericAccessor

createHandednessAccessor :: Survey a -> a -> Maybe Handedness
createHandednessAccessor = genericAccessor


type TabSurveyType = ((FullName, Gender), Handedness)
tabSurvey :: Survey TabSurveyType
tabSurvey = askName :-: askGender :-: askHandedness

tabAnswers :: [TabSurveyType]
tabAnswers = [
        ((("Wyatt", "Allen"),           Male),      LeftHanded),
        ((("Margaret", "Allen"),        Female),    RightHanded),
        ((("Hugo", "Gernsback"),        Male),      LeftHanded),
        ((("Ursula", "LeGuin"),         Female),    RightHanded),
        ((("Ada", "Lovelace"),          Female),    RightHanded),
        ((("Catherine", "The Great"),   Female),    LeftHanded)
    ]

tabGenderDist :: Distribution TabSurveyType Gender
tabGenderDist = collate (createGenderAccessor tabSurvey) tabAnswers

tabulation :: Table
tabulation = crossTab (createHandednessAccessor tabSurvey) tabGenderDist

main = print tabulation

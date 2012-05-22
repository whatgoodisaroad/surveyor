
import Surveyor
import Surveyor.Analysis

import Maybe

-- Example for tabulation

createGenderAccessor :: Survey a -> Maybe (a -> Maybe Gender)
createGenderAccessor = (flip genericAccessor) Male

createHandednessAccessor :: Survey a -> Maybe (a -> Maybe Handedness)
createHandednessAccessor = (flip genericAccessor) LeftHanded


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
tabGenderDist = collate (fromJust $ createGenderAccessor tabSurvey) tabAnswers

tabulation :: Table
tabulation = crossTab (fromJust $ createHandednessAccessor tabSurvey) tabGenderDist

main = print tabulation

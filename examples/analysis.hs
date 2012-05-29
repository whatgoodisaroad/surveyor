
import Surveyor
import Surveyor.Analysis

import Data.Maybe






-- Here's a type used by this example:
type TabSurveyType = ((FullName, Gender), Handedness)


















-- Here's a survey which produces that type:
tabSurvey :: Survey TabSurveyType
tabSurvey = askName :-: askGender :-: askHandedness






-- These are a set of responses that could have come from the survey:
tabAnswers :: [TabSurveyType]
tabAnswers = [
        ((("Robert", "Heinline"),       Male),      LeftHanded),
        ((("Timothy", "Zahn"),          Male),      RightHanded),
        ((("Neal", "Stephenson"),       Male),      RightHanded),
        ((("Ursula", "LeGuin"),         Female),    LeftHanded),
        ((("Edith", "Wharton"),         Female),    RightHanded),
        ((("Virginia", "Woolf"),        Female),    LeftHanded),
        ((("Cormac", "McCarthy"),       Male),      RightHanded),
        ((("Michael", "Chricton"),      Male),      RightHanded),
        ((("James", "Schmitz"),         Male),      LeftHanded)
    ]









-- A distribution over gender.
tabGenderDist :: Distribution TabSurveyType Gender
tabGenderDist = collate (genericAccessor tabSurvey) tabAnswers


-- A distribution over handedness
tabHandednessDist :: Distribution TabSurveyType Handedness
tabHandednessDist = collate (genericAccessor tabSurvey) tabAnswers




-- A crosstab of the distributions:
tabulation :: Table Gender Handedness
tabulation = tabGenderDist `crosstab` tabHandednessDist







main = print tabulation

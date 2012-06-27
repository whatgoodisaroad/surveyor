
import Surveyor
import Surveyor.Analysis
import Surveyor.Execute

import Data.Maybe






-- Here's a type used by this example:
type TabSurveyType = ((FullName, Gender), Handedness)


















-- Here's a survey which produces that type:
tabSurvey :: Survey TabSurveyType
tabSurvey = fullName :+: gender :+: handedness






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
tabGenderDist :: Dist TabSurveyType Gender
tabGenderDist =  guidedBy tabSurvey `collate` tabAnswers


-- A distribution over handedness
tabHandednessDist :: Dist TabSurveyType Handedness
tabHandednessDist = guidedBy tabSurvey `collate` tabAnswers




-- A crosstab of the distributions:
tabulation :: Table Gender Handedness
tabulation = tabGenderDist `crosstab` tabHandednessDist




maleHandedness = dependent (== Just Male) tabGenderDist tabHandednessDist


main = print tabulation

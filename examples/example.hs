import Surveyor

basicQuestions =
        askName
    :-: askAgeRange
    :-: MultipleChoice "Occupation" (
                stringOption "Retail"
            :+: stringOption "Education"
            :*: OptionPlus "Government" "Government" govtQuestions
        )
    where
        govtQuestions :: Survey (String, String)
        govtQuestions =
                MultipleChoice "Department" (
                        stringOption "Federal"
                    :+: stringOption "Municipal"
                )
            :-: freeResponse "Boss' last name"

groupedSurvey = Group "My amazing survey" (
            Group "Basic questions" basicQuestions
        :-: Group "Pointed questions" (
                    {- likert "Life is good"
                :-: likert "Ignorance is bliss"
                :-: likert "Free will is an illusion"
                :-: likert "The Smiths are a valuable contribution to music" -}

                Coll [
                        likert "Life is good",
                        likert "Ignorance is bliss",
                        likert "Free will is an illusion",
                        likert "The Smiths are a valuable contribution to music"
                    ]
            )
    )

main = surveyCli groupedSurvey >>= print
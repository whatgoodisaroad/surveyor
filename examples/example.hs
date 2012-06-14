import Surveyor
import Surveyor.Execute
import Surveyor.Happstack

basicQuestions = Group "Basic questions" $ 
        fullName 
    :+: gender 
    :+: askAgeRange
    :+: handedness

voteQuestion :: Survey (Either Char Bool)
voteQuestion = Choose
    "Did you vote/who did you vote for?" $
         (Item "Candidate A" 'a' :|:
          Item "Candidate B" 'b' :|:
          Item "Candidate C" 'c')
    :||: (Item "Didn't vote" False :|:
          Item "Rather not say" True)

customerQuestions = Group "Customer Satisfaction Questions" $ 
        likert "You liked the product."
    :+: likert "You would recommend it to a friend."
    :+: likert "Customer sevice was prompt and courteous."

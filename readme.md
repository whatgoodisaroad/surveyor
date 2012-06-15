# Surveyor
##### Wyatt Allen, School of EECS, Oregon State University

## Introduction

Surveyor is a DSEL for type-safe, composable surveys in Haskell. Whereas many survey systems are neither compositional nor typed, Survey takes an approach where the type of a survey is defined in terms of how it is composed. Within Surveyor one can do the following:

* Describe the structure of a survey in terms of headers, questions, choices and sub-surveys.
* Describe the questions to be included in a survey as free-response questions or multiple-choice questions.
* Describe the set of options available in a multiple choice question.
* Determine the type of the questions and the type of their corresponding answers.
* Design analysis tasks on the answers in terms of the survey they came from.

A DSL is needed here because, although surveys themselves are, in principle, structurally compositional, current ways of building surveys are not commonly structured in this way. Consequently, surveys cannot be reasoned about in terms of their separate components. Additionally, survey components are more difficult to reuse.

A related issue with existing survey systems is that the surveys are not generally strongly-typed in the sense that a survey's structure would indicate the type of answer data. In the case of Surveyor, the well-defined way in which surveys can be composed lends itself to a clear way that surveys can have types and how those types compose.

With strongly-typed surveys, it becomes possible to define analysis tasks in terms of the types, and to provide constructs for designing these analyses within the Surveyor language.

The Ruby DSEL published by Cunningham takes a different approach which is neither compositional nor sternly typed. We, however borrow useful concepts from this language while avoiding its shortcomings.

## Users

This DSL can be used to generate an survey. It can target different media, for example perhaps HTML
or an on-paper ``fill out the blanks'' style survey. The medium is irrelevant, and beyond the scope
of the DSL.

Therefore, the users of this DSL are those who wish to construct a survey to be presented to survey-takers. They will be domain experts only in the sense that they are experts in the information they wish to collect through the survey, but will not need to be experts in programming or in the target medium. For example, the survey writer need not be an expert in HTML in order to target HTML with their Surveyor code. These details are abstracted by the execution mechanisms provided by Surveyor.

## Outcomes

Because Surveyor provides survey tools for building, conducting, answering and analyzing, there are a handful of different outcomes.

* The outcomes of executing a Surveyor expression is a survey in some format, which can be presented and answered. For example, using the `runSurvey` function, the outcome is an interactive command-line interface to be presented in some terminal. On the other hand, using the `runServer` function, the outcome would be a webpage, which can be visited and answered through a web browser over a network.
* A set of answers is the outcome from the executed survey when attended by a respondent. Because the details of different survey media are abstracted by execution mechanisms, the type of answer resulting from using any of them on the same survey expression is uniform. The outcome of responding to a survey is an Haskell value of a type determined by the survey alone.
* Statistical results are the outcome of applying Surveyor's analysis constructs to the answer data. These can analyze information over any arbitrary type and seek relationships of variable dimensionality. (Ultimately, the dimensionality of analysis will also be able to be arbitrary.)

## Use Cases / Scenarios

The simplest survey would have one or more questions. Questions may be choices between 2 or more options (multiple-choice), may prompt the survey taker for a numeric value, or may be a text response.

A more complex use-case involves a survey where certain questions may or may not be presented to 
the survey taker based on how they've answered questions which were already presented. This can be
handled through modeling "sub-surveys", or "continuation-surveys" which are surveys on their 
own. They may be attached as children of choice expressions.

A survey-writer may wish to reuse parts of a survey, for example, if a certain set of questions are
only to be presented to a survey-taker in two answer configurations, he or she will not want to 
write this question set twice, but should be able to bind this set to a name, and then refer that 
name twice.

A survey-writer may wish to conduct a survey after having written it. This can be accomplished using a mechanism such as the `runSurvey` or `runServer` functions, depending on the format which the survey writer wishes to target (in these cases, a command-line terminal or webpage, respectively).

Having collected a large number of responses to a survey (via some medium), he or she may wish to make judgements regarding the data or discover relationships between answers. Surveyor allows for 0-dimensional analysis (analysis which does not take any particular type into account) via the list functions already available in Haskell. It allows for 1-dimensional analysis (analysis that takes only one inner data type into account when analyzing answer data) using the `Dist` (distribution) functionality which is provided as part of the language. Finally, it allows for 2-dimensional analysis (cross tabulation of two inner data types) through the composition of distributions. (Ultimately, the surveyor should allow for analysis of arbitrary dimensionality by defining the composition of these distributions, as well as providing dependent tabulations in addition to cross tabulations.)

## Basic Objects

The overview of basic objects in Surveyor is listed below.

* `Respond` questions are questions to which a respondent may reply with text. They are defined by the text used as a prompt for the question as well as a parsing function which is able to take the input from a respondent and produce a value of the appropriate type. Consequently, the constructor for this basic object is given by `Respond :: Typeable a => Prompt -> (String->a) -> Survey a`.
* `Choose` questions are questions to which a respondent must select an answer from a set of choices. They are defined by a prompt to display as well as a choice expression which represents the set of options for the question. The constructor type is given by `Choose :: Typeable a => Prompt -> Choice a -> Survey a`.
* Choice `Item`s are individual choices that can be used as answers to `Choose` questions. They are defined by a prompt to display as the textual representation of the option, as well as a value of some type to be used as the resulting value should that option be selected. The constructor for an `Item` is given by the following signature. `Item :: Typeable a => Prompt -> a -> Choice a`.
* Accessors are functions that follow a pattern wherein they can generically arrest values of  a given type from a survey answer when guided by the survey to which the answer corresponds. For example, the simplest accessor in Surveyor is `guidedBy`, which, given a `Survey a` and a value of type `a`, will attempt to extract a value of type `b`. If such an extraction is impossible, it will return `Nothing`. It is given by the following signature. `guidedBy :: Typeable b => Survey a -> a -> Maybe b`
* Distributions are analytical results demonstrating how values of some type find themselves in a set of answers. They bear the following datatype. `data Dist a b = Dist [(Maybe b, [a])]`.

These are the basic objects of Surveyor in the sense that they are not strictly composed of simpler objects. However, since one of the focuses of Surveyor is compositionality, this does not paint a representative picture of Surveyor's core. To gain a more complete perspective, we must examine the composition of these objects and how they are defined together with the objects themselves using GADT definitions.

## Operators and Combinators

### Combinators and GADT Definitions

In particular, questions and their composition is given by the following GADT definition. Within, the `Respond` and `Choose` constructors, familiar to us from the previous section, are shown where they are defined. Following them, the `Group` constructor is a way of attaching a label to a part of a survey, without changing the type of the survey. Finally, the `:+:` constructor combines two surveys together, and the result is a `Survey` of the pair of the types of the two tributary `Survey`s.

    type Prompt = String
    data Survey a where
        Respond :: Typeable a => Prompt -> (String->a) -> Survey a
        Choose :: Typeable a => Prompt -> Choice a -> Survey a
        Group :: String -> Survey a -> Survey a
        (:+:) :: Survey b -> Survey c -> Survey (b,c)

`Choice` expressions are given by the following GADT definition. Within, the `Item` constructor from the previous section is shown where it is defined. Composition of choice expressions is given by the `:|:` and `:||:` constructors, which compose `Choice` expressions of the same, and different types, respectively. The composition of two choices of the same type is results in a choice of that type, however when the types are different, the two types are unified under an `Either` construct. Finally, the `:->:` constructor attaches a sub-survey to a choice expression, meaning, should any choice within that expression be chosen, the sub-survey should be presented, but not otherwise.

    data Choice a where
        Item :: Typeable a => Prompt -> a -> Choice a
        (:|:) :: Choice a -> Choice a -> Choice a
        (:||:) :: Choice b -> Choice c -> Choice (Either b c)
        (:->:) :: Typeable b => Choice b -> Survey c -> Choice (b,c)

Distributions can be derived by the composition of an accessor with a set of answers. If we have an accessor function, which, after being guided by some `Survey a`, is of type `a -> Maybe b`, and we have a set of answers to that survey (which would be of type `[a]`) then a distribution of `b` values over those `a` answers can be produced with the `collate` function.

    collate :: Eq b => (a -> Maybe b) -> [a] -> Dist a b

Cross tabulations are defined by the composition of distributions from the same set of answers. For example, if we determined distributions of different inner-types (`b` and `c`) over answers from the same survey (of type `a`), then we can correlate these types into a `Table b c` using the `crosstab` function.

    crosstab :: Eq a => Dist a b -> Dist a c -> Table b c

### Smart Constructors and Operators

We provide a collection of smart constructors and operators which transform information from common survey scenarios into objects in the core types of the language. The simplest example of such a function is the `text` constructor, which can be used when a free response question is desired, but the text provided by the respondent is to be used as the result (i.e. no parsing). This can be handled by using `id` as the parsing function and packaging it up in the following way.

    text :: Prompt -> Survey String
    text p = Respond p id
    
In a similar way, it might be common to desire a choice item which simply uses a prompt `String` as its display text and as its value. This situation is handled by the `prompt` function. `prompts` is provided to give the same behavior for a set of `String`s.

    prompt :: Prompt -> Choice Prompt
    prompt p = Item p p

    prompts :: [Prompt] -> Choice Prompt
    prompts = foldr1 (:|:) . map prompt
    
Likewise, if a value is an instance of `Show`, then its `String` representation can be used as the display prompt, and its self used as the value for an `Item`.

    showItem :: (Show v, Typeable v) => v -> Choice v
    showItem v = Item (show v) v

    showItems :: (Show a, Typeable a) => [a] -> Choice a
    showItems = foldr1 (:|:) . map showItem

We can also provide survey parts, which are common bits of survey which can be freely composed and assembled into full-scale survey scenarios. For example, it is common for surveys to ask participants to answer along a Likert scale. We can provide an ADT for this datatype and a parameterized survey part which, given a `Prompt`, will present a multiple choice question with options from the scale.

    data LikertScale = 
          StronglyAgree 
        | Agree 
        | NeitherNor
        | Disagree 
        | StronglyDisagree
        deriving (Eq, Typeable, Enum, Bounded)

    likert :: Prompt -> Survey LikertScale
    likert prompt = MultipleChoice prompt $
            showOption StronglyDisagree
        :+: showOption Disagree
        :+: showOption NeitherNor
        :+: showOption Agree
        :+: showOption StronglyAgree

## Interpretation

Surveyor provides two execution mechanisms for running surveys against different target media. The `Surveyor.Execute` module exports a function given by the following signature `runSurvey :: Survey a -> IO a`, which, when given a survey of type `a`, produces an `IO` action which results in a value of type `a`. In particular, `runSurvey` creates a command-line interface for the survey which runs in a terminal. The result of this interface is, naturally, a value of the same type as the survey.

Additionally, we the `Surveyor.Happstack` provides a similar function given by `runServer :: Show a => Survey a -> IO [a]` which translates a survey into a website which outputs the survey as HTML and accepts and processes input over HTTP.

## Cognitive Dimension Evaluation

Surveyor exemplifies the following Cognitive Dimensions.

* The construction of **abstraction** is facilitated by the host language, Haskell. For example: 
    * Parameterized survey parts like the `likert` function are an abstraction of a common type of question.
    * In the `showItems` tool, the details of composing several options of the same type and with `Show`able values is abstracted away into a simple interface which makes use of Haskell's list type.
    * With the `guidedBy` function, the details of extracting values of a certain type from complex and deeply-nested tuple structure is abstracted away with a type-directed algorithm.
* Important links between surveyor objects cannot be hidden as described in the **Hidden Dependencies** dimension because all relationships need to be explicitly stated in Surveyor. This is, in part, thanks to the aggressively compositional nature of Surveyor expressions, which ensures a kind of referential transparency.
* **Premature Commitment** can become an issue in Surveyor if the survey-designer specifies type annotations to accompany survey constructions and they become outdated over the course of the survey's evolution. The survey designer must exercise caution and discretion in this regard.
* Surveyor strives to achieve **Closeness of Mapping** with its simple, unembellished syntax and explicitly specified type composition.
* The use of strong-typing helps to minimize the effects of the **Error-proneness** dimension.
* The dimension of **Progressive Evaluation** is supported by the compositional nature of surveyor expressions and the ease of using the execution functions such as `runSurvey`.

## Implementation Strategy

The keys to Surveyor's implementation are GADTs and SYB. GADTs provided a means to closely-connect survey structure to its type as well as provide a simple way to pattern match against constructors while traversing a surveyor expression. SYB provided a framework for the `guidedBy` accessor to generically seek values of arbitrary types, and which acted as a foundation for the analysis part of the language altogether. The Happstack package for Haskell is used in the `Surveyor.Happstack` package to handle the myriad details of a web server.

## Find Similar/Related DSLs

Cunningham published a DSEL in Ruby for representing surveys. Within, surveys are collections of multiple choice questions only. Further, each question can potentially have an expression which indicates whether the question is to be included in the survey. Each option in the question is defined with a closure which destructively assigns values in scope. For example, an option in a multiple choice question might look like `response "Female" { @female = true }`. This closure would be executed should that option be selected. The result of this design is that surveys in this language are not truly compositional because a conflict in the names assigned in the closures of the options will result in loss of data. Similarly, since Ruby is dynamically typed, there is no way to make type guarantees for the data. Cunningham's DSEL is itself based on a DSL by John Bentley, which is a very simple language implemented in BASIC, and which uses database column index numbers for storage in the question definition.

## Design Evolution

The most important change in the design of this language as it evolved was the change of using conventional Haskell ADTs for the language constructors. The code for these ADTs are listed below. In result, it was impossible to achieve the goal of type-safety using approach, so GADTs were used instead.

    type Prompt = String
    data Question = MultipleChoice Prompt [Choice] 
                  | FreeResponse Prompt
    data Choice = Choice String
                | ChoicePlus String Survey
    data Survey = Pose Question
                | Survey :-: Survey
    type Answer = (Prompt, String)

The degree to which the language would concern itself with analysis was not immediately clear, but when the goal of type safety was achieved by swwitching to GADTs, and it was clear how much type information could be preserved in a survey, it became a greater area of focus for the DSL.

## Future Work

The clearest next steps would be to improve upon the `Haskell.Happstack` package to be able to handle conditional surveys. It should also use HTML5 validation and be able to store answers and do analytics within the web environment before it could be considered ready to be internet-facing.

Another clear course of improvement would be to add to the existing analytics facilities. For example, the types and data storage should be reworked between the `Dist` and `Table` types such that they are really instances of the same type at different dimensionalities. Subsequently, arbitrary dimensionality could be achieved by repeated composions of analytical results.

## References

List references to similar DSLs identified in Section \ref{sec:related} and
potentially other related work.


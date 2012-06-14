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

The basic objects in this DSL are questions and answers.

* Surveys, which are made up of questions,
* Questions, some of which are made up of choices,
* Choices, which are either simply a string of text, or a string of text with another sub-survey,
* Answers, which are the product of a question and some value representing how it was answered, and
* Answer Sets, from which statistical statements can be drawn.


What are the basic objects that are manipulated and used by the DSL? Basic
objects are those that are not composed out of other objects. 

As a general rule, the fewer basic objects one needs, the better, because the
resulting DSL design will be more concise and elegant.

The basic objects should be described by a set of Haskell \prog{type} and
\prog{data} definitions. Use the \prog{program} environment to show code, as
illustrated below.

\begin{program}
type Point = (Int,Int)
\end{program}

Show how (some (parts) of) the examples from Section \ref{sec:examples} will be
represented by values of the envisioned types.

Also, list current limitations that you expect in a future iterations to overcome.

## Operators and Combinators

Identify operators that either transform objects into one another or
build more complex objects out of simpler (and ultimately basic) ones.

Depending on what implementation or form of embedding will be chosen,
operators may be given as constructors of data types or functions.

Combinators are higher-order functions that encode control structures of the
DSL. The function \prog{map} is a combinator that realizes a looping construct
for lists. The operations of the parser library Parsec are called \emph{parser
combinators} since parsers themselves are represented as functions.

The identification of the right set of combinators is a key step in the design
of the DSL.

With basic objects, operators, and combinators, you should be able to
demonstrate how the examples from Section \ref{sec:examples} can be
represented. All limitations encountered here should be classified as either:


* Temporary
* Fundamental

Temporary limitations should be noted in this section and in  Section
objects as TO DO items for future revisions of the design.

Fundamental limitations should be reported and listed in detail in Section


## Interpretation and Analyses

Provide a precise description of the different outcomes of the DSL in form of
how DSL programs can be interpreted (or compiled) and (statically or
dynamically) analyzed. 

These functions should be given as Haskell function signatures.

## Cognitive Dimension Evaluation

An assessment of cognitive dimension of your notation, such as closeness of
mapping, viscosity, hidden dependencies, and others, will help you with the
re-design of the DSL.

## Implementation Strategy

Discuss how the advantages and disadvantages of a deep or shallow embedding
play out in your DSL.

## Find Similar/Related DSLs

Try to find DSLs that are similar to the one described here and compare your
DSL with those. Note that similarity can be understood as *topical* as
well as *technical* similarity .

*Topically similar* DSLs are DSLs for the same or a closely related
domain. They have in principle the same or slightly different outcomes, but
they may be implemented quite differently. These DSLs help you refine the
design of the DSL requirements described in Sections users and
outcomes.

*Technically similar* DSLs are Haskell DSLs whose types and functions are
similar to the ones used in Sections objects and comb.
They can help sharpen the DSL modeling and implementation described in those
sections.

Ideally, you can find both, topically and technically similar DSLs. Be sure to
properly cite the DSLs as references.

**You will have to present these DSLs in a class presentation.**

## Design Evolution

As you iterate over different designs of your DSL, it is quite instructive to
document some of the old, obsolete designs, that is, show the type definitions
and function signatures, explain why this design seemed attractive at first
and then what motivated you to change it.

This part may seem like an unnecessary burden to you, but it helps you and
others to understand your current design, and it probably answers questions
that users (or reviewers) of the DSL might have about the design, because they
may have thought of your initial design also and are wondering why it has not
been adopted.

## Future Work

A speculation about what it takes to remove some of the limitations and
whether it seems worth the effort.

Moreover, what would be the concrete benefits to extend a shallow DSL into a
deep DSL? Or, would it be helpful to create an external DSL? What role could a
visual syntax or a GUI interface play?

## References

List references to similar DSLs identified in Section \ref{sec:related} and
potentially other related work.


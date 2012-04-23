# From Sheng

Wyatt, your surveyor idea is very interesting to me. At this phase, I think there are some chances that we may improve it.

* You may define a combinator to combine two surveys:

    comb:: [Prompt] -> Survey -> Survey

where `[Prompt]` specifies where the sub-survey should be attached to.

* You may parameterize the Survey and Question data constructor so you can define it as Functor or Monad instances.

* The answer data type is defined as `(Prompt, String)`, which allow users to specify any Prompt even though they are not in your surveyor. The problem is also the case for the Choice answer, user my answer something that is not listed as a choice. So probably you need to add more type information. GADTS may help here but I am not sure. I will probably have more comments when I have more idea about this.

Sheng

# From Karl

Hi Wyatt,

I think you are off to a strong start on your survey DSL, so here are a few rather simplistic comments:

* As I mentioned briefly in class, I think the structure of a survey probably fits into the Haskell typeclasses well. Functor comes to mind, or maybe Monoid.

* What about allowing for page breaks? One of the important things in designing surveys and questionnaires is ensuring that the user isn't overwhelmed with the number of visible questions (resulting in a lower completion rate).

* Continue thinking about how to type your questions and answers. Like the discussion about Duc's ideas, I think untyped (or String) representations are not very interesting. Perhaps some of the stuff from your GUI language is appropriate here, where you can specify that answers should be numeric, alphanumeric, a particular format for a telephone number or address etc.

* Maybe you could offer some method of offering a template for common question types. Likert scale questions are a reasonable example of what I mean.

* I can't remember the specifics of your multiple choice representation, but consider differentiating between only allowing one choice and allowing for "check any that apply" style questions.

Nice work.


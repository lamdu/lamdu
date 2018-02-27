# Localization plan

Soon, in Lamdu, the same code could have names in several languages,
and the standard library will be provided with multi-lingual names.

Each `Tag` (see
[lamdu-calculus](https://github.com/lamdu/lamdu-calculus/blob/master/README.md))
has an associate name, soon it will have associated names for each
language.  Users will set display language preferences and symbols
would be displayed in the preferred language, or in a fallback
language when the symbol doesn't have a name in the preferred one.

## Same name - different tags

Some words have several meanings.
[For example "bark"](https://www.espressoenglish.net/15-english-vocabulary-words-with-multiple-meanings/) (the noun bark refers to the outer covering of a tree. The verb bark refers to the sound a dog makes).

Such words may have different translations for each of their different meanings.
For example a tree bark is "corteza" in Spanish and a dog bark is "ladrido".

In this case one should have a different tag for each meaning and they should have extra associated tags: -
Each `Bark` tag would have an associated tag. One would be associated
with a `Tree` tag, the other with a `Dog` tag.

## Using the same name for several functions / variables

One may have several `circle` functions.
For example one for SVG diagrams and one for an SDL backend.
We don't want to use the C convention `sdl-circle`,
because it is verbose and will also unnecessarily lose the common defined translations for the word `circle`.
Therefore to share the name and its translations variables should have an associated `Tag`
and will also be disambiguated by their types, and additional associated tags for top-level definitions,
in this case one will be associated to `SVG` and the other will be associated to `SDL`.

Note that this will add an additional indirection between variables and their names.

## Disambiguation

When two tags have the same translation in the current language and
are both displayed simultaneously, their associated tags will be used
to disambiguate (where blue integers are currently used). When two
different variables have the exact same `Tag`, or when two tags don't
both have associated tags, integers will be used.

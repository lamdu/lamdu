# Localization plan

In Lamdu the same code could have names in several languages,
and the standard library is provided with multi-lingual names.

Each `Tag` (see
[lamdu-calculus](https://github.com/lamdu/lamdu-calculus/blob/master/README.md))
has an associated name for each language.
Users set display language preferences and symbols
are displayed in their preferred language,
or fallback to English when a name in their language isn't available
(fallback languages will also be a preference in the future).

## Using the same name for several functions / variables

One may have several `circle` functions.
For example one for SVG diagrams and one for an SDL backend.
We don't want to use the C convention `sdl-circle`,
because it is verbose and will also unnecessarily lose the common defined translations for the word `circle`.
Therefore to share the name and its translations variables should have an associated `Tag`
and will also be disambiguated by their types, and additional associated tags for top-level definitions (TBD),
in this case one will be associated to `SVG` and the other will be associated to `SDL`.

Note that this will add an additional indirection between variables and their names.

## Same name - different tags

Some words have several meanings.
[For example "bark"](https://www.espressoenglish.net/15-english-vocabulary-words-with-multiple-meanings/) (the noun bark refers to the outer covering of a tree. The verb bark refers to the sound a dog makes).

Such words may have different translations for each of their different meanings.
For example a tree bark is "corteza" in Spanish and a dog bark is "ladrido".

In this case one should have a different tags for the different meanings of "bark".
Lamdu has (TDB actually) disambiguation texts for the tags:
So that the user will know which tag they should choose.

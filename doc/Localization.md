# Localization plan

Status: This document describes something we plan to do but have yet to implement.

Currently all the function, type, and field names in Lamdu's standard library are in English.

In our vision, the exact same code could have names in several languages,
and the standard library will be provided with names in many languages.

A `Tag` (better name for this?) would have names in several languages stored.
Users will set display language preferences and symbols would be displayed in the preferred
language, or in a fallback language when the symbol doesn't have a name in the preferred one.

## Same name - different tags

Some words have several meanings.
[For example "bark"](https://www.espressoenglish.net/15-english-vocabulary-words-with-multiple-meanings/) (the noun bark refers to the outer covering of a tree. The verb bark refers to the sound a dog makes).

Such words may have different translations for each of their different meanings.
For example a tree bark is "corteza" in Spanish and a dog bark is "ladrido".

In this case one should have a different tag for each meaning and they should have extra associated tags -
Bark(Tree) and Bark(Dog).

When both tags are displayed and disambiguation tags are needed, these associated tags will be used rather than integers.

## Using the same name for several functions / variables

One may have several "circle" functions.
For example one for SVG diagrams and one for an SDL backend.
We don't want to use the C convention "sdl-circle",
because it is verbose and will also unnecessarily lose the common defined translations for the word "circle".
Therefore to share the name and its translations variables should have an associated name (`Tag`?)
and will also be disambiguated by their types, and additional associated tags for top-level definitions,
in this case one will be associated to "SVG" and the other will be associated to "SDL".

Note that this will add an additional indirection between variables and their names.

# Names

## When names clash

In textual programming languages, functions and variables are referred to by their names.

One cannot give two variables the same name in the same scope,
as there would be no way for the compiler to know which variable is being referring to.

## Name overloading

Some langauges, like C++, have some mechanisms for "overloading".

The compiler resolves which of several functions with the same name to call
based on the "function signature" inferred at the call site.

## When names clash in Lamdu

In Lamdu the situation is different: The compiler always knows which variable is used,
because in the stored AST, variables are referred to by unique identifiers,
which are different from their user-readable displayed names.

But the compiler isn't the only one who needs to distinguish between variables,
the programmer looking at the code should be able to distinguish between them too!

For this reason when the user gives two variables the same name,
they get displayed with "disambiguation suffixes" added to their names.

In a similar manner to "overloading",
when it's clear from context that the same name refers to different entities,
no disambiguation suffixes are added.

As an example: `1..100` and `1..100 step=2` are two uses of functions called "`..`", but their usage disambiguates (different parameter list).

## Auto-generated names in Lamdu

In Lamdu, parameters and let items are added via structural edits that do not require naming them.
i.e: one may create code without naming all variables.
But variables would still need to be displayed,
and when unnamed by the user they will get auto-generated names.

Auto-generated names are often fine for small lambdas like `num → num + 1`, and not having to name those variables is convenient.

To clarify that the auto-generated names are not meaningful, ephemeral and could automatically
change, they are displayed with *italic* fonts at their binding site.

## When do names clash in Lamdu

### Variable/function names

When two local variables are given the same name, but they are in different scopes, there is no
clash between them.  Hence, variable names clash when two variables which are accessible in a common
scope have the same name. This is trivially true for a global name and any other name that is
displayed.

A variable from an outer scope may clash with several inner scope variables that do not clash with
each other. In this case all of them get different disambiguation suffixes.

#### Clashes in hole results

When browsing global variables or Nominal types in hole results - that doesn't change the expression
outside the hole, so currently there are no disambiguation suffixes for these. The name inside the
hole result gets a question-mark as its disambiguation suffix in this cases.

## Implementation of Lamdu's naming process

The name auto-generation and disambiguation is done for the "LamduSugar" language (Lamdu's language
with "syntax sugars"), and the relevant code for it is under `Lamdu.Sugar.Names`.

* `Lamdu.Name` defines the `Name` data type for the names specifying their disambiguation numbers
  and whether they are given by the user or are auto-generated. This type is used as the "name"
  type-parameter for the sugar's expression types under `Lamdu.Sugar.Types`.

* `Lamdu.Sugar.Names.Add` is given expressions with `InternalName`s and converts them to `Name`s.

To find which names clash with which, and auto-generating names, `Add` needs to do
three passes on the expression tree.

These passes "walk" on the tree and change its "name" type-parameter. Because the traversal of the
tree is the same for all three passes, we created a type-class, `MonadNameWalk`, for the passes, so
that common AST-traversal code could be used for all of them. It can be thought of as an advanced
traversal (in the lens sense) that also informs about scoping rules.

* `Lamdu.Sugar.Names.Walk` defines the `MonadNameWalk` class and the expressions traversals for it.

The three passes each convert an `OldName m` to a `NewName m` and are:

### Pass 0: Load names

`Pass0LoadNames` loads the user-given names.  The monad used is a simple Transaction monad, used to
read the names from the database.

### Pass 1: Propagate inner scopes (upwards)

`Pass1PropagateUp` collects all used names "upwards", tagging each name with all names used in its underlying scope.

### Pass 2: Give final names (downwards)

`Pass2MakeNames` makes the final `Name`.

The monad used is a `Reader` because it flows the naming information downwards and decides on the
automatic names and assigns disambiguation suffixes.

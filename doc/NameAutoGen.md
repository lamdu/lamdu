# When names clash

In textual programming languages, functions and variables are referred to by their names.
One cannot give two variables the same name, as there would be no way for the compiler to know which of variables a user is referring to.

Some langauges, like C++, do have some mechanisms for "overloading", where the compiler infers which of several functions with the same name to call based on the number of arguments is was given and their types. In that case there may still be a clash (a compilation error) when the types of the functions also match.

## When names clash in Lamdu

In Lamdu the situation is different: The compiler always knows which variable is used where, because in the stored AST, variables are referred to by unique identifiers, which aren't their user-readable displayed names.

But the compiler isn't the only one who needs to distinguish between variables,
the programmer looking at the code also needs to distinguish between them!
For this reason when the user gives two variables the same name, they get displayed with "disambiguation tags" added to their names.

Disambiguation tags are small red numbers at the end of a variable name that Lamdu generates so that the user can understand which variable is used where.

# Auto-generated names in Lamdu

In Lamdu, program ASTs are edited, and one may create code without actually giving names to all of their variables. But variables would still need to be displayed, and when not named by the user they will get auto-generated names.

Auto-generated names are often fine for small lambdas like `λx → x+1`, and not having to name those variables is convenient.

Lamdu also makes sure that the auto-generated names will never clash with names given by the user. So if the user renames a variable so that it has the same name as an auto-generated name had, the auto-generated name changes, so that there will not be a clash! To clarify that the auto-generated names are ephemeral and could change when the user renames other variables, they are drawn with an *italic* font where they are introduced.

For local variables, Lamdu gives names such as `x`, `y`, etc. And in case is infers that the variable stands for a function it will name it `f`, `g`, etc.

The auto-generated names for global variables and type/tag names are ugly verbose identifiers. This is because we really want to encourage giving names to those.

# When do names clash?

## Variable/function names

When two variables are given the same name, but they are in different scopes, there is no clash between them.
Hence, variable names clash when two variables which are accessible in a common scope have the same name.

A variable from an outer scope may clash with several inner scope variables that do not clash with each other. In this case too all of them get different disambiguation tags.

## Type names and tags

Type (and tags) are not limited to a specific scope so when two types have the same name they always clash.

*(Note that this would be less of an issue for tags once we implement proper tag holes in Lamdu)*

# Implementation of Lamdu's naming process

The name auto-generation and disambiguation is done for the "LamduSugar" language (Lamdu's language with "syntax sugars"), and the relevant code for it is under `Lamdu.Sugar.Names`.

* `Lamdu.Sugar.Names.Types` defines the `Name` data type for the names specifying their disambiguation numbers and whether they are given by the user or are auto-generated. This type is used as the "name" type-parameter for the sugar's expression types under `Lamdu.Sugar.Types`.
* `Lamdu.Sugar.Names.Add` is given expressions with `UUID`s for names and does the naming process to output `Name`s for names.

To find which names clash with which, and auto-generating non-clashing names, `Add` needs to do three passes on the expression tree.

These passes "walk" on the tree and change its "name" type-parameter. Because the traversal of the tree is the same for all three passes, we created a type-class, `MonadNaming`, for the passes, so that common AST-traversal code could be used for all of them.

* `Lamdu.Sugar.Names.Walk` defines the `MonadNaming` class and the expressions traversals it.

The `MonadNaming` class also defines three type families, `OldName`, `NewName`, and `TM` (**TODO**: better name?), so that the walking functions have types such as:

    toWorkArea ::
        MonadNaming m =>
        WorkArea (OldName m) (TM m) a ->
        m (WorkArea (NewName m) (TM m) a)

The three passes are:

* `Pass0M`: Converts `UUID`s to `MStoredName` (where the "M" stands for maybe, as not all identifiers are given names by the user). It just loads the user-given names.
* `Pass1M`: Converts the `MStoredName`s to the `StoredNames` structure which adds to it a `storedNamesWithin` field which describes the names in the inner scopes (which could be clashed with).
* `Pass2M`: Converts `StoredNames` to the final `Name`.

**TODO**: Add some descriptions for what exactly `Pass1M` and `Pass2M` do and how they work!

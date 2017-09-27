# Parentheses and precedence

In traditional programming, the programmer uses parentheses to tell anyone reading the code, including the compiler, what the expression's actual tree structure is. Most languages employ the well known order of operations from mathematical notation to save the programmer from writing many parentheses, as well as making reading easier (once you get used to it).

In Lamdu, things work in opposite direction. The AST is known (that's how the code is stored), but to display it without ambiguity, the same method of parenthesis and an order of operations is employed.

In traditional languages a programmer may over-parenthesize, and a linter tool may warn about it. In Lamdu, the same AST is always displayed in the same way.

## Precedence in Lamdu

### How layout affects precedence

Lamdu's responsive design means that code may be "broken into lines" differently on different screen widths or font sizes.
The top-level parts have vertical layouts while subtrees that can fit horizontally are laid out as such.

Where horizontal layouts would use parentheses to disambiguate, vertically laid out trees use indentation instead.

Sometimes, the layout makes the tree structure unambiguous and saves parentheses - this happens for horizontal subtrees inside vertical layouts.

### Precedence on both sides

Here's the definition of `Precedence`

```Haskell
data Precedence = Precedence
    { _before :: {-# UNPACK #-}!Int
    , _after  :: {-# UNPACK #-}!Int
    }
```

Why does it have two parts? Consider the following expressions:

    (x → 1 .. x) ; x → 1 .. x
    f x → 1 + 2
    f (x → 1 + 2) + 3

* The Lambda to the left of the `;` operator requires parentheses and the one to its right does not.
* A lambda with nothing to its right does not need parentheses.

`makeSubexpressionWith` provides the subexpression editors this `Precedence` "from above" (which they read from environment via `outerPrecedence`).

**TODO:** resume this description

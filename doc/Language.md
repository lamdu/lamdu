# The Lamdu Language

Lamdu is a statically typed, purely functional language and an IDE.

The Lamdu language is different from traditional programming languages:

In traditional languages, the code that you see in your text editor is
the data is stored on disk.

In Lamdu, what you see in the editor and the data structure stored on
disk are different.  For example, the name of each variable is only
stored once, and all references to it use a unique identifier that
isn't visible to the user.

Lamdu's code is stored in the "Lamdu Calculus" language, a very small
lower level language.  In addition to this lower level calculus, Lamdu
stores associated meta-data, such as names, which is required to
present it.

Lamdu displays the code with additional "Projectional Syntactic
Sugars", which aren't part of the underlying Lamdu Calculus. The user
views and edits the code in this language, which is simply called the
"Lamdu Language".

## Syntactic sugar

In traditional programming, "syntactic sugar" lets the user write
things in clear and concise ways that expand to a lower level syntax.

* In C `a->x` expands to `(*a).x`
* In Python `f'My name is {name}'` expands to `'My name is {name}'.format(name=name)`
* In Haskell `[1,2,3]` expands to `1:2:3:[]` which further expands to `(:) 1 ((:) 2 ((:) 3 []))`
* In Scala `{_ * 2 + _}` expands to `{(x,y) => x * 2 + y}`

Traditionally, one may choose which syntactic sugars to use.
For example these two Python functions are the functionaly the same,
but syntactically different:

```Python
def sweet(n):
  if n % 3 == 0 and n % 5 == 0:
    return 'FizzBuzz'
  elif n % 3 == 0:
    return 'Fizz'
  elif n % 5 == 0:
    return 'Buzz'
  else:
    return str(n)

def sour(n):
  if n % 3 == 0 and n % 5 == 0:
    return 'FizzBuzz'
  else:
    if n % 3 == 0:
      return 'Fizz'
    else:
      if n % 5 == 0:
        return 'Buzz'
      else:
        return str(n)
```

There’s benefit in having short and simple ways to write code. But
there are also downsides:

* More choices to make (which syntax to use?) and less uniformity
* In some cases, small edits require rewrites. `{_ * 2 + _}` can't be
  changed to `{(x,y) => x * 2 + y*y}` (`y` changed to `y*y`) without
  "breaking the sugar".

## Lamdu's Projectional Syntactic Sugar

The Lamdu language works in the opposite direction - the code is
always presented in "sugared form", and the user's edits on the
sugared form are translated to edits on the underlying calculus.

## Underlying Lamdu Calculus language

[Lamdu Calculus](https://github.com/lamdu/lamdu-calculus) is an
extention of the [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

The calculus is decribed below in a human readable "pseudo-syntax".
Lamdu does not store code this way, but as an Hyper.

These are the terms of the language
(defined by [`Lamdu.Calc.Term`](https://github.com/lamdu/lamdu-calculus/blob/master/src/Lamdu/Calc/Term.hs):

* `_` - Holes (Used to store incomplete code)
* `5` - Literals
* Lambda Calculus terms
  * `a` - Variable references
  * `a → b` - Anonymous functions ("lambdas")
  * `a b` - Function application with a single argument
* Records
  * `{}` - Unit (Empty record)
  * `{ field value, rest }` - "Record Extention". Adds a field to a record
  * `record.field` - Get field from a record
* Variants
  * `'alt injected` - Inject a value to a variant type
  * `.case { alt: handler, rest }` - Pattern match a single variant constructor, "peeling off" the variant type
  * `.case {}` - The "absurd" (empty variant type) handler (used as "rest" in pattern match when no more cases remain to match)
* Nominal types
  * `Nom value` - Wrap a value in a nominal type
  * `value.Nom` - Unwrap a value from a nominal type

### Structural types and Nominal types

Most statically typed languages have nominal types. These are types that:

* Are declared explicitly
* Have an explicit name/identity
* Two nominal types are considered "the same" if their given name/identity is the same

For example, `Maybe` is a nominal type in Haskell:

```Haskell
data Maybe a = Nothing | Just a
```

Some languages also have `structural types`. These are types that are
defined by their underlying structure.

In contrast, anonymous tuples are structural types - they don't have
names and are defined by their structure.

Unlike nominal types, you don't need to ceremoniously declare the data
type - it is inferred from use.

#### Lamdu's structural types

Instead of anonymous tuples, Lamdu has structural records where each
field is named but the type is not.
One can create records "on the spot" anywhere without declarations.

Instead of explicitly declared variant types (also called "variant types"
or "tagged unions") Lamdu has structural variant types where each data
constructor is named but the type is not. These are also inferred from
use.

One can inject into a variant type "on the spot" or case-analyze a value
with no declarations.

#### Lamdu's nominal types

In addition to structural types, Lamdu also has explicitly declared
nominal types.

Nominal types may be parameterized by other types. Unlike traditional
"generics" and "templates", Lamdu's nominal types take keyword type
parameters.

These exist to serve several purposes:

* Facilitate recursive data types
* Safely distinguishing between similarly structured data of different meanings
* [Rank-N types](https://wiki.haskell.org/Rank-N_types)

More information can be found in the
[Lamdu Calculus](https://github.com/lamdu/lamdu-calculus#nominal-types) document.

## Lamdu's type language

Lamdu Calculus's type language is defined in [`Lamdu.Calc.Type`](https://github.com/lamdu/lamdu-calculus/blob/master/src/Lamdu/Calc/Type.hs).

The Lamdu Language uses the underlying type language directly without sugaring.

**TODO: Describe**

## Lamdu's syntax sugars

In addition to the Lamdu Calculus constructs which are all embedded in
the Lamdu Language, these syntactic sugars are also part of the Lamdu
Language:

### Labeled Apply and presentation modes

While Lamdu-Calculus only has unary functions (functions of a single argument),
Lamdu supports multiple arguments via an underlying record of parameters.

A call may be displayed as an infix operator application (`1 + 2`),
if the applied function's presentation mode is set to infix.

A function applied to multiple arguments whose presentation mode is
set to infix will always be displayed as infix, unlike Haskell where
one could use both `1 + 2` and `(+) 1 2`.

Like names, presentation modes are metadata which isn't part of the
underlying calculus.

#### Field parameters

Multiple parameters of a function are passed as a record in the underlying
calculus but are multiple different parameters in the Lamdu Language.

In the underlying language, each use of the parameter is a get-field
expression (`paramRecord.field`) and is sugared to a simple variable
access (`field`).

#### Punned arguments

When passing a field parameter as a field argument (i.e: both the
caller and callee are multi-parameter functions) of the same field, a
"punned arguments" sugar is used.

Instead of showing `bound: bound`, "punned arguments" are listed non-redundantly as
the last arguments after a `➾` symbol, and appears as `➾ bound`.

This is especially useful in recursive code where some of parameters
do not change in recursive calls.

### Binder Left-Hand-Side

This:

```Haskell
<var> = <param> → <body>
```

Gets sugared to:

```Haskell
<var> <param> = <body>
```

### Let bindings

Let bindings are expressed in the underlying calculus as
[redexes](https://en.wikipedia.org/wiki/Lambda_calculus#Reduction).

Redexes which occur at the top-level of a "binder body" are sugared to let bindings.

For example:

```Haskell
<arg> →
(<var> → <body>) <value>
```

Gets sugared to

```Haskell
<arg> →
let <var> = <value>
<body>
```

### Flattened records and pattern match functions

The Lamdu-Calculus constructs records by adding fields to the empty
record.  Lamdu displays these as a flat record, and re-orders the
fields according to field order out-of-band metadata.  Each field has
an ordering priority which is used as a sorting key.

The same is done for pattern matches / case-statements,
which in the underlying calculus are similarly constructed by adding
one variant-type handler at a time for each constructor.

TODO: Open records, Open cases

### Pattern matches / case-statements

Applied case statements (rather than un-applied [lambda cases](https://prime.haskell.org/wiki/LambdaCase)),
are shown with a pattern match syntax sugar which appears like:

```Haskell
case <argument> of
<cons0>: <handler1>
<cons1>: <handler1>
```

#### FromNoms in arguments

In the common case that the case function's argument is a FromNom, ie:

    <case-function> (<argument> Maybe»)

Then the FromNom part is "swallowed in the sugar" and shown as

```Haskell
case <argument> of
Nothing: <handler0>
Just: <handler1>
```

### If/elif/else

While the underlying Lamdu-Calculus expresses conditionals as pattern
matches on booleans, i.e:

```Haskell
( \case
  True: <handle0>
  False: <handle1>
) (condition Bool»)
```

Lamdu sugars it to an ordinary "if" expression:

```Python
if <cond0>:
  <handle0>
else:
  <handle1>
```

When the else clause is itself an if-expression, it gets sugared via "elif":

```Python
if <cond0>:
  <then0>
elif <cond1>:
  <then1>
else:
  <else>
```

### Suspended computations

Unlike Haskell, Lamdu is strict/eager.
Lazy style of programming is supported via suspended computations -
these are lambdas which get a "unit" (the empty record) parameter,
and rather than being displayed like normal lambdas:

    <arg> → <computation>

They are displayed with a lightweight vertical line:

    | <computation>

These can be used for [call-by-name](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_name)
evaluation.

### Light lambdas

Lamdu's equivalent of the `{_ * 2 + _}` syntax sugar for anonymous functions
from Scala is "light lambdas".

Light lambdas are displayed like so:

<u>`λ`</u>`.`<u>`accumulator`</u>`+`<u>`item`</u>

They are used for:

* Lambdas with multiple parameters
* Whose parameters are all used
* And are "simple". i.e: Whose bodies don't contain:
  * Variable bindings (Let bindings, lambdas)
  * Holes

(see `Lamdu.Sugar.Convert.Binder.useNormalLambda`)

### String Literals

The `Text` nominal type contains UTF-8 encoded `Bytes` values.

Instead of showing string literals as

    «Text #48656c6c6f

Lamdu sugars such expressions to string literals, i.e

    "Hello"

## Missing Features (TODOs)

TODO: elaborate more on these -

* Higher kinded type-parameters
* Type-classes
* UI to edit nominal types

# The Lamdu Language

In traditional programming,
the code that you see in your text editor is exactly the same thing that is stored on disk.

In Lamdu, what you see in the editor and the data structure stored on disk are different.
For example, the name of each variable is only stored once,
and all references to it are done by its unique identifier (which isn't visible to the user).

Lamdu's code is stored in the "Lamdu Calculus" language, a low-level and very small language.
In addition to the low level calculus, Lamdu stores meta-data, such as names,
which is required to present it.

Lamdu displays the code with additional "Projectional Syntactic Sugars",
which aren't part of the Lamdu Calculus,
and this displayed language is simply called the "Lamdu Language".

## How Lamdu's syntactic sugar is different from traditional syntactic sugar

In traditional programming, "syntactic sugar" lets the user write things in clear and concise ways that expand to a lower level syntax.

* In C `a->x` expands to `(*a).x`
* In Python `f'My name is {name}'` expands to `'My name is {name}'.format(name=name)`
* In Haskell `[1,2,3]` expands to `1:2:3:[]` which further expands to `(:) 1 ((:) 2 ((:) 3 []))`
* In Scala `{_ * 2 + _}` expands to `{(x,y) => x * 2 + y}`

In traditional programming languages one may choose to use syntactic sugars.
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

There’s benefit in having short and simple ways to write code. But there are also downsides:

* More choices to make (which syntax to use?)
* In some cases, small edits require rewrites. `{_ * 2 + _}` can't be changed to `{(x,y) => x * 2 + y*y}` (`y` changed to `y*y`) without "breaking the sugar".

So Lamdu's "Projectional Syntactic Sugar" works in the opposite direction -
the code is always presented in "sugared form" when applicable,
and the user's edits on the sugared form are translated to edits on the underlying calculus.

## Lamdu Calculus's value language

Lamdu Calculus is an extention of the [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

The calculus is decribed below in a human readable "pseudo-syntax".
(The code is actually stored as an AST, so the pseudo-syntax purposes is for documentation only)

These are the terms of the language:

* `?` - Holes (Used to store incomplete code)
* `5` - Literals
* Lambda Calculus terms
  * `a` - Variable references
  * `a → b` - Anonymous functions ("lambdas")
  * `a b` - Function application with a single argument
* Records
  * `()` - Unit (Empty record)
  * `{ field : value, rest }` - "Record Extention". Adds a field to a record
  * `record.field` - Get field from a record
* Sum types
  * `alt: injected` - Inject a value to a sum-type
  * `case value of { alt: handler, rest }` - Pattern match a single sum-type constuctor
  * `Ø` - The void sum type handler (used as "rest" in pattern match when no more cases remain to match)
* Nominal types
  * `«Nom value` - Wrap a value in a nominal type
  * `value »Nom` - Unwrap a value from a nominal type

(defined in [`Lamdu.Calc.Val`](https://github.com/lamdu/lamdu-calculus/blob/master/src/Lamdu/Calc/Val.hs))

### Structural types and Nominal types

Most statically typed languages have both anonymous tuples and nominal types which one needs to declare:

```Haskell
data Maybe a = Nothing | Just a
```

Anonymous tuples are structural types - they don't have names and what matters is their structure.
In Lamdu we have structural records instead of anonymous tuples - each field is named.
But unlike nominal types, you don't need to ceremoniously declare the data type - it is simply inferred.

Nominal types are also provided, and are useful to use in many cases
(this is elaborated in more detail in Lamdu Calculus's
[documentation](https://github.com/lamdu/lamdu-calculus/#nominal-types))

## Lamdu's type language

Lamdu's type language is currently identical to Lamdu Calculus's type language.

TODO: Describe

(defined in [`Lamdu.Calc.Type`](https://github.com/lamdu/lamdu-calculus/blob/master/src/Lamdu/Calc/Type.hs))

## Lamdu's syntax sugars

### Labeled Apply and definition presentation modes

While Lamdu-Calculus only has functions which accept a single argument,
Lamdu uses an arguments record to denote multiple arguments to a function.

A call may be displayed as an infix operator application, like `1 + 2`,
if the applied function's presentation mode is set to "infix".
A function whose presentation mode is set to infix will always be displayed as infix,
unlike Haskell where one could use both `1 + 2` and `(+) 1 2`.

Like names, presentation modes are metadata which isn't part of the underlying calculus.

Another presentation mode option is "OO",
where one of the arguments appears after the function name and does not have a label
describing it because it is considered obvious.
This is similar to calling conventions in some other languages like Objective-C and Swift.

#### Field parameters

To define the list of parameters, which usually can't be inferred from the incomplete code,
the argument list is stored in out-of-band metadata.

In the body of the function the parameters are also displayed as different parameters.
When used, instead of showing as `paramRecord.field` it simply gets displayed as `field`.

When the record of parameters is used directly, the multiple parameters sugaring isn't used.

#### Relayed arguments

When passing a field parameter as a field argument in a labeled application,
using the exact same field tag, instead of showing `bound: bound`,
"relayed" arguments sugar is used and all the relayed args are listed as the last arguments
after a `➾` symbol, and appears as `➾ bound`.

### Binder Left-Hand-Side

This:

```Haskell
<var> = <param> → <body>
```

Gets sugared to

```Haskell
<var> <param> = <body>
```

### Let bindings

Let bindings are expressed in the underlying calculus as
[redexes](https://en.wikipedia.org/wiki/Lambda_calculus#Reduction).

Redexes which occur at the top-level of a binder body are sugared to let bindings.

So this:

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

The Lamdu-Calculus constructs records by adding fields to the empty record.
Lamdu displays these as a flat record,
and re-orders the fields according to field order out-of-band metadata.
Each field has an ordering priority which is used to sort the field.

The same is done for pattern matches / case-statements,
which in the underlying calculus are similarly constructed by adding
one sum-type handler at a time for each constructor.

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

While the underlying Lamdu-Calculus expresses conditionals as pattern matches of booleans, i.e:

```Haskell
( \case
  True: <handle0>
  False: <handle1>
) (condition Bool»)
```

Lamdu sugars it to the "if" expression
familiar to programmers from traditional programming languages:

```Python
if <cond0>:
  <handle0>
else:
  <handle1>
```

When the else clause is itself an if-expression, it gets sugar as "elif" like so:

```Python
if <cond0>:
  <then0>
elif <cond1>:
  <then1>
else:
  <else>
```

### Suspended computations

Unlike Haskell, Lamdu is a strict/eager evaluation language.
Lazy style of programming is supported via suspended computations -
these are lambdas which get a "unit" (the empty record) parameter,
and rather than being displayed like normal lambdas:

    <arg> → <computation>

They are displayed with a lightweight line denoting the suspension:

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
* And don't contain in their bodies any:
  * Let bindings
  * Holes
  * Lambdas (except for lazy expressions which are allowed)

(see `Lamdu.Sugar.Convert.Binder.useNormalLambda`)

### String Literals

The `Text` nominal type just wraps UTF-8 encoded `Bytes` values.

Instead of showing string literals as

    «Text #48656c6c6f

Lamdu sugars such expressions to string literals, i.e

    "Hello"

## Missing Features (TODOs)

TODO: elaborate more on these -

* Higher order type-parameters
* Type-classes
* UI to edit nominal types

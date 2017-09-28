# Syntactic Sugar

In traditional programming, "syntactic sugar" lets the user write things in clear and concise ways that expand to a lower level syntax.

Examples:

* In C `a->x` expands to `(*a).x`
* In Python `f'My name is {name}'` expands to `'My name is {name}'.format(name=name)`
* In Haskell `[1,2,3]` expands to `1:2:3:[]` which further expands to `(:) 1 ((:) 2 ((:) 3 []))`
* In Scala `{_ * 2 + _}` expands to `{(x,y) => x * 2 + y}`

There’s benefit in having short and simple ways to write code. But there are also downsides:

* More choices to make (which syntax to use?)
* In some cases, small edits require rewrites. `{_ * 2 + _}` can't be changed to `{(x,y) => x * 2 + y*y}` (`y` changed to `y*y`) without "breaking the sugar".

Lamdu's "Projectional Syntactic Sugar" works in the opposite direction.
The code is stored in the lower level [Lamdu-Calculus](https://github.com/lamdu/lamdu-calculus/) language,
which is converted to the "Sugared" Lamdu language before being displayed to the user.

The user's edits on the sugared language are translated to edits on the underlying calculus.

The sugared language and its editing actions are defined in `Lamdu.Sugar.Types`.
The conversion process is implemented in `Lamdu.Sugar.Convert`.

## Lamdu's syntax sugars

### Names as metadata

The underlying Lamdu-Calculus does not have human readable variable names,
only machine-readable identifiers.
The Lamdu Sugar language stores the human readable given names as "out-of-band metadata".
By "out-of-band" we mean that this data isn't stored in the Lamdu-Calculus program,
so things that process it, like the type inference and compiler, do not see it.

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
familiar to programmers from traditional programming languages.

Nested "if" expressions, i.e:

```Python
if <cond0>:
  <then0>
else:
  if <cond1>:
    <then1>
  else:
    <else>
```

Get sugared as "elif" like so:

```Python
if <cond0>:
  <then0>
elif <cond1>:
  <then1>
else:
  <else>
```

This is useful to avoid over-indentation.

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

### Underlying Lamdu-Calculus

Anything which doesn't fit the syntactic sugar forms gets
shown as the underlying Lamdu-Calculus syntax, defined in
[`Lamdu.Calc.Val`](https://github.com/lamdu/lamdu-calculus/blob/master/src/Lamdu/Calc/Val.hs)

Theses include:

* `f x` - Function application with a single argument.
* `x → <body>` - Lambdas of a single argument.
* `record.field` - Get a field from a record.
* `<constructor>: <value>` - Inject a value into a sum type.
* `«Nominal value`, `value Nominal»` - wrap and extract values from Nominal types.
* `x` - Reference a variable.
* `3.14`, `#48656c6c6f` - Literals.
* `()` - Empty record / unit.
* `absurd` - The base case for pattern matches (handler for the void sum type).

## Design principles

While it may be exciting to add many wonderful syntax-sugars,
having too many of them means the user needs to learn more syntax forms.

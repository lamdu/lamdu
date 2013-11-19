# Project Lamdu

This project aims to create a "next-generation", "live programming" environment that radically improves the programming experience.

The main idea behind the Lamdu project is that the canonical representation of programs should not be text, but rich data structures: Abstract syntax trees.

Our programming tools, UIs and ecosystems should take advantage and expose this structure. This has some far-reaching implications, which will be described below.

## Programming Language

We believe the Haskell language is a great language for rapid development of reliable programs, due to 2 main traits:

### Effect typing

Haskell has explicit type-level separation of effectful components from pure components.
Real Haskell programs demonstrate that the majority of code can be written as pure components.
This allows a live environment to actually execute code as it is being edited, safely, and bring the benefits of spreadsheets to general purpose programming.

### Rich and powerful type system

Haskell has one of the richest and most powerful type systems, which allows a smart IDE to guide the programmer quickly through the space of type-correct programs. In practice, this reduces much of the cognitive burden of writing programs and the number of input gestures/keystrokes required to write or edit programs.

### Why not Haskell

Haskell was designed for textual editing. The Programming Language for Lamdu is optimized for rich structural editing.

Therefore, Lamdu implements a programming language very similar to Haskell, with some modifications.

#### Explicit argument names

In order to present explicit argument names for most function applications, records are used for conventional parameters. This means that the Lamdu programming language does not conventionally use currying. Haskell's use of currying affords very concise notation for partial function application, which is a wonderful feature. In a rich IDE, however, partial application can be visualized and edited concisely without currying.

#### Explicit type variables

Haskell, like most Hindley Milner languages, hides the type-variable lambdas from types and type variable applications/instantiations in use-sites. In Lamdu, the same hiding can be done at the UI level, while making the underlying language simpler. It also allows explicitly specifying type variable instantiations in a simple way.

#### Structural type system

This isn't strictly about the textual paradigm, but we believe that a structural type system (with anonymous products and sums) should help providing more precise types for intermediate expressions, leveraging the type system more powerfully.

## Lamdu Benefits

### No Syntax errors

Editing programs as rich structures means there are no syntax errors

### Incremental type errors

Type errors are incremental, meaning that type information available form previous, valid states of the program is available to provide better, more localized type errors.

Incremental type checking is much cheaper than full recompilation as featured by mainstream IDE's, allowing a snappy experience.

Type error feedback is immediate, while the edit is still fresh in the programmer's mind.

Type errors are confined and localized to the sub-expression involved, made simple and comprehensible.

Type information is not lost when type errors exist, allowing the IDE to guide the programmer in resolving type errors intelligently.

### Better completions

When types are rich enough, much of the program structure can be inferred from the types.

This affords significantly more powerful and intelligent completions than found in mainstream IDE's.

### Live execution

Frees the programmer from having to trace code execution in their head.

Debugging pure code means expanding sub-expressions that have the wrong result, and following through the sub-expressions that contain the mistakes. This should be a much easier experience than stepping through code.

### Safe refactoring

The IDE has much stronger guarantees about the meaning and structure of the code. This allows refactoring to be *completely* safe.

Safe refactoring allows programmers to keep the code tidy, without fear of regressions.

### Better Collaboration

One of the most difficult things about collaborative development is handling merge conflicts.

The vast majority of merge conflicts are results of non-functional changes: Renames, reformatting, textual movement of code lines between files, etc.

In Lamdu, "names", the "position" of the code and other non-functional aspects of code are separate from the code itself, and avoid conflicts.

#### Rename conflicts

To get a conflict due to "rename" operations, two developers must rename the same variable to two different names. Even then, the code is still executable, but its on-screen rendering will display a localized conflict.

#### Formatting conflicts

Formatting is automatic, so there is no way to generate a conflict.

#### Code movement conflicts

The "position" of code is meta-data attached to the code, helping to find that code and position its rendering.

Since code is not structured into text files, code "position" conflicts are similarly less harmful, less likely and localized.

#### Change tracking

Instead of heuristically guessing what changed in the code, as traditional version control systems do, Lamdu uses an integrated revision control system and records the changes by the programmer as revisions.

This acts as a record of the developer's intent, allowing the RCS to distinguish, for example, between function deletion and writing of a similar function, and the modification of that same function.  This record of intent will be helpful in preventing and resolving conflicts later.

### Bringing Haskell* to the masses

We believe the main thing that prevents the masses from using a Haskell-like language, is a difficult learning experience and lack of good tooling.

#### Beginners

By removing syntax errors, having a discoverable set of valid program transformations, radically simplifying type errors, giving immediate feedback on editing and visualizing execution, learnability drastically improves.

#### Experts

By creating a state-of-the-art, powerful IDE that out-rivals existing IDEs for mainstream languages, we can appeal to the experts, as well.

### Regression Debugging

Integrated revision control and live test cases will allow "Regression Debugging".

When a change causes a regression, the root of the problem can be found quickly, by finding the deepest function application whose result value diverged from the correct version of the code.

### Automatic Formatting and Sugaring

Lamdu attempts to take away as much inconsequential freedom from the developer, to free his mind and his key strokes to deal with the parts that matter in the code. Thus, Lamdu does not provide means to edit formatting on a case-by-case basis. Generalized changes to layout rules can be provided, instead.

Additionally, to avoid further stylistic dilemmas, Lamdu uses automatic sugaring of code, as a duel of the typical "de-sugaring" done by textual languages.

The code is edited and displayed in its sugared form. The edits to this form are translated to lower-level, simpler edits of the stored language, which is de-sugared.  Lamdu uses "co-macros" that capture patterns in the lower-level code and automatically sugar it to canonical form. This frees the programmer from worrying about whether to use sugar for each particular case.

### Visualization

Textual languages can have extensible syntax for rendering and parsing values of custom-defined data-types. A rich structural IDE can take this further, and allow defining extensible UI components to render and edit values of custom-defined data-types.

In effect, this allows for more powerful DSL extensions to the language. These DSLs are not just custom data-types and combinators, but also rich UIs with optimized key bindings to work with that DSL.

For example "do notation" can be implemented as a co-macro (see above) to capture the >>= to lambda syntactic form.  Then, a customized UI that edits the "do notation" sugared form can be provided, extending the language.

Similarly, a binary tree implemented may be edited with a rich UI that visualizes the trees. The inputs, outputs and subexpressions with tree-typed values will use such rich visualizations, instead of flat textual renderings.

### Transparent build process

Today's tools treat the compiler as an opaque, black box, which may or may not apply a set of optimizations to the source code, with great or catastrophic results. Some experts are aware of tooling options to display the intermediate steps in the compiler in an arcane, difficult-to-understand notation.

We intend to make the build process, including compilation, be an explicit pure transformation of the code into intermediate and final forms. The steps of this transformation can be visualized by the live programming environment similarly to the visualization of execution of all pure code.

This will allow far easier inspection of the optimization process, taking away much of the "magic" involved in getting optimizations to fire properly.

# Similar Efforts

## Inventing on Principle

Bret Victor presented amazing demos in his [inspiring talk in January 2012](http://vimeo.com/36579366).

## Light Table

Inspired by Bret Victor, [Light Table](http://www.kickstarter.com/projects/ibdknox/light-table) by Chris Granger is an IDE for Clojure with a "live coding" REPL.

Chris launched the project at [Kickstarter](http://www.kickstarter.com/) at April 2012 and raised more than 300K$ for the project.
Soon after Chris founded a company, [Kodowa](http://www.kodowa.com/), with Robert Attorri, which joined Y-Combinator's "Summer 2012 batch".

## Projucer

[Projucer](http://www.rawmaterialsoftware.com/viewtopic.php?f=12&t=9793) by Julian Storer (Jules), also inspired by Bret Victor, is a live programming IDE for C++ using the Juce cross-platform framework.

It integrates with [LLVM](http://llvm.org/)'s just-in-time compiler to provide fast compilation and advanced features similar to those in Bret Victor's demos. It aims to be a "Light Table for 'Real Programmers'".

It initially aims to provide live coding specifically to GUIs, but later for other uses, specifically uses in the Audio niches in which Jules and Juce focus on.

The project is not open source and is planned to be a commercial IDE.

The project launched at September 2012, and as of October, Jules said he'll be release an alpha version soon.

## Subtext

[Subtext](http://subtextual.org/) by Jonathan Edwards is a similar effort from 2004.

In his [Subtext 1](http://subtextual.org/demo1.html) demo and talk, Edwards presents a live programming environment of a simple dynamic language.

In his [Subtext 2](http://www.subtext-lang.org/subtext2.html) demo, Edwards presents an inspiring way to edit canonical representations of conditional and type-dispatch code.

The Subtext project was the main inspiration for the Lamdu project.

## Other related links

* [We're doing it all wrong](http://www.youtube.com/watch?v=TS1lpKBMkgg) by Paul Phillips: Paul describes his frustrations with Scala and programming general, and describes a vision we believe overlaps with the Lamdu project greatly.

* [Functional programs that explain their work](http://www.youtube.com/watch?v=pqtqaL_ojpk&feature=plcp).

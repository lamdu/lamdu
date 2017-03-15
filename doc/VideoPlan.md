# Plan for making a new video for Lamdu

**This document is work in progress.**

## Previous video summary

*This part isn't in the video. I'm just describing the previous video as a reference when planning the new one.*

In October 2014, before Lamdu executed the code written in it, and when it looked a lot different, we made a video presenting it (vimeo.com/97648370, vimeo.com/97713439) which got more than 3000 views.

It presented, in order of appearance:

* The user interface and the language demonstrated with a code example.
 * Benefit: Renaming a variable is easy
 * Benefit: No syntax errors
 * Verbose typing mode option can help understand the code.
* Demonstrating writing some new code.
 * Introducing holes. Holes are the key for editing valid programs.
 * Notes that holes have their inferred types shown.
 * Show that holes de-prioritize type errors.
 * Demonstrate that transformation-holes choose to put the argument where appropriate.
 * Show creation of lambdas, automatic naming and how it's useful.
 * Demonstrate how type errors work in Lamdu.
 * Show how sometimes the definition's inferred type help us understand what is left to implement.
* Fibonacci coding example, demonstrating structural types and how Lamdu writes the record's boiler-plate for us. *Note that this will be nicer in the current version due to the addition "light lambdas" and updated visual design.*

Last slide, titled "Summary", reads (*and says*):
* No syntax errors or name errors *(making programming more approachable while also helping experts)*
* Type errors are localized - static typing becomes friendly rather than scary *(instead of inaccurate or difficult feedback from the compiler, type errors in Lamdu are incrementally detected, allowing error localization and better blame assignment. Immediate type feedback helps detect type errors as soon as possible, when they are easiest to correct)*
* Type inference works even in intermediate states *(unlike textual editors which necessarily go through invalid states where even parsing correctly is difficult. Lamdu maintains not only a correct structure of the program, but also fully functional type inference)*
* Type driven editing reduces keystrokes and cognitive load. *(Lamdu prunes most invalid program, saving keystrokes and making programming more discoverable and enjoyable)*
* *There's a lot more work to be done, and many more ideas to explore, but we hope that this presentation already gave you some idea of where we're going*

### Feedback we got from the video

* We got some feedback about the visual design of Lamdu and have acted on it. Now Lamdu looks a lot better imho.
* It wasn't clear that the screen shows what keys we are pressing. So people didn't know what keystrokes the user is doing and also thought that the keystroke shown are noise that they don't understand.. We should explain that in any new video.

### What changed since the last video

There were a lot of minor improvements.
These are the major visible stuff that changed:

* Lamdu evaluates the code!
* Lamdu is somewhat useful! At least we can use it for Project Euler and Google Code Jam problems :) Though are still important stuff missing like creating new nominal types.
* Automatic code layouting, which dynamically adapts according the window size and zoom level to avoid horizontal scroll!
* The language and standard library are more mature due to use using it. Instead of lists we have `Stream`. We also have `Array`s, `Text`, `Bytes`, `Tree`, and the `Mut` monad.
* Solved the "blame-assignment" problem when a definition's type changes.

**TODO: What else?**

## What goals do we want to achieve with a video? ##

* The best thing would be to get someone excited enough to join us and contribute to the project!
* Get feedback.
* As Lamdu is already somewhat useful, make people aware of it, so that they could use it. The ultimate goal of course is for Lamdu to be very useful :) This will also get us some user feedback on things that we need to improve, and possibly provide some motivation.
* Improve's people's knowledge about Lamdu / show that the project is still doing steady progress. Otherwise, similar minded folks, who could had joined us, will keep starting new projects with similar goals instead of joining forces and tackling the hard problems together.
* Internet points / motivation / etc
* Who knows - someone seeing this or using Lamdu after this might even get us grants (money) to develop Lamdu? Then maybe we could work on it a lot more rather than just on our own free time.

## Video Script ##

### Introduce Lamdu ###

* A fancy calculator / REPL. Show with a simple calculation like (1+sqrt 5)/2
  * Explain a bit about our annotations
    * Unlike common repl here we see the results for all the subexpressions.
* Switch to a slightly bigger example - an approximation of Pi.
  * Lamdu evaluates expressions as you type them.
  * That's a lot to look at - introduce annotation modes.
    * Show how they can be turned off (Kinda looks like Python)
    * Show how they can be switched to type mode (Kinda looks like Java)
      * Explain about type inference and how it allows one to write in a type-safe language but with a feel similar to dynamic languages like Python.
  * Demonstrate navigating on scopes in the lambda of Pi's series.
  * Talk about how evaluation is enabled by purity.
    * Sidenote: Creating interactive processes is also possible and those parts are not constantly evaluated.
  * Extract the computation from the repl.
* Demonstrate defining functions with factorial.
* Talk about differences from textual editing
  * Simply cannot create syntax errors nor name errors.
    * All editing is done via filling holes and doing structural edits.
    * Renaming variables renames them in all places of use.
  * No need to manage parentheses nor indentation.
* Switch to digits function "map (% 10) . takeWhile (> 0) . iterate (/ 10)""
  * Pause on type error in take/while's predicate.
    * Introduce localized type errors
* Test the digits function.
  * Demonstrate blame assignment when the type of a definition that we used changed. Can demonstrate this with function converting a number to its digits, with a "base" parameter added.
* Demonstrate presentation modes by turning "digits" to OO mode
* Demonstrate automatic layouting on a bigger code example (without typing it in)
  * Code is systematically layed out, making sure that no horizontal scroll is needed.
  * In textual programming language, this take concious effort, and causes spurious flame wars and merge conflicts. These conflicts and wars are avoided in Lamdu.

**TODO**: Demonstrate structural types. We did this in the previous demo with a fibonacci example, but it is a bit embarassing that if we call the field "next" then we get disambiguation tags. We should probably solve this in some way - tag holes or less spurious disambiguations..

### Present cool mockups ###

Things that aren't yet implemented in Lamdu but that will be in the future:

* Switching languages. Can mock it up by switching branches to a branch where things are in Spanish.
* Rich customizable value annotations. Show a function that calculates the possible moves for a chess horse with chess boards as annotations. ("Photoshop"ed)

### A bit about Lamdu's vision ###

Additional things we plan on integrating into the environment:

* Version control. As it happens, existing tools like git were designed for text files and we may need to create new source control tools for non-textual code.
* Testing being integrated. It just makes sense when the IDE already runs your code.

### And a little bit about us ###

* Eyal and Yair. Been working on Lamdu on our free time since 2011.
  * Been programming for a really long while in BASIC, C, C++, Python, Haskell, D, and more.
  * We think that many accumulating small frustrustions faced by programmers, like us, can be solved and this is what we are trying to do with Lamdu.
  * Yair left Google so that he could have enough time to work on this.
* Lamdu is free and open-source.
  * There's no institution or company behind us - But if anyone wants to give us grants, by all means do :) We would love to be able to spend more of our time developing Lamdu.
* We would really like other people to join our effort to bring forth a brighter future for programming.
  * Lamdu is implemented in Haskell and being proficient in it is a pre-requisite for programmers wanting to get involved in coding.
* For more info about our technical road-map see our article about that. **TODO** create this status/roadmap article.
  * There are a few key features still missing, like a UI to define more Nominal types, and type-classes.

## TODOs that can improve video

* Option to disable LCD subpixel rendering
* Clearer UI for update-type
* Hole result ordering

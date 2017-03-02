# Plan for making a new video for Lamdu

**This document is work in progress.**

## Previous video summary

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

* A fancy calculator / REPL
  * Evaluates expressions as you type them.
    * Word about purity and how it enables this.
      * Creating interactive processes is also possible and those parts are not constantly evaluated.
* Switch to a slightly bigger example - an approximation of Pi.
  * Explain our annotations.
    * Show how they can be turned off (Kinda looks like Python)
    * Show how they can be switched to type mode (Kinda looks like Java)
      * Explain about type inference and how it allows one to write in a type-safe language but with a feel similar to dynamic languages like Python.
  * Demonstrate navigating on scopes in the lambda of Pi's series.
  * Extract the computation from the repl.
* Demonstrate defining functions with factorial.
* Demonstrate differences from textual editing
  * Simply cannot create syntax errors nor name errors.
    * All editing is done via filling holes and doing structural edits.
    * Renaming variables renames them in all places of use.
  * No need to manage parentheses nor indentation.
    * Demonstrate automatic layouting.
  * Localized type errors
    * Blame assignment when the type of a definition that we used changed. Can demonstrate this with function converting a number to its digits, with a "base" parameter added.

### Discuss Lamdu's vision ###

* We want to support richer annotations: Images, waveforms, graphs, etc.
  * Displays for different types will be user-defined.
* Make version-control and team-work better.

**TODO**

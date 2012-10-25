This README is a work in progress (Peaker have a review please :)

# Project Bottle / Lamdu

This project aims to create a "next-generation", "live programming" environment that is not centered around text files and gives much added value. The programming language would be similar to Haskell, which we think is very suitable language generally, and even more so for live programming.

Assorted bullet points of benefits we strive to provide:

* Editing code directly lets beginners avoid syntax errors.
* Editing type checked code avoids type errors, helps programmers and helps inteligent completions.
* Integrated unit-testing / code with examples / live programming - helps the programmers avoid silly bugs as they program, and helps programmers understand code better.
* Non textual code would allow rich visualization and editing of objects (such as trees) and mathematical formulas.
* Code stored as code solves most merge conflicts. With textual tools a line where two variables were renamed causes a conflict.

# Similar Efforts

## Text based environments

Text based environments seem to have more promise in the short term, as they add on to existing "eco systems" of efficient compilers and common source control systems and interoperate with the current ways of doing things.

### Bret Victor's talk

Bret Victor made some demos and [demonstrated them in an inspiring talk in January 2012](http://vimeo.com/36579366).

While only producing demos, and not striving to create real tools, his demos inspired both Light Table and Projucer which are striving to become real tools.

### Light Table

[Light Table](http://www.kickstarter.com/projects/ibdknox/light-table) by Chris Granger is an IDE for Clojure with a "live coding" REPL inspired by Bret Victor's talk.

While still storing code as text files, it aims to hide this implementation details and allow more useful ways of browsing the code.

It is currently usable (?).

Chris launched the project at [Kickstarter](http://www.kickstarter.com/) at April 2012 and raised more than 300K$ for the project.
Soon after Chris founded a company, [Kodowa](http://www.kodowa.com/), with Robert Attorri, which joined Y-Combinator's "Summer 2012 batch".

### Projucer

[Projucer](http://www.rawmaterialsoftware.com/viewtopic.php?f=12&t=9793) by Julian Storer (Jules) is a live programming IDE for C++ using the Juce cross-platform framework.

It integrates with [LLVM](http://llvm.org/)'s just-in-time compiler to provide fast compilation and advanced features similar to those in Bret Victor's demos. It aims to be a "Light Table for 'Real Programmers'".

It initially aims to provide live coding specifically to GUIs, but later for other uses, specifically uses in the Audio niches in which Jules and Juce focus on.

The project is not open source and is planned to be a commercial IDE.

The project launched at September 2012, and as of October, Jules said he'll be release an alpha version soon.

## Subtext

[Subtext](http://subtextual.org/) by Jonathan Edwards is an effort similar to ours which started with great hopes, expectations, a nice manifesto, and lots of common sense back in 2004.

In his [Subtext 1](http://subtextual.org/demo1.html) demo and talk, JE presents a live programming environment to a language similar to Haskell. In his impressive second demo he dives into more topics such as canonical representations of code and filtered views of code.

The project seems to be stuck as JE works on inventing a suitable programming language for his vision.
JE seems relunctant to adopt existing languages or concepts from the functional programming community even as these seem close to what he had in mind.

In part, we started our project as we grew tired of waiting for Subtext to bear useful fruits.

## Other interesting links

* Functional programs that explain their work: http://www.youtube.com/watch?v=pqtqaL_ojpk&feature=plcp

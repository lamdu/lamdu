# Project Lamdu

[![Join the chat at https://gitter.im/lamdu/lamdu](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lamdu/lamdu?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This project aims to create a "next-generation", "live programming" environment that radically improves the programming experience.

See the [Main Page](http://lamdu.org/)


## Installation

### [NodeJS](https://nodejs.org/en/) & Build Time

To drastically speed up Lamdu's installation under any OS, you can install
`nodejs >= 6.2.1`<sup>**\***</sup> beforehand, such that `node` is in your `$PATH`.

Enter `node -v` into terminal. If NodeJS is installed (and in your `$PATH`), this will print your current version. If it isn't, you'll get an error.

If you do not install NodeJS, Lamdu's installation will build it from
source.

**<sup>\*</sup>** <u>*For Fedora Users*</u><b>&#x200A;:</b>
&#x200A;Fedora packages have very long names. This may lead to some confusion.<br/>&nbsp;&nbsp;Consider `nodejs-1:6.11.2-1.fc25.x86_64`. This example indicates a NodeJS version of `6.11`, plus a little.<br/>&nbsp;&nbsp;The `-1:` is not a part of the version.


#### osx

requires [brew](http://brew.sh/) and [git](https://git-scm.com/):

```shell
brew install leveldb haskell-stack
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu

```

#### ubuntu

Optional: Install NodeJS from node's apt repository:

```shell
curl -sL https://deb.nodesource.com/setup_6.x | sudo -E bash -
sudo apt-get install -y nodejs
```

requires [stack](https://github.com/commercialhaskell/stack/releases)
```shell
sudo apt-get update -qq
sudo apt-get install git zlib1g-dev libglew-dev libleveldb-dev libglfw-dev libxrandr-dev libxi-dev libxcursor-dev libxinerama-dev -yq
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu

```

#### fedora

Optional: Install NodeJS with `sudo dnf insall nodjs`. Please see the starred note under "NodeJS & Build Time".

requires [stack](https://github.com/commercialhaskell/stack/releases)
```shell
sudo dnf install -y gcc gcc-c++ gmp-devel libXrandr-devel libXi-devel libXcursor-devel mesa-libGL-devel libGLU-devel libXinerama-devel leveldb-devel glew-devel
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu
```

#### arch linux

requires [stack](https://github.com/commercialhaskell/stack/releases)
```shell
sudo pacman -S leveldb glfw libxrandr libxi libxcursor libxinerama
git clone --recursive https://github.com/lamdu/lamdu
cd lamdu
stack setup
stack install
~/.local/bin/lamdu

```

## Tutorial

*Note: In the shortcut key combinations, "left" refers to the left cursor key.*

### Simple expressions

At the top we have an interactive shell, where we can type calculations.
<br/>The `⋙` is our prompt to this shell. Think of it like a calculator: you enter an
expression, hit a button, and it tells you the answer. The next time you use the
calculator, you clear whatever's in there and enter a new expression. Same here.

![Golden ratio example](https://i.imgur.com/vbPRcCO.png)

To type the calculation above:

* Type "**`1+s`**" at the prompt (`⋙`). Notice we have chosen "`1`" for the addition's left argument. However, we have only begun to type the second argument: it starts with an "s". Lamdu knows we have finalized the left argument because we have moved on from it, indicated by the `+`. But we have done nothing to indicate that just `s` is the second argument. To help us finalize the right argument, Lamdu has presented a menu of type-appropriate choices containing "s" in their names &ndash; "containing", not just "starting with". This menu updates as we type.
* Next, we will flesh out the "s" into a "sqrt". As of September 2017, "sqrt" should already be selected in the nearby menu, because it is alphabetically the first function in the library to contain an "s" in its name and to output a number. However your menu, take the path of fewest keystrokes: continuing to type the function's name reduces the menu options to just those that match. Cursor keys allow you to select from the menu. Hit **space** to chose your selected menu option.
* Type "**`5`**" and press **space**.
* Select the whole expression by pressing **shift+left** twice.
* Type "**`/2`**" and press **space**. Notice that Lamdu just automatically inserted the parentheses.

Lamdu displays the evaluation of each expression, whether the whole or a subexpression. Such an automatic display is called an "annotation." The annotation of an expression appears below that of any child expression. For example, the evaluation of `(1 + sqrt 5) / 2` appears below that of its child expression, `(1 + sqrt 5)`. The former is `1.61...` and the latter is `3.23...`.

To keep the expression's font size consistent, some annotations are shrunk, like that of the `sqrt 5` above, which is `2.23...`. To see this in normal size, navigate to the expression by going to the `sqrt`, or to the `5`, and press **shift+left**.

We have just expressed the golden ratio. To save it and give it a name, select the entire expression from the prompt by repeatedly pressing **shift+left**. Press 'x' to extract the code into a new definition. Notice the "x" took us away from the prompt and put us below it. Type "**`golden`**" and **escape**. The escape takes you out of "name editing" mode and puts you back into "selection" mode. Lamdu will automatically save "golden" because it is below the prompt, not at it, whereas prompt-work is temporary. There is no need to say where on your file-system you want this definition kept. With Lamdu, disk directory structure is no longer a part of your programming project: your project's structure is in Lamdu.

### Creating a function
Again, we will be working underneath the prompt, rather than at it. So our work will be saved.<br/>

*Note: Ctrl-Z is undo.*

![Factorial function](http://i.imgur.com/BVcLBLX.png)

To create the function above:

* Navigate to the "New..." button and press **space**.
* Remember: Lamdu does spacing automatically. If you type a space after the first "x" below, Lamdu will think you want to specify a second argument to the function. So don't type that space. Further, you'll notice that the first equals sign after "fac" gets onto your screen without you typing it. After "fac x", type the equals sign anyways, or skip over it with the right cursor key. Either moves you from the left side of the equals sign to the right. Neither will create an extra `=`.<br/>
Type "**`fac x=x=0`**" and press **space**. &nbsp;Notice that Lamdu replaced the second `=` with `==`.<br/>
* Select `x == 0` by pressing **shift+left**.
* Press "**`:`**" and press **space** to create a pattern-match on this boolean expression.
* Type "**`1`**" and press **space** for the `True` branch.
* Type "**`x*f x  -1 `**" and press **space**. &nbsp;Again, Lamdu auto-completes: `f` becomes `fac`.
* If need be, tell Lamdu that we are through defining the function. If any portion of the definition is in a colored perimeter, this means that the function is still open for editing, in which case Lamdu will not allow the function to run. In such a case, navigate to each colored box with the cursor keys and press **space**. The space takes the selected portion of the definition out of "editing mode". You may also need a combination of 1) leaving the selection and coming back in, 2) **escape** and 3)  **space**. Fiddle until it's happy. If you hit a wrong button in the process, recall that Ctrl-Z is an option. *Note that this step will be removed soon due to new design choices.*

We've now written the function. Let's use it.

* Go back up to the interactive shell, just right of the ⋙ symbol. Like with
calculators, we want to clear anything in there before using it. If there is,
press **shift+left** until all is selected, then hit **delete**.
* Type "**`fac 5`**" and press **space**.

Lamdu should now display the evaluation of the whole function, as well as its subexpresssions. The evaluation of `x == 0` is displayed by a highlight on the active pattern match path (`False`).

This function is recursive and invoked additional calls to itself. To navigate between these calls, go to the arrows under its parameter `x` and press **right** or **left**.
If your arrows are grayed-out, this means that the function definition is still open for editing and therefore Lamdu is not allowing it to be ran: see the last bullet point under *"To create the function..."*.

To rename any symbol, navigate to it, press **return** twice and edit the name. Note that all references are displayed with the updated name correctly.


## Further Exploration / Help Documentation

In the lower-right of Lamdu's screen, you'll see that F1 brings up help.
It changes based on what you have selected in the environment.

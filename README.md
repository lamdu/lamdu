# Project Lamdu

[![Join the chat at https://gitter.im/lamdu/lamdu](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lamdu/lamdu?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This project aims to create a "next-generation", "live programming" environment that radically improves the programming experience.

See the [Main Page](http://lamdu.org/)


## Installation

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

Optionally: install nodejs from node's apt repository:

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

### Optionally installing [nodejs](https://nodejs.org/en/)

To speed up Lamdu's installation under any OS, you can install
`nodejs >= 6.2.1`, such that `node` is in your `$PATH`.

If you do not install nodejs, Lamdu's installation will build it from
source.

## Tutorial

### Simple expressions

At the top we have an interactive shell, where we can type calculations.

![Golden ratio example](https://i.imgur.com/vbPRcCO.png)

To type the calculation above:

* type "**`1+s`**"
* at this point we apply the addition operator to the number 1 and are picking the other argument to the addition. we are offered different options which contain the string "s" in their names
* pick the option we desire ("sqrt _") by filtering the option by typing and navigating to it with the cursor keys, and then pressing **space** (as of writing this, it is the first option offered, partly due to a currently minimal library of available functions).
* type "**`5`**" and press **space**
* select the whole expression by pressing **shift+left** twice
* type "**`/2`**" and press **space**

Directly under each expression in the code we see annotations of its evaluation results. Some annotations are more far apart from the expression's code due to its subexpression's annotation appearing between them.

So as to not expand the expression, some annotations are shrunk. In this case `sqrt 5`'s annotation is shrunk. To see it in normal size - navigate to this expression, by going to the `sqrt` or `5` and pressing **shift+left**.

To name the golden ratio you have just declared, navigate to the top-level expression using repeated pressing of **shift+left**. Press 'x' to extract the code into a new definition. Type "**`golden`**" and **escape**.

### Creating a function

![Factorial function](http://i.imgur.com/BVcLBLX.png)

To create the function above:

* Navigate to the "New..." button and then press **space**
* type "**`fac x=x=0`**" and press **space**
* Select `x == 0` by pressing **shift+left**
* Press "**`:`**" and press **space** to create a pattern-match on this boolean expression
* Type "**`1`**" and press **space** for the `True` branch
* Type "**`x*f x-1`**" and press **space**
* Final step to creating the function: Review its inferred type and press **space** to confirm it. *Note that this step will be removed soon due to new design choices*

We've now written the function. Let's use it.

* Go to the interactive shell (the top row in right of the ⋙ symbol), 
  navigate to the top-level expression (repeatedly pressing **shift+left** if necessary) and then press **delete** to delete it.
* Type "**`fac 5`**" and press **space**

The function is now being evaluated and we see subexpression evaluation results within it. The result of `x == 0` is displayed by a highlight on the active pattern match path (`False`).

This function is recursive and invoked additional calls to itself. To navigate between these calls go to the arrows under its parameter `x` and press **right** or **left**.

To rename any symbol, press **return** twice and edit the name. Note that all references are displayed with the updated name correctly.

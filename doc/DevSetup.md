# Tips for setting up environment for Lamdu development

Lamdu is developed in Haskell and is reproducibly built using haskell-stack.

You can use any text-editor to edit its code. @Peaker uses Emacs on Linux and @yairchu uses Visual Studio Code on macOS. Below are some useful tips for the setup of these editors to develop Lamdu.

## Visual Studio Code

Install the following VS Code extensions:

* ctagsx: for jump to symbol
* haskell-ghcid: for compiler errors

Install the following tools using `stack install`:

* hasktags
* ghcid

To build the jump-to-symbol index or refresh it run:

    hasktags -c src --ignore-close-implementation

To have errors show up in VS Code's problems pane:

* First build the project using `stack build`.
* Now `mkdir autogen` and copy the `Paths_Lamdu.hs` file from where it was created in `.stack` to this folder.
* Now you're ready to use `ghcid`. Simply start it with the "Start Ghcid" from VS code's command pallete.

## macOS

When running a full build, this command shows a notification when it is done:

    osascript -e 'display notification "'`stack build && echo Success || echo Failure`'" with title "Stack build"'

## Other notes

### Migrating the test programs to new schemas

    for x in $(grep schemaVersion -l test/programs/*); do echo $x; ./lamdu --lamduDB tmp deletedb; ./lamdu --lamduDB tmp import $x --no-implicit-prelude; ./lamdu --lamduDB tmp export $x; done
    rm -r tmp/

# Tips for setting up environment for Lamdu development

Lamdu is developed in Haskell and is reproducibly built using haskell-stack.

You can use any text-editor to edit its code. @Peaker uses Emacs and @yairchu uses Visual Studio Code. Below are some useful tips for the setup of these editors to develop Lamdu.

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
* In VS Code's terminal - run `ghcid -o ghcid.txt -c "stack ghci --test --ghci-options=\"-fno-code -iautogen -ferror-spans\""`
* Open `ghcid.txt` and on its tab "Watch Ghcid Output" from the command palette. VS Code now reports the compilation errors quickly following file changes!

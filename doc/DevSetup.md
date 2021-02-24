# Tips for setting up environment for Lamdu development

Lamdu is developed in Haskell and is reproducibly built using haskell-stack.

You can use any text-editor to edit its code. @Peaker uses Emacs on Linux and @yairchu uses Visual Studio Code on macOS. Below are some useful tips for the setup of these editors to develop Lamdu.

## Visual Studio Code

Works great with the Haskell extension! (Haskell Language Server)

## macOS

When running a full build, this command shows a notification when it is done:

    osascript -e 'display notification "'`stack build && echo Success || echo Failure`'" with title "Stack build"'

## Other notes

### Migrating the test programs to new schemas

    for x in $(grep schemaVersion -l test/programs/*); do echo $x; ./lamdu --lamduDB tmp deletedb; ./lamdu --lamduDB tmp import $x --no-implicit-prelude; ./lamdu --lamduDB tmp export $x; done
    rm -r tmp/

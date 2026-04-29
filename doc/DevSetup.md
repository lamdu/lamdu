# Tips for setting up environment for Lamdu development

Lamdu is developed in Haskell and is reproducibly built using haskell-stack.

You can use any text-editor to edit its code. @Peaker uses Emacs on Linux and @yairchu uses Visual Studio Code on macOS. Below are some useful tips for the setup of these editors to develop Lamdu.

## Visual Studio Code

Works great with the Haskell extension! (Haskell Language Server)

## Helix

For Helix, you'll need to edit your `languages.toml` either globally or in the project root `.helix/languages.toml`.

```toml
[language-server.haskell-language-server]
command = "haskell-language-server"
args = ["--lsp"]

[[language]]
name = "haskell"
language-servers = [ { name = "haskell-language-server" }]
```

If you get a bunch of errors in your editor. The fix is to run a program called `hpack` to generate a `Lamdu.cabal` file. After that paths should resolve and the language server should work perfectly.

## macOS

When running a full build, this command shows a notification when it is done:

    osascript -e 'display notification "'`stack build && echo Success || echo Failure`'" with title "Stack build"'

## Other notes

### Migrating the test programs to new schemas

    for x in $(grep schemaVersion -l test/programs/*); do echo $x; ./lamdu --lamduDB tmp deletedb; ./lamdu --lamduDB tmp import $x --no-implicit-prelude; ./lamdu --lamduDB tmp export $x; done
    rm -r tmp/

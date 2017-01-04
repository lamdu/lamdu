# Lamdu's main-loop

The layers which Lamdu's main-loop consists of, from low-level to high-level:

## GLFW

GLFW is a cross-platform library that translates the native Windows/Linux/macOS APIs into a single interface.

## Graphics.UI.GLFW.Events

The GLFW.Events module translates GLFW's multiple-callbacks based model to one where there's a single ADT - `Event`, for all the events, and caller user provides a single handler function to the main-loop - `eventLoop :: GLFW.Window -> ([Event] -> IO Next) -> IO ()`.

It's `Event` type is more high-level than GLFW's raw events, so that mouse click events have mouse cursor positions, and corresponding Char and Key events are merged.

## Graphics.UI.Bottle.Main.Image

Main.Image is a thin layer on top of GLFW.Events whose event handlers return `graphics-drawcombinators`'s `Image`s which it draws.

    data Handlers = Handlers
        { eventHandler :: Event -> IO ()
        , update :: IO (Maybe Image)
        , refresh :: IO Image
        }

    mainLoop :: GLFW.Window -> (Size -> Handlers) -> IO ()

## Graphics.UI.Bottle.Main.Animation

Main.Animation runs Lamdu's animation engine, whose `Frame` datatype consists of many `Image` components. The animation engine morphs between `Frame`s to show what happens in structural edits.

It performs animation and event-handling on separate threads. This way time-consuming event handlers don't disrupt smooth animation.

## Graphics.UI.Bottle.Main

Bottle.Main's main-loop builds on top the lower layers to work in terms of the high-level `Widget` type.

This main-loop gets used in Lamdu's `main` function.

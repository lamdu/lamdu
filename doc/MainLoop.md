# Lamdu's main-loop

The layers which Lamdu's main-loop consists of, from low-level to high-level:

## GLFW

GLFW is a cross-platform library that translates the native Windows/Linux/macOS APIs into a single interface.

## GUI.Momentu.Main.Events

Translate multiple callbacks into a single callback with a higher-level `Event` sum type.

For example, mouse click events have mouse cursor positions.

## GUI.Momentu.Main.Image

Main.Image is a thin layer on top of Main.Events.Loop, that also draws an image.

    data Handlers = Handlers
        { eventHandler :: Event -> IO ()
        , update :: IO (Maybe Image)
        , refresh :: IO Image
        }

    mainLoop :: GLFW.Window -> (Size -> Handlers) -> IO ()

## GUI.Momentu.Main.Animation

Main.Animation runs Lamdu's animation engine, whose `Frame` datatype consists of many `Image` components. The animation engine morphs between `Frame`s to show what happens in structural edits.

It performs animation and event-handling on separate threads. This way time-consuming event handlers don't disrupt smooth animation.

## GUI.Momentu.Main

Bottle.Main's main-loop builds on top the lower layers to work in terms of the high-level `Widget` type.

This main-loop gets used in Lamdu's `main` function.

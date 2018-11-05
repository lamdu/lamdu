# Lamdu's main-loop

The layers which Lamdu's main-loop consists of, from low-level to high-level:

## GLFW

GLFW is a cross-platform library that translates the native Windows/Linux/macOS APIs into a single interface.

## GUI.Momentu.Main.Events.Loop

Translate many GLFW callbacks into a single callback with a
higher-level `Event` sum type, and an "iteration" callback that can
determine whether to poll or wait for events.

For example, mouse click events have mouse cursor positions.

## GUI.Momentu.Main.Animation

Main.Animation creates a new bound (OS) thread that is in charge of
drawing the changing images to the window. It uses
GUI.Momentu.Animation.Engine to interpolate between the current
animation "frame" and the destination frame, rendering the current
state each frame. When the destination frame is reached, the animation
thread sleeps waiting for the next frame.

The event loop takes over the caller's thread (which is the main
thread in Lamdu's use). It reuses GUI.Momentu.Main.Events.Loop,
handles the events, generating new animation frames and sending them
to the animation thread.

The threads are meant to allow smooth animations to continue even if
events consume the entire CPU core. When an event takes a long time to
generate a new animation frame, it is sent to the animation thread
timestamped with the event arrival. This allows the animation thread
to skip some of the animation to avoid the feeling of lag.

## GUI.Momentu.Main

Bottle.Main's main-loop builds on top the lower layers to work in
terms of the high-level `Widget` type.

This main-loop gets used in Lamdu's `main` function.

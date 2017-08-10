# The types in Lamdu's GUI

From low-level to high-level:

## Graphics.DrawingCombinators - Image

An unsized vector-graphics image type.

## GUI.Momentu.Animation - Image

A graphics-drawingcombinators "unit-image" with a target rectangle. When the image is drawn it is translated and scaled to fit the rectangle.

An `Image` also has an identifier used for animations.

## GUI.Momentu.Animation - Frame

An ordered collection of `Animation.Image` (the order defines which `Image` is in front).

When animating between frames the matching identities are used for moving corresponding objects to their new positions.

## GUI.Momentu.View

`View` builds on `Animation.Frame` to support composability.

It has a `size` field so it can have a `backgroundColor` or `pad`ding added to it, and it enables multiple views to be put together in a `Grid`.

For hovering submenus and their like it has several layers of `Frame`s, so that when two views are composed together the hovers still hover.

## GUI.Momentu.Widget

`Widget` is like a view, but also supports interactivity and responsive hovers.

## GUI.Momentu.Align - WithTextPos

`WithTextPos` is simply a `Widget` with an added alignment-point for the vertical position of texts,
so that when widgets with text are glued together the text stays properly aligned.

## GUI.Momentu.Responsive

`Responsive` is a way of "laying out" a widget given layout constraints, designed for automatic layout of program code. Different nodes may be layed out either horizontally or vertically depending on the constraints.

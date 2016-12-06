# The types in Lamdu's GUI

From low-level to high-level:

## Graphics.DrawingCombinators - Image

An unsized vector-graphics image type.

## Graphics.UI.Bottle.Animation - Image

A graphics-drawingcombinators "unit-image" with a target rectangle. When the image is drawn it is translated and scaled to fit the rectangle.

*NOTE: Currently it also contains a "layer" field but this will be removed soon*

## Graphics.UI.Bottle.Animation - Frame

A collection of `Animation.Image`s with identifiers. When animating between one frame and another the matching identities are used for moving corresponding objects to their new positions.

## Graphics.UI.Bottle.View

`View` is an `Animation.Frame` with a size.

The size is enables various operations such as adding a `backgroundColor` or `pad`ing, and allows layouts combining multiple view such as a `Grid`.

## Graphics.UI.Bottle.Widget

`Widget` is a view with added interactivity, via events that may affect things, and navigation related events and cursor information.

## Graphics.UI.Bottle.Widget.Aligned

`AlignedWidget` is simply a `Widget` with an added alignment-point, used for positioning widgets when laying them out together in horizontal or vertical boxes.

## Graphics.UI.Bottle.Widget.TreeLayout

`TreeLayout` is a way of "laying out" a widget given layout constraints, designed for automatic layout of program code. Different nodes may be layed out either horizontally or vertically depending on the constraints.

# Anim-Id-Mappings

This is a temporary document that describes
a currently implemented mechanism which we intend to replace.
When we replace this mechanism we will also remove this document.

`GUI.Momentu.State.Update` has the `uAnimIdMapping` field.
This mapping is then used by `GUI.Momentu.Main.Animation.animThread` to modify
the current frame's anim-ids.
When an item changes its anim-id (when should this happen?) one may use anim-id mappings
to signal to the animation engine that it is the same item.

This mechanism is error-prone and we have/had bugs where
an action's animation is nicer than the animation of its undo.

## Places where it is/was used

### Var-to-Tags, Tags-To-Vars

Lamdu presents multi-param functions as functions of a record parameter,
when adding the second parameter the sugar actions returns a
`Lamdu.Sugar.Types.Binder.VarToTags` result, and `Lamdu.GUI.ParamEdit`
translates this result to animate that the previous variable became a "field parameter".
A similar process also occurs when changing from multi-parameter to single parameter.

**Planned change**:

According to our [localization plan](Localization.md) variables will be named by a tag.
The anim-id (as well as `Sugar.EntityId`) for parameters could then be the same one it would be
when the function is transformed to a multi-parameter function (based on the lambda parameter id and the tag).

### Hole results

Hole results used to regenerate on every cursor movement etc with random written ids
which were normalized for display purposes.
When a hole result is chosen we applied a translation between the normalized ids and the written ids.

**What changed**:

We create each hole result with consistent ids in the first place.

### TextEdit

We used to have the `TextEdit` animate letters smoothly when adding letters between letters.
We didn't have an id for each letter (which would be very expensive) so we had to use anim id translations
to identify each letter by an index.

**What changed**:

We found that these animations are not helpful and transitioned to have no fancy text animations.

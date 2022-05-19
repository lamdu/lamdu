## Meta (JSExportReadMe.md)

When sharing Lamdu code via the "fancy export" option (which is intended to share code with people who don't necessarilly have Lamdu installed),
this README file is included in the .zip to explain in short what Lamdu is and what's inside the .zip.
(except for everything prior to the line that contains exactly '== ExportFromHere ==').

== ExportFromHere ==
## Lamdu export bundle

This bundle is an export of a Lamdu program.

### Lamdu

[Lamdu](http://lamdu.org) is a non-textual programming language/environment (source code is stored and edited as an AST).
As such, it has no textual format, but Lamdu code can be exported and shared in other means (see section below).

### Export bundle contents

* `source.lamdu`: Lamdu source of the exported program in a JSON-based
  format. It can be drag-and-dropped into Lamdu to load the code.

* `js/main.js`: The program compiled to JavaScript. One may run it to
  obtain the program output value. Run it with `$ NODE_PATH=. node main.js`.

* `js/rts.js`, `js/rtsConfig.js`: Support files for
  `js/main.js`. These modules implement the basic primitives of Lamdu
  in JavaScript (arithmetic, arrays, etc).

* `README.md`: The README which file you are reading right now.

### How to use this bundle

If you have Lamdu installed, just drag and drop `source.lamdu` to Lamdu to load the code.

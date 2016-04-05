## Meta (CodeJamReadMe.md)

When sharing Lamdu code via the "fancy export" option (which is intended to share code with people who don't necessarilly have Lamdu installed),
this README file is included in the .zip to explain in short what is Lamdu and what's inside the .zip.
(except for this paragraph which is not included in the export's README)

## Lamdu export bundle

This bundle is an export of a Lamdu program.

### Lamdu

[Lamdu](http://lamdu.org) is a non-textual programming language/environment (source code is stored and edited as an AST).
As such, it has no textual format, but Lamdu code can be exported and shared in other means (see section below).

### Export bundle contents

* `source.lamdu`: Lamdu source of the exported program in a JSON-based format. It can be drag-and-dropped into Lamdu to load the code.
* `screenshot.png`: A screenshot of the program as seen in Lamdu. Note that this is just an overview of the program as not all subroutines used in it may be included in the screenshot.
* `js/main.js`: The program compiled to JavaScript. One may run it to obtain the program output value. It requires a recent version of nodejs which supports the `--harmony-tailcalls` flag, or possibly with other ES6 compliant JS engines.
* `js/rts.js`: Support file for `js/main.js`. This module implements the basic primitives of Lamdu in JavaScript (arithmetic, arrays, etc).
* `README.md`: The README which file you are reading right now.

### How to use this bundle

If you have Lamdu installed, just drag and drop `source.lamdu` to Lamdu to load the code.

Otherwise, start with the screenshot to get an overview of the code, and then you may further examine the compiled Javascript code (though note that it is a compiler output rather than handwritten Javascript), even examine the `source.lamdu` JSON file, but it isn't very human-readable and you can simply install Lamdu instead..

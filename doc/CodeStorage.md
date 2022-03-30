# How code is stored

Traditionally, code is stored in a bunch of text files.

While Lamdu's interface is text-like, code in Lamdu is not stored as text files. There are differences between the vision of how code should be stored how Lamdu currently implements it.

## The vision

We won't go into very specific details about the vision, partly because we're still figuring it out..

* Version control will be an integrated part of Lamdu
  * Merges will be much easier than in traditional programming because currently most merges are spurious and related to [artifacts of textual programming](https://www.reddit.com/r/nosyntax/comments/6wtztz/how_textual_programming_doesnt_properly_get_the/).
  * With tests also being an integrated part of Lamdu, both tests and version control could be used together to automatically find the changes breaking new tests created after discovering bugs (an improved automated `git bisect`).
* Having everything available as hole results might not be effective due to too many choices being available. Therefore we'd implement "Labels"
  * You would add labels to your session like in traditional programming one imports a library. Then its code will be offered in hole results.
  * The labels would be used to disambiguate between different tags whose short names clash, such as "Image(Graphics)" and "Image(Virtualization)".
  * These labels could be used to make component dependency diagrams to help grasp the general structure of code.
* Something equivalent to Github/Bitbucket will be a part of Lamdu

## The current implementation

### The database

Currently Lamdu stores the code in a RocksDB database, with a versioning system built on top of it.

Lamdu's "undo" is implemented using the versioning system, and you can currently play with branching - that's what the "master" label at the bottom of the screen is for. See the toggle-able help for its shortcut keys.
Merges are not yet implemented.

As we evolve Lamdu's data model, occasionally we also change the format of things stored in the database. That's when one needs to run `lamdu deletedb` to get it to read the database again. This is unfortunate and we would need to come up with something better, however we do allow migrating to the new format via our json-based interchange format.

### The interchange `.json` format

Currently one may export and import code into Lamdu via `.json` files.

Lamdu's standard library is also stored is such a file - `freshdb.json`.

The format is not intended to be very human-readable, although currently that Lamdu doesn't have an interface to edit Nominal types, they can currently only be added by importing hand-crafted files of this format..

#### Format migrations

Unlike the RocksDB based database, when Lamdu's data model changes, even though the format of these `.json` files changes, Lamdu is still backwards compatible and is able to load files in its previous `.json` formats. So you can feel safe saving your code in these files and having future Lamdu version still support them.

This process is implemented via migrations, in `Lamdu.Data.Export.JSON.Migration.migrateAsNeeded`.

The `.json` file has a `schemaVersion` entry which is used to know which migrations to run.

For example, when loading a file with `schemaVersion` of 0 in a Lamdu build whose current schema version is 2, both `ToVersion1.migrate` and `ToVersion2.migrate` will process the `.json` file and update its format before it is loaded.
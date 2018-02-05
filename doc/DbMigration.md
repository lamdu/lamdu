# Database Migration

Lamdu is still ongoing rapid development, and as such its code storage format isn't final and is still subject to change.

When we change the database schema, Lamdu isn't always able to parse and automatically migrate the previous data.
In that case, one may migrate it manually via our json-based interchange format using this process:

* With the version of Lamdu which you've used before, export all the data using `lamdu export migration.json`.
  * The `export` command line action is only available since 2018.02.01, before then it is only available from within the Lamdu editor with the `Alt+Shift+E` shortcut (which exports it to `export.json`), available since 2016.03.31.
* With the current version of Lamdu, run `lamdu import migration.json`.

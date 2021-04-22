# Sugar conversion passes

`sugarWorkArea` consists of 4 passes in the following order:

* Conversion of LamduCalc AST to raw Sugar AST
  * This stage is cached in `OnceT` so that hole results stay consistent,
    and to improve performance, avoiding reinference etc.
* Adding evaluation results (may change more frequently as results continue to arrive)
* Name loading and generation
* Parentheses introduction and operator precedence resolution

The stages have to be in this order because operator ordering depends on loading their names,
and because evaluation results require name loading too.
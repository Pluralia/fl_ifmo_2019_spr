# Правила для 2 задания
 - Or   -> (And ||)\* And
 - And  -> (Ord &&)\* Ord
 - Ord  -> Sum (== | /= | <= | < | >= | >) Sum
 - Sum  -> Prod ((+ | -) Prod)\*
 - Prod -> Deg ((\* | /) Deg)\*
 - Deg  -> (Fin ^)\* Fin
 - Fin  -> \sum | '(' Or ')'

Or стартовое

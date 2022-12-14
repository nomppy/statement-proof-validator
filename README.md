# proof-validator
### _Kenneth Sun <kennett.sun@gmail.com>_

This program takes in .txt file(s) as input, where each line is a line in a proof,

The input should be formatted like so:

```

```


;The first line must have an integer indicating how many lines are in the initial premise,
;followed by what conclusion the proof should reach.
;Every line thereafter must contain a statement and the logical step used to obtain it, separated by a semicolon (;).
;
;;; Use of notation:
;   =>      Implies
;   <=>     Biconditional
;   ^       And
;   v       Or
;   [A-Z]*  Sequence of capital letters represent atomic statements. Space delimited (note that lowercase "v" is processed as the logical Or)
;   -       Negation
;
;
;;; Glossary of accepted logical operations, as well as their inputs:
;   MP      Modus Ponnens   Requires 2 line arguments
;   MT      Modus Tonens    Requires 2 line arguments
;   MTP     Modus tollendo ponnens  Requires 2 line arguments
;   DN      Double Negative Requires 1 line argument
;   Cond    Assume for conditional proof; if a line argument is given, indicates end of conditional proof
;   Ind     Assume for indirect proof; if a line argument is given, indicates end of indirect proof
;   Simp    Simplification  Requires 1 line argument
;   Add     Addition    Requires 1 line argument

## License
MIT


Boolean data type has two possible truth values to represent logic.<br>
:package: [Package](https://package.elm-lang.org/packages/elmw/extra-boolean/latest/),
:blue_book: [Wiki](https://github.com/elmw/extra-boolean/wiki).

> Stability: Experimental.

<br>

```elm
import Boolean exposing (..)

parse "inactive"
-- False

parse "not off"
-- True

parse "truthy"
-- True

xor True True
-- False

imp True False
-- False

eqv False False
-- True
```

<br>
<br>


## Index

| Method  | Action                                    |
| ------- | ----------------------------------------- |
| [parse] | Convert string to boolean.                |
| [xor]   | Get exclusive-or of 2 boolean values.     |
| [eqv]   | Check if antecedent ⇔ consequent (x ⇔ y). |
| [imp]   | Check if antecedent ⇒ consequent (x ⇒ y). |

[parse]: https://github.com/elmw/extra-boolean/wiki/parse
[xor]: https://github.com/elmw/extra-boolean/wiki/xor
[eqv]: https://github.com/elmw/extra-boolean/wiki/eqv
[imp]: https://github.com/elmw/extra-boolean/wiki/imp

<br>
<br>

[![](https://img.youtube.com/vi/Pfs6SChEXmc/maxresdefault.jpg)](https://www.youtube.com/watch?v=Pfs6SChEXmc)

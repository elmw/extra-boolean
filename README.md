Boolean data type has two possible truth values to represent logic.<br>
:package: [Package](https://package.elm-lang.org/packages/elmw/extra-boolean/latest/),
:blue_book: [Wiki](https://github.com/elmw/extra-boolean/wiki).

> Stability: Experimental.

<br>

```elm
import Boolean exposing (..)

parse "1"
parse "truthy"
parse "not off"
-- True

parse "not true"
parse "inactive"
parse "disabled"
-- False

imply True False
-- False

eq False False
-- True

xor3 True True True
-- True

select3 1 True False True
-- False         ^

count3 True True False
-- 2    ^    ^
```

<br>
<br>


## Index

| Method   | Action                                |
| -------- | ------------------------------------- |
| [parse]  | Convert string to boolean.            |
| [not]    | Check if value is false.              |
| [and]    | Check if all values are true.         |
| [or]     | Check if any value is true.           |
| [xor]    | Check if odd no. of values are true.  |
| [nand]   | Check if any value is false.          |
| [nor]    | Check if all values are false.        |
| [xnor]   | Check if even no. of values are true. |
| [eq]     | Check if antecedent ⇔ consequent.     |
| [imply]  | Check if antecedent ⇒ consequent.     |
| [nimply] | Check if antecedent ⇏ consequent.     |
| [select] | Check if ith value is true.           |
| [count]  | Count no. of true values.             |

[parse]: https://github.com/elmw/extra-boolean/wiki/parse
[not]: https://github.com/elmw/extra-boolean/wiki/not
[and]: https://github.com/elmw/extra-boolean/wiki/and
[or]: https://github.com/elmw/extra-boolean/wiki/or
[xor]: https://github.com/elmw/extra-boolean/wiki/xor
[nand]: https://github.com/elmw/extra-boolean/wiki/nand
[nor]: https://github.com/elmw/extra-boolean/wiki/nor
[xnor]: https://github.com/elmw/extra-boolean/wiki/xnor
[eq]: https://github.com/elmw/extra-boolean/wiki/eq
[imply]: https://github.com/elmw/extra-boolean/wiki/imply
[nimply]: https://github.com/elmw/extra-boolean/wiki/nimply
[select]: https://github.com/elmw/extra-boolean/wiki/select
[count]: https://github.com/elmw/extra-boolean/wiki/count

<br>
<br>

[![](https://img.youtube.com/vi/Pfs6SChEXmc/maxresdefault.jpg)](https://www.youtube.com/watch?v=Pfs6SChEXmc)

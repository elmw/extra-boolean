Boolean data type has two possible truth values to represent logic.<br>
:package: [Package](https://package.elm-lang.org/packages/elmw/extra-boolean/latest/),
:blue_book: [Wiki](https://github.com/elmw/extra-boolean/wiki).

Here is my implementation of digital logic gates in software. That includes
the basic gates [not], [and], [or], [xor]; their complements [nand], [nor],
[xnor]; and 2 propositional logic (taught in discrete mathematics) gates
[imply], [eq]; and their complements [nimply], [neq]. There is also a
multiplexer, called [select], and a `true` counter, called [count]. [count]
can help you make custom gates, such as an *alternate* concept of **xnor**
which returns `true` only if all inputs are the same (standard [xnor] returns
`true` if even inputs are `true`). All of them can handle upto 8 inputs.

You know the [and] of 2-inputs, but what of 1-input? What of 0? And what of
the other gates? I answer them here. BTW, Mr. [parse] is quite good at
translating `string` to `boolean`.

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
| [neq]    | Check if antecedent ⇎ consequent.     |
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
[neq]: https://github.com/elmw/extra-boolean/wiki/neq
[imply]: https://github.com/elmw/extra-boolean/wiki/imply
[nimply]: https://github.com/elmw/extra-boolean/wiki/nimply
[select]: https://github.com/elmw/extra-boolean/wiki/select
[count]: https://github.com/elmw/extra-boolean/wiki/count

<br>
<br>

[![](https://img.youtube.com/vi/6mMK6iSZsAs/maxresdefault.jpg)](https://www.youtube.com/watch?v=6mMK6iSZsAs)

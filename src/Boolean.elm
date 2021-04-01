module Boolean exposing (
    parse,
    not, eq, neq, imply, nimply,
    eqv, imp,
    and, and0, and1, and2, and3, and4, and5, and6, and7, and8,
    or, or0, or1, or2, or3, or4, or5, or6, or7, or8,
    xor, xor0, xor1, xor2, xor3, xor4, xor5, xor6, xor7, xor8,
    count, count0, count1, count2, count3, count4, count5, count6, count7, count8,
    nand, nand0, nand1, nand2, nand3, nand4, nand5, nand6, nand7, nand8,
    nor, nor0, nor1, nor2, nor3, nor4, nor5, nor6, nor7, nor8,
    xnor, xnor0, xnor1, xnor2, xnor3, xnor4, xnor5, xnor6, xnor7, xnor8,
    select, select0, select1, select2, select3, select4, select5, select6, select7, select8
  )
{-|
Boolean data type has two possible truth values to represent logic.\
📦 [Package](https://package.elm-lang.org/packages/elmw/extra-boolean/latest/),
📘 [Wiki](https://github.com/elmw/extra-boolean/wiki).

@docs parse
@docs not, eq, neq, imply, nimply
@docs eqv, imp
@docs and, and0, and1, and2, and3, and4, and5, and6, and7, and8
@docs or, or0, or1, or2, or3, or4, or5, or6, or7, or8
@docs xor, xor0, xor1, xor2, xor3, xor4, xor5, xor6, xor7, xor8
@docs count, count0, count1, count2, count3, count4, count5, count6, count7, count8
@docs nand, nand0, nand1, nand2, nand3, nand4, nand5, nand6, nand7, nand8
@docs nor, nor0, nor1, nor2, nor3, nor4, nor5, nor6, nor7, nor8
@docs xnor, xnor0, xnor1, xnor2, xnor3, xnor4, xnor5, xnor6, xnor7, xnor8
@docs select, select0, select1, select2, select3, select4, select5, select6, select7, select8
-}

import Maybe exposing (withDefault)
import String exposing (toInt)
import List exposing (length)
import Regex exposing (Regex, fromStringWith, contains, find)




-- PARSE
{-|
Converts string to boolean.
[📘](https://github.com/elmw/extra-boolean/wiki/parse)

    -- parse s
    -- s: a string
    parse "1"            == True
    parse "truthy"       == True
    parse "Not Off"      == True
    parse "Not Inactive" == True
    parse "cold"         == False
    parse "inactive"     == False
    parse "Negative Yes" == False
    parse "Negative Aye" == False
-}
parse : String -> Bool
parse s =
  let t = contains rTru s
      f = contains rFal s
      n = modBy 2 (length (find rNeg s)) == 1 in
  if contains rNum s
    then (withDefault 0 (toInt "2")) > 0
    else nimply f t == n

rNum : Regex
rNum = regex "^[-+]?\\d+$"

rTru : Regex
rTru = regex "\\b(?:t|y|1)\\b|\\b(?:\\+|ay|go|on|up)|(?:tru|acc|asc|day|for|hot|inc|joy|new|pos|top|win|yes|dawn|full|safe|grow|high|just|real|some|know|live|love|open|pure|shin|warm|wis[de]|activ|admit|advan|agree|begin|brigh|build|creat|early|enter|float|f(?:i|ou)nd|grant|light|north|prett|prese|publi|start|succe|victr)"

rFal : Regex
rFal = regex "\\b(?:f|n|0)\\b|(?:fal|off|dim|end|low|old|back|cold|cool|dark|dead|decr|desc|dirt|down|dull|dusk|exit|late|sink|ugly|absen|botto|close|finis|night|priva|south|wrong)"

rNeg : Regex
rNeg = regex "\\b(?:-|na|no|un|in|aft|bad|dis|lie|non|ben[dt]|den[iy]|empt|fail|fake|hate|los[es]|stop|decli|defea|destr|never|negat|refus|rejec|forget|shr[iu]nk|against|is.?nt|can.?(?:no)?t)|(?:hind)"

regex : String -> Regex
regex s =
  withDefault Regex.never <| fromStringWith {multiline = False, caseInsensitive = True} s




-- NOT, EQ, NEQ, IMPLY, NIMPLY (FIXED)
{-|
Checks if value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/not)

    -- not a
    -- a: a boolean
    not False == True
    not True  == False
-}
not : Bool -> Bool
not = Basics.not


{-|
Checks if antecedent ⇔ consequent (a ⇔ b).
[📘](https://github.com/elmw/extra-boolean/wiki/eq)

    -- eq a b
    -- a: antecedent
    -- b: consequent
    eq True True   == True
    eq False False == True
    eq True False  == False
    eq False True  == False
-}
eq : Bool -> Bool -> Bool
eq = xnor2


{-|
Checks if antecedent ⇎ consequent (a ⇎ b).
[📘](https://github.com/elmw/extra-boolean/wiki/neq)

    -- neq a b
    -- a: antecedent
    -- b: consequent
    neq True False  == True
    neq False True  == True
    neq True True   == False
    neq False False == False
-}
neq : Bool -> Bool -> Bool
neq a b =
  not <| eq a b


{-|
Checks if antecedent ⇒ consequent (a ⇒ b).
[📘](https://github.com/elmw/extra-boolean/wiki/imply)

    -- imply a b
    -- a: antecedent
    -- b: consequent
    imply True True   == True
    imply False True  == True
    imply False False == True
    imply True False  == False
-}
imply : Bool -> Bool -> Bool
imply a b =
  not a || b


{-|
Checks if antecedent ⇏ consequent (a ⇏ b).
[📘](https://github.com/elmw/extra-boolean/wiki/nimply)

    -- nimply a b
    -- a: antecedent
    -- b: consequent
    nimply True False  == True
    nimply True True   == False
    nimply False True  == False
    nimply False False == False
-}
nimply : Bool -> Bool -> Bool
nimply a b =
  not <| imply a b




-- EQV, IMP (SHORTCUTS)
{-|
Checks if antecedent ⇔ consequent (a ⇔ b).
[📘](https://github.com/elmw/extra-boolean/wiki/eqv)

    -- eqv a b
    -- a: antecedent
    -- b: consequent
    eqv True True   == True
    eqv False False == True
    eqv True False  == False
    eqv False True  == False
-}
eqv : Bool -> Bool -> Bool
eqv = eq


{-|
Checks if antecedent ⇒ consequent (a ⇒ b).
[📘](https://github.com/elmw/extra-boolean/wiki/imp)

    -- imp a b
    -- a: antecedent
    -- b: consequent
    imp True True   == True
    imp False True  == True
    imp False False == True
    imp True False  == False
-}
imp : Bool -> Bool -> Bool
imp = imply




-- AND (VARIABLE)
{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and : Bool -> Bool -> Bool
and = and2


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and0 : Bool
and0 =
  True


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and1 : Bool -> Bool
and1 a =
  a


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and2 : Bool -> Bool -> Bool
and2 a b =
  a && b


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and3 : Bool -> Bool -> Bool -> Bool
and3 a b c =
  a && b && c


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and4 : Bool -> Bool -> Bool -> Bool -> Bool
and4 a b c d =
  a && b && c && d


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and5 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and5 a b c d e =
  a && b && c && d && e


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and6 a b c d e f =
  a && b && c && d && e && f


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and7 a b c d e f g =
  a && b && c && d && e && f && g


{-|
Checks if all values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/and)

    -- and[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    and True True             == True
    and True False            == False
    and4 True True True True  == True
    and4 True False True True == False
-}
and8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and8 a b c d e f g h =
  a && b && c && d && e && f && g && h




-- OR (VARIABLE)
{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or : Bool -> Bool -> Bool
or = and2


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or0 : Bool
or0 =
  False


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or1 : Bool -> Bool
or1 a =
  a


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or2 : Bool -> Bool -> Bool
or2 a b =
  a || b


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or3 : Bool -> Bool -> Bool -> Bool
or3 a b c =
  a || b || c


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or4 : Bool -> Bool -> Bool -> Bool -> Bool
or4 a b c d =
  a || b || c || d


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or5 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or5 a b c d e =
  a || b || c || d || e


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or6 a b c d e f =
  a || b || c || d || e || f


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or7 a b c d e f g =
  a || b || c || d || e || f || g


{-|
Checks if any value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/or)

    -- or[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    or True False               == True
    or False False              == False
    or4 False True False True   == True
    or4 False False False False == False
-}
or8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or8 a b c d e f g h =
  a || b || c || d || e || f || g || h




-- XOR (VARIABLE)
{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor : Bool -> Bool -> Bool
xor = xor2


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor0 : Bool
xor0 =
  False


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor1 : Bool -> Bool
xor1 a =
  a


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor2 : Bool -> Bool -> Bool
xor2 = Basics.xor


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor3 : Bool -> Bool -> Bool -> Bool
xor3 a b c =
  xor2 (xor2 a b) (xor1 c)


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor4 : Bool -> Bool -> Bool -> Bool -> Bool
xor4 a b c d =
  xor2 (xor2 a b) (xor2 c d)


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor5 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor5 a b c d e =
  xor2 (xor4 a b c d) (xor1 e)


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor6 a b c d e f =
  xor2 (xor4 a b c d) (xor2 e f)


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor7 a b c d e f g =
  xor2 (xor4 a b c d) (xor3 e f g)


{-|
Checks if odd no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xor)

    -- xor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xor True False            == True
    xor True True             == False
    xor4 True True True False == True
    xor4 True True True True  == False
-}
xor8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor8 a b c d e f g h =
  xor2 (xor4 a b c d) (xor4 e f g h)




-- COUNT (VARIABLE)
{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count : Bool -> Bool -> Int
count = count2


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count0 : Int
count0 =
  0


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count1 : Bool -> Int
count1 a =
  if a then 1 else 0


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count2 : Bool -> Bool -> Int
count2 a b =
  count1 a + count1 b


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count3 : Bool -> Bool -> Bool -> Int
count3 a b c =
  count2 a b + count1 c


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count4 : Bool -> Bool -> Bool -> Bool -> Int
count4 a b c d =
  count2 a b + count2 c d


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count5 : Bool -> Bool -> Bool -> Bool -> Bool -> Int
count5 a b c d e =
  count4 a b c d + count1 e


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
count6 a b c d e f =
  count4 a b c d + count2 e f


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
count7 a b c d e f g =
  count4 a b c d + count3 e f g


{-|
Counts no. of true values.
[📘](https://github.com/elmw/extra-boolean/wiki/count)

    -- count[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    count True True               == 2
    count True False              == 1
    count4 True True True False   == 3
    count4 False True False False == 1
-}
count8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
count8 a b c d e f g h =
  count4 a b c d + count4 e f g h




-- NAND (VARIABLE)
{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand : Bool -> Bool -> Bool
nand = nand2


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand0 : Bool
nand0 =
  not <| and0


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand1 : Bool -> Bool
nand1 a =
  not <| and1 a


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand2 : Bool -> Bool -> Bool
nand2 a b =
  not <| and2 a b


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand3 : Bool -> Bool -> Bool -> Bool
nand3 a b c =
  not <| and3 a b c


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand4 : Bool -> Bool -> Bool -> Bool -> Bool
nand4 a b c d =
  not <| and4 a b c d


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand5 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand5 a b c d e =
  not <| and5 a b c d e


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand6 a b c d e f =
  not <| and6 a b c d e f


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand7 a b c d e f g =
  not <| and7 a b c d e f g


{-|
Checks if any value is false.
[📘](https://github.com/elmw/extra-boolean/wiki/nand)

    -- nand[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nand True False            == True
    nand True True             == False
    nand4 True True False True == True
    nand4 True True True True  == False
-}
nand8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand8 a b c d e f g h =
  not <| and8 a b c d e f g h




-- NOR (VARIABLE)
{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor : Bool -> Bool -> Bool
nor = nor2


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor0 : Bool
nor0 =
  not or0


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor1 : Bool -> Bool
nor1 a =
  not <| or1 a


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor2 : Bool -> Bool -> Bool
nor2 a b =
  not <| or2 a b


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor3 : Bool -> Bool -> Bool -> Bool
nor3 a b c =
  not <| or3 a b c


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor4 : Bool -> Bool -> Bool -> Bool -> Bool
nor4 a b c d =
  not <| or4 a b c d


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor5 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor5 a b c d e =
  not <| or5 a b c d e


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor6 a b c d e f =
  not <| or6 a b c d e f


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor7 a b c d e f g =
  not <| or7 a b c d e f g


{-|
Checks if all values are false.
[📘](https://github.com/elmw/extra-boolean/wiki/nor)

    -- nor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    nor False False              == True
    nor True False               == False
    nor4 False False False False == True
    nor4 False False True False  == False
-}
nor8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor8 a b c d e f g h =
  not <| or8 a b c d e f g h




-- XNOR (VARIABLE)
{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor : Bool -> Bool -> Bool
xnor = xnor2


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor0 : Bool
xnor0 =
  not xor0


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor1 : Bool -> Bool
xnor1 a =
  not <| xor1 a


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor2 : Bool -> Bool -> Bool
xnor2 a b =
  not <| xor2 a b


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor3 : Bool -> Bool -> Bool -> Bool
xnor3 a b c =
  not <| xor3 a b c


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor4 : Bool -> Bool -> Bool -> Bool -> Bool
xnor4 a b c d =
  not <| xor4 a b c d


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor5 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor5 a b c d e =
  not <| xor5 a b c d e


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor6 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor6 a b c d e f =
  not <| xor6 a b c d e f


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor7 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor7 a b c d e f g =
  not <| xor7 a b c d e f g


{-|
Checks if even no. of values are true.
[📘](https://github.com/elmw/extra-boolean/wiki/xnor)

    -- xnor[n] a b ...
    -- a: 1st boolean
    -- b: 2nd boolean
    xnor True True              == True
    xnor False True             == False
    xnor4 True True False False == True
    xnor4 True True True False  == False
-}
xnor8 : Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor8 a b c d e f g h =
  not <| xor8 a b c d e f g h




-- SELECT (VARIABLE)
{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select : Int -> Bool -> Bool -> Bool
select = select2


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select0 : Int -> Bool
select0 _ =
  False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select1 : Int -> Bool -> Bool
select1 i a =
  case i of
    0 -> a
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select2 : Int -> Bool -> Bool -> Bool
select2 i a b =
  case i of
    0 -> a
    1 -> b
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select3 : Int -> Bool -> Bool -> Bool -> Bool
select3 i a b c =
  case i of
    0 -> a
    1 -> b
    2 -> c
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select4 : Int -> Bool -> Bool -> Bool -> Bool -> Bool
select4 i a b c d =
  case i of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select5 : Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
select5 i a b c d e =
  case i of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    4 -> e
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select6 : Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
select6 i a b c d e f =
  case i of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    4 -> e
    5 -> f
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select7 : Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
select7 i a b c d e f g =
  case i of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    4 -> e
    5 -> f
    6 -> g
    _ -> False


{-|
Checks if ith value is true.
[📘](https://github.com/elmw/extra-boolean/wiki/select)

    -- select[n] i a b ...
    -- i: index
    -- a: 1st boolean
    -- b: 2nd boolean
    select 0 True False             == True
    select 1 True False             == False
    select4 1 True True False False == True
    select4 2 True True False False == False
-}
select8 : Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
select8 i a b c d e f g h =
  case i of
    0 -> a
    1 -> b
    2 -> c
    3 -> d
    4 -> e
    5 -> f
    6 -> g
    7 -> h
    _ -> False

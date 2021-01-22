module Boolean exposing (
    parse,
    not, eq, imply, nimply,
    eqv, imp,
    and, and0, and1, and2, and3, and4, and5, and6, and7, and8,
    or, or0, or1, or2, or3, or4, or5, or6, or7, or8,
    xor, xor0, xor1, xor2, xor3, xor4, xor5, xor6, xor7, xor8,
    count, count0, count1, count2, count3, count4, count5, count6, count7, count8,
    nand, nand0, nand1, nand2, nand3, nand4, nand5, nand6, nand7, nand8,
    nor, nor0, nor1, nor2, nor3, nor4, nor5, nor6, nor7, nor8,
    xnor, xnor0, xnor1, xnor2, xnor3, xnor4, xnor5, xnor6, xnor7, xnor8
  )
{-|
Boolean data type has two possible truth values to represent logic.

@docs parse
@docs not, eq, imply, nimply
@docs eqv, imp
@docs and, and0, and1, and2, and3, and4, and5, and6, and7, and8
@docs or, or0, or1, or2, or3, or4, or5, or6, or7, or8
@docs xor, xor0, xor1, xor2, xor3, xor4, xor5, xor6, xor7, xor8
@docs count, count0, count1, count2, count3, count4, count5, count6, count7, count8
@docs nand, nand0, nand1, nand2, nand3, nand4, nand5, nand6, nand7, nand8
@docs nor, nor0, nor1, nor2, nor3, nor4, nor5, nor6, nor7, nor8
@docs xnor, xnor0, xnor1, xnor2, xnor3, xnor4, xnor5, xnor6, xnor7, xnor8
-}

import Regex exposing (Regex, fromStringWith, contains, find)
import Maybe exposing (withDefault)
import List exposing (length)



-- PARSE
{-|
Convert string to boolean.

    -- parse s
    -- s: a string
    parse "truthy"   == True
    parse "not off"  == True
    parse "not true" == False
    parse "inactive" == False
-}
parse: String -> Bool
parse s =
  let fal = contains (regex "(negati|never|refus|wrong|fal|off)|\\b(f|n|0)\\b") s
      neg = modBy 2 (length (find (regex "\\b(nay|nah|no|dis|un|in)") s)) == 1 in
  not (xor fal neg)

regex: String -> Regex
regex s =
  withDefault Regex.never <| fromStringWith {multiline = False, caseInsensitive = True} s




-- NOT, EQ, IMPLY, NIMPLY (FIXED)
{-|
Check if value is false.

    -- not a
    -- a: a boolean value
    not False == True
    not True  == False
-}
not: Bool -> Bool
not = Basics.not


{-|
Check if antecedent ⇔ consequent (a ⇔ b).

    -- eq a b
    -- a: antecedent
    -- b: consequent
    eq True True   == True
    eq False False == True
    eq True False  == False
    eq False True  == False
-}
eq: Bool -> Bool -> Bool
eq = xnor2


{-|
Check if antecedent ⇒ consequent (a ⇒ b).

    -- imply a b
    -- a: antecedent
    -- b: consequent
    imply True True   == True
    imply False True  == True
    imply False False == True
    imply True False  == False
-}
imply: Bool -> Bool -> Bool
imply a b =
  (not a) || b


{-|
Check if antecedent ⇏ consequent (a ⇏ b).

    -- nimply a b
    -- a: antecedent
    -- b: consequent
    nimply True False  == True
    nimply True True   == False
    nimply False True  == False
    nimply False False == False
-}
nimply: Bool -> Bool -> Bool
nimply = not imply




-- EQV, IMP (SHORTCUTS)
{-|
Check if antecedent ⇔ consequent (a ⇔ b).

    -- eqv a b
    -- a: antecedent
    -- b: consequent
    eqv True True   == True
    eqv False False == True
    eqv True False  == False
    eqv False True  == False
-}
eqv: Bool -> Bool -> Bool
eqv = eq


{-|
Check if antecedent ⇒ consequent (a ⇒ b).

    -- imp a b
    -- a: antecedent
    -- b: consequent
    imp True True   == True
    imp False True  == True
    imp False False == True
    imp True False  == False
-}
imp: Bool -> Bool -> Bool
imp = imply




-- AND (VARIABLE)
{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and: Bool -> Bool -> Bool
and = and2


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and0: Bool
and0 =
  True


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and1: Bool -> Bool
and1 a =
  a


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and2: Bool -> Bool -> Bool
and2 a b =
  a && b


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and3: Bool -> Bool -> Bool -> Bool
and3 a b c =
  a && b && c


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and4: Bool -> Bool -> Bool -> Bool -> Bool
and4 a b c d =
  a && b && c && d


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and5: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and5 a b c d e =
  a && b && c && d && e


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and6 a b c d e f =
  a && b && c && d && e && f


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and7 a b c d e f g =
  a && b && c && d && e && f && g


{-|
Check if all values are true.

    -- and[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    and True True   == True
    and True False  == False
    and False True  == False
    and False False == False
-}
and8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
and8 a b c d e f g h =
  a && b && c && d && e && f && g && h




-- OR (VARIABLE)
{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or: Bool -> Bool -> Bool
or = and2


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or0: Bool
or0 =
  False


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or1: Bool -> Bool
or1 a =
  a


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or2: Bool -> Bool -> Bool
or2 a b =
  a || b


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or3: Bool -> Bool -> Bool -> Bool
or3 a b c =
  a || b || c


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or4: Bool -> Bool -> Bool -> Bool -> Bool
or4 a b c d =
  a || b || c || d


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or5: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or5 a b c d e =
  a || b || c || d || e


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or6 a b c d e f =
  a || b || c || d || e || f


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or7 a b c d e f g =
  a || b || c || d || e || f || g


{-|
Check if any value is true.

    -- or[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    or True True   == True
    or True False  == True
    or False True  == True
    or False False == False
-}
or8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
or8 a b c d e f g h =
  a || b || c || d || e || f || g || h




-- XOR (VARIABLE)
{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor: Bool -> Bool -> Bool
xor =
  xor2


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor0: Bool
xor0 =
  False


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor1: Bool -> Bool
xor1 a =
  a


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor2: Bool -> Bool -> Bool
xor2 = Basics.xor


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor3: Bool -> Bool -> Bool -> Bool
xor3 a b c =
  xor2 (xor2 a b) (xor1 c)


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor4: Bool -> Bool -> Bool -> Bool -> Bool
xor4 a b c d =
  xor2 (xor2 a b) (xor2 c d)


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor5: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor5 a b c d e =
  xor2 (xor4 a b c d) (xor1 e)


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor6 a b c d e f =
  xor2 (xor4 a b c d) (xor2 e f)


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor7 a b c d e f g =
  xor2 (xor4 a b c d) (xor3 e f g)


{-|
Check if odd no. of values are true.

    -- xor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xor True False  == True
    xor False True  == True
    xor True True   == False
    xor False False == False
-}
xor8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xor8 a b c d e f g h =
  xor2 (xor4 a b c d) (xor4 e f g h)




-- COUNT (VARIABLE)
{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count: Bool -> Bool -> Int
count = count2


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count0: Int
count0 = 0


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count1: Bool -> Int
count1 a =
  if a then 1 else 0


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count2: Bool -> Bool -> Int
count2 a b =
  count1 a + count1 b


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count3: Bool -> Bool -> Bool -> Int
count3 a b c =
  count2 a b + count1 c


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count4: Bool -> Bool -> Bool -> Bool -> Int
count4 a b c d =
  count2 a b + count2 c d


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count5: Bool -> Bool -> Bool -> Bool -> Bool -> Int
count5 a b c d e =
  count4 a b c d + count1 e


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
count6 a b c d e f =
  count4 a b c d + count2 e f


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
count7 a b c d e f g =
  count4 a b c d + count3 e f g


{-|
Count no. of true values.

    -- count[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    count True True   == 2
    count True False  == 1
    count False True  == 1
    count False False == 0
-}
count8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Int
count8 a b c d e f g h =
  count4 a b c d + count4 e f g h




-- NAND (VARIABLE)
{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand: Bool -> Bool -> Bool
nand = nand2


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand0: Bool
nand0 = not and0


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand1: Bool -> Bool
nand1 = not and1


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand2: Bool -> Bool -> Bool
nand2 = not and2


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand3: Bool -> Bool -> Bool -> Bool
nand3 = not and3


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand4: Bool -> Bool -> Bool -> Bool -> Bool
nand4 = not and4


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand5: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand5 = not and5


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand6 = not and6


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand7 = not and7


{-|
Check if any value is false.

    -- nand[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nand True False  == True
    nand False True  == True
    nand False False == True
    nand True True   == False
-}
nand8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nand8 = not and8




-- NOR (VARIABLE)
{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor: Bool -> Bool -> Bool
nor = nor2


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor0: Bool
nor0 = not or0


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor1: Bool -> Bool
nor1 = not or1


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor2: Bool -> Bool -> Bool
nor2 = not or2


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor3: Bool -> Bool -> Bool -> Bool
nor3 = not or3


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor4: Bool -> Bool -> Bool -> Bool -> Bool
nor4 = not or4


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor5: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor5 = not or5


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor6 = not or6


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor7 = not or7


{-|
Check if all values are false.

    -- nor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    nor False False == True
    nor True True   == False
    nor True False  == False
    nor False True  == False
-}
nor8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
nor8 = not or8




-- XNOR (VARIABLE)
{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor: Bool -> Bool -> Bool
xnor = xnor2


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor0: Bool
xnor0 = not xor0


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor1: Bool -> Bool
xnor1 = not xor1


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor2: Bool -> Bool -> Bool
xnor2 = not xor2


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor3: Bool -> Bool -> Bool -> Bool
xnor3 = not xor3


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor4: Bool -> Bool -> Bool -> Bool -> Bool
xnor4 = not xor4


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor5: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor5 = not xor5


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor6: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor6 = not xor6


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor7: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor7 = not xor7


{-|
Check if even no. of values are true.

    -- xnor[n] a b ...
    -- a: a boolean value
    -- b: another boolean value
    xnor True True   == True
    xnor False False == True
    xnor True False  == False
    xnor False True  == False
-}
xnor8: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool
xnor8 = not xor8

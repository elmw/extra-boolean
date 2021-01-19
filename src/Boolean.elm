module Boolean exposing (parse, xor, eqv, imp)
{-|
Boolean data type has two possible truth values to represent logic.

@docs xor, eqv, imp, parse
-}

import Regex exposing (Regex, fromStringWith, contains, find)
import Maybe exposing (withDefault)
import List exposing (length)



{-|
Convert string to boolean.

    -- s: a string
    parse "not true" == False
    parse "inactive" == False
    parse "not off"  == True
    parse "truthy"   == True
-}
parse: String -> Bool
parse s =
  let fal = contains (regex "(negati|never|refus|wrong|fal|off)|\\b(f|n|0)\\b") s
      neg = modBy 2 (length (find (regex "\\b(nay|nah|no|dis|un|in)") s)) == 1 in
  not (xor fal neg)

regex: String -> Regex
regex s =
  withDefault Regex.never <| fromStringWith {multiline = False, caseInsensitive = True} s



{-|
Get exclusive-or of 2 boolean values.

    -- x: first
    -- y: second
    xor True True  == False
    xor True False == True
-}
xor: Bool -> Bool -> Bool
xor =
  Basics.xor



{-|
Check if antecedent ⇔ consequent (x ⇔ y).

    -- x: antecedent
    -- y: consequent
    eqv False False == True
    eqv True True   == True
    eqv False True  == False
    eqv True False  == False
-}
eqv: Bool -> Bool -> Bool
eqv x y =
  (x && y) || ((not x) && (not y))



{-|
Check if antecedent ⇒ consequent (x ⇒ y).

    -- x: antecedent
    -- y: consequent
    imp False False == True
    imp False True  == True
    imp True True   == True
    imp True False  == False
-}
imp: Bool -> Bool -> Bool
imp x y =
  (not x) || y

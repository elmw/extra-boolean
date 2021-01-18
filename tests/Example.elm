module Example exposing (..)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)



addTests : Test
addTests =
  describe "x + y" [
    test "4 + 3  == 7" <|
      \_ -> 4 + 3 |> equal 7,
    test "19 + 48 == 67" <|
      \_ -> 19 + 48 |> equal 67,
    test "831 + 111 == 942" <|
      \_ -> 831 + 111 |> equal 942
  ]



multiplyTests : Test
multiplyTests =
  describe "x * y" [
    test "4 * 3  == 12" <|
      \_ -> 4 * 3 |> equal 12,
    test "19 * 48 == 912" <|
      \_ -> 19 * 48 |> equal 912,
    test "831 * 111 == 92241" <|
      \_ -> 831 * 111 |> equal 92241
  ]

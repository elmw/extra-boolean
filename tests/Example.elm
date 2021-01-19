module Example exposing (..)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Boolean exposing (..)



xorTests : Test
xorTests =
  describe "xor x y" [
    test "xor True True == False" <|
      \_ -> Boolean.xor True True |> equal False,
    test "xor True False == True" <|
      \_ -> Boolean.xor True False |> equal True
  ]



eqvTests : Test
eqvTests =
  describe "eqv x y" [
    test "eqv False False == True" <|
      \_ -> eqv False False |> equal True,
    test "eqv True True == True" <|
      \_ -> eqv True True |> equal True,
    test "eqv False True == False" <|
      \_ -> eqv False True |> equal False,
    test "eqv True False == False" <|
      \_ -> eqv True False |> equal False
  ]



impTests : Test
impTests =
  describe "imp x y" [
    test "imp False False == True" <|
      \_ -> imp False False |> equal True,
    test "imp False True == True" <|
      \_ -> imp False True |> equal True,
    test "imp True True == True" <|
      \_ -> imp True True |> equal True,
    test "imp True False == False" <|
      \_ -> imp True False |> equal False
  ]



parseTests : Test
parseTests =
  describe "parse s" [
    test "parse \"not true\" == False" <|
      \_ -> parse "not true" |> equal False,
    test "parse \"inactive\" == False" <|
      \_ -> parse "inactive" |> equal False,
    test "parse \"not off\" == True" <|
      \_ -> parse "not off" |> equal True,
    test "parse \"truthy\" == True" <|
      \_ -> parse "truthy" |> equal True
  ]

module Example exposing (..)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Boolean exposing (..)




-- PARSE
parseTests : Test
parseTests =
  describe "parse s" [
    test "1.0" <|
      \_ -> parse "not true" |> equal False,
    test "1.1" <|
      \_ -> parse "inactive" |> equal False,
    test "1.2" <|
      \_ -> parse "not off" |> equal True,
    test "1.3" <|
      \_ -> parse "truthy" |> equal True
  ]




-- NOT, EQ, IMPLY, NIMPLY (FIXED)
notTests : Test
notTests =
  describe "not a" [
    test "1.0" <|
      \_ -> Boolean.not False |> equal True,
    test "1.1" <|
      \_ -> Boolean.not True |> equal False
  ]


eqTests : Test
eqTests =
  describe "eq a b" [
    test "2.3" <|
      \_ -> eq True True |> equal True,
    test "2.0" <|
      \_ -> eq False False |> equal True,
    test "2.2" <|
      \_ -> eq True False |> equal False,
    test "2.1" <|
      \_ -> eq False True |> equal False
  ]


implyTests : Test
implyTests =
  describe "imply a b" [
    test "2.3" <|
      \_ -> imply True True |> equal True,
    test "2.1" <|
      \_ -> imply False True |> equal True,
    test "2.0" <|
      \_ -> imply False False |> equal True,
    test "2.2" <|
      \_ -> imply True False |> equal False
  ]


nimplyTests : Test
nimplyTests =
  describe "nimply a b" [
    test "2.2" <|
      \_ -> nimply True False |> equal True,
    test "2.3" <|
      \_ -> nimply True True |> equal False,
    test "2.1" <|
      \_ -> nimply False True |> equal False,
    test "2.0" <|
      \_ -> nimply False False |> equal False
  ]




-- EQV, IMP (SHORTCUTS)
eqvTests : Test
eqvTests =
  describe "eqv a b" [
    test "2.3" <|
      \_ -> eqv True True |> equal True,
    test "2.0" <|
      \_ -> eqv False False |> equal True,
    test "2.2" <|
      \_ -> eqv True False |> equal False,
    test "2.1" <|
      \_ -> eqv False True |> equal False
  ]


impTests : Test
impTests =
  describe "imp a b" [
    test "2.3" <|
      \_ -> imp True True |> equal True,
    test "2.1" <|
      \_ -> imp False True |> equal True,
    test "2.0" <|
      \_ -> imp False False |> equal True,
    test "2.2" <|
      \_ -> imp True False |> equal False
  ]




-- AND (VARIABLE)
andTests : Test
andTests =
  describe "and[n] a b ..." [
    test "0.0" <|
      \_ -> and0 |> equal True,
    test "1.1" <|
      \_ -> and1 True |> equal True,
    test "1.0" <|
      \_ -> and1 False |> equal False,
    test "2.3" <|
      \_ -> and2 True True |> equal True,
    test "2.2" <|
      \_ -> and2 True False |> equal False,
    test "2.1" <|
      \_ -> and2 False True |> equal False,
    test "2.0" <|
      \_ -> and2 False False |> equal False,
    test "8.255" <|
      \_ -> and8 True True True True True True True True |> equal True,
    test "8.254" <|
      \_ -> and8 True True True True True True True False |> equal False
  ]




-- OR (VARIABLE)
orTests : Test
orTests =
  describe "or[n] a b ..." [
    test "0.0" <|
      \_ -> or0 |> equal False,
    test "1.1" <|
      \_ -> or1 True |> equal True,
    test "1.0" <|
      \_ -> or1 False |> equal False,
    test "2.3" <|
      \_ -> or2 True True |> equal True,
    test "2.2" <|
      \_ -> or2 True False |> equal True,
    test "2.1" <|
      \_ -> or2 False True |> equal True,
    test "2.0" <|
      \_ -> or2 False False |> equal False,
    test "8.1" <|
      \_ -> or8 False False False False False False False True |> equal True,
    test "8.0" <|
      \_ -> or8 False False False False False False False False |> equal False
  ]




-- XOR (VARIABLE)
xorTests : Test
xorTests =
  describe "xor[n] a b ..." [
    test "0.0" <|
      \_ -> xor0 |> equal False,
    test "1.1" <|
      \_ -> xor1 True |> equal True,
    test "1.0" <|
      \_ -> xor1 False |> equal False,
    test "2.2" <|
      \_ -> xor2 True False |> equal True,
    test "2.1" <|
      \_ -> xor2 False True |> equal True,
    test "2.3" <|
      \_ -> xor2 True True |> equal False,
    test "2.0" <|
      \_ -> xor2 False False |> equal False,
    test "8.1" <|
      \_ -> xor8 False False False False False False False True |> equal True,
    test "8.3" <|
      \_ -> xor8 False False False False False False True True |> equal False
  ]




-- COUNT (VARIABLE)
countTests : Test
countTests =
  describe "count[n] a b ..." [
    test "0.0" <|
      \_ -> count0 |> equal 0,
    test "1.1" <|
      \_ -> count1 True |> equal 1,
    test "1.0" <|
      \_ -> count1 False |> equal 0,
    test "2.3" <|
      \_ -> count2 True True |> equal 2,
    test "2.2" <|
      \_ -> count2 True False |> equal 1,
    test "2.1" <|
      \_ -> count2 False True |> equal 1,
    test "2.0" <|
      \_ -> count2 False False |> equal 0,
    test "8.1" <|
      \_ -> count8 False False False False False False False True |> equal 1,
    test "8.7" <|
      \_ -> count8 False False False False False True True True |> equal 3
  ]




-- NAND (VARIABLE)
nandTests : Test
nandTests =
  describe "nand[n] a b ..." [
    test "0.0" <|
      \_ -> nand0 |> equal False,
    test "1.1" <|
      \_ -> nand1 False |> equal True,
    test "1.0" <|
      \_ -> nand1 True |> equal False,
    test "2.2" <|
      \_ -> nand2 True False |> equal True,
    test "2.1" <|
      \_ -> nand2 False True |> equal True,
    test "2.0" <|
      \_ -> nand2 False False |> equal True,
    test "2.3" <|
      \_ -> nand2 True True |> equal False,
    test "8.254" <|
      \_ -> nand8 True True True True True True True False |> equal True,
    test "8.255" <|
      \_ -> nand8 True True True True True True True True |> equal False
  ]




-- NOR (VARIABLE)
norTests : Test
norTests =
  describe "nor[n] a b ..." [
    test "0.0" <|
      \_ -> nor0 |> equal True,
    test "1.1" <|
      \_ -> nor1 False |> equal True,
    test "1.0" <|
      \_ -> nor1 True |> equal False,
    test "2.0" <|
      \_ -> nor2 False False |> equal True,
    test "2.3" <|
      \_ -> nor2 True True |> equal False,
    test "2.2" <|
      \_ -> nor2 True False |> equal False,
    test "2.1" <|
      \_ -> nor2 False True |> equal False,
    test "8.0" <|
      \_ -> nor8 False False False False False False False False |> equal True,
    test "8.1" <|
      \_ -> nor8 False False False False False False False True |> equal False
  ]




-- XNOR (VARIABLE)
xnorTests : Test
xnorTests =
  describe "xnor[n] a b ..." [
    test "0.0" <|
      \_ -> xnor0 |> equal True,
    test "1.1" <|
      \_ -> xnor1 False |> equal True,
    test "1.0" <|
      \_ -> xnor1 True |> equal False,
    test "2.3" <|
      \_ -> xnor2 True True |> equal True,
    test "2.0" <|
      \_ -> xnor2 False False |> equal True,
    test "2.2" <|
      \_ -> xnor2 True False |> equal False,
    test "2.1" <|
      \_ -> xnor2 False True |> equal False,
    test "8.0" <|
      \_ -> xnor8 False False False False False False False False |> equal True,
    test "8.7" <|
      \_ -> xnor8 False False False False False True True True |> equal False
  ]

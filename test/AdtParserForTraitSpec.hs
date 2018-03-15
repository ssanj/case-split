module AdtParserForTraitSpec where

import Data.Either (Either(..))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import AdtParser
import Common

import Text.Parsec

traitSimpleTest :: TestTree
traitSimpleTest =
  assertParser (Command "sealed trait MessageError")
               "ADT Parser should match simple trait"
               traitDefP
               (Right "MessageError")

traitWithBodyTest :: TestTree
traitWithBodyTest =
  assertParser (Command "sealed trait Sim {")
               "ADT Parser should match a trait with a body"
               traitDefP
               (Right "Sim")

traitWithPolyTest :: TestTree
traitWithPolyTest =
  assertParser (Command "sealed trait RichString[A]")
               "ADT Parser should match trait with polymorphic types"
               traitDefP
               (Right "RichString")

traitWithPolyAndSpacesTest :: TestTree
traitWithPolyAndSpacesTest =
  assertParser (Command "sealed trait Traffic  [A]")
               "ADT Parser should match trait with spaces between polymorphic type declaration"
               traitDefP
               (Right "Traffic")

traitWithPolyCovariantTest :: TestTree
traitWithPolyCovariantTest =
  assertParser (Command "sealed trait MyOption[+A]")
               "ADT Parser should match trait with covariant polymorphic types"
               traitDefP
               (Right "MyOption")

traitWithPolyContravariantTest :: TestTree
traitWithPolyContravariantTest =
  assertParser (Command "sealed trait FunctionWrapper[-A]")
               "ADT Parser should match trait with contravariant polymorphic types"
               traitDefP
               (Right "FunctionWrapper")

traitWithMultiplePolyTest :: TestTree
traitWithMultiplePolyTest =
  assertParser (Command "sealed trait MyContainer[+A, -B,+C,D]")
               "ADT Parser should match trait with multiple polymorphic types"
               traitDefP
               (Right "MyContainer")

test_ADT_Parser_for_Trait :: TestTree
test_ADT_Parser_for_Trait =
  testGroup "ADT Parser for Trait" [
                                      traitSimpleTest,
                                      traitWithPolyTest,
                                      traitWithPolyCovariantTest,
                                      traitWithPolyContravariantTest,
                                      traitWithPolyAndSpacesTest,
                                      traitWithBodyTest,
                                      traitWithMultiplePolyTest
                                    ]


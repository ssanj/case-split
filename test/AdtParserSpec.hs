module AdtParserSpec where

import Data.Either (Either(..))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import AdtParser

import Text.Parsec

data Command = Command String deriving Show

assertParser :: (Eq r, Show r) => Command -> String -> P r -> Either ParseError r -> TestTree
assertParser (Command commands) testName parser expectedResult =
  let result = parse parser "" commands in
  testCase testName $
    assertBool ("failed with:" ++ (show result) ++ ", expected:" ++ (show expectedResult))
               (result == expectedResult)


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

test_ADT_Parser :: TestTree
test_ADT_Parser = testGroup "ADT parser" [
                                            traitSimpleTest,
                                            traitWithPolyTest,
                                            traitWithPolyCovariantTest,
                                            traitWithPolyContravariantTest,
                                            traitWithPolyAndSpacesTest,
                                            traitWithBodyTest,
                                            traitWithMultiplePolyTest
                                          ]


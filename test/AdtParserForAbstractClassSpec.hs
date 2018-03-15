module AdtParserForAbstractClassSpec where

import Data.Either (Either(..))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import AdtParser
import Common

import Text.Parsec

abstractClassSimpleTest :: TestTree
abstractClassSimpleTest =
  assertParser (Command "sealed abstract class Person(name: String)")
               "ADT Parser should match simple abstract class"
               abstractClassDefP
               (Right "Person")

abstractClassWithBodyTest :: TestTree
abstractClassWithBodyTest =
  assertParser (Command "sealed abstract class Universe extends Symbols {")
               "ADT Parser should match a abstract class with a body"
               abstractClassDefP
               (Right "Universe")

abstractClassPolyTest :: TestTree
abstractClassPolyTest =
  assertParser (Command "sealed abstract class SilentResult[T]")
               "ADT Parser should match abstract class with polymorphic types"
               abstractClassDefP
               (Right "SilentResult")

abstractClassPolyAndSpacesTest :: TestTree
abstractClassPolyAndSpacesTest =
  assertParser (Command "sealed abstract class DoorState  [A]")
               "ADT Parser should match abstract class with spaces between polymorphic type declaration"
               abstractClassDefP
               (Right "DoorState")

abstractClassWithPolyCovariantTest :: TestTree
abstractClassWithPolyCovariantTest =
  assertParser (Command "sealed abstract class Try[+T] extends Product with Serializable")
               "ADT Parser should match abstract class with covariant polymorphic types"
               abstractClassDefP
               (Right "Try")

abstractClassWithPolyContravariantTest :: TestTree
abstractClassWithPolyContravariantTest =
  assertParser (Command "sealed abstract class Tree[-A, +B]")
               "ADT Parser should match abstract class with contravariant polymorphic types"
               abstractClassDefP
               (Right "Tree")

abstractClassMultiplePolyTest :: TestTree
abstractClassMultiplePolyTest =
  assertParser (Command "sealed abstract class Query[+E, U, C[_]] extends QueryBase[C[U]]")
               "ADT Parser should match abstract class with multiple polymorphic types"
               abstractClassDefP
               (Right "Query")

test_ADT_Parser_for_Abstract_Class :: TestTree
test_ADT_Parser_for_Abstract_Class =
  testGroup "ADT Parser for Abstract Class"
  [
    abstractClassSimpleTest,
    abstractClassPolyTest,
    abstractClassWithPolyCovariantTest,
    abstractClassWithPolyContravariantTest,
    abstractClassPolyAndSpacesTest,
    abstractClassWithBodyTest,
    abstractClassMultiplePolyTest
  ]

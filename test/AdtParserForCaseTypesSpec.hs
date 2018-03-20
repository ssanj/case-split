module AdtParserForCaseTypesSpec where

import Text.Parsec

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Common
import AdtParser

caseObjectMatchTest :: TestTree
caseObjectMatchTest =
  assertParser (Command "case object Red extends Traffic")
               "ADT Parser should match a case object class"
               (caseObjectDefP "Traffic")
               (Right "Red")

caseObjectMissTest :: TestTree
caseObjectMissTest =
  assertParser2 (Command "case object Open extends DoorState")
                "ADT Parser should not match a case object class with the wrong root type"
                (caseObjectDefP "Traffic")
                (hasError "Traffic")

caseClassMatchTest :: TestTree
caseClassMatchTest =
  assertParser (Command "case class SomeError(reason: String, error: Option[Throwable]) extends SError")
               "ADT Parser should match a case class"
               (caseClassDefP "SError")
               (Right $ CaseClass "SomeError"
                                  [
                                   ClassParam (PName "reason") (PType "String"),
                                   ClassParam (PName "error")  (PType "Option")
                                  ])

test_ADT_Parser_for_Abstract_Class :: TestTree
test_ADT_Parser_for_Abstract_Class =
  testGroup "ADT Parser for Case Types"
  [
    caseObjectMatchTest,
    caseObjectMissTest,
    caseClassMatchTest
  ]

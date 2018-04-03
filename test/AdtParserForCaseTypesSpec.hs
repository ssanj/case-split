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
               (Right $ CaseObject "Red")

caseObjectMissTest :: TestTree
caseObjectMissTest =
  assertParser2 (Command "case object Open extends DoorState")
                "ADT Parser should not match a case object class with the wrong root type"
                (caseObjectDefP "Traffic")
                "Traffic"

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

caseClassMatchWithDefaultArgumentTest :: TestTree
caseClassMatchWithDefaultArgumentTest =
  assertParser (Command "final case class ScalaVersionSupplied(org: String, name: String, version: String, config: Option[String] = None) extends Dependency")
               "ADT Parser should match a case class with default arguments"
               (caseClassDefP "Dependency")
               (Right $ CaseClass "ScalaVersionSupplied"
                                  [
                                   ClassParam (PName "org")     (PType "String"),
                                   ClassParam (PName "name")    (PType "String"),
                                   ClassParam (PName "version") (PType "String"),
                                   ClassParam (PName "config")  (PType "Option")
                                  ])

caseClassWithParamAnnotationsMatchTest :: TestTree
caseClassWithParamAnnotationsMatchTest =
  assertParser (Command "final case class Some[+A](@deprecatedName('x, \"2.12.0\") value: A) extends Option[A] {")
               "ADT Parser should match a case class with param annotations"
               (caseClassDefP "Option")
               (Right $ CaseClass "Some" [ClassParam (PName "value") (PType "A")])

caseClassWithNonVariantMatchTest :: TestTree
caseClassWithNonVariantMatchTest =
  assertParser (Command "final case class InvalidResourceRef[F](t: Throwable) extends ResourceProviderError[F]")
               "ADT Parser should match a case class with non variant type params"
               (caseClassDefP "ResourceProviderError")
               (Right $ CaseClass "InvalidResourceRef" [ClassParam (PName "t") (PType "Throwable")])

caseClassMissTest :: TestTree
caseClassMissTest =
  assertParser2 (Command "case class KeyNotFound(key: String) extends LookupError")
                "ADT Parser should not match a case class with the wrong root type"
                (caseClassDefP "KeyNotFound")
                "KeyNotFound"

test_ADT_Parser_for_Abstract_Class :: TestTree
test_ADT_Parser_for_Abstract_Class =
  testGroup "ADT Parser for Case Types"
  [
    caseObjectMatchTest,
    caseObjectMissTest,
    caseClassMatchTest,
    caseClassMatchWithDefaultArgumentTest,
    caseClassWithNonVariantMatchTest,
    caseClassWithParamAnnotationsMatchTest,
    caseClassMissTest
  ]

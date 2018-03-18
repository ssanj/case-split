module AdtParserForCaseTypesSpec where

import Data.Either (Either(..), isLeft, isRight)
import Data.List (intercalate, isInfixOf)

import Text.Parsec
import Text.Parsec.Error (Message, Message(..), errorMessages)

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
    where hasError text = either (\pe -> (containsMatches text pe, displayMessageStrings pe)) (\r -> (False, show r))

          messageStrings = fmap toMessageString . errorMessages

          toMessageString :: Message -> String
          toMessageString (SysUnExpect string) = "library generated unexpected:" ++ string
          toMessageString (UnExpect    string) = "unexpected:" ++ string
          toMessageString (Expect      string) = "expected:" ++ string
          toMessageString (Message     string) = "raw message:" ++ string

          displayMessageStrings = intercalate "\n" . ("" :)  . messageStrings

          containsMatches text = (> 0) . length . filter (isInfixOf text) . messageStrings

test_ADT_Parser_for_Abstract_Class :: TestTree
test_ADT_Parser_for_Abstract_Class =
  testGroup "ADT Parser for Case Types"
  [
    caseObjectMatchTest,
    caseObjectMissTest
  ]

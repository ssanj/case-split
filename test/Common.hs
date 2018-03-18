module Common(Command(..),
              assertParser,
              assertParser2) where

import Data.Either (Either(..))
import Test.Tasty
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

assertParser2 :: Command -> String -> P r -> (Either ParseError r -> (Bool, String)) -> TestTree
assertParser2 (Command commands) testName parser assertResult =
  let result = parse parser "" commands in
  testCase testName $
    assertBool ("failed with:" ++ (snd $ assertResult result)) (fst $ assertResult result)
module Common(Command(..),
              assertParser,
              assertParser2,
              hasError) where

import Data.Either (Either(..))
import Data.List (intercalate, isInfixOf)


import Text.Parsec
import Text.Parsec.Error (Message, Message(..), errorMessages)

import Test.Tasty
import Test.Tasty.HUnit

import AdtParser

data Command = Command String deriving Show

assertParser :: (Eq r, Show r) => Command -> String -> P r -> Either ParseError r -> TestTree
assertParser (Command commands) testName parser expectedResult =
  let result = parse parser "" commands in
  testCase testName $
    assertBool ("failed with:" ++ (show result) ++ ", expected:" ++ (show expectedResult))
               (result == expectedResult)

hasError :: Show r => String -> Either ParseError r  -> (Bool, String)
hasError text = either (\pe -> (containsMatches text pe, displayMessageStrings pe)) (\r -> (False, show r))
        where
          messageStrings = fmap toMessageString . errorMessages

          toMessageString :: Message -> String
          toMessageString (SysUnExpect string) = "library generated unexpected:" ++ string
          toMessageString (UnExpect    string) = "unexpected:" ++ string
          toMessageString (Expect      string) = "expected:" ++ string
          toMessageString (Message     string) = "raw message:" ++ string

          displayMessageStrings = intercalate "\n" . ("" :)  . messageStrings

          containsMatches text = (> 0) . length . filter (isInfixOf text) . messageStrings


assertParser2 :: Command -> String -> P r -> (Either ParseError r -> (Bool, String)) -> TestTree
assertParser2 (Command commands) testName parser assertResult =
  let result = parse parser "" commands in
  testCase testName $
    assertBool ("failed with:" ++ (snd $ assertResult result)) (fst $ assertResult result)
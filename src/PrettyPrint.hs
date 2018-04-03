module PrettyPrint (printAdtFormats) where

import Data.List (concat, intersperse)
import Data.Maybe (catMaybes, Maybe)
import Text.Printf (printf)
import AdtParser

printAdtFormats :: [(String, [AdtType])] -> String
printAdtFormats xs = concat $ intersperse "\n" $ catMaybes $ fmap printAdtFormat xs

printAdtFormat :: (String, [AdtType]) -> Maybe String
printAdtFormat (_, [])   = Nothing -- Error
printAdtFormat (key, xs) = Just $ printf "%s;%s" key (printAdts xs)

printAdts :: [AdtType] -> String
printAdts xs = concat $ intersperse ";" $ fmap printAdt xs

printAdt :: AdtType -> String
printAdt (CaseObject name)       = printf "%s" name
printAdt (CaseClass name params) = printf "%s(%s)" name (printClassParams params)

printClassParams :: [ClassParam] -> String
printClassParams xs = concat $ intersperse "," $ fmap printClassParam xs

printClassParam :: ClassParam -> String
printClassParam (ClassParam (PName pname) _) = printf "%s" pname

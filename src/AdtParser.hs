module AdtParser
    ( adtP,
      adtTypeP,
      getAdt,
      getAdtType,
      hasADT,
      traitDefP,
      abstractClassDefP,
      caseObjectDefP,
      caseClassDefP,
      paramP,
      paramsP,
      methodParamsP,
      annotationP,
      P,
      ClassParam(..),
      PName(..),
      PType(..),
      AdtType(..),
    ) where

import Text.Parsec
import Data.Either (rights)

type P = Parsec String ()

data PName = PName String deriving (Show, Eq)

data PType = PType String deriving (Show, Eq)

data ClassParam = ClassParam PName PType deriving (Show, Eq)

data AdtType = CaseClass String [ClassParam]
             | CaseObject String deriving (Show, Eq)

typeIdP :: P String
typeIdP = many1 alphaNum

valP :: P String
valP = many1 alphaNum

hasADT :: String -> Bool
hasADT line = either (const False) (const True) (parse adtP "" line)

getAdt :: String -> Either ParseError String
getAdt line = parse adtP "" line

getAdtType :: String -> [String] -> [AdtType]
getAdtType key content = rights $ fmap (parse (adtTypeP key) "") content

adtP :: P String
adtP = try (traitDefP) <|> abstractClassDefP

adtTypeP :: String -> P AdtType
adtTypeP baseType = try (caseObjectDefP baseType) <|> caseClassDefP baseType

-- trait Blah[+A] {
traitDefP :: P String
traitDefP = do
            _ <- string "sealed"
            _ <- spaces
            _ <- string "trait"
            _ <- spaces
            typeIdP

abstractClassDefP :: P String
abstractClassDefP = do
                    _ <- string "sealed"
                    _ <- spaces
                    _ <- string "abstract"
                    _ <- spaces
                    _ <- string "class"
                    _ <- spaces
                    typeIdP

caseObjectDefP :: String -> P AdtType
caseObjectDefP adt = do
                     _ <- optional (string "final")
                     _ <- spaces
                     _ <- string "case"
                     _ <- spaces
                     _ <- string "object"
                     _ <- spaces
                     name <- typeIdP
                     _ <- spaces
                     _ <- string "extends"
                     _ <- spaces
                     _ <- string adt
                     return (CaseObject name)

upperChar :: P Char
upperChar = oneOf ['A' .. 'Z']

polyP :: P ()
polyP = do
        _ <- optional $ between (char '[') (char ']') $ many1 (noneOf "]" >> upperChar)
        -- many1 $ oneOf ("[]+-" ++ ['A' .. 'Z'])
        return ()

paramP :: P ClassParam
paramP = do
         pname <- valP
         _ <- spaces
         _ <- char ':'
         _ <- spaces
         ptype <- typeIdP
         _ <- optional (spaces >> char '[' >> spaces >> typeIdP >> spaces >> char ']')
         _ <- optional (spaces >> char '=' >> spaces >> typeIdP >> spaces)
         return $ ClassParam (PName pname) (PType ptype)

paramsP :: P [ClassParam]
paramsP = do
          _ <- spaces
          first <- paramP
          rest <- try (spaces >> char ',' >> paramsP) <|> (pure [])
          return (first : rest)

methodParamsP :: P [ClassParam]
methodParamsP = do
                _ <- char '('
                _ <- spaces
                _ <- optional annotationP
                params <- paramsP
                _ <- spaces
                _ <- char ')'
                return params

caseClassDefP :: String -> P AdtType
caseClassDefP adt = do
                    _ <- optional (string "final")
                    _ <- spaces
                    _ <- string "case"
                    _ <- spaces
                    _ <- string "class"
                    _ <- spaces
                    name <- typeIdP
                    _ <- spaces
                    _ <- polyP
                    params <- methodParamsP
                    _ <- spaces
                    _ <- string "extends"
                    _ <- spaces
                    _ <- string adt
                    return (CaseClass name params)

-- @deprecatedName('x, "2.12.0")
annotationP :: P (String, String)
annotationP = do
              _ <- char '@'
              name <- valP
              annotation <- between (char '(') (char ')') (many1 $ noneOf ")" >> anyChar)
              return (name, annotation)

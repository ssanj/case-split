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
      singleParamP,
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

-- TODO: test
hasADT :: String -> Bool
hasADT line = either (const False) (const True) (parse adtP "" line)

-- TODO: test
getAdt :: String -> Either ParseError String
getAdt line = parse adtP "" line

-- TODO: test
getAdtType :: String -> [String] -> [AdtType]
getAdtType key content = rights $ fmap (parse (adtTypeP key) "") content

-- TODO: test
adtP :: P String
adtP = try (traitDefP) <|> abstractClassDefP

-- TODO: test
adtTypeP :: String -> P AdtType
adtTypeP baseType = try (caseObjectDefP baseType) <|> caseClassDefP baseType

-- trait Blah[+A] {
traitDefP :: P String
traitDefP = do _ <- string "sealed"
               _ <- spaces
               _ <- string "trait"
               _ <- spaces
               typeIdP

abstractClassDefP :: P String
abstractClassDefP = do _ <- string "sealed"
                       _ <- spaces
                       _ <- string "abstract"
                       _ <- spaces
                       _ <- string "class"
                       _ <- spaces
                       typeIdP

caseObjectDefP :: String -> P AdtType
caseObjectDefP adt = do _ <- optional (string "final")
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

singleTypeParamP :: P ()
singleTypeParamP = do _ <- spaces
                      _ <- optional (oneOf "-+")
                      _ <- (many1 upperChar)
                      _ <- spaces
                      return ()

multipleTypeParamP :: P ()
multipleTypeParamP = do _ <- singleTypeParamP `sepBy` (char ',')
                        return ()

typeParamP :: P ()
typeParamP = optional $ between (char '[') (char ']') $ multipleTypeParamP

singleClassParamP :: P ()
singleClassParamP = do _ <- spaces
                       _ <- typeIdP
                       _ <- spaces
                       return ()

multipleClassParamP :: P ()
multipleClassParamP = do _ <- singleClassParamP `sepBy` (char ',')
                         return ()

classParamP :: P ()
classParamP = optional $ between (char '[') (char ']') $ multipleClassParamP

defaultParamP :: P ()
defaultParamP = optional (spaces >> char '=' >> spaces >> typeIdP >> spaces)

singleParamP :: P ClassParam
singleParamP = do pname <- valP
                  _ <- spaces
                  _ <- char ':'
                  _ <- spaces
                  ptype <- typeIdP
                  _ <- classParamP
                  _ <- defaultParamP
                  return $ ClassParam (PName pname) (PType ptype)

paramsP :: P [ClassParam]
paramsP = do _ <- spaces
             first <- singleParamP
             rest <- try (spaces >> char ',' >> paramsP) <|> (pure [])
             return (first : rest)

methodParamsP :: P [ClassParam]
methodParamsP = do _ <- char '('
                   _ <- spaces
                   _ <- optional annotationP
                   params <- paramsP
                   _ <- spaces
                   _ <- char ')'
                   return params

caseClassDefP :: String -> P AdtType
caseClassDefP adt = do _ <- optional (string "final")
                       _ <- spaces
                       _ <- string "case"
                       _ <- spaces
                       _ <- string "class"
                       _ <- spaces
                       name <- typeIdP
                       _ <- spaces
                       _ <- typeParamP
                       params <- methodParamsP
                       _ <- spaces
                       _ <- string "extends"
                       _ <- spaces
                       _ <- string adt
                       return (CaseClass name params)

-- @deprecatedName('x, "2.12.0")
annotationP :: P (String, String)
annotationP = do _ <- char '@'
                 name <- valP
                 annotation <- between (char '(') (char ')') (many1 $ noneOf ")" >> anyChar)
                 return (name, annotation)

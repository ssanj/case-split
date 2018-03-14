module AdtParser
    ( traitDefP,
      abstractClassDefP,
      caseObjectDefP,
      caseClassDefP,
      paramP,
      paramsP,
      methodParamsP,
      annotationP,
      P
    ) where

-- import Prelude (IO, Char, String, putStrLn, return, (++), ($))
import Text.Parsec

type P = Parsec String ()

-- sealed trait MessageError
-- final case class DecodeError(reason: String, error: Option[Throwable]) extends MessageError
-- final case class ValidationError(reason: String) extends MessageError

-- TODO: use between (char '[') (char ']')
typeIdP :: P String
typeIdP = many1 alphaNum

valP :: P String
valP = many1 alphaNum

whitespace :: P Char
whitespace = char ' ' <|> char '\t'

whitespaces :: P [Char]
whitespaces = many whitespace

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
                    name <- typeIdP
                    _ <- char '(' <|>  char '[' <|> endOfLine
                    return name

caseObjectDefP :: String -> P String
caseObjectDefP adt = do
                     _ <- string "case"
                     _ <- spaces
                     _ <- string "object"
                     _ <- spaces
                     name <- typeIdP
                     _ <- spaces
                     _ <- string "extends"
                     _ <- spaces
                     _ <- string adt
                     _ <- spaces
                     _ <- char '[' <|> endOfLine
                     return name

-- TODO: use between (char '[') (char ']') many1 (noneOf "]" >> anyChar)
polyP :: P ()
polyP = do
        _ <- optional $ many1 $ oneOf ("[]+-" ++ ['A' .. 'Z'])
        return ()

data PName = PName String deriving Show

data PType = PType String deriving Show

data ClassParam = ClassParam PName PType deriving Show

data CaseClass = CaseClass String [ClassParam] deriving Show

paramP :: P ClassParam
paramP = do
         pname <- valP
         _ <- spaces
         _ <- char ':'
         _ <- spaces
         ptype <- typeIdP
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

-- final case class Some[+A](@deprecatedName('x, "2.12.0") value: A) extends Option[A] {
-- final case class Some[+A](value: A) extends Option[A] {

-- TODO: fix issue with endOfLine
--- "final case class ReceiveError(reason: String, error: Option[Throwable]) extends QError\n"
caseClassDefP :: String -> P CaseClass
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
                    _ <- spaces
                    _ <- char '[' <|> char '{' <|> endOfLine
                    return (CaseClass name params)

-- @deprecatedName('x, "2.12.0")
annotationP :: P (String, String)
annotationP = do
              _ <- char '@'
              name <- valP
              annotation <- between (char '(') (char ')') (many1 $ noneOf ")" >> anyChar)
              return (name, annotation)

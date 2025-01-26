{-# LANGUAGE BinaryLiterals #-}

module Scheme (schemeToken, SchemeToken (..)) where

import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Void
import           GHC.Utils.Panic.Plain    (panic)
import           Numeric
import           Text.Megaparsec
import           Text.Megaparsec.Char

data SchemeToken
  = Identifier String
  | Integer Int
  | Float Float
  | Imaginary Float
  | Boolean Bool
  | Character Char
  | String String
  | SexpStart
  | SexpEnd
  | VectorLiteralStart
  | Quote
  | Quasiquote
  | Unquote
  | StrippedQuote
  | Dot
  deriving (Show, Eq)

type Tokenizer = Parsec Void String

delimiter :: Tokenizer ()
delimiter = choice [void whitespace, void $ oneOf "()\";", eof] <?> "delimiter"

whitespace :: Tokenizer Char
whitespace = spaceChar

schemeToken :: Tokenizer SchemeToken
schemeToken =
  try identifier
    <|> boolLiteral
    <|> numberLiteral
    <|> characterLiteral
    <|> stringLiteral
    <|> sexpStart
    <|> sexpEnd
    <|> vectorLiteralStart
    <|> quote
    <|> dot

-- <identifier> ::= <peculiarIdentifier> | <typicalIdentifier>
identifier :: Tokenizer SchemeToken
identifier = do
  ident <- peculiarIdentifier <|> typicalIdentifier
  _ <- lookAhead delimiter
  return $ Identifier ident

-- <peculiarIdentifier> ::= "+" | "-" | "..."
peculiarIdentifier :: Tokenizer String
peculiarIdentifier = string "+" <|> string "-" <|> string "..."

-- <typicalIdentifier> ::= <identifierInitial> <identifierSubsequent>*
typicalIdentifier :: Tokenizer String
typicalIdentifier = do
  initial <- identifierInitial
  subsequent <- many identifierSubsequent
  return (initial : subsequent)

-- <identifierInitial> ::= <letter> | <specialInitial>
identifierInitial :: Tokenizer Char
identifierInitial = letter <|> specialInitial

-- <letter> ::= [a-zA-Z]
letter :: Tokenizer Char
letter = letterChar

-- <specialInitial> ::= any of !$&*/:<=>?^_~
specialInitial :: Tokenizer Char
specialInitial = oneOf "!$%&*/:<=>?^_~"

-- <identifierSubsequent> ::=
--     <identifierInitial> | <digit> | <specialSubsequent>
identifierSubsequent :: Tokenizer Char
identifierSubsequent = identifierInitial <|> decimalDigit <|> specialSubsequent

-- <specialSubsequent> ::= any of +-.@
specialSubsequent :: Tokenizer Char
specialSubsequent = oneOf ("+-.@" :: String)

-- <boolLiteral> ::= "#t" | "#f"
boolLiteral :: Tokenizer SchemeToken
boolLiteral = do
  literal <- string "#t" <|> string "#f"
  return (Boolean (case literal of
                      "#t" -> True
                      "#f" -> False
                      _ -> panic "scheme bool literal")) <?> "bool literal"

-- <numberLiteral> ::= <integerLiteral> | <imaginaryLiteral> | <floatLiteral>
numberLiteral :: Tokenizer SchemeToken
numberLiteral =
  try integerLiteral
  <|> try (imaginaryLiteral <?> "imaginary literal")
  <|> (floatLiteral <?> "float literal")

-- <integerLiteral> ::=
--     <binaryLiteral> | <octalLiteral> | <hexLiteral> | <decimalLiteral>
integerLiteral :: Tokenizer SchemeToken
integerLiteral = do
  integer <-
    choice
      [ try binaryLiteral <?> "binary int literal",
        try octalLiteral <?> "octal int literal",
        try hexLiteral <?> "hex int literal",
        try decimalLiteral <?> "int literal"
      ]
  return $ Integer integer

-- <binaryLiteral> ::= "#b" <binaryDigit>+
binaryLiteral :: Tokenizer Int
binaryLiteral = do
  sgn <- sign
  _ <- string "#b"
  number <- someTill binaryDigit (lookAhead delimiter)
  return $ case sgn of
    ""  -> fst (head (readBin number))
    "-" -> (-fst (head (readBin number)))
    _ -> panic "should be a Haskell-parsable sign"

-- <binaryDigit> ::= [01]
binaryDigit :: Tokenizer Char
binaryDigit = binDigitChar

-- <octalLiteral> ::= "#o" <octalDigit>+
octalLiteral :: Tokenizer Int
octalLiteral = do
  sgn <- sign
  _ <- string "#o"
  number <- someTill octalDigit (lookAhead delimiter)
  return $ read $ sgn ++ "0o" ++ number

-- <octalDigit> ::= [0-7]
octalDigit :: Tokenizer Char
octalDigit = octDigitChar

-- <hexLiteral> ::= "#x" <hexDigit>+
hexLiteral :: Tokenizer Int
hexLiteral = do
  sgn <- sign
  _ <- string "#x"
  number <- someTill hexDigit (lookAhead delimiter)
  return $ read $ sgn ++ "0x" ++ number

-- <hexDigit> ::= [0-9a-fA-F]
hexDigit :: Tokenizer Char
hexDigit = hexDigitChar

-- <decimalLiteral> ::= "#d"? <decimalDigit>+
decimalLiteral :: Tokenizer Int
decimalLiteral = do
  sgn <- sign
  number <-
    optional (string "#d")
      *> someTill decimalDigit (lookAhead delimiter)
  return $ read $ sgn ++ number

-- <decimalDigit> ::= [0-9]
decimalDigit :: Tokenizer Char
decimalDigit = digitChar

-- <floatLiteral> ::= <floatExactPrefix> (
--     <floatLiteralExp> | <floatLiteralNoExp>)
floatLiteral :: Tokenizer SchemeToken
floatLiteral = do
  number <-
    optional floatExactPrefix
      *> (try floatLiteralExp <|> floatLiteralNoExp)
  return $ Float number

-- <floatExactPrefix> ::= "#e" | "#i"
floatExactPrefix :: Tokenizer String
floatExactPrefix = string "#e" <|> string "#i"

-- <floatLiteralExp> ::=
--     (<floatLiteralNoExp> | <floatIntPart>)
--     <exponentMarker> <sign> <floatIntPart>
floatLiteralExp :: Tokenizer Float
floatLiteralExp = do
  numberSign <- sign
  mantissa <- try floatLiteralNoExpChars <|> floatIntPart
  void exponentMarker
  exponentSign <- sign
  exponentValue <- floatIntPart
  return $
    read (numberSign ++ mantissa ++ "e" ++ exponentSign ++ exponentValue)

-- <exponentMarker> ::= "s" | "f" | "d" | "l" | "e"
exponentMarker :: Tokenizer Char
exponentMarker = choice [char 's', char 'f', char 'd', char 'l', char 'e'] <?> "exponent marker"

-- <sign> ::= ("+" | "-")?
sign :: Tokenizer String
sign = do
  sgn <- optional (choice [string ("+" :: String), string ("-" :: String)])
  return $ case sgn of
    Just "-" -> "-"
    Just "+" -> ""
    Nothing  -> ""
    _ -> panic "should be a sign or nothing"

-- <floatLiteralNoExp> ::= <floatWithoutIntPart> | <floatWithIntPart>
floatLiteralNoExp :: Tokenizer Float
floatLiteralNoExp = do
  read <$> floatLiteralNoExpChars

floatLiteralNoExpChars :: Tokenizer String
floatLiteralNoExpChars = do
  numberSign <- sign
  digits <- try floatWithoutIntPart
            <|> floatWithIntPart
  return $ numberSign ++ digits

-- <floatWithIntPart> ::= <floatIntPart> "." <decimalDigit>*
floatWithIntPart :: Tokenizer String
floatWithIntPart = do
  truncated <- floatIntPart
  _ <- char '.'
  frac <- many decimalDigit
  -- add the 0 because 1. doesn't parse in Haskell
  return $ truncated ++ "." ++ frac ++ "0"

-- <floatIntPart> ::= <decimalDigit>+
floatIntPart :: Tokenizer String
floatIntPart = some decimalDigit

-- <floatWithoutIntPart> ::= "." <decimalDigit>+
floatWithoutIntPart :: Tokenizer String
floatWithoutIntPart = do
  void $ char '.'
  frac <- some decimalDigit
  return $ "0." ++ frac

-- <imaginaryLiteral> ::= <floatLiteral> "i"
imaginaryLiteral :: Tokenizer SchemeToken
imaginaryLiteral = do
  magnitude <- floatLiteral
  void $ char 'i'
  return $ case magnitude of
    Float f -> Imaginary f
    _       -> panic "should be float"

-- <characterLiteral> ::= "#\" (<namedCharacter> | <visibleCharacter>)
-- must be terminated by <delimiter>
characterLiteral :: Tokenizer SchemeToken
characterLiteral = do
  void $ string "#\\"
  character <- namedCharacter <|> visibleCharacter
  _ <- lookAhead delimiter
  return (Character character) <?> "character literal"

-- <namedCharacter> ::= "space" | "newline" | "tab" | "linefeed"
namedCharacter :: Tokenizer Char
namedCharacter = do
  choice [string "space" $> ' ',
    string "newline" $> '\n',
    string "tab" $> '\t',
    string "linefeed" $> '\r'
    ]

-- <visibleCharacter> ::= any non-whitespace character
visibleCharacter :: Tokenizer Char
visibleCharacter = do
  satisfy (not . isSpace)

-- <stringLiteral> ::= '"' (<escapedCharacter> | <nonEscapedCharacter>)* '"'
stringLiteral :: Tokenizer SchemeToken
stringLiteral = do
  _ <- char '"'
  contents <- many (escapedCharacter <|> nonEscapedCharacter)
  _ <- char '"'
  return $ String contents

escapedCharacter :: Tokenizer Char
escapedCharacter = do
  _ <- char '\\'
  choice [char 'n' $> '\n',
          char 't' $> '\t',
          char 'r' $> '\r',
          char '\\' $> '\\',
          char '"' $> '"'
         ]

nonEscapedCharacter :: Tokenizer Char
nonEscapedCharacter = noneOf ("\"\\" :: String)

-- <sexpStart> ::= "("
sexpStart :: Tokenizer SchemeToken
sexpStart = do
  _ <- string "("
  return SexpStart

-- <sexpEnd> ::= ")"
sexpEnd :: Tokenizer SchemeToken
sexpEnd = do
  _ <- string ")"
  return SexpEnd

-- <vectorLiteralStart> ::= "#("
vectorLiteralStart :: Tokenizer SchemeToken
vectorLiteralStart = do
  _ <- string "#("
  return VectorLiteralStart

-- <quote> ::= <regularQuote> | <quasiquote> | <strippedQuote> | <unquote>
quote :: Tokenizer SchemeToken
quote = regularQuote <|> quasiquote <|> strippedQuote <|> unquote

-- <regularQuote> ::= "'"
regularQuote :: Tokenizer SchemeToken
regularQuote = do
  _ <- string "'"
  return Quote

-- <quasiquote> ::= "`"
quasiquote :: Tokenizer SchemeToken
quasiquote = do
  _ <- string "`"
  return Quasiquote

-- <unquote> ::= ","
unquote :: Tokenizer SchemeToken
unquote = do
  _ <- string ","
  return Unquote

-- <strippedQuote> ::= ",@"
strippedQuote :: Tokenizer SchemeToken
strippedQuote = do
  _ <- string ",@"
  return StrippedQuote

-- <dot> ::= "."
dot :: Tokenizer SchemeToken
dot = do
  _ <- char '.'
  _ <- lookAhead delimiter
  return Dot

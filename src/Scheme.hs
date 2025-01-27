{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TupleSections  #-}

module Scheme (schemeToken, SchemeToken (..)) where

import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Void
import           GHC.Utils.Panic.Plain (panic)
import           Numeric
import           Text.Megaparsec
import           Text.Megaparsec.Char

data SchemeToken
  = Identifier String
  | Integer Int
  | Float Float
  | Complex (Float, Float)
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
  Identifier <$> try identifier
    <|> Boolean <$> boolLiteral
    <|> numberLiteral
    <|> Character <$> characterLiteral
    <|> String <$> stringLiteral
    <|> sexpStart $> SexpStart
    <|> sexpEnd $> SexpEnd
    <|> vectorLiteralStart $> VectorLiteralStart
    <|> quote
    <|> dot $> Dot

-- <identifier> ::= <peculiarIdentifier> | <typicalIdentifier>
identifier :: Tokenizer String
identifier = do
  ident <- peculiarIdentifier <|> typicalIdentifier
  _ <- lookAhead delimiter
  return ident

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
boolLiteral :: Tokenizer Bool
boolLiteral = do
  literal <- string "#t" <|> string "#f"
  return (case literal of
                      "#t" -> True
                      "#f" -> False
                      _    -> panic "scheme bool literal") <?> "bool literal"

-- <numberLiteral> ::= <integerLiteral> | <complexLiteral> | <floatLiteral>
numberLiteral :: Tokenizer SchemeToken
numberLiteral =
  Integer <$> try integerLiteral
  <|> Complex <$> (try complexLiteral <?> "imaginary literal")
  <|> (Float <$> floatLiteral <?> "float literal")

-- <integerLiteral> ::=
--     <binaryLiteral> | <octalLiteral> | <hexLiteral> | <decimalLiteral>
integerLiteral :: Tokenizer Int
integerLiteral = choice
      [ try binaryLiteral,
        try octalLiteral,
        try hexLiteral,
        try decimalLiteral
      ] <?> "int literal"

-- <binaryLiteral> ::= "#b" <binaryDigit>+
binaryLiteral :: Tokenizer Int
binaryLiteral = do
  sgn <- sign
  _ <- string "#b"
  number <- someTill binaryDigit (lookAhead delimiter)
  return (case sgn of
    ""  -> fst (head (readBin number))
    "-" -> (-fst (head (readBin number)))
    _   -> panic "should be a Haskell-parsable sign") <?> "binary int literal"

-- <binaryDigit> ::= [01]
binaryDigit :: Tokenizer Char
binaryDigit = binDigitChar

-- <octalLiteral> ::= "#o" <octalDigit>+
octalLiteral :: Tokenizer Int
octalLiteral = do
  sgn <- sign
  _ <- string "#o"
  number <- someTill octalDigit (lookAhead delimiter)
  return (read $ sgn ++ "0o" ++ number) <?> "octal int literal"

-- <octalDigit> ::= [0-7]
octalDigit :: Tokenizer Char
octalDigit = octDigitChar

-- <hexLiteral> ::= "#x" <hexDigit>+
hexLiteral :: Tokenizer Int
hexLiteral = do
  sgn <- sign
  _ <- string "#x"
  number <- someTill hexDigit (lookAhead delimiter)
  return (read $ sgn ++ "0x" ++ number) <?> "hex int literal"

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
  return (read $ sgn ++ number) <?> "decimal int literal"

-- <decimalDigit> ::= [0-9]
decimalDigit :: Tokenizer Char
decimalDigit = digitChar

-- <floatLiteral> ::= <floatExactPrefix> (
--     <floatLiteralExp> | <floatLiteralNoExp>)
floatLiteral :: Tokenizer Float
floatLiteral = optional floatExactPrefix *> (try floatLiteralExp <|> floatLiteralNoExp)

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
  sgn <- optional (choice [string "+", string "-"])
  return (case sgn of
    Just "-" -> "-"
    Just "+" -> ""
    Nothing  -> ""
    _        -> panic "should be a sign or nothing") <?> "sign"

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
  _ <- char '.'
  frac <- some decimalDigit
  return $ "0." ++ frac

-- <complexLiteral> ::= <complex> | <imaginary>
complexLiteral :: Tokenizer (Float, Float)
complexLiteral = try complex <|> imaginary

-- <complex> ::= <floatLiteral> <imaginaryPart>
complex :: Tokenizer (Float, Float)
complex = do
  real <- floatLiteral
  imag <- imaginaryPart
  return (real, imag)

-- <imaginary> ::= <imaginaryPart>
imaginary :: Tokenizer (Float, Float)
imaginary = (0.0,) <$> imaginaryPart

-- <imaginaryPart> ::= <valuedImaginary> | <plainImaginary>
imaginaryPart :: Tokenizer Float
imaginaryPart = try valuedImaginary <|> plainImaginary

-- <valuedImaginary> ::= <floatLiteral> "i"
valuedImaginary :: Tokenizer Float
valuedImaginary = do
  imag <- floatLiteral
  _ <- char 'i'
  return imag

-- <plainImaginary> ::= <definiteSign> "i"
plainImaginary :: Tokenizer Float
plainImaginary = do
  sgn <- definiteSign
  _ <- char 'i'
  return $ case sgn of
    ""  -> 1.0
    "-" -> -1.0
    _   -> panic "should be a Haskell-parsable sign"

-- | Either "+" or "-", and positive is not implicit
-- <definiteSign> ::= "+" | "-"
definiteSign :: Tokenizer String
definiteSign = choice [string "+" $> "", string "-"]

-- <characterLiteral> ::= "#\" (<namedCharacter> | <visibleCharacter>)
-- must be terminated by <delimiter>
characterLiteral :: Tokenizer Char
characterLiteral = do
  void $ string "#\\"
  character <- namedCharacter <|> visibleCharacter
  _ <- lookAhead delimiter
  return character <?> "character literal"

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
stringLiteral :: Tokenizer String
stringLiteral = do
  _ <- char '"'
  contents <- many (escapedCharacter <|> nonEscapedCharacter)
  _ <- char '"'
  return contents

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
sexpStart :: Tokenizer ()
sexpStart = do
  _ <- string "("
  return ()

-- <sexpEnd> ::= ")"
sexpEnd :: Tokenizer ()
sexpEnd = do
  _ <- string ")"
  return ()

-- <vectorLiteralStart> ::= "#("
vectorLiteralStart :: Tokenizer ()
vectorLiteralStart = do
  _ <- string "#("
  return ()

-- <quote> ::= <regularQuote> | <quasiquote> | <strippedQuote> | <unquote>
quote :: Tokenizer SchemeToken
quote = regularQuote $> Quote
  <|> quasiquote $> Quasiquote
  <|> strippedQuote $> StrippedQuote
  <|> unquote $> Unquote

-- <regularQuote> ::= "'"
regularQuote :: Tokenizer ()
regularQuote = do
  _ <- string "'"
  return ()

-- <quasiquote> ::= "`"
quasiquote :: Tokenizer ()
quasiquote = do
  _ <- string "`"
  return ()

-- <unquote> ::= ","
unquote :: Tokenizer ()
unquote = do
  _ <- string ","
  return ()

-- <strippedQuote> ::= ",@"
strippedQuote :: Tokenizer ()
strippedQuote = do
  _ <- string ",@"
  return ()

-- <dot> ::= "."
dot :: Tokenizer ()
dot = do
  _ <- char '.'
  _ <- lookAhead delimiter
  return ()

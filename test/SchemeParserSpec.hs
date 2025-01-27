{-# LANGUAGE BinaryLiterals #-}

module SchemeParserSpec where

import           Scheme
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

spec :: Spec
spec = do
  describe "scheme tokenizer" $ do
    bool
    character
    identifier
    number
    string
    sexpStart
    sexpEnd
    vectorLiteralStart
    quote
    dot

bool :: Spec
bool = do
  describe "bool literal" $ do
    it "parses #t" $
      parse schemeToken "" "#t" `shouldParse` Boolean True
    it "parses #f" $
      parse schemeToken "" "#f" `shouldParse` Boolean False

character :: Spec
character = do
  describe "character literal" $ do
    it "parses named characers" $ do
      parse schemeToken "" "#\\space" `shouldParse` Character ' '
      parse schemeToken "" "#\\newline" `shouldParse` Character '\n'
      parse schemeToken "" "#\\tab" `shouldParse` Character '\t'
      parse schemeToken "" "#\\linefeed" `shouldParse` Character '\r'
    it "parses letters" $ do
      parse schemeToken "" "#\\a" `shouldParse` Character 'a'
    it "parses numbers" $ do
      parse schemeToken "" "#\\5" `shouldParse` Character '5'
    it "parses specials" $ do
      parse schemeToken "" "#\\#" `shouldParse` Character '#'
      parse schemeToken "" "#\\%" `shouldParse` Character '%'
      parse schemeToken "" "#\\\\" `shouldParse` Character '\\'
      parse schemeToken "" "#\\;" `shouldParse` Character ';'
    context "when not followed by a <delimiter>" $ do
      it "doesn't parse" $ do
        parse schemeToken "" `shouldFailOn` "#\\"
        parse schemeToken "" `shouldFailOn` "#\\aa"
        parse schemeToken "" `shouldSucceedOn` "#\\a)"
        parse schemeToken "" `shouldFailOn` "#\\spacen"

identifier :: Spec
identifier = do
  describe "identifier" $ do
    it "can be peculiar" $ do
      parse schemeToken "plus (+)" "+" `shouldParse` Identifier "+"
      parse schemeToken "minus (-)" "-" `shouldParse` Identifier "-"
      parse schemeToken "dotdotdot (...)" "..." `shouldParse` Identifier "..."
    it "can be typical" $ do
      parse schemeToken "basic" "potato" `shouldParse` Identifier "potato"
      parse schemeToken "basic with special" "foo-bar"
        `shouldParse` Identifier "foo-bar"
      parse schemeToken "all specials" "!$%&*/:<=>?^_~+-.@"
        `shouldParse` Identifier "!$%&*/:<=>?^_~+-.@"
      parse schemeToken "with a digit" "foo/bar0"
        `shouldParse` Identifier "foo/bar0"
      parse schemeToken "all lowercase letters" "abcdefghijklmnopqrstuvwxyz"
        `shouldParse` Identifier "abcdefghijklmnopqrstuvwxyz"
      parse schemeToken "all uppercase letters" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        `shouldParse` Identifier "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      parse schemeToken "" "x01234567890"
        `shouldParse` Identifier "x01234567890"
    context "when the identifier begins with an invalid character" $ do
      it "fails to parse" $ do
        parse schemeToken "initial digit" `shouldFailOn` "0potato"
        parse schemeToken "initial +" `shouldFailOn` "+potato"
        parse schemeToken "initial -" `shouldFailOn` "-potato"
        parse schemeToken "initial ." `shouldFailOn` ".potato"
        parse schemeToken "initial @" `shouldFailOn` "@potato"
    context "when the identifier contains an invalid character" $ do
      it "fails to parse" $ do
        parse schemeToken "internal #" `shouldFailOn` "before-invalid-char#after-invalid-char"

number :: Spec
number = do
  describe "number" $ do
    integer
    float
    imaginary

integer :: Spec
integer = do
  describe "integer literal" $ do
    it "can be binary" $ do
      parse schemeToken "" "#b1000101" `shouldParse` Integer 0b1000101
      parse schemeToken "" "#b0" `shouldParse` Integer 0
      parse schemeToken "" "#b0000" `shouldParse` Integer 0
      parse schemeToken "" "#b1111" `shouldParse` Integer 0b1111
    it "can be octal" $ do
      parse schemeToken "" "#o0001234567" `shouldParse` Integer 0o0001234567
    it "can be hexadecimal hexadecimal" $ do
      parse schemeToken "" "#x0123456789abcdef"
        `shouldParse` Integer 0x0123456789abcdef
      parse schemeToken "" "#xABCDEF" `shouldParse` Integer 0xABCDEF
    it "can be decimal" $ do
      parse schemeToken "" "#d01234567890" `shouldParse` Integer 01234567890
      parse schemeToken "" "01234567890" `shouldParse` Integer 01234567890
    it "may have a sign" $ do
      parse schemeToken "" "+1234" `shouldParse` Integer 1234
      parse schemeToken "" "-1234" `shouldParse` Integer (-1234)
    context "when there are out-of-radix digits" $ do
      it "fails to parse" $ do
        parse schemeToken "" `shouldFailOn` "#b01012010"
        parse schemeToken "" `shouldFailOn` "#o876543210"
        parse schemeToken "" `shouldFailOn` "#d1234d"
    context "when there are non-digit characters" $ do
      it "fails to parse" $ do
        parse schemeToken "" `shouldFailOn` "#xDECAFBAG"
        parse schemeToken "" `shouldFailOn` "00O00"
        parse schemeToken "" `shouldFailOn` "11i11"

float :: Spec
float = do
  describe "float literal" $ do
    it "must have a dot if it doesn't have an exponent" $ do
      parse schemeToken "" "0.0" `shouldParse` Float 0.0
      parse schemeToken "" ".5" `shouldParse` Float 0.5
      parse schemeToken "" "1." `shouldParse` Float 1.0
    it "only has decimal digits" $ do
      parse schemeToken "" "12345.67890" `shouldParse` Float 12345.67890
      parse schemeToken "" `shouldFailOn` "5C.5C"
    it "may have an exponent marker" $ do
      parse schemeToken "" "1.0e5" `shouldParse` Float 1.0e5
      parse schemeToken "" "1.0e-5" `shouldParse` Float 1.0e-5
      parse schemeToken "" "1.0e+5" `shouldParse` Float 1.0e5
      parse schemeToken "" "1e5" `shouldParse` Float 1.0e5
      parse schemeToken "short" "1s5" `shouldParse` Float 1.0e5
      parse schemeToken "float" "1f5" `shouldParse` Float 1.0e5
      parse schemeToken "double" "1d5" `shouldParse` Float 1.0e5
      parse schemeToken "long" "1l5" `shouldParse` Float 1.0e5
    it "may have an exactness prefix" $ do
      parse schemeToken "" "#e1.0" `shouldParse` Float 1.0
      parse schemeToken "" "#i1.0" `shouldParse` Float 1.0
    it "may be signed" $ do
      parse schemeToken "" "+0.0" `shouldParse` Float 0.0
      parse schemeToken "" "-0.0" `shouldParse` Float 0.0
      parse schemeToken "" "+1.0" `shouldParse` Float 1.0
      parse schemeToken "" "-1.0" `shouldParse` Float (-1.0)
      parse schemeToken "" "+1e-5" `shouldParse` Float 1.0e-5
      parse schemeToken "" "-1e+5" `shouldParse` Float (-1.0e5)

imaginary :: Spec
imaginary = do
  describe "imaginary literal" $ do
    it "is a float followed by 'i'" $ do
      parse schemeToken "" "1.i" `shouldParse` Complex (0.0, 1.0)
      parse schemeToken "" ".1i" `shouldParse` Complex (0.0, 0.1)
      parse schemeToken "" "1.1i" `shouldParse` Complex (0.0, 1.1)
      parse schemeToken "" "1e5i" `shouldParse` Complex (0.0, 1e5)
      parse schemeToken "" "1e-5i" `shouldParse` Complex (0.0, 1e-5)
    it "is a sign followed by 'i'" $ do
      parse schemeToken "" "+i" `shouldParse` Complex (0.0, 1.0)
      parse schemeToken "" "-i" `shouldParse` Complex (0.0, -1.0)
      parse schemeToken "" "i" `shouldParse` Identifier "i"
    it "is a float literal, a sign, and then 'i'" $ do
      parse schemeToken "" "1.+i" `shouldParse` Complex (1.0, 1.0)
      parse schemeToken "" "1e5-i" `shouldParse` Complex (1e5, -1.0)
    it "is a float literal, a sign, another float, and then 'i'" $ do
      parse schemeToken "" "+1.0+1.0i" `shouldParse` Complex (1.0, 1.0)
      parse schemeToken "" "-1e-5-1e-5i" `shouldParse` Complex (-1e-5, -1e-5)

string :: Spec
string = do
  describe "string literal" $ do
    it "is a sequence of characters between double quotes" $ do
      parse schemeToken "" "\"\"" `shouldParse` String ""
      parse
        schemeToken
        ""
        "\"I am the very model of a modern major general\""
        `shouldParse` String "I am the very model of a modern major general"
      parse schemeToken "special" "\"!@#$%^&*()-_+=,.<>/?;:'[]{}|`~\""
        `shouldParse` String "!@#$%^&*()-_+=,.<>/?;:'[]{}|`~"
      parse schemeToken "digits" "\"1234567890\""
        `shouldParse` String "1234567890"
    it "may contain escaped characters" $ do
      parse schemeToken "backslash" "\"\\\\\"" `shouldParse` String "\\"
      parse schemeToken "double quote" "\"\\\"\"" `shouldParse` String "\""
      parse schemeToken "newline" "\"\\n\"" `shouldParse` String "\n"
      parse schemeToken "tab" "\"\\t\"" `shouldParse` String "\t"
      parse schemeToken "carriage return" "\"\\r\""
        `shouldParse` String "\r"
    it "may contain literal tabs, newlines, and carriage returns" $ do
      parse schemeToken "literal whitespace" "\"\n\t\r\""
        `shouldParse` String "\n\t\r"
    it "doesn't contain lone backslashes" $ do
      parse schemeToken "lone backslash" `shouldFailOn` "\"abc \\ 123\""

sexpStart :: Spec
sexpStart = do
  describe "S-expression start" $ do
    it "is an open parenthesis" $ do
      parse schemeToken "" "(" `shouldParse` SexpStart

sexpEnd :: Spec
sexpEnd = do
  describe "S-expression end" $ do
    it "is an end parenthesis" $ do
      parse schemeToken "" ")" `shouldParse` SexpEnd

vectorLiteralStart :: Spec
vectorLiteralStart = do
  describe "vector literal start" $ do
    it "is a #(" $ do
      parse schemeToken "" "#(" `shouldParse` VectorLiteralStart

quote :: Spec
quote = do
  describe "quotes" $ do
    it "parses a regular quote" $ do
      parse schemeToken "" "'" `shouldParse` Quote
    it "parses a quasiquote" $ do
      parse schemeToken "" "`" `shouldParse` Quasiquote
    it "parses an unquote" $ do
      parse schemeToken "" "," `shouldParse` Unquote
    it "parses a stripped quote" $ do
      parse schemeToken "" ",@" `shouldParse` StrippedQuote

dot :: Spec
dot = do
  describe "dot" $ do
    it "parses a dot" $ do
      parse schemeToken "" "." `shouldParse` Dot

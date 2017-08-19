{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-unused-binds #-}
module Language.Java.Lexer (JavaToken(..), lexer) where

import Numeric
import Data.Char
import Language.Java.Position
import Data.Maybe (maybe)
import           GHC.Generics        (Generic)
}

%wrapper "posn"

$digit      = [0-9]
$nonzero    = [1-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

$javaLetter = [a-zA-Z\_\$]
$javaDigit = $digit
$javaLetterOrDigit = [a-zA-Z0-9\_\$]

@octEscape = [0123]? $octdig{1,2}
@hexEscape = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

@expsuffix = [\+\-]? $digit+
@exponent = [eE] @expsuffix
@pexponent = [pP] @expsuffix

tokens  :-

    $white+         ;
    @comm           ;

    "@interface"    { ignore KW_AnnInterface }
    abstract        { ignore KW_Abstract }
    assert          { ignore KW_Assert }
    boolean         { ignore KW_Boolean }
    break           { ignore KW_Break }
    byte            { ignore KW_Byte }
    case            { ignore KW_Case }
    catch           { ignore KW_Catch }
    char            { ignore KW_Char }
    class           { ignore KW_Class }
    const           { ignore KW_Const }
    continue        { ignore KW_Continue }
    default         { ignore KW_Default }
    do              { ignore KW_Do }
    double          { ignore KW_Double }
    else            { ignore KW_Else }
    enum            { ignore KW_Enum }
    extends         { ignore KW_Extends }
    final           { ignore KW_Final }
    finally         { ignore KW_Finally }
    float           { ignore KW_Float }
    for             { ignore KW_For }
    goto            { ignore KW_Goto }
    if              { ignore KW_If }
    implements      { ignore KW_Implements }
    import          { ignore KW_Import }
    instanceof      { ignore KW_Instanceof }
    int             { ignore KW_Int }
    interface       { ignore KW_Interface }
    long            { ignore KW_Long }
    native          { ignore KW_Native }
    new             { ignore KW_New }
    package         { ignore KW_Package }
    private         { ignore KW_Private }
    protected       { ignore KW_Protected }
    public          { ignore KW_Public }
    return          { ignore KW_Return }
    short           { ignore KW_Short }
    static          { ignore KW_Static }
    strictfp        { ignore KW_Strictfp }
    super           { ignore KW_Super }
    switch          { ignore KW_Switch }
    synchronized    { ignore KW_Synchronized }
    this            { ignore KW_This }
    throw           { ignore KW_Throw }
    throws          { ignore KW_Throws }
    transient       { ignore KW_Transient }
    try             { ignore KW_Try }
    void            { ignore KW_Void }
    volatile        { ignore KW_Volatile }
    while           { ignore KW_While }

    0               { ignore (IntTok 0) }
    0 [lL]          { ignore (LongTok 0) }
    0 $digit+       { readTk (\s -> IntTok (pickyReadOct s)) }
    0 $digit+ [lL]  { readTk (\s -> LongTok (pickyReadOct (init s))) }
    $nonzero $digit*        { readTk (\s -> IntTok (read s)) }
    $nonzero $digit* [lL]   { readTk (\s -> LongTok (read (init s))) }
    0 [xX] $hexdig+         { readTk (\s -> IntTok (fst . head $ readHex (drop 2 s))) }
    0 [xX] $hexdig+ [lL]    { readTk (\s -> LongTok (fst . head $ readHex (init (drop 2 s)))) }

    $digit+ \. $digit* @exponent? [dD]?           { readTk (\s -> DoubleTok (fst . head $ readFloat $ '0':s)) }
            \. $digit+ @exponent? [dD]?           { readTk (\s -> DoubleTok (fst . head $ readFloat $ '0':s)) }
    $digit+ \. $digit* @exponent? [fF]            { readTk (\s -> FloatTok  (fst . head $ readFloat $ '0':s)) }
            \. $digit+ @exponent? [fF]            { readTk (\s -> FloatTok  (fst . head $ readFloat $ '0':s)) }
    $digit+ @exponent                             { readTk (\s -> DoubleTok (fst . head $ readFloat s)) }
    $digit+ @exponent? [dD]                       { readTk (\s -> DoubleTok (fst . head $ readFloat s)) }
    $digit+ @exponent? [fF]                       { readTk (\s -> FloatTok  (fst . head $ readFloat s)) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [dD]? { readTk (\s -> DoubleTok (readHexExp (drop 2 s))) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [fF]  { readTk (\s -> FloatTok  (readHexExp (drop 2 s))) }

    true            { ignore (BoolTok True)    }
    false           { ignore (BoolTok False)   }

    ' (@charEscape | ~[\\\']) '               { readTk (\s -> CharTok (readCharTok s)) }

    \" (@charEscape | ~[\\\"])* \"            { readTk (\s -> StringTok (readStringTok s)) }

    null            {ignore NullTok }

    $javaLetter $javaLetterOrDigit*     { readTk (\s -> IdentTok s) }

    \(              { ignore OpenParen }
    \)              { ignore CloseParen }
    \[              { ignore OpenSquare }
    \]              { ignore CloseSquare }
    \{              { ignore OpenCurly }
    \}              { ignore CloseCurly }
    \;              { ignore SemiColon }
    \,              { ignore Comma }
    \.              { ignore Period }
    "->"            { ignore LambdaArrow }
    "::"            { ignore MethodRefSep }

    "="             { ignore Op_Equal }
    ">"             { ignore Op_GThan }
    "<"             { ignore Op_LThan }
    "!"             { ignore Op_Bang }
    "~"             { ignore Op_Tilde }
    "?"             { ignore Op_Query }
    ":"             { ignore Op_Colon }
    "=="            { ignore Op_Equals }
    "<="            { ignore Op_LThanE }
    ">="            { ignore Op_GThanE }
    "!="            { ignore Op_BangE }
    "&&"            { ignore Op_AAnd }
    "||"            { ignore Op_OOr }
    "++"            { ignore Op_PPlus }
    "--"            { ignore Op_MMinus }
    "+"             { ignore Op_Plus }
    "-"             { ignore Op_Minus }
    "*"             { ignore Op_Star }
    "/"             { ignore Op_Slash }
    "&"             { ignore Op_And }
    "|"             { ignore Op_Or }
    "^"             { ignore Op_Caret }
    "%"             { ignore Op_Percent }
    "<<"            { ignore Op_LShift }
    "+="            { ignore Op_PlusE }
    "-="            { ignore Op_MinusE }
    "*="            { ignore Op_StarE }
    "/="            { ignore Op_SlashE }
    "&="            { ignore Op_AndE }
    "|="            { ignore Op_OrE }
    "^="            { ignore Op_CaretE }
    "%="            { ignore Op_PercentE }
    "<<="           { ignore Op_LShiftE }
    ">>="           { ignore Op_RShiftE }
    ">>>="          { ignore Op_RRShiftE }
    "@"             { ignore Op_AtSign }


{

pickyReadOct :: String -> Integer
pickyReadOct s =
  if not $ null remStr
  then lexicalError $ "Non-octal digit '" ++ take 1 remStr ++ "' in \"" ++ s ++ "\"."
  else n
    where (n,remStr) = head $ readOct s

readHexExp :: (Floating a, Eq a) => String -> a
readHexExp initial =
    let (m, suf) = head $ readHex initial
        (e, _) = case suf of
                      p:s | toLower p == 'p' -> head $ readHex s
                      _                      -> (0, "")
     in m ** e

readCharTok :: String -> Char
readCharTok s = head . convChar . dropQuotes $ s
readStringTok :: String -> String
readStringTok = convChar . dropQuotes

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> String
convChar ('\\':'u':s@(d1:d2:d3:d4:s')) =
  -- TODO: this is the wrong place for handling unicode escapes
  -- according to the Java Language Specification. Unicode escapes can
  -- appear anywhere in the source text, and are best processed
  -- before lexing.
  if all isHexDigit [d1,d2,d3,d4]
  then toEnum (read ['0','x',d1,d2,d3,d4]):convChar s'
  else lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':'u':s) =
  lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':c:s) =
  if isOctDigit c
  then convOctal maxRemainingOctals
  else (case c of
          'b' -> '\b'
          'f' -> '\f'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          '\'' -> '\''
          '\\' -> '\\'
          '"' -> '"'
          _ -> badEscape):convChar s
  where maxRemainingOctals =
          if c <= '3' then 2 else 1
        convOctal n =
          let octals = takeWhile isOctDigit $ take n s
              noctals = length octals
              toChar = toEnum . fst . head . readOct
          in toChar (c:octals):convChar (drop noctals s)
        badEscape = lexicalError $ "bad escape \"\\" ++ c:"\""
convChar ("\\") =
  lexicalError "bad escape \"\\\""
convChar (x:s) = x:convChar s
convChar "" = ""

lexicalError :: String -> a
lexicalError = error . ("lexical error: " ++)

ignore :: JavaToken -> AlexPosn -> String -> Located JavaToken
ignore tk = readTk (const tk)

readTk :: (String -> JavaToken) -> AlexPosn -> String -> Located JavaToken
readTk tkf p str = Loc spn (tkf str)
    where
        spn :: Segment
        spn = posnToSpan p (length str)

        posnToSpan :: AlexPosn -> Int -> Segment
        posnToSpan (AlexPn _ l c) w = Segment (Position l c) (Position l (c + w))

data JavaToken
    -- Keywords
    = KW_Abstract
    | KW_AnnInterface
    | KW_Assert
    | KW_Boolean
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Default
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Extends
    | KW_Final
    | KW_Finally
    | KW_Float
    | KW_For
    | KW_Goto
    | KW_If
    | KW_Implements
    | KW_Import
    | KW_Instanceof
    | KW_Int
    | KW_Interface
    | KW_Long
    | KW_Native
    | KW_New
    | KW_Package
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Return
    | KW_Short
    | KW_Static
    | KW_Strictfp
    | KW_Super
    | KW_Switch
    | KW_Synchronized
    | KW_This
    | KW_Throw
    | KW_Throws
    | KW_Transient
    | KW_Try
    | KW_Void
    | KW_Volatile
    | KW_While

    -- Separators
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | SemiColon
    | Comma
    | Period
    | LambdaArrow
    | MethodRefSep

    -- Literals
    | IntTok  Integer
    | LongTok Integer
    | DoubleTok Double
    | FloatTok Double
    | CharTok Char
    | StringTok String
    | BoolTok Bool
    | NullTok

    -- Identifiers
    | IdentTok String

    -- Operators
    | Op_Equal
    | Op_GThan
    | Op_LThan
    | Op_Bang
    | Op_Tilde
    | Op_Query
    | Op_Colon
    | Op_Equals
    | Op_LThanE
    | Op_GThanE
    | Op_BangE
    | Op_AAnd
    | Op_OOr
    | Op_PPlus
    | Op_MMinus
    | Op_Plus
    | Op_Minus
    | Op_Star
    | Op_Slash
    | Op_And
    | Op_Or
    | Op_Caret
    | Op_Percent
    | Op_LShift
    | Op_PlusE
    | Op_MinusE
    | Op_StarE
    | Op_SlashE
    | Op_AndE
    | Op_OrE
    | Op_CaretE
    | Op_PercentE
    | Op_LShiftE
    | Op_RShiftE
    | Op_RRShiftE
    | Op_AtSign
  deriving (Show, Eq, Generic)

data NoTokens = NoTokens

lexer :: String -> [Located JavaToken]
lexer = alexScanTokens

}

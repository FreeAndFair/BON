{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module BON.Parser.Lexer
  ( module BON.Parser.Position
  , Token(..)
  , ppToken
  , AlexInput
  , initialAlexInput
  , alexScan
  , alexGetByte
  , AlexReturn(..)
  , scan
  ) where

import Data.Char (isAscii, isDigit)
import qualified Data.Char as Char
import Data.Word (Word8)

import BON.Parser.Position

}

$digit     = [0-9]
$lower     = [a-z]
$upper     = [A-Z]
$alpha     = [$lower $upper]
$alphanum  = [$alpha $digit]
$idchar    = [$alphanum \_]
$graphic   = $printable # $white

@gap       = \\ $white+ \\
@string    = $graphic # [\"] | " " | @gap

@nat       = $digit+
@real      = $digit+ \. $digit+
@ident     = $alpha ($idchar* $alphanum)?


@punct =
  "(" | ")" | "*" | "+" | "," | "-" | "->" | "." | ".." | "..." |
  "/" | "//" | "/=" | ":" | ":{" | ";" | "<" | "<-" | "<->" | "<=" |
  "=" | ">" | ">=" | "[" | "\\\\" | "]" | "^" | "{" | "}"

@keywords =
  "Current" | "Result" | "Void" | "action" | "and" | "class" |
  "class_chart" | "client" | "cluster" | "cluster_chart" | "command" |
  "component" | "constraint" | "creates" | "creation_chart" |
  "creator" | "deferred" | "delta" | "description" | "dictionary" |
  "dynamic_diagram" | "effective" | "end" | "ensure" | "event" |
  "event_chart" | "exists" | "explanation" | "false" | "feature" |
  "for_all" | "incoming" | "indexing" | "inherit" | "interfaced" |
  "invariant" | "involves" | "it_holds" | "member_of" | "nameless" |
  "not" | "object" | "object_group" | "object_stack" | "old" | "or" |
  "outgoing" | "part" | "persistent" | "query" | "redefined" |
  "require" | "reused" | "root" | "scenario" | "scenario_chart" |
  "static_diagram" | "such_that" | "system_chart" | "true" | "xor"

@key = @punct | @keywords

tokens :-

$white+;
"--".*         { TComment }
\" @string* \" { TString . read }
@nat           { TNat . read }
@real          { TReal . read }
@key           { TKey }
@ident         { TIdent }
.              { TIllegal }

{
data Token
  = TIdent String     -- ^ Name/identifier
  | TNat Integer      -- ^ Natural number literal
  | TReal Double      -- ^ Real number literal
  | TString String    -- ^ String literal
  | TKey String       -- ^ Keyword or predefined symbol
  | TComment String   -- ^ Comment string
  | TEnd              -- ^ End of file
  | TIllegal String   -- ^ Illegal character
  deriving (Show)

ppToken :: Token -> String
ppToken tkn =
  case tkn of
    TIdent s -> s
    TNat n -> show n
    TReal d -> show d
    TString s -> show s
    TKey s -> s
    TComment s -> s
    TEnd -> "END"
    TIllegal s -> "illegal " ++ show s

type Text = String

uncons :: Text -> Maybe (Char, Text)
uncons (c : cs) = Just (c, cs)
uncons ""       = Nothing

data AlexInput
  = Inp { alexPos           :: !Position
        , alexInputPrevChar :: !Char
        , input             :: !Text
        } deriving Show

initialAlexInput :: Text -> AlexInput
initialAlexInput t =
  Inp { alexPos           = start
      , alexInputPrevChar = '\n'
      , input             = t
      }

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte i =
  do (c, rest) <- uncons (input i)
     let i' = i { alexPos = move (alexPos i) c, input = rest }
         b  = byteForChar c
     return (b, i')

byteForChar :: Char -> Word8
byteForChar c
  | c <= '\6' = non_graphic
  | isAscii c = fromIntegral (ord c)
  | otherwise = case Char.generalCategory c of
                  Char.LowercaseLetter       -> lower
                  Char.OtherLetter           -> lower
                  Char.UppercaseLetter       -> upper
                  Char.TitlecaseLetter       -> upper
                  Char.DecimalNumber         -> digit
                  Char.OtherNumber           -> digit
                  Char.ConnectorPunctuation  -> symbol
                  Char.DashPunctuation       -> symbol
                  Char.OtherPunctuation      -> symbol
                  Char.MathSymbol            -> symbol
                  Char.CurrencySymbol        -> symbol
                  Char.ModifierSymbol        -> symbol
                  Char.OtherSymbol           -> symbol
                  Char.Space                 -> sp
                  Char.ModifierLetter        -> other
                  Char.NonSpacingMark        -> other
                  Char.SpacingCombiningMark  -> other
                  Char.EnclosingMark         -> other
                  Char.LetterNumber          -> other
                  Char.OpenPunctuation       -> other
                  Char.ClosePunctuation      -> other
                  Char.InitialQuote          -> other
                  Char.FinalQuote            -> tick
                  _                          -> non_graphic
  where
  non_graphic     = 0
  upper           = 1
  lower           = 2
  digit           = 3
  symbol          = 4
  sp              = 5
  other           = 6
  tick            = 7

scan :: String -> [Token]
scan str = go (initialAlexInput str) where
  go inp =
    case alexScan inp 0 of
      AlexEOF -> [TEnd]
      AlexError _ -> error "lexical error"
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> act (take len (input inp)) : go inp'

}

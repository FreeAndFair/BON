module BON.Parser.Monad where

import Control.Monad (ap)
import qualified Data.Char as Char
import Data.Char (isAscii)
import Data.Word (Word8)

import BON.Parser.Lexer
--import BON.Parser.Position


data S = S [Token]

data ParseError
  = ParseError String
  | ParseErrorEOF
  deriving Show

-- | Parsing monad
newtype ParseM a
  = ParseM { unParseM :: S -> Either ParseError (a, S) }

runParseM :: ParseM a -> [Token] -> Either ParseError a
runParseM m toks = fmap fst $ unParseM m (S toks)

instance Functor ParseM where
  fmap f m = ParseM $ \s ->
    case unParseM m s of
      Left err -> Left err
      Right (x, s') -> Right (f x, s')

instance Monad ParseM where
  return x = ParseM $ \s -> Right (x, s)
  m >>= k  = ParseM $ \s ->
    case unParseM m s of
      Left err -> Left err
      Right (x, s') -> unParseM (k x) s'

instance Applicative ParseM where
  pure = return
  (<*>) = ap

nextToken :: ParseM Token
nextToken = ParseM $ \(S ts) ->
  case ts of
    [] -> Right (TEnd, S [])
    t : ts' -> Right (t, S ts')

-- | Return all upcoming comment tokens
getComments :: ParseM [String]
getComments = ParseM $ \(S ts) -> go [] ts
  where
    go cs (TComment c : ts') = go (c : cs) ts'
    go cs ts' = Right (reverse cs, S ts')

-- | Return the next non-comment token
nextNonCommentToken :: ParseM Token
nextNonCommentToken = do
  t <- nextToken
  case t of
    TComment _ -> nextNonCommentToken
    _ -> return t

lexerP :: (Token -> ParseM a) -> ParseM a
lexerP = (nextNonCommentToken >>=)

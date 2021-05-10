{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module HMGit.Internal.Parser.Pathspecs.Glob (
    transpile
  , isLiteral
  , match
  , match'
) where

import           HMGit.Internal.Exceptions (MonadThrowable (..))

import           Control.Exception.Safe    (MonadThrow)
import           Data.Char                 (isAlpha, isAscii, isDigit)
import           Data.Void                 (Void)
import qualified Text.Megaparsec           as M
import           Text.Regex.Posix          ((=~))

escapeRegular :: String -> String
escapeRegular = concatMap escaper
    where
        escaper x
            | x `elem` ("\'`" :: String)
                || isDigit x
                || (isAscii x && isAlpha x)
                || x `elem` ("<>" :: String) = [x]
            | otherwise = [ '\\', x ]

type GlobIRParser = M.Parsec Void String

data GlobIRToken = GlobIRSymbol String
    | GlobIRLiteral String

instance Show GlobIRToken where
    show (GlobIRSymbol s)  = s
    show (GlobIRLiteral s) = s

type GlobIR = [GlobIRToken]

stringify :: (Foldable t, Show a) => t a -> String
stringify = concatMap show

isLiteral :: GlobIR -> Bool
isLiteral = all (\case GlobIRLiteral _ -> True; _ -> False)

-- wildcard (glob) BNF
--
-- wildcard : expr
--          | expr wildcard
--
-- expr : br
--      | '*'
--      | '?'
--      | word
--
-- br : pos_bracket_expr
--    | neg_bracket_expr
--
-- pos_bracket_expr : '[' word ']'
--
-- neg_bracket_expr : '[' '!' word ']'
--
ast, que, lbr, rbr, exc, word, br, expr, wildcard :: GlobIRParser GlobIR

wildcard = M.choice [
    mempty <$ M.eof
  , (<>) <$> expr <*> wildcard
  ]

ast = [GlobIRSymbol ".*"] <$ M.single '*'

que = [GlobIRSymbol "."] <$ M.single '?'

lbr = (:[]) . GlobIRSymbol . (:[]) <$> M.single '['

rbr = (:[]) . GlobIRSymbol . (:[]) <$> M.single ']'

exc = [GlobIRSymbol "^"] <$ M.single '!'

word = (:[]) . GlobIRLiteral . escapeRegular
    <$> M.some (M.noneOf [ '*', '?', '[', ']', '!' ])

br = (\x y z -> x <> y <> z)
    <$> lbr
    <*> M.choice [ mappend <$> exc <*> word, word ]
    <*> rbr

expr = M.choice [
    br
  , ast
  , que
  , word
  ]

transpile :: MonadThrow m => String -> m GlobIR
transpile = fromMonad (Nothing :: Maybe Void)
    . M.runParser wildcard mempty

match :: String -> GlobIR -> Bool
match s = (s =~)
    . stringify
    . ([GlobIRSymbol "^"] <>) . (<> [GlobIRSymbol "$"])

match' :: String -> String -> Bool
match' s = maybe False (match s)
    . transpile


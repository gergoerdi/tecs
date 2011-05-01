-- -*- haskell-mode -*-
{
module Language.TECS.Deck.Parser.Lexer (lexer) where

import Language.TECS.Deck.Parser.Tokens
import Text.Parsec.Pos

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import Control.Arrow (first)
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Maybe (fromMaybe)
}

%wrapper "monad-bytestring"

$digit      = [0-9]
$symbol     = [a-zA-Z\-\_\.\:]

@ident = $symbol [$symbol $digit]*

@lineterm   = [\n\r] | \r\n
@cchar      = ~\* | ("*" ~\/)

tokens :-

<0> $white+           { skip }
<0> "//" .*           { accept $ Comment . BS.drop 2 }
<0> "/*"              { accept (const CommentStart) `andBegin` c }
<c> @cchar+           { accept Comment }
<c> @lineterm         { skip }
<c> "*/"              { accept (const CommentEnd) `andBegin` 0 }
<0> @ident            { accept $ Identifier . BSC.unpack }
<0> $digit+           { accept $ Value . read . BSC.unpack }

{
alexEOF = return Nothing
    
accept f ((AlexPn offset line col), _, str) len = return $ Just $ L loc tok
  where
    loc = newPos "" line col
    tok = f $ BS.take (fromIntegral len) str
  
scanner str = runAlex str $ do          
  fix $ \loop -> do
    tok <- alexMonadScan
    case tok of
      Nothing -> return []
      Just t -> do ts <- loop
                   ts `seq` return $ t:ts

lexer :: BS.ByteString -> Either String [L Token]
lexer = (fmap . map . fmap) toKeyword . scanner
  where    
    toKeyword :: Token -> Token
    toKeyword token@(Identifier s) = token `fromMaybe` keyword s
    toKeyword token = token

keyword :: String -> Maybe Token
keyword s = Map.lookup (CI.mk s) keywords

keywords :: Map (CI String) Token
keywords = Map.fromList . map (first CI.mk) $
             [ ("function", Function)
             , ("label", Label)
             , ("add", Add)
             , ("sub", Sub)
             , ("neg", Neg)
             , ("eq", Eq)
             , ("gt", Gt)
             , ("lt", Lt)
             , ("and", And)
             , ("or", Or)
             , ("not", Not)
             , ("push", Push)
             , ("pop", Pop)
             , ("goto", Goto)
             , ("if-goto", IfGoto)
             , ("call", Call)
             , ("return", Return)
             , ("argument", Argument)
             , ("local", Local)
             , ("static", Static)
             , ("constant", Constant)
             , ("pointer", Pointer)
             , ("this", This)
             , ("that", That)
             , ("temp", Temp)
             ]
}

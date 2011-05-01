-- -*- haskell-mode -*-
{
module Language.TECS.Jack.Parser.Lexer (lexer) where

import Language.TECS.Jack.Parser.Tokens
import Language.TECS.Located
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
$symbol     = [a-zA-Z\_]

@ident = $symbol [$symbol $digit]*

@lineterm   = [\n\r] | \r\n
@cchar      = ~\* | ("*" ~\/)
@schar      = ~\"

tokens :-

<0> $white+           { skip }
<0> "//" .*           { accept $ Comment . BS.drop 2 }
<0> "/*"              { accept (const CommentStart) `andBegin` c }
<c> @cchar+           { accept Comment }
<c> @lineterm         { skip }
<c> "*/"              { accept (const CommentEnd) `andBegin` 0 }

<0> "("               { accept $ const ParenOpen }
<0> ")"               { accept $ const ParenClose }

<0> "["               { accept $ const BracketOpen }
<0> "]"               { accept $ const BracketClose }

<0> "{"               { accept $ const BraceOpen }
<0> "}"               { accept $ const BraceClose }

<0> ","               { accept $ const Comma }
<0> ";"               { accept $ const Semicolon }
<0> "="               { accept $ const Eq }
<0> "."               { accept $ const Dot }

<0> "+"               { accept $ const Plus }
<0> "-"               { accept $ const Minus }
<0> "*"               { accept $ const Mul }
<0> "/"               { accept $ const Div }
<0> "&"               { accept $ const And }
<0> "|"               { accept $ const Or }
<0> "~"               { accept $ const Tilde }
<0> "<"               { accept $ const Lt }
<0> ">"               { accept $ const Gt }

<0> "class"           { accept $ const Class }
<0> "field"           { accept $ const Field }
<0> "static"          { accept $ const Static }
<0> "constructor"     { accept $ const Constructor }
<0> "method"          { accept $ const Method }
<0> "function"        { accept $ const Function }

<0> "var"             { accept $ const Var }
<0> "let"             { accept $ const Let }
<0> "do"              { accept $ const Do }
<0> "if"              { accept $ const If }
<0> "then"            { accept $ const Then }
<0> "else"            { accept $ const Else }
<0> "while"           { accept $ const While }
<0> "return"          { accept $ const Return }

<0> "true"            { accept $ const $ BoolLit True }
<0> "false"           { accept $ const $ BoolLit False }
<0> "null"            { accept $ const Null }
<0> "this"            { accept $ const This }

<0> "int"             { accept $ const Int }
<0> "boolean"         { accept $ const Boolean }
<0> "char"            { accept $ const Char }
<0> "void"            { accept $ const Void }

<0> \"\"              { accept $ const $ StringLit $ BSC.pack "" }
<0> \"                { begin s }
<s> @schar+           { accept $ StringLit }
<s> \"                { begin 0 }

<0> @ident            { accept $ Identifier . BSC.unpack }
<0> $digit+           { accept $ IntLit . read . BSC.unpack }

{
alexEOF = return Nothing
    
accept f ((AlexPn offset line col), _, str) len = return $ Just $ L loc tok
  where
    loc = newPos "" line col
    tok = f $ BS.take (fromIntegral len) str
  
lexer str = runAlex str $ do          
  fix $ \loop -> do
    tok <- alexMonadScan
    case tok of
      Nothing -> return []
      Just t -> do ts <- loop
                   ts `seq` return $ t:ts
}

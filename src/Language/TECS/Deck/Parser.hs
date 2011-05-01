module Language.TECS.Deck.Parser (deck, eof) where

import Data.ByteString.Lazy (ByteString)
import Language.TECS.Deck.Parser.Tokens (Token, L(..))
import qualified Language.TECS.Deck.Parser.Tokens as T
import Language.TECS.Deck.Syntax
import Text.Parsec.Pos

import Text.Parsec hiding (label, anyToken, eof)
import Control.Applicative ((*>), (<*>), (<*), (<$>), (<$))
import Control.Monad

import Language.TECS.Deck.Parser.Lexer
import Data.ByteString.Lazy.Char8 (pack)

type DeckParser a = Parsec [L Token] () a

tok :: (L Token -> Maybe a) -> DeckParser a
tok f = token (show . unLoc) getLoc f

anyToken = tok (Just . unLoc)
eof = notFollowedBy anyToken <?> "end of input"

identifier :: DeckParser String
identifier = tok $ \ (L loc token) ->
  case token of
    T.Identifier x -> Just x
    _ -> Nothing
    
value :: DeckParser Value    
value = tok $ \ (L loc token) ->
  case token of
    T.Value n -> Just n
    _ -> Nothing

keyword :: Token -> DeckParser ()
keyword kw = lexeme $ tok $ \ (L loc token) -> do
  guard $ token == kw
  return ()

commentContent :: DeckParser ByteString
commentContent = tok $ \ (L loc token) -> do
  case token of 
    T.Comment s -> Just s
    _ -> Nothing

multilineComment :: DeckParser [ByteString]
multilineComment = keyword T.CommentStart *> many commentContent <* keyword T.CommentEnd

comment :: DeckParser [ByteString]
comment = ((:[]) <$> commentContent) <|> multilineComment <?> "comment"

lexeme :: DeckParser a -> DeckParser a
lexeme p = p <* skipMany comment

functionDef :: DeckParser FunctionDef
functionDef = 
  (keyword T.Function >> FunctionDef <$> function <*> arity <*> many directive)
  <?> "function definition"

directive :: DeckParser Directive
directive = 
  (keyword T.Label >> Label <$> label)
  <|> (Stmt <$> stmt)
  <?> "directive"  

stmt :: DeckParser Stmt
stmt = 
  (keyword T.Add >> return Add) 
  <|> (keyword T.Sub >> return Sub) 
  <|> (keyword T.Neg >> return Neg) 
  <|> (keyword T.Eq >> return Eq) 
  <|> (keyword T.Gt >> return Gt) 
  <|> (keyword T.Lt >> return Lt) 
  <|> (keyword T.And >> return And) 
  <|> (keyword T.Or >> return Or) 
  <|> (keyword T.Not >> return Not) 
  <|> (keyword T.Push >> Push <$> addr)
  <|> (keyword T.Pop >> Pop <$> addr)
  <|> (keyword T.Goto >> Goto <$> label)
  <|> (keyword T.IfGoto >> IfGoto <$> label)
  <|> (keyword T.Call >> Call <$> function <*> arity)
  <|> (keyword T.Return >> return Return)
  <?> "statement"  
  
label = identifier <?> "label"
function = identifier <?> "function"
arity = value <?> "arity"
  
addr :: DeckParser Addr  
addr =     
  (keyword T.Argument >> Arg <$> offset)
  <|> (keyword T.Local >> Local <$> offset)
  <|> (keyword T.Static >> Static <$> offset)
  <|> (keyword T.Constant >> Constant <$> value)
  <|> (keyword T.Pointer >> toRef <$> offset)
  <|> (keyword T.This >> This <$> offset)
  <|> (keyword T.That >> That <$> offset)
  <|> (keyword T.Temp >> Temp <$> offset)
  <?> "address"
  where toRef 0 = RefThis
        toRef 1 = RefThat        
  
offset :: DeckParser Offset  
offset = value <?> "offset"

deck :: DeckParser [FunctionDef]
deck = skipMany comment *> many functionDef

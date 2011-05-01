module Language.TECS.Jack.Parser where
-- module Language.TECS.Jack.Parser (jack, eof) where

import Data.ByteString.Lazy (ByteString, unpack)
import Language.TECS.Located
import Language.TECS.Jack.Parser.Tokens (Token)
import qualified Language.TECS.Jack.Parser.Tokens as T
import Language.TECS.Jack.Syntax
import Text.Parsec.Pos

import Text.Parsec hiding (label, anyToken, eof)
import Text.Parsec.Expr
import Control.Applicative ((*>), (<*>), (<*), (<$>), (<$))
import Control.Monad

type JackParser a = Parsec [L Token] () a

tok :: Show t => (t -> Maybe a) -> Parsec [L t] () a
tok f = token (show . unLoc) getLoc (f . unLoc)

anyToken :: JackParser Token
anyToken = tok Just

eof :: JackParser ()
eof = notFollowedBy anyToken <?> "end of input"

keyword :: Token -> JackParser ()
keyword kw = lexeme $ tok $ \ token -> do
  guard $ token == kw
  return ()

commentContent :: JackParser ByteString
commentContent = tok $ \ token -> do
  case token of 
    T.Comment s -> Just s
    _ -> Nothing

multilineComment :: JackParser [ByteString]
multilineComment = keyword T.CommentStart *> many commentContent <* keyword T.CommentEnd

comment :: JackParser [ByteString]
comment = ((:[]) <$> commentContent) <|> multilineComment <?> "comment"

lexeme :: JackParser a -> JackParser a
lexeme p = p <* skipMany comment

identifier :: JackParser String
identifier = lexeme $ tok $ \ token ->
  case token of
    T.Identifier x -> Just x
    _ -> Nothing  
    
name :: JackParser Name    
name = MkName <$> identifier

braces :: JackParser a -> JackParser a
braces = between (keyword T.BraceOpen) (keyword T.BraceClose)

parens :: JackParser a -> JackParser a
parens = between (keyword T.ParenOpen) (keyword T.ParenClose)

brackets :: JackParser a -> JackParser a
brackets = between (keyword T.BracketOpen) (keyword T.BracketClose)


comma = keyword T.Comma
semi = keyword T.Semicolon
dot = keyword T.Dot

klass :: JackParser (Class Name)
klass = do
  keyword T.Class
  cls <- identifier
  (fields, methods) <- braces $ (,) <$> many fieldDef <*> many methodDef
  return $ Class cls fields methods
  
fieldDef :: JackParser (FieldDef Name)  
fieldDef = field <*> ty <*> name `sepBy` comma <* semi
  where field = (keyword T.Field >> return Field)
                <|> (keyword T.Static >> return Static)

methodDef :: JackParser (MethodDef Name)
methodDef = method <*> identifier <*> parens formals <*> braces body
  where method = (keyword T.Constructor >> Constructor <$> ty)
                 <|> (keyword T.Function >> Function <$> returnTy)
                 <|> (keyword T.Method >> Method <$> returnTy)        
        formals = varDef `sepBy` comma

varDef :: JackParser (VarDef Name)
varDef = VarDef <$> ty <*> name

body :: JackParser (Body Name)
body = Body <$> many varDecl <*> many stmt

varDecl :: JackParser (VarDecl Name)
varDecl = keyword T.Var *> (VarDecl <$> ty <*> name `sepBy1` comma) <* semi

stmt :: JackParser (Stmt Name)
stmt = (keyword T.Let *> (Let <$> lval <*> (keyword T.Eq *> expr)) <* semi)
       <|> (keyword T.If >> If <$> parens expr <*> braces body <*> optionMaybe (keyword T.Else >> braces body))
       <|> (keyword T.While >> While <$> parens expr <*> braces body)
       <|> (keyword T.Do >> (Do <$> call) <* semi)
       <|> (keyword T.Return >> (Return <$> optionMaybe expr) <* semi)
       <?> "statement"

lval :: JackParser (LValue Name)
lval = (Var <$> name) 
       <|> (VarIndex <$> name <*> brackets expr)
       <?> "lvalue"
       
expr :: JackParser (Expr Name)
expr = buildExpressionParser table term <?> "expression"
    where 
      table = [ [binary T.Mul (:*:), binary T.Div (:/:)]
              , [binary T.Plus (:+:), binary T.Minus (:-:)]
              , [binary T.Eq (:=:), binary T.Lt (:<:), binary T.Gt (:>:)]
              , [prefix T.Minus Neg]                
              , [prefix T.Tilde Not]
              , [binary T.Or (:|:)]
              , [binary T.And (:&:)]
              ]  
          where binary token op = Infix (keyword token >> return op) AssocLeft
                prefix token op = Prefix (keyword token >> return op)
                                  
      term = parens expr <|> this <|> null <|> intLit <|> boolLit <|> stringLit <|> try (Sub <$> call) <|> (Ref <$> lval)
      
      this = keyword T.This >> return This
      null = keyword T.Null >> return Null
      
      intLit = lexeme $ tok $ \token -> 
        case token of
          T.IntLit n -> Just $ IntLit n
          _ -> Nothing
          
      boolLit = lexeme $ tok $ \token -> 
        case token of
          T.BoolLit b -> Just $ BoolLit b
          _ -> Nothing
          
      stringLit =  lexeme $ tok $ \token -> 
        case token of
          T.StringLit s -> Just $ StringLit s
          _ -> Nothing

call :: JackParser (Call Name)
call = fun <*> parens (expr `sepBy` comma)
  where fun = try (MemberCall <$> identifier <*> (dot *> identifier)) 
              <|> (FunCall <$> identifier)

ty :: JackParser Type
ty = (keyword T.Int >> return TyInt)
     <|> (keyword T.Boolean >> return TyBool)
     <|> (keyword T.Char >> return TyChar)
     <|> (TyClass <$> identifier)
     <?> "type specifier"
  
returnTy :: JackParser (Maybe Type)
returnTy = (keyword T.Void >> return Nothing) <|> (Just <$> ty) <?> "return type specifier"
     
jack :: JackParser (Class Name)
jack = skipMany comment >> klass

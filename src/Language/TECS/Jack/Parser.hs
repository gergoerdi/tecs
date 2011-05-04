module Language.TECS.Jack.Parser (parseJack) where

import Data.ByteString.Lazy (ByteString)
import Language.TECS.Located
import Language.TECS.Jack.Parser.Tokens (Token)
import Language.TECS.Jack.Parser.Lexer (lexer)
import qualified Language.TECS.Jack.Parser.Tokens as T
import Language.TECS.Jack.Syntax
import qualified Data.ByteString.Lazy as BS

import Text.Parsec hiding (label, anyToken, eof)
import Text.Parsec.Expr
import Control.Applicative ((*>), (<*>), (<*), (<$>))
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

type JackParser a = Parsec [L Token] (Set Name) a

tok :: (Show t) => (t -> Maybe a) -> Parsec [L t] u a
tok f = token (show . unLoc) getLoc (f . unLoc)

anyToken :: JackParser Token
anyToken = tok Just

eof :: JackParser ()
eof = notFollowedBy anyToken <?> "end of input"

keyword :: Token -> JackParser ()
keyword kw = lexeme $ tok $ \ token -> 
  do
    guard $ token == kw
    return ()

commentContent :: JackParser ByteString
commentContent = tok $ \ token -> 
  do
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
klass = 
  do
    keyword T.Class
    cls <- identifier
    (fields, methods) <- braces $ scope $ (,) <$> many fieldDef <*> many methodDef
    return $ Class cls fields methods
  
fieldDef :: JackParser (FieldDef Name)  
fieldDef = field <*> ty <*> varNames <* semi
  where field = (keyword T.Field >> return Field)
                <|> (keyword T.Static >> return Static)

methodDef :: JackParser (MethodDef Name)
methodDef = scope $ method <*> identifier <*> parens formals <*> braces body
  where 
    method = (keyword T.Constructor >> Constructor <$> ty)
             <|> (keyword T.Function >> Function <$> returnTy)
             <|> (keyword T.Method >> Method <$> returnTy)        
    formals = varDef `sepBy` comma

varDef :: JackParser (VarDef Name)
varDef = VarDef <$> ty <*> varName
        
body :: JackParser (Body Name)
body = scope $ Body <$> many varDecl <*> many stmt

varDecl :: JackParser (VarDecl Name)
varDecl = keyword T.Var *> (VarDecl <$> ty <*> varNames) <* semi

varName = 
  do
    var <- name
    addVar var
    return var

varNames = varName `sepBy1` comma
    
addVar = modifyState . Set.insert
scope f =
  do
    vars <- getState
    f <* setState vars

stmt :: JackParser (Stmt Name)
stmt = (keyword T.Let *> (Let <$> lval <*> (keyword T.Eq *> expr)) <* semi)
       <|> (keyword T.If >> If <$> parens expr <*> braces body <*> optionMaybe (keyword T.Else >> braces body))
       <|> (keyword T.While >> While <$> parens expr <*> braces body)
       <|> (keyword T.Do >> (Do <$> call) <* semi)
       <|> (keyword T.Return >> (Return <$> optionMaybe expr) <* semi)
       <?> "statement"

lval :: JackParser (LValue Name)
lval = try (VarIndex <$> name <*> brackets expr)
       <|> (Var <$> name)       
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
call = callType <*> identifier <*> parens (expr `sepBy` comma)
  where 
    callType = 
      do
        qualifier <- optionMaybe (try $ identifier <* dot)
        case qualifier of
          Nothing -> return $ MemberCall Nothing
          Just qualifier -> 
            do
              vars <- getState
              let var = MkName qualifier
              return $ if Set.member var vars 
                         then MemberCall (Just var)
                         else StaticCall qualifier

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

parseJack :: BS.ByteString -> FilePath -> Either String (Class Name)
parseJack s filename = 
  case lexer s of
    Left err -> Left err
    Right tokens -> 
      case parse tokens of 
        Left err -> Left $ show err
        Right jack -> Right jack
    
  where parse = runParser (jack <* eof) Set.empty filename

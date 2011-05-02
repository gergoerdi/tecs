module Language.TECS.Jack.ToDeck.Compile where

import Language.TECS.Jack.Syntax as Jack
import qualified Language.TECS.Deck.Syntax as Deck
import Language.TECS.Jack.ToDeck.Layout

import Control.Monad.RWS
import Control.Applicative
import Data.Sequence (Seq, singleton)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow (first, second, (***))
import qualified Data.ByteString.Lazy as BS

compile :: Jack.Class Id -> Deck.Deck 
compile (Class cls _ methods) = Deck.Deck [] funs
  where 
    funs = map compileMethod methods
    
    compileMethod (Constructor _ name _ body) = compileMember name body
    compileMethod (Function _ name _ body) = compileMember name body
    compileMethod (Method _ name _ body) = compileMember name body
    
    compileMember name body = Deck.FunctionDef name (fromIntegral localCount) (toList code)
      where 
        (code, localCount) = (id *** getSum) $ snd $ evalRWS (compileBody body) (JackPos cls name) ()
          
data JackPos = JackPos { jackClass, jackMethod :: Label }
type Compiler a = RWS JackPos (Seq Deck.Directive, Sum Int) () a

emitStmt :: Deck.Stmt -> Compiler ()
emitStmt = emitDir . Deck.Stmt 

emitDir :: Deck.Directive -> Compiler ()
emitDir d = tell $ (singleton d, mempty)

emitLocalCount :: Int -> Compiler ()
emitLocalCount c = tell $ (mempty, Sum c)

compileBody :: Jack.Body Id -> Compiler ()
compileBody (Body locals ss) = 
  do
    let localCount = sum $ map count locals      
    (_, counts) <- unzip <$> map snd <$> (censor (id *** const mempty) $ mapM (listen . compileStmt) ss)
    emitLocalCount $ localCount + maximum (0:(map getSum counts))
  where 
    count (VarDecl _ vs) = length vs

compileStmt :: Jack.Stmt Id -> Compiler ()
compileStmt (Let (Var v) e) = 
  do
    compileExpr e
    emitStmt $ Deck.Pop $ addressOf v
compileStmt (Do call) =     
  do  
    compileCall call
    emitStmt $ Deck.Pop $ Deck.Temp 0
compileStmt _ = return ()
compileStmt s = error (show s)

compileExpr :: Jack.Expr Id -> Compiler ()
compileExpr (IntLit n) = 
  emitStmt $ Deck.Push $ Deck.Constant n
compileExpr (BoolLit b) = 
  do  
    emitStmt $ Deck.Push $ Deck.Constant value
  where value | b = -1         
              | otherwise = 0
compileExpr (StringLit s) =    
  do
    emitStmt $ Deck.Push $ Deck.Constant (fromIntegral $ BS.length s)
    emitStmt $ Deck.Call "String.new" 1
    forM_ (BS.unpack s) $ \c -> do
      emitStmt $ Deck.Push $ Deck.Constant (fromIntegral c)
      emitStmt $ Deck.Call "String.appendChar" 2  
compileExpr (Ref (Var v)) = 
  do
    emitStmt $ Deck.Push $ addressOf v
compileExpr (x :+: y) = 
  do
    compileExpr x
    compileExpr y
    emitStmt Deck.Add
compileExpr (x :-: y) = 
  do
    compileExpr x
    compileExpr y
    emitStmt Deck.Sub  
compileExpr (Sub call) = 
  compileCall call
compileExpr e = return () -- error (show e)

compileCall call = 
  do
    mapM_ compileExpr actuals
    emitStmt $ Deck.Call fun (fromIntegral $ length actuals)
  where 
    (fun, actuals) = case call of
      MemberCall cls member actuals -> (cls ++ "." ++ member, actuals)
      FunCall fun actuals -> (fun, actuals)

addressOf (IdArg n) = Deck.Arg (fromIntegral n)
addressOf (IdLocal n) = Deck.Local (fromIntegral n)
addressOf (IdStatic n) = Deck.Static (fromIntegral n)
addressOf (IdThis n) = Deck.This (fromIntegral n)

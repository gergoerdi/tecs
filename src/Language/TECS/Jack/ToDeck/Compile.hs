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
import Data.Maybe (fromMaybe)

compile :: Jack.Class Id -> Deck.Deck 
compile (Class cls _ methods) = Deck.Deck [] funs
  where 
    funs = map compileMethod methods
    
    compileMethod (Constructor _ name _ body) = compileMember name body
    compileMethod (Function _ name _ body) = compileMember name body
    compileMethod (Method _ name _ body) = compileMember name body
    
    compileMember name body = Deck.FunctionDef (cls ++ "." ++ name) (fromIntegral localCount) (toList code)
      where 
        (code, localCount) = (id *** getSum) $ snd $ evalRWS (compileBody body) (JackPos cls name) [0..]
          
data JackPos = JackPos { jackClass, jackMethod :: Label }
type Compiler a = RWS JackPos (Seq Deck.Directive, Sum Int) [Int] a

emitStmt :: Deck.Stmt -> Compiler ()
emitStmt = emitDir . Deck.Stmt 

emitDir :: Deck.Directive -> Compiler ()
emitDir d = tell $ (singleton d, mempty)

emitLocalCount :: Int -> Compiler ()
emitLocalCount c = tell $ (mempty, Sum c)

emitLabel :: Label -> Compiler ()
emitLabel = emitDir . Deck.Label

freshLoopLabels :: Compiler (Label, Label)
freshLoopLabels = 
  do
    (i:is) <- get
    put is
    return ("WHILE_EXP" ++ show i, "WHILE_END" ++ show i)
    
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
compileStmt (Let (VarIndex v idx) e) =    
  do
    compileIndex v idx
    compileExpr e
    -- Pop the second item from the stack into refThat
    emitStmt $ Deck.Pop $ Deck.Temp 0
    emitStmt $ Deck.Pop Deck.RefThat
    emitStmt $ Deck.Push $ Deck.Temp 0    
    -- Pop the top of the stack (the value of e) into that[0]
    emitStmt $ Deck.Pop $ Deck.That 0    
compileStmt (Do call) =     
  do  
    compileCall call
    emitStmt $ Deck.Pop $ Deck.Temp 0
compileStmt (While cond body) =    
  do
    (start, end) <- freshLoopLabels
    emitLabel start
    compileExpr cond
    emitStmt $ Deck.Not
    emitStmt $ Deck.IfGoto end
    compileBody body
    emitStmt $ Deck.Goto start
    emitLabel end    
compileStmt (Return mx) =  
  do
    compileExpr (IntLit 0 `fromMaybe` mx)
    emitStmt Deck.Return    
-- compileStmt _ = return ()
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
  emitStmt $ Deck.Push $ addressOf v
compileExpr (Ref (VarIndex v idx)) = 
  do
    compileIndex v idx
    emitStmt $ Deck.Pop Deck.RefThat
    emitStmt $ Deck.Push $ Deck.That 0
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
compileExpr (x :/: y) = 
  compileExpr (Sub $ MemberCall "Math" "divide" [x, y])
compileExpr (x :<: y) = 
  do
    compileExpr x
    compileExpr y
    emitStmt Deck.Lt
compileExpr (Sub call) = 
  compileCall call  
-- compileExpr e = return ()
compileExpr e = error (show e)

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

compileIndex v idx = 
  do    
    compileExpr idx
    emitStmt $ Deck.Push $ addressOf v
    emitStmt $ Deck.Add

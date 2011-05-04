module Language.TECS.Jack.ToDeck.Compile (compile) where

import Language.TECS.Jack.Syntax as Jack
import qualified Language.TECS.Deck.Syntax as Deck
import Language.TECS.Jack.ToDeck.Layout

import Control.Monad.RWS
import Control.Applicative
import Data.Sequence (Seq, singleton)
import Data.Foldable (toList)
import Control.Arrow ((***))
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)

compile :: Jack.Class Id -> Deck.Deck 
compile (Class cls fields methods) = Deck.Deck [] funs
  where 
    size = sum $ map sizeOf fields    
    funs = map compileMethod methods
    sizeOf (Static _ _) = 0
    sizeOf (Field _ vs) = length vs
    
    compileMethod (Constructor _ name _ body) = compileMember alloc name body
    compileMethod (Function _ name _ body) = compileMember (return ()) name body
    compileMethod (Method _ name _ body) = compileMember popThis name body
    
    alloc = 
      do
        emitStmt $ Deck.Push $ Deck.Constant (fromIntegral size)
        emitStmt $ Deck.Call "Memory.alloc" 1
        emitStmt $ Deck.Pop $ Deck.RefThis
    
    popThis =
      do
        emitStmt $ Deck.Push $ Deck.Arg 0
        emitStmt $ Deck.Pop $ Deck.RefThis
    
    compileMember intro name body = Deck.FunctionDef (cls ++ "." ++ name) (fromIntegral localCount) (toList code)
      where 
        (code, localCount) = (id *** getSum) $ snd $ evalRWS (intro >> compileBody body) (JackPos cls name) [0..]
          
data JackPos = JackPos { jack_class, jack_method :: Label }
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
    
freshIfLabels :: Compiler (Label, Label)
freshIfLabels = 
  do
    (i:is) <- get
    put is
    return ("IF_TRUE" ++ show i, "IF_END" ++ show i)
    
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
compileStmt (If cond thn els) =    
  do
    (trueBranch, end) <- freshIfLabels
    compileExpr cond
    emitStmt $ Deck.IfGoto trueBranch
    (return () `maybe` compileBody) els
    emitStmt $ Deck.Goto end
    emitLabel trueBranch
    compileBody thn
    emitLabel end      
compileStmt (Return mx) =  
  do
    compileExpr (IntLit 0 `fromMaybe` mx)
    emitStmt Deck.Return    

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
compileExpr (x :+: y) = binary Deck.Add x y
compileExpr (x :-: y) = binary Deck.Sub x y
compileExpr (x :/: y) = compileExpr (Sub $ StaticCall "Math" "divide" [x, y])
compileExpr (x :*: y) = compileExpr (Sub $ StaticCall "Math" "multiply" [x, y])
compileExpr (x :=: y) = binary Deck.Eq x y 
compileExpr (x :<: y) = binary Deck.Lt x y
compileExpr (x :>: y) = binary Deck.Gt x y
compileExpr (x :&: y) = binary Deck.And x y
compileExpr (x :|: y) = binary Deck.Or x y
compileExpr (Not x) = 
  do
    compileExpr x
    emitStmt $ Deck.Not
compileExpr (Neg x) = 
  do
    compileExpr x
    emitStmt $ Deck.Neg
compileExpr (Sub call) = 
  compileCall call  
compileExpr Null = 
  compileExpr $ IntLit 0  
compileExpr This =  
  emitStmt $ Deck.Push $ Deck.RefThis

compileCall (StaticCall cls name actuals) = 
  do
    mapM_ compileExpr actuals
    emitStmt $ Deck.Call (cls ++ "." ++ name) (fromIntegral $ length actuals)
compileCall (MemberCall inst name actuals) = 
  do    
    emitStmt $ Deck.Push $ (Deck.RefThis `maybe` addressOf) inst    
    mapM_ compileExpr actuals
    cls <- case fmap id_type inst of
          Nothing -> asks jack_class
          Just (TyClass cls) -> return cls
    -- add 1 to number of arguments to account for 'this'
    emitStmt $ Deck.Call (cls ++ "." ++ name) (succ $ fromIntegral $ length actuals) 

addressOf :: Id -> Deck.Addr
addressOf var = 
  case id_alloc var of  
    AllocArg n -> Deck.Arg (fromIntegral n)
    AllocLocal n -> Deck.Local (fromIntegral n)
    AllocStatic n -> Deck.Static (fromIntegral n)
    AllocField n -> Deck.This (fromIntegral n)

binary op x y =
  do
    compileExpr x
    compileExpr y
    emitStmt op

compileIndex v idx = 
  do    
    compileExpr idx
    emitStmt $ Deck.Push $ addressOf v
    emitStmt $ Deck.Add

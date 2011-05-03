module Language.TECS.Jack.ToDeck.Layout (layout, Id(..), Alloc(..)) where

import Language.TECS.Jack.Syntax
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow (first, second, (***))
import qualified Data.Traversable as Trav
import Data.Maybe
import Text.PrettyPrint.HughesPJClass
import Text.PrettyPrint.HughesPJ
import Data.Function (on)

data SymbolTable k v = SymbolTable (Map k v) (Maybe (SymbolTable k v))
                     deriving Show
                              
mkSymbolTable elems = SymbolTable (Map.fromList elems) Nothing

descend elems = SymbolTable (Map.fromList elems) . Just

lookupSym key (SymbolTable local parent) = msum [Map.lookup key local, lookupSym key =<< parent]

data Alloc = AllocArg Int
           | AllocLocal Int
           | AllocStatic Int
           | AllocField Int
           deriving (Eq, Ord, Show)

data Id = Id { id_alloc :: Alloc, id_type :: Type }
        deriving Show
                 
instance Eq Id where                 
  (==) = (==) `on` id_alloc
  
instance Ord Id where  
  compare = compare `on` id_alloc

instance Pretty Alloc where
  pPrint v = braces $ case v of    
    AllocArg n -> text "arg" <+> text (show n)
    AllocLocal n -> text "local" <+> text (show n)
    AllocStatic n -> text "static" <+> text (show n)
    AllocField n -> text "this" <+> text (show n)

instance Pretty Id where
  pPrint (Id alloc ty) = parens (pPrint ty) <> pPrint alloc

layout :: Class Name -> Class Id
layout (Class name fields methods) = Class name fields' $ runReader (mapM layoutMethod methods) $ mkSymbolTable (concat mappings)
  where 
    (fields', mappings) = unzip $ evalState (mapM collectField fields) (0, 0)    

collectField :: FieldDef Name -> State (Int, Int) (FieldDef Id, [(Name, Id)])
collectField (Field ty vs) = do
  mapping <- forM vs $ \v -> do    
    i <- gets fst
    modify (first succ)
    return (v, Id (AllocField i) ty)
  return (Field ty $ map snd mapping, mapping)
collectField (Static ty vs) = do
  mapping <- forM vs $ \v -> do    
    i <- gets snd
    modify (second succ)
    return (v, Id (AllocStatic i) ty)
  return (Static ty $ map snd mapping, mapping)

layoutMethod :: MethodDef Name -> Reader (SymbolTable Name Id) (MethodDef Id)
layoutMethod (Constructor ty name formals body) = layoutParams (Constructor ty name) formals body
layoutMethod (Function ty name formals body) = layoutParams (Function ty name) formals body
layoutMethod (Method ty name formals body) = layoutParams (Method ty name) formals body

layoutParams f formals body = f formals' <$> local (descend mapping) doBody
  where 
    (formals', mapping) = unzip $ zipWith arg [0..] formals
    arg i (VarDef ty v) = (VarDef ty id, (v, id))
      where 
        id = Id (AllocArg i) ty         
    doBody = do
      r <- ask
      return $ runReader (layoutBody body) (r, 0)
    
collectLocal :: VarDecl Name -> State Int (VarDecl Id, [(Name, Id)])
collectLocal (VarDecl ty vs) = do
  mapping <- forM vs $ \v -> do
    i <- get
    modify succ
    return (v, Id (AllocLocal i) ty)
  return (VarDecl ty $ map snd mapping, mapping)

layoutBody :: Body Name -> Reader (SymbolTable Name Id, Int) (Body Id)
layoutBody (Body locals stmts) = do  
  localCount <- asks snd
  let (locals', mappings) = unzip $ evalState (mapM collectLocal locals) localCount
  Body locals' <$> local (descend (concat mappings) *** (+ (length mappings))) (mapM layoutStmt stmts)
  
layoutStmt :: Stmt Name -> Reader (SymbolTable Name Id, Int) (Stmt Id)  
layoutStmt (Let lval e) = Let <$> layoutLval lval <*> layoutExpr e
layoutStmt (If cond thn els) = If <$> layoutExpr cond <*> layoutBody thn <*> Trav.mapM layoutBody els
layoutStmt (While cond body) = While <$> layoutExpr cond <*> layoutBody body
layoutStmt (Do call) = Do <$> Trav.mapM layoutVar call
layoutStmt (Return e) = Return <$> Trav.mapM layoutExpr e

layoutLval = Trav.mapM layoutVar
layoutExpr = Trav.mapM layoutVar

layoutVar v = fromJust <$> asks (lookupSym v . fst)

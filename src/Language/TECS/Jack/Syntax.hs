{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module Language.TECS.Jack.Syntax (
  Name(..), Label,
  Class(..), FieldDef(..), MethodDef(..), 
  Body(..), VarDecl(..), VarDef(..), Stmt(..),
  Expr(..), LValue(..), Type(..), Call(..)
  ) where

import Text.PrettyPrint.HughesPJClass
import Text.PrettyPrint.HughesPJ
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Foldable
import Data.Traversable

newtype Name = MkName { getName :: String }
             deriving (Eq, Ord, Show)

instance Pretty Name where
  pPrint = text . getName

type Label = String

block header body = header $+$ lbrace $+$ nest 4 body $+$ rbrace

data Class name = Class Label [FieldDef name] [MethodDef name]
                deriving Show
                         
instance Pretty name => Pretty (Class name) where                         
  pPrint (Class cls fields methods) = block (text "class" <+> text cls) content
    where content = vcat (map pPrint fields) $$ vcat (map pPrint methods)
                         
data FieldDef name = Field Type [name]
                   | Static Type [name]
                   deriving Show
                            
instance Pretty name => Pretty (FieldDef name) where                            
  pPrint (Field ty vs) = text "field" <+> pPrint ty <+> cat (punctuate comma $ map pPrint vs) <> semi
  pPrint (Static ty vs) = text "static" <+> pPrint ty <+> cat (punctuate comma $ map pPrint vs) <> semi

data MethodDef name = Constructor Type Label [VarDef name] (Body name)
                    | Function (Maybe Type) Label [VarDef name] (Body name)
                    | Method (Maybe Type) Label [VarDef name] (Body name)
                    deriving Show
                             
instance Pretty name => Pretty (MethodDef name) where                             
  pPrint (Constructor ty name formals body) = 
    block 
      (text "constructor" <+> pPrint ty <+> text name <+> parens (vcat $ punctuate comma $ map pPrint formals)) 
      (pPrint body)
  pPrint (Function mty name formals body) = 
    block
      (text "function" <+> (text "void" `maybe` pPrint) mty <+> text name <+> parens (vcat $ punctuate comma $ map pPrint formals))
      (pPrint body)
  pPrint (Method mty name formals body) = 
    block
      (text "method" <+> (text "void" `maybe` pPrint) mty <+> text name <+> parens (vcat $ punctuate comma $ map pPrint formals))
      (pPrint body)
                             
data VarDecl name = VarDecl Type [name]      
                  deriving Show
                           
instance Pretty name => Pretty (VarDecl name) where                          
  pPrint (VarDecl ty names) = pPrint ty <+> (vcat $ punctuate comma $ map pPrint names)
      
data VarDef name = VarDef Type name                             
                 deriving Show
                          
instance Pretty name => Pretty (VarDef name) where                          
  pPrint (VarDef ty name) = pPrint ty <+> pPrint name

data Body name = Body [VarDecl name] [Stmt name]
               deriving Show
                        
instance Pretty name => Pretty (Body name) where                        
  pPrint (Body vars stmts) = vcat (map (\var -> text "var" <+> pPrint var <> semi) vars) $+$ vcat (map pPrint stmts)

data Stmt name = Let (LValue name) (Expr name)
               | If (Expr name) (Body name) (Maybe (Body name))
               | While (Expr name) (Body name)
               | Do (Call name)
               | Return (Maybe (Expr name))
               deriving Show
                        
instance Pretty name => Pretty (Stmt name) where                        
  pPrint (Let lv e) = text "let" <+> pPrint lv <+> equals <+> pPrint e <> semi
  pPrint (If cond thn els) = (block (text "if" <+> parens (pPrint cond)) (pPrint thn)) $$ 
                             (empty `maybe` (block (text "else") . pPrint)) els
  pPrint (While cond body) = block (text "while" <+> parens (pPrint cond)) $ pPrint body
  pPrint (Do call) = text "do" <+> pPrint call <> semi
  pPrint (Return me) = text "return" <+> (empty `maybe` pPrint) me <> semi

data Expr name = IntLit Word16
               | StringLit ByteString
               | BoolLit Bool
               | Null
               | This
               | Ref (LValue name)
               | Sub (Call name)
               | Expr name :+: Expr name
               | Expr name :-: Expr name
               | Expr name :*: Expr name
               | Expr name :/: Expr name
               | Expr name :&: Expr name
               | Expr name :|: Expr name
               | Expr name :<: Expr name
               | Expr name :>: Expr name
               | Expr name :=: Expr name
               | Neg (Expr name)
               | Not (Expr name)
               deriving (Show, Functor, Foldable, Traversable)

instance Pretty name => Pretty (Expr name) where
  pPrint (IntLit n) = text $ show n
  pPrint (StringLit s) = doubleQuotes $ text $ BSC.unpack s
  pPrint (BoolLit True) = text "true"
  pPrint (BoolLit False) = text "false"
  pPrint Null = text "null"
  pPrint This = text "this"
  pPrint (Ref lval) = pPrint lval
  pPrint (Sub call) = pPrint call
  pPrint (e :+: f) = pPrint e <+> char '+' <+> pPrint f
  pPrint (e :-: f) = pPrint e <+> char '-' <+> pPrint f
  pPrint (e :*: f) = pPrint e <+> char '*' <+> pPrint f
  pPrint (e :/: f) = pPrint e <+> char '/' <+> pPrint f
  pPrint (e :&: f) = pPrint e <+> char '&' <+> pPrint f
  pPrint (e :|: f) = pPrint e <+> char '|' <+> pPrint f
  pPrint (e :<: f) = pPrint e <+> char '<' <+> pPrint f
  pPrint (e :>: f) = pPrint e <+> char '>' <+> pPrint f
  pPrint (e :=: f) = pPrint e <+> char '=' <+> pPrint f
  pPrint (Neg e) = text "-" <> pPrint e
  pPrint (Not e) = text "~" <> pPrint e
  
data LValue name = Var name  
                 | VarIndex name (Expr name)
                 deriving (Show, Functor, Foldable, Traversable)
                          
data Call name = StaticCall Label Label [Expr name]
               | MemberCall (Maybe name) Label [Expr name]
               deriving (Show, Functor, Foldable, Traversable)
                        
instance Pretty name => Pretty (Call name) where                        
  pPrint (StaticCall cls f es) = text cls <> char '.' <> text f <+> parens (hcat $ punctuate comma $ map pPrint es)
  pPrint (MemberCall v f es) = qualifier <> char '.' <> text f <+> parens (hcat $ punctuate comma $ map pPrint es)
    where 
      qualifier = case v of
        Nothing -> empty
        Just v -> pPrint v <> char '.'
                     
instance Pretty name => Pretty (LValue name) where                     
  pPrint (Var v) = pPrint v
  pPrint (VarIndex v e) = pPrint v $$ brackets (pPrint e)

data Type = TyInt 
          | TyChar 
          | TyBool 
          | TyClass Label
          deriving Show

instance Pretty Type where            
  pPrint TyInt = text "int"
  pPrint TyChar = text "char"
  pPrint TyBool = text "boolean"
  pPrint (TyClass cls) = text cls

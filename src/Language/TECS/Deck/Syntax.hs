module Language.TECS.Deck.Syntax where

import Text.PrettyPrint.HughesPJClass
import Text.PrettyPrint.HughesPJ
import Data.Word

data Stmt = Add
          | Sub
          | Neg
          | Eq
          | Gt
          | Lt
          | And
          | Or
          | Not
          | Push Addr
          | Pop Addr
          | Goto Label
          | IfGoto Label
          | Call Function Arity
          | Return
          deriving Show

instance Pretty Stmt where            
  pPrint Add = text "add"
  pPrint Sub = text "sub"
  pPrint Neg = text "neg"
  pPrint Eq = text "eq"
  pPrint Gt = text "gt"
  pPrint Lt = text "lt"
  pPrint And = text "and"
  pPrint Or = text "or"
  pPrint Not = text "not"
  pPrint (Push a) = text "push" <+> pPrint a
  pPrint (Pop a) = text "pop" <+> pPrint a
  pPrint (Goto l) = text "goto" <+> text l
  pPrint (IfGoto l) = text "if-goto" <+> text l
  pPrint (Call f n) = text "call" <+> text f <+> text (show n)
  pPrint Return = text "return"  
  
data Directive = Label Label
               | Stmt Stmt
               deriving Show
                        
instance Pretty Directive where                        
  pPrint (Label l) = text "label" <+> text l
  pPrint (Stmt s) = nest 2 $ pPrint s
  
data FunctionDef = FunctionDef Function Arity [Directive]
                 deriving Show

instance Pretty FunctionDef where
  pPrint (FunctionDef f arity ds) = text "function" <+> text f <+> text (show arity) $$ vcat (map pPrint ds)
  
newtype Deck = Deck { getFunctions :: [FunctionDef] }

instance Pretty Deck where
  pPrint (Deck funs) = vcat $ map pPrint funs

type Value = Word16                   
type Offset = Word16
type Label = String
type Function = String
type Arity = Word16
                   
data Addr = Arg Offset        
          | Local Offset
          | Static Offset
          | Constant Value
          | RefThis
          | RefThat
          | This Offset
          | That Offset
          | Temp Offset          
          deriving Show
            
instance Pretty Addr where            
  pPrint addr = 
    case addr of
      Arg n -> p "argument" n
      Local n -> p "local" n
      Static n -> p "static" n
      Constant v -> p "constant" v
      RefThis -> p "pointer" 0
      RefThat -> p "pointer" 1
      This n -> p "this" n
      That n -> p "that" n
      Temp n -> p "temp" n
    where 
      p s n = text s <+> text (show n)

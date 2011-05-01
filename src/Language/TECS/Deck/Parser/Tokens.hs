module Language.TECS.Deck.Parser.Tokens where

import Data.Word
import Data.ByteString.Lazy (ByteString)
import Language.TECS.Located

data Token = CommentStart
           | Comment ByteString
           | CommentEnd
           | Identifier String
           | Value Word16
           | Function
           | Label
           | Add
           | Sub
           | Neg
           | Eq
           | Gt
           | Lt
           | And
           | Or
           | Not
           | Push 
           | Pop
           | Goto 
           | IfGoto 
           | Call 
           | Return
           | Argument
           | Local
           | Static
           | Constant
           | Pointer
           | This
           | That
           | Temp
           deriving (Eq, Show)

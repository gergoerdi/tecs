module Language.TECS.Jack.Parser.Tokens where

import Data.Word
import Data.ByteString.Lazy (ByteString)
       
data Token = CommentStart
           | Comment ByteString
           | CommentEnd             
           | Identifier String
           | BoolLit Bool
           | IntLit Word16
           | StringLit ByteString
           | ParenOpen
           | ParenClose
           | BracketOpen
           | BracketClose
           | BraceOpen
           | BraceClose
           | Comma
           | Semicolon
           | Eq
           | Dot
           | Plus
           | Minus
           | Mul
           | Div
           | And
           | Or
           | Tilde
           | Lt
           | Gt
           | Class
           | Constructor
           | Method
           | Function
           | Int
           | Boolean
           | Char
           | Void
           | Var
           | Static
           | Field
           | Let
           | Do
           | If
           | Then
           | Else
           | While
           | Return
           | Null
           | This
           deriving (Eq, Show)

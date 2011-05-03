module Main where

import Text.Parsec (runParser)
import Control.Applicative
import Language.TECS.Jack.Syntax
import Language.TECS.Jack.Parser
import Language.TECS.Jack.Parser.Lexer
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Text.PrettyPrint.HughesPJClass
import Text.PrettyPrint.HughesPJ

main = do
  [filename] <- getArgs
  s <- BS.readFile filename
  case parseJack s filename of
    Left err -> putStrLn err 
    Right jack -> print $ pPrint jack  

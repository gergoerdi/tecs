module Main where

import Text.Parsec (runParser)
import Control.Applicative
import Language.TECS.Jack.Syntax
import Language.TECS.Jack.ToDeck.Layout
import Language.TECS.Jack.ToDeck.Compile
import Language.TECS.Jack.Parser
import Language.TECS.Jack.Parser.Lexer
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Text.PrettyPrint.HughesPJClass
import Text.PrettyPrint.HughesPJ

main = do
  [filename] <- getArgs
  s <- BS.readFile filename
  case lexAndParse s of
    Left err -> putStrLn err 
    Right jack -> print $ pPrint $ compile $ layout jack  

parse = runParser (jack <* eof) () filename
  where filename = ""
        
lexAndParse s = case lexer s of
  Left err -> Left err
  Right tokens -> case parse tokens of 
    Left err -> Left $ show err
    Right jack -> Right jack

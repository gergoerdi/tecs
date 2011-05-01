module Main where

import Text.Parsec (runParser)
import Control.Applicative
import Language.TECS.Deck.Syntax
import Language.TECS.Deck.Parser
import Language.TECS.Deck.Parser.Lexer
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Text.PrettyPrint.HughesPJClass
import Text.PrettyPrint.HughesPJ

main = do
  [filename] <- getArgs
  s <- BS.readFile filename
  case lexAndParse s of
    Left err -> putStrLn err 
    Right deck -> print $ pPrint deck  

parse = runParser (deck <* eof) () filename
  where filename = ""
        
lexAndParse s = case lexer s of
  Left err -> Left err
  Right tokens -> case parse tokens of 
    Left err -> Left $ show err
    Right deck -> Right deck

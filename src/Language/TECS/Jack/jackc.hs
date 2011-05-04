module Main where

import Language.TECS.Jack.ToDeck.Layout
import Language.TECS.Jack.ToDeck.Compile
import Language.TECS.Jack.Parser

import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr, withFile, IOMode(..))

import qualified Data.ByteString.Lazy as BS (readFile)
import Text.PrettyPrint.HughesPJClass (pPrint)
import Text.PrettyPrint.HughesPJ (render)

main = 
  do
    filenames <- getArgs
    mapM_ compileFile filenames

compileFile filename =
  do
    putStr $ unwords ["Compiling", filename, "... "]
    bs <- BS.readFile filename
    case parseJack bs filename of
      Left err -> 
        do
          putStrLn ""
          hPutStrLn stderr err
      Right jack -> 
        do
          -- Use withFile/hPutStrLn to ensure terminating newline
          withFile outfile WriteMode $ \h -> do
            hPutStrLn h $ render . pPrint $ compile . layout $ jack
          putStrLn $ unwords ["created", outfile]
  where
    outfile = replaceExtension filename "deck"

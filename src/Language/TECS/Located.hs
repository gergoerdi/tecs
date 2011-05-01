{-# LANGUAGE DeriveFunctor #-}
module Language.TECS.Located (L(..)) where

import Text.Parsec.Pos
       
data L a = L { getLoc :: SourcePos, unLoc :: a }
         deriving Functor

instance Show a => Show (L a) where
  show (L _ x) = unwords ["L", show x]

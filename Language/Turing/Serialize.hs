module Language.Turing.Serialize
       (
         machineToDot
       ) where

import Control.Monad.Writer
import qualified Data.Set as S
import Language.Turing.Machine
import Text.Printf

stateToDot :: (Show q) => q -> Writer String ()
stateToDot x = tell $ printf "%s;\n" $ show x

ruleToDot :: (Show q, Show l) => Rule q l -> Writer String ()
ruleToDot r = do
  tell $ show $ condState r
  tell " -> "
  tell $ show $ nextState r
  tell " [label=\""
  tell $ show $ condPat r
  tell " -> "
  tell $ show $ actPat r
  tell " -> "
  tell $ show $ delta r
  tell "\"];\n"

machineToDot :: (Show q, Show l) => Machine q l -> String
machineToDot m = execWriter $ do
  tell "digraph {\n"
  mapM_ stateToDot $ S.toList $ states m
  mapM_ ruleToDot $ rules m
  tell "}\n"

module Language.Turing.Serialize
       (
         machineToDot,
         machineToSexpr
       ) where

import Control.Monad.Writer
import Data.List
import qualified Data.Set as S
import Language.Turing.Machine
import Text.Printf

showPattern :: (TapeSymbol l) => Pattern l -> String
showPattern p = "[" ++ (foldl (++) "" $ intersperse " " $ map showEnt p) ++ "]"
  where showEnt (a,b) = show b ++ ":" ++ shortSymbol a

ruleToDot :: (Show q, TapeSymbol l) => Rule q l -> Writer String ()
ruleToDot r = do
  tell $ show $ condState r
  tell " -> "
  tell $ show $ nextState r
  tell " [label=\""
  tell $ showPattern $ condPat r
  tell " / "
  tell $ showPattern $ actPat r
  tell " / "
  tell $ show $ delta r
  tell "\"];\n"

machineToDot :: (Show q, TapeSymbol l) => Machine q l -> String
machineToDot m = execWriter $ do
  tell "digraph {\n"
  mapM_ ruleToDot $ rules m
  tell "}\n"

machineToSexpr :: (Enum q, Enum l) => Machine q l -> String
machineToSexpr m =
  let list s = '(' : (foldl (++) "" $ intersperse " " s) ++ ")"
      ent (l, t) = list [show $ fromEnum $ l, show t]
      pat p = list $ map ent p
      rule r = list $ [show $ fromEnum $ condState r,
                       show $ fromEnum $ nextState r,
                       pat $ condPat r,
                       pat $ actPat r]
  in list [show $ fromEnum $ initState m,
           show $ fromEnum $ haltState m,
           list $ map rule $ rules m]

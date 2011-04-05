module Language.Turing.Simulate
       (
         Tape(..),
         readTape,
         writeTape,
         emptyTape,
         moveHead,
         debugMachine
       ) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Data.List
import GHC.Exts(sortWith)
import Language.Turing.Machine
import Text.Printf

data Tape l = Tape Int (M.Map (Int, Int) l)

readTape :: (Enum l) => Tape l -> Int -> l
readTape (Tape i m) w = M.findWithDefault (toEnum 0) (i, w) m
                          
writeTape :: Tape l -> Int -> l -> Tape l
writeTape (Tape i m) w x = Tape i $ M.insert (i, w) x m 

emptyTape :: Tape l
emptyTape = Tape 0 M.empty

moveHead :: Tape l -> Int -> Tape l
moveHead (Tape i m) d = Tape (i + d) m

type Context q l = (q, Tape l)

checkRule :: (Eq q, Eq l, Enum l) => Context q l -> Rule q l -> Bool
checkRule (q, t) r = checkPattern (condPat r) && condState r == q
  where checkPattern = all (\(l, x) -> readTape t x == l)

bestRule :: (Eq q, Eq l, Enum l) => Machine q l -> Context q l -> Rule q l
bestRule m cx = last $ sortWith ruleLength $ filter (checkRule cx) $ rules m
  where ruleLength = length . condPat

applyRule :: Rule q l -> Context q l -> Context q l
applyRule r (_q, t) = (nextState r, moveHead (applyPattern t $ actPat r) $ delta r)
  where applyPattern = foldl (\t' (l,x) -> writeTape t' x l)
 
stepMachine :: (Eq q, Eq l, Enum l) => Machine q l -> Context q l -> Context q l
stepMachine m cx = applyRule (bestRule m cx) cx

debugMachine :: (Show q, Eq q, TapeSymbol l, Eq l, Enum l) => Machine q l -> 
                Context q l -> String
debugMachine m cx = execWriter $ evalStateT body cx
  where
    body = do
      (q, Tape i t) <- get
      tell $ printf "-------------------------------------------------------\n"
      tell $ printf " State: %s\n" $ show q
      tell $ printf "   Pos: %d\n" i
      tell $ printf "\n"
      let (tmin, tmax) = findTs t
      let (cmin, cmax) = findCs t
      tell $ printf "  Tape: %sv\n" $ replicate (2 * (i - cmin)) ' '
      mapM_ (showTape t [cmin..(max cmax i)]) [tmin..tmax]
      when (q /= haltState m) $ do
        modify $ stepMachine m
        body
    
    showTape t r n = do
      tell $ printf "%6d: " n
      tell $ foldl1 (++) $ map (\x -> shortSymbol (readTape (Tape x t) n) ++ " ") r
      tell $ printf "\n"
      return ()
    
    findTs t =
      if M.size t == 0 then
        (0, 0)
      else
        (head $ sort $ map snd $ M.keys t, last $ sort $ map snd $ M.keys t)
    
    findCs t = 
      if M.size t == 0 then
        (0, 0)
      else
        (head $ sort $ map fst $ M.keys t, last $ sort $ map fst $ M.keys t)

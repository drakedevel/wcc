module Language.Turing.Machine
       (
         Machine(..),
         Pattern,
         Rule(..),
         TapeSymbol(..),
         addState,
         setInitState,
         setHaltState,
         addTransition
       ) where

import qualified Data.Set as S

class TapeSymbol l where
  shortSymbol :: l -> String

type Pattern l = [(l, Int)]

data Rule q l = Rule { condState :: q,
                       condPat :: Pattern l,
                       nextState :: q,
                       actPat :: Pattern l,
                       delta :: Int }
                deriving (Show)

data Machine q l = Machine { states :: S.Set q, 
                             initState :: q,
                             haltState :: q,
                             rules :: [Rule q l] }
                 deriving (Show)

addState :: Ord q => q -> Machine q l -> Machine q l
addState q m = Machine { states = S.insert q $ states m,
                         initState = initState m,
                         haltState = haltState m,
                         rules = rules m }

setInitState :: q -> Machine q l -> Machine q l
setInitState q m = Machine { states = states m,
                             initState = q,
                             haltState = haltState m,
                             rules = rules m }

setHaltState :: q -> Machine q l -> Machine q l
setHaltState q m = Machine { states = states m,
                             initState = initState m,
                             haltState = q,
                             rules = rules m }

addTransition :: q -> Pattern l -> q -> Pattern l -> Int -> Machine q l -> Machine q l
addTransition q0 p q1 a d m = Machine { states = states m,
                                        initState = initState m,
                                        haltState = haltState m,
                                        rules = Rule q0 p q1 a d : rules m }

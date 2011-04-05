module Language.While.Compiler
       (
         Symbol(..),
         compileAbt
       ) where

import Control.Monad.State
import qualified Data.Set as S
import Language.Turing.Machine
import Language.While.Parser

data Symbol = Zero
            | One
            | Hash
            deriving (Enum, Eq, Show)

instance TapeSymbol Symbol where
  shortSymbol Zero = "0"
  shortSymbol One = "1"
  shortSymbol Hash = "#"

type OutputMachine = Machine Int Symbol

newState :: State OutputMachine Int
newState = do
  m <- get
  let i = S.size $ states m
  modify $ addState i
  return i

produceState :: ABT -> State OutputMachine (Int, Int)
produceState (Initialize v) = do
  sInit <- newState
  sHashBack <- newState
  sEnd <- newState
  
  modify $ addTransition sInit [] sHashBack [(Hash, v)] (-1)
  modify $ addTransition sHashBack [] sEnd [(Hash, v)] 1
  return (sInit, sEnd)

produceState (Increment v) = do
  sSeekR0 <- newState
  sSeekR1 <- newState
  sSeekRF <- newState
  sSeekL <- newState
  sEnd <- newState
  
  modify $ addTransition sSeekR0 [(Hash, v)] sSeekL [] (-1)
  modify $ addTransition sSeekR0 [] sSeekR0 [] 1  
  
  modify $ addTransition sSeekR1 [(Zero, v)] sSeekR0 [(One, v)] 1
  modify $ addTransition sSeekR1 [(One, v)] sSeekR1 [(Zero, v)] 1
  modify $ addTransition sSeekR1 [(Hash, v)] sSeekRF [(One, v)] 1

  modify $ addTransition sSeekRF [] sSeekL [(Hash, v)] (-1)
  
  modify $ addTransition sSeekL [(Hash, v)] sEnd [] 1
  modify $ addTransition sSeekL [] sSeekL [] (-1) 

  return (sSeekR1, sEnd)
  
produceState (Decrement v) = do  
  sSeekRC0 <- newState
  sSeekRC1 <- newState
  sSeekL0 <- newState
  sSeekL <- newState
  sEnd <- newState

  modify $ addTransition sSeekRC1 [(Zero, v)] sSeekRC1 [(One, v)] 1
  modify $ addTransition sSeekRC1 [(One, v)] sSeekRC0 [(Zero, v)] 1
  modify $ addTransition sSeekRC1 [(Hash, v)] sSeekL0 [] (-1)
  
  modify $ addTransition sSeekRC0 [(Hash, v)] sSeekL0 [] (-1)
  modify $ addTransition sSeekRC0 [] sSeekRC0 [] 1
  
  modify $ addTransition sSeekL0 [(Zero, v)] sSeekL0 [(Hash, v)] (-1)
  modify $ addTransition sSeekL0 [(One, v)] sSeekL [] (-1)
  modify $ addTransition sSeekL0 [(Hash, v)] sEnd [] 1
  
  modify $ addTransition sSeekL [(Hash, v)] sEnd [] 1
  modify $ addTransition sSeekL [] sSeekL [] (-1)
  
  return (sSeekRC1, sEnd)
  
produceState (Assign d s) = do  
  sInit <- newState
  sHashBack <- newState
  sCopyR <- newState
  sSeekL <- newState
  sEnd <- newState

  modify $ addTransition sInit [] sHashBack [] (-1)
  
  modify $ addTransition sHashBack [] sCopyR [(Hash, d)] 1
  
  modify $ addTransition sCopyR [(Zero, s)] sCopyR [(Zero, d)] 1
  modify $ addTransition sCopyR [(One, s)] sCopyR [(One, d)] 1
  modify $ addTransition sCopyR [(Hash, s)] sSeekL [(Hash, d)] (-1)
  
  modify $ addTransition sSeekL [(Hash, s)] sEnd [] 1
  modify $ addTransition sSeekL [] sSeekL [] (-1)

  return (sInit, sEnd)

produceState (Sequence s s') = do
  (i, e) <- produceState s
  (i', e') <- produceState s'
  modify $ addTransition e [] i' [] 0
  return (i, e')

produceState (While l b) = do
  (bi, be) <- produceState b
  
  sCheck <- newState
  sEnd <- newState
  
  modify $ addTransition sCheck [(Hash, l)] sEnd [] 0
  modify $ addTransition sCheck [] bi [] 0

  modify $ addTransition be [] sCheck [] 0
  
  return (sCheck, sEnd)

compileAbt :: ABT -> OutputMachine
compileAbt abt = flip execState (Machine S.empty 0 0 []) $ do
  (i, e) <- produceState abt
  modify $ setInitState i
  modify $ setHaltState e
  return ()
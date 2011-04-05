import Control.Monad
import Data.List
import qualified Data.Map as M
import Language.While.Compiler
import Language.While.Parser
import Language.Turing.Serialize
import Language.Turing.Simulate
import Language.Turing.Machine
import System
import System.IO
import Text.Parsec
import Text.Printf

usage :: IO ()
usage = error "Invalid parameters."

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) usage
  
  let fileName = head args
  let funcName = args !! 1
  fileText <- readFile fileName
  case runParser whileModule () fileName fileText of
    Left err -> error $ show err
    Right funcs -> do
      Function _ arity abt <- getFunc funcs funcName
      when (length args - 2 /= arity) $ error "Arity mismatch."
      let machine = compileAbt abt
      let cx = (initState machine, makeTape $ map read $ drop 2 args)
      writeFile "out.dot" $ machineToDot machine
      hPrintf stdout $ debugMachine machine cx

numSeq n = Hash : f n
  where
    f 0 = [Hash]
    f n = (if mod n 2 == 0 then Zero else One) : f (div n 2)

getFunc funcs funcName =
  case find (\(Function n _ _) -> n == funcName) funcs of
    Nothing -> error "No function of that name defined."
    Just f -> return f

makeTape :: [Int] -> Tape Symbol
makeTape params = Tape 0 $ foldl' (\t (x, ys) -> foldl' (\t' (y, v) -> M.insert (y,x) v t') t ys) M.empty $ zip [0..] $ map (zip [-1..] . numSeq) params

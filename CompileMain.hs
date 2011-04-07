import Control.Applicative
import Control.Monad
import Data.List
import Language.While.Parser
import Language.While.Compiler
import Language.Turing.Machine
import Language.Turing.Serialize
import System.Environment
import System.IO
import Text.Parsec

munge :: String -> String
munge inF = if isSuffixOf ".whl" inF
            then (take (length inF - 4) inF) ++ ".sxp"
            else inF ++ ".sxp"

parseFile :: String -> IO [Function]
parseFile inF = do
  text <- readFile inF
  case runParser whileModule () inF text of
    Left err -> error $ show err
    Right funcs -> return $ funcs

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ error "Usage: wcc <source> [out]"
  let inF = head args
  let outF = if length args > 1 
             then args !! 1 
             else munge inF
  functions <- fmap compileFunction <$> parseFile inF
  writeFile outF $ list $ map serialize functions
  where 
    compileFunction (Function n a abt) = (Function n a abt, compileAbt abt)
    list s = '(' : (foldl (++) "" $ intersperse " " s) ++ ")"
    serialize (Function n a abt, m) = list [n, show a, machineToSexpr m]
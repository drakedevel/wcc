module Language.While.Parser 
       (
         ABT,
         AST,
         AT(..),
         Function(..),
         whileModule
       ) where

import Control.Monad
import Control.Monad.State
import Data.Map
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P

type Variable = String

type Identifier = Int

data AT v = Initialize v
          | Increment v
          | Decrement v
          | Assign v v
          | Sequence (AT v) (AT v)
          | While v (AT v)
          deriving (Show)
                   
type AST = AT Variable

type ABT = AT Identifier

data Function = Function String Int ABT
              deriving (Show)

languageDef = P.LanguageDef {
  P.commentStart = "",
  P.commentEnd = "",
  P.commentLine = "#",
  P.nestedComments = False,
  P.identStart = letter,
  P.identLetter = letter,
  P.opStart = oneOf "+:-",
  P.opLetter = oneOf "+:-",
  P.reservedNames = ["while", "od", "fun", "nuf"],
  P.reservedOpNames = ["+", ":", "-"],
  P.caseSensitive = True
  }

lexer = P.makeTokenParser languageDef
identifier = P.identifier lexer
lexeme = P.lexeme lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
semi = P.semi lexer
semiSep1 = P.semiSep1 lexer

initializeExpr :: Parser AST
initializeExpr = do
  name <- identifier
  reservedOp "="
  _ <- lexeme $ char '0'
  return $ Initialize name

incrementExpr :: Parser AST
incrementExpr = do
  name <- identifier
  reservedOp "="
  _ <- lexeme $ string name
  reservedOp "+"
  _ <- lexeme $ char '1'
  return $ Increment name
  
decrementExpr :: Parser AST
decrementExpr = do
  name <- identifier
  reservedOp "="
  _ <- lexeme $ string name
  reservedOp "-"
  _ <- lexeme $ char '1'
  return $ Decrement name

assignExpr :: Parser AST
assignExpr = do
  dest <- identifier
  reservedOp "="
  source <- identifier
  return $ Assign dest source

whileExpr :: Parser AST
whileExpr = do
  reserved "while"
  condition <- identifier
  reservedOp ":"
  body <- exprList
  reserved "od"
  return $ While condition body

expr :: Parser AST
expr = try initializeExpr
   <|> try incrementExpr
   <|> try decrementExpr
   <|> try assignExpr
   <|> try whileExpr

exprList :: Parser AST
exprList = do 
  list <- semiSep1 expr
  return $ case list of
    [x] -> x
    l -> foldr1 Sequence l

astToAbt :: AST -> [String] -> Either String (ABT, Map String Identifier)
astToAbt ast params = flip runStateT empty $ do
  mapM_ newVar params
  astToAbt' ast
  where
    astToAbt' a =  do
      b <- get
      case a of
        Initialize v -> do
          vi <- getVarOrAdd v
          return $ Initialize vi
        Increment v -> do
          vi <- getVar v
          return $ Increment vi
        Decrement v -> do
          vi <- getVar v
          return $ Decrement vi
        Assign d s -> do
          si <- getVar s
          di <- getVarOrAdd d
          return $ Assign di si
        Sequence f s -> do
          ff <- astToAbt' f
          ss <- astToAbt' s
          return $ Sequence ff ss
        While v l -> do
          vi <- getVar v
          ll <- astToAbt' l
          return $ While vi ll
    
    getVar v = do
      b <- get
      unless (member v b) $ fail "Unbound variable."
      return $ b ! v

    getVarOrAdd v = do
      b <- get
      if member v b then return (b ! v)
        else newVar v

    newVar v = do
      b <- get
      let i = size b
      modify $ insert v i
      return i

functionBody :: [String] -> Parser ABT
functionBody params = do
  ast <- exprList
  case astToAbt ast params of
    Left s -> fail s
    Right (r, _) -> return r

functionDecl :: Parser Function
functionDecl = do
  reserved "fun"
  name <- identifier
  params <- many identifier
  reservedOp ":"
  body <- functionBody params
  reserved "nuf"
  return $ Function name (length params) body

whileModule :: Parser [Function]
whileModule = many1 functionDecl
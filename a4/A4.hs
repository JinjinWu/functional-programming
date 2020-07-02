module A4 where

import           Control.Applicative
import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Expr
mainParser = whitespaces *> blockPsr <* eof

blockPsr :: Parser Expr
blockPsr = condPsr <|> lambdaPsr <|> letPsr <|> infixPsr

infixPsr :: Parser Expr
infixPsr = cmpPsr <|> arithPsr
    where
        cmpPsr = do
            lhs <- arithPsr
            op <- cmp
            rhs <- arithPsr
            pure (Prim2 op lhs rhs)
        cmp = fmap match (operator "==" <|> operator "<")
        match "==" = Eq
        match "<" = Lt

arithPsr :: Parser Expr
arithPsr = chainl1 addendPsr addop
    where
        addop = plus <|> minus
        plus = operator "+" *> pure (Prim2 Plus)
        minus = operator "-" *> pure (Prim2 Minus)

addendPsr :: Parser Expr
addendPsr = chainl1 factorPsr mulop
    where
        mulop = multiply <|> divide <|> modulus
        multiply = operator "*" *> pure (Prim2 Mul)
        divide = operator "/" *> pure (Prim2 Div)
        modulus = operator "%" *> pure (Prim2 Mod)

factorPsr :: Parser Expr
factorPsr = chainl1 atomPsr (pure App)

atomPsr :: Parser Expr
atomPsr = between (char '(' *> whitespaces) (char ')' *> whitespaces) blockPsr
          <|> literalPsr <|> fmap Var var

condPsr :: Parser Expr
condPsr = do
    keyword "if"
    b1 <- blockPsr
    keyword "then"
    b2 <- blockPsr
    keyword "else"
    b3 <- blockPsr
    pure (Cond b1 b2 b3)

lambdaPsr :: Parser Expr
lambdaPsr = do
    operator "\\"
    v <- var
    operator "->"
    b <- blockPsr
    pure (Lambda v b)

letPsr :: Parser Expr
letPsr = do
    keyword "let"
    eqns <- many equationPsr
    keyword "in"
    b <- blockPsr
    pure (Let eqns b)

-- parses 1 equation and returns Parser (var, block)
equationPsr :: Parser (String, Expr)
equationPsr = do
    v <- var
    char '=' *> whitespaces
    b <- blockPsr
    char ';' *> whitespaces
    pure (v, b)

literalPsr :: Parser Expr
literalPsr = fmap Num integer <|> boolPsr

boolPsr :: Parser Expr
boolPsr = fmap Bln bool
    where
        bool = fmap match (keyword "True" <|> keyword "False")
        match "True" = True
        match "False" = False

var :: Parser String
var = identifier ["if", "then", "else", "let", "in", "True", "False"]


mainInterp :: Expr -> Either Error Value
mainInterp = interp Map.empty

intOrDie :: Value -> Either Error Integer
intOrDie (VN i) = pure i
intOrDie _ = Left TypeError

checkZero :: Integer -> Either Error Integer
checkZero 0 = Left DivByZero
checkZero x = pure x

interp :: Map String Value -> Expr -> Either Error Value

interp _ (Num i) = pure (VN i)

interp _ (Bln b) = pure (VB b)

interp env (Var v) = case Map.lookup v env of
    Just a -> pure a
    Nothing -> Left VarNotFound

interp env (Prim2 Eq e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i == j))

interp env (Prim2 Lt e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VB (i < j))

interp env (Prim2 Plus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i + j))

interp env (Prim2 Minus e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i - j))

interp env (Prim2 Mul e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (i * j))

interp env (Prim2 Div e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    d <- checkZero j
    pure (VN (div i d))

interp env (Prim2 Mod e1 e2) = do
    a <- interp env e1
    i <- intOrDie a
    b <- interp env e2
    j <- intOrDie b
    pure (VN (mod i j))

interp env (Cond test eThen eElse) = do
    a <- interp env test
    case a of
        VB True -> interp env eThen
        VB False -> interp env eElse
        _ -> Left TypeError

interp env (Let eqns evalMe) = do
    env' <- extend env eqns
    interp env' evalMe
    where
        extend = foldM extendOnce
        extendOnce env (v, rhs) = do
            a <- interp env rhs
            pure (Map.insert v a env)

interp env (Lambda v body) = pure (VClosure env v body)

interp env (App f e) = do
    c <- interp env f
    case c of
        VClosure fEnv v body -> do
            eVal <- interp env e
            let bEnv = Map.insert v eVal fEnv
            interp bEnv body
        _ -> Left TypeError

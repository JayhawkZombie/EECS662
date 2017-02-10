{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Author: Kurt Slagle
-- Date  : Feb 9, 2017
-- Project0 - EECS 662, exercises 1-3, using Either monad

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  If0 :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

parseABE :: String -> ABE

expr :: Parser ABE
expr = buildExpressionParser operators term

operators = [ [ inFix "*" Mult AssocLeft
              , inFix "/" Div AssocLeft ]
            , [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
            ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

if0Expr :: Parser ABE
if0Expr = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

term = parens lexer expr <|> numExpr <|> trueExpr <|> falseExpr <|> if0Expr

parseABE = parseString expr
parseABEFile= parseFile expr

eval :: ABE -> Either String ABE
eval (Num x) = (Right (Num x))
eval (Boolean b) = (Right (Boolean b))
eval (Plus t1 t2) = let m1 = eval t1
                        m2 = eval t2
                    in  case m1 of
                          (Left e) -> (Left e)
                          (Right (Num n)) -> case m2 of
                                               (Left l) -> (Left l)
                                               (Right (Num r)) -> (Right(Num(n + r)))
                                               (Right _) -> (Left "Invalid right argument to Plus")
                          (Right _) -> (Left "Invalid left argument to Plus")

eval (Minus t1 t2) = let m1 = eval t1
                         m2 = eval t2
                     in case m1 of
                          (Left e) -> (Left e)
                          (Right (Num n)) -> case m2 of
                                               (Left l) -> (Left l)
                                               (Right (Num r)) -> (Right (Num(n - r)))
                                               (Right _) -> (Left "Invalid right argument to Minus")
                          (Right _) -> (Left "Invalid left argument to Minus")

eval (Mult t1 t2) = let m1 = eval t1
                        m2 = eval t2
                    in case m1 of
                          (Left e) -> (Left e)
                          (Right (Num n)) -> case m2 of
                                               (Left l) -> (Left l)
                                               (Right (Num r)) -> (Right (Num(n * r)))
                                               (Right _) -> (Left "Invalid right argument to Mul")
                          (Right _) -> (Left "Invalid left argument to Mul")

eval (Div t1 t2) = let m1 = eval t1
                       m2 = eval t2
                     in case m1 of
                          (Left e) -> (Left e)
                          (Right (Num n)) -> case m2 of
                                               (Left l) -> (Left l)
                                               (Right (Num r)) -> (Right (Num (div n r)))
                                               (Right _) -> (Left "Invalid right argument to Div")
                          (Right _) -> (Left "Invalid left argument to Div")

eval (If0 t1 t2 t3) = let b = eval t1
                      in case b of
                           (Left l) -> (Left l)
                           (Right (Num n)) -> if (n == 0) then (eval t2) else (eval t3)
                           (Right _) -> (Left "Invalid right argument to if0")

interp :: String -> Either String ABE
interp = eval . parseABE

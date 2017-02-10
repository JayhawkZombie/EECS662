{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Author: Kurt Slagle
-- Date  : Feb 9, 2017
-- Project0 - EECS 662, exercises 1-2, not using any monad

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

term = parens lexer expr <|> numExpr <|> if0Expr

parseABE = parseString expr
parseABEFile= parseFile expr

eval :: ABE -> ABE
eval (Num x) = (Num x)
eval (Boolean b) = (Boolean b)
eval (Plus t1 t2) = let (Num v1) = eval t1
                        (Num v2) = eval t2
                    in  (Num (v1 + v2))
eval (Minus t1 t2) = let (Num v1) = eval t1
                         (Num v2) = eval t2
                     in  (Num (v1 - v2))
eval (Mult t1 t2) = let (Num v1) = eval t1
                        (Num v2) = eval t2
                    in  (Num (v1 * v2))
eval (Div t1 t2) = let (Num v1) = eval t1
                       (Num v2) = eval t2
                   in  (Num (div v1 v2))
eval (If0 t1 t2 t3) = let (Num v1) = (eval t1)
                      in if (v1 == 0) then (eval t2) else (eval t3)

interp :: String -> ABE
interp = eval . parseABE

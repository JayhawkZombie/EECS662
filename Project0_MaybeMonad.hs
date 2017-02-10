{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Author: Kurt Slagle
-- Date  : Feb 9, 2017
-- Project0 - EECS 662, exercises 1-3, using Maybe monad

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

eval :: ABE -> Maybe ABE
eval (Num x) = (Just (Num x))
eval (Boolean b) = (Just (Boolean b))
eval (Plus t1 t2) = case (eval t1) of
                       Just (Num x) -> case (eval t2) of
                                         Just (Num y) -> Just (Num (x + y))
                                         _ -> Nothing
                       _ -> Nothing

eval (Minus t1 t2) = case (eval t1) of
                       Just (Num x) -> case (eval t2) of
                                         Just (Num y) -> Just (Num (x - y))
                                         _ -> Nothing
                       _ -> Nothing

eval (Mult t1 t2) = case (eval t1) of
                      Just (Num x) -> case (eval t2) of
                                        Just (Num y) -> Just (Num (x * y))
                                        _ -> Nothing
                      _ -> Nothing

eval (Div t1 t2) = case (eval t1) of
                     Just (Num x) -> case (eval t2) of
                                       Just (Num y) -> Just (Num (div x y))
                                       _ -> Nothing
                     _ -> Nothing

eval (If0 t1 t2 t3) = case (eval t1) of
                        Just (Num x) -> if (x == 0) then (eval t2) else (eval t3)
                        _ -> Nothing

interp :: String -> Maybe ABE
interp = eval . parseABE

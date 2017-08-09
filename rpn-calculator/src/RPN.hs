module RPN (BinaryOperator, parseBinaryOperator, applyBinaryOperator, evaluate) where

import Text.Read (readMaybe)
import Control.Monad ((<=<), foldM)
import Control.Applicative ((<|>))



-- represent operator as an enum, not just a string
data BinaryOperator = Add | Mul deriving (Show)

data Token = Number Integer
           | BinOp BinaryOperator
           deriving (Show)


parseBinaryOperator :: String -> Maybe BinaryOperator
parseBinaryOperator str = case str of
  "*" -> Just Mul
  "+" -> Just Add
  _   -> Nothing

applyBinaryOperator :: BinaryOperator -> Integer -> Integer -> Integer
applyBinaryOperator Mul = (*)
applyBinaryOperator Add  = (+)


-- NOTES:
-- <|> returns its first argument if it's a `Just`, otherwise the second one
parseToken :: String -> Either String Token
parseToken str = addErrorMessage (number <|> binop)
  where
    number = fmap Number (readMaybe str)
    binop  = fmap BinOp (parseBinaryOperator str)

    addErrorMessage :: Maybe Token -> Either String Token
    addErrorMessage = maybe (Left ("invalid token: " ++ show str)) Right


-- NOTES:
-- Monads simplify error handling! (<=<, foldM)
-- traverse gives us `Either _ [Token]` instead of `[Either _ Token]`
evaluate :: String -> Either String Integer
evaluate = takeOnlyOne <=< foldM step [] <=< parse
  where
    parse = traverse parseToken . words

    step :: [Integer] -> Token -> Either String [Integer]
    step stack (Number n) = Right (n:stack)
    step stack (BinOp op) = case stack of
      m:n:rest -> Right (applyBinaryOperator op m n : rest)
      _ -> Left "not enough values on stack for operator"

    takeOnlyOne [n] = Right n
    takeOnlyOne ns = Left ("more than one value left on stack: " ++ show ns)

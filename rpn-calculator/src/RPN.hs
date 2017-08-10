module RPN (BinaryOperator, parseBinaryOperator, applyBinaryOperator, evaluate) where

import Text.Read (readMaybe)
import Control.Monad ((<=<), foldM)
import Control.Applicative ((<|>))
import Data.Bifunctor (first)



-- represent operator as an enum, not just a string
data BinaryOperator = Add | Mul deriving (Show)

data Token = Number Integer
           | BinOp BinaryOperator
           deriving (Show)

data RpnError = InvalidToken String
              | NotEnoughStackValues BinaryOperator
              | NotOneValueLeft [Integer]
              deriving (Show)


showRpnError :: RpnError -> String
showRpnError e = case e of
  InvalidToken token ->
    "invalid token: " ++ show token
  NotOneValueLeft stack ->
    "not exactly one value left on stack: " ++ show stack
  NotEnoughStackValues op ->
    "not enough values on stack for operator \"" ++ showBinaryOperator op ++ "\""


parseBinaryOperator :: String -> Maybe BinaryOperator
parseBinaryOperator str = case str of
  "*" -> Just Mul
  "+" -> Just Add
  _   -> Nothing

showBinaryOperator :: BinaryOperator -> String
showBinaryOperator op = case op of
  Add -> "+"
  Mul -> "*"

applyBinaryOperator :: BinaryOperator -> Integer -> Integer -> Integer
applyBinaryOperator Mul = (*)
applyBinaryOperator Add  = (+)


-- NOTES:
-- <|> returns its first argument if it's a `Just`, otherwise the second one
parseToken :: String -> Either RpnError Token
parseToken str = addErrorMessage (number <|> binop)
  where
    number = fmap Number (readMaybe str)
    binop  = fmap BinOp (parseBinaryOperator str)

    addErrorMessage :: Maybe Token -> Either RpnError Token
    addErrorMessage = maybe (Left (InvalidToken str)) Right


-- NOTES:
-- wrapper around evaluate' to provide textual errors as well
evaluate :: String -> Either String Integer
evaluate = first showRpnError . evaluate'

-- NOTES:
-- Monads simplify error handling! (<=<, foldM)
-- traverse gives us `Either _ [Token]` instead of `[Either _ Token]`
evaluate' :: String -> Either RpnError Integer
evaluate' = takeOnlyOne <=< foldM step [] <=< parse
  where
    parse = traverse parseToken . words

    step :: [Integer] -> Token -> Either RpnError [Integer]
    step stack (Number n) = Right (n:stack)
    step stack (BinOp op) = case stack of
      m:n:rest -> Right (applyBinaryOperator op m n : rest)
      _ -> Left (NotEnoughStackValues op)

    takeOnlyOne [n] = Right n
    takeOnlyOne ns = Left (NotOneValueLeft ns)

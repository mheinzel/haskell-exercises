module RPN (BinaryOperator, parseBinaryOperator, applyBinaryOperator, evaluate) where


type BinaryOperator = String  -- TODO: think of something better


parseBinaryOperator :: String -> Maybe BinaryOperator
parseBinaryOperator = undefined


applyBinaryOperator :: BinaryOperator -> Integer -> Integer -> Integer
applyBinaryOperator = undefined


evaluate :: String -> Either String Integer
evaluate = undefined

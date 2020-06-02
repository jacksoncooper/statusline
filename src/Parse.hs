{-# LANGUAGE InstanceSigs #-}

module Parse where

import Data.Char

data Result a =
    Success a
  | Failure String
  deriving Show

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Success a) = Success $ f a
  fmap _ (Failure s) = Failure s

newtype Parser a =
  Parser { runParser :: String -> Result (String, a) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser $ (fmap . fmap . fmap) f p

parseDigit :: Parser Int
parseDigit =
  Parser $ \s ->
    case s of
      (c:cs) ->
        if isDigit c
        then Success (cs, digitToInt c)
        else Failure $ "The character '" ++ [c] ++ "' is not a digit."
      [] -> Failure "Encountered and empty string."


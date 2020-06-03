{-# LANGUAGE InstanceSigs #-}

module Parse where

import Data.Char

data State = State
  { position :: Int
  , remaining :: String
  } deriving Show

newtype Parser a =
  Parser { runParser :: State -> Either String (State, a) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser parser) =
    Parser $ (fmap . fmap . fmap) f parser

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \_ -> Right (State 0 "", a)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser parser) <*> (Parser parser') =
    Parser $ \state ->
      parser state >>=
        \(state', f) ->
          (fmap . fmap) f (parser' state')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser parser) >>= f =
    Parser $ \state ->
      parser state >>=
        \(state', a) ->
          runParser (f a) state'

consumeCharacter :: Parser Char
consumeCharacter =
  Parser $ \(State position remaining) ->
    case remaining of
      (character : rest) ->
        Right (State (position + 1) rest, character)
      "" ->
        Left "Encountered an empty string."

validateCharacter :: (Char -> Bool) -> (Char -> a) -> (Char -> String) -> Parser a
validateCharacter isValid transform explanation =
  -- The parser's monad instance doesn't allow us to fail the parser using a
  -- bind. We have to operate over the Either type that wraps it. Consider a
  -- different implementation of the parser's data constructor.

  Parser $ \state ->
    runParser consumeCharacter state >>=
      \(state', consumed) ->
        if isValid consumed
        then Right (state', transform consumed)
        else Left (explanation consumed)

parseCharacter :: Char -> Parser Char
parseCharacter target =
  let explanation consumed =
        "The character '"
        ++ pure consumed
        ++ "' is not '"
        ++ pure target
        ++ "'."
  in validateCharacter (target ==) id explanation

parseDigit :: Parser Int
parseDigit =
  let explanation consumed =
        "The character '"
        ++ pure consumed
        ++ "' is not a digit."
  in validateCharacter isDigit digitToInt explanation

-- parseDigits :: Parser [Int]
-- parseDigits =
--   Parser $ \s -> undefined


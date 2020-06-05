{-# LANGUAGE InstanceSigs #-}

module Parse
  ( Parser (..)
  , State (..)
  , Result
  , parseDigits
  , toState
  , getValue
  )
where

import Data.Char (isDigit, digitToInt)

-- According to 'Haskell Programming from First Principles', these sorts of
-- parsers have fallen out of style for newer designs. I'm using this
-- representation because of its similarity to the State type synonym, defined
-- in Control.Monad.Trans.State.

type Result a = Either String (State, a)

data State = State
  { line :: Int
  , column :: Int
  , input :: String
  } deriving Show

newtype Parser a =
  Parser { runParser :: State -> Result a }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser parser) =
    Parser $ (fmap . fmap . fmap) f parser

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \state -> Right (state, a)

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

consume :: Parser Char
consume =
  Parser $ \state@(State line column input) ->
    case input of
      (inputHead : restOfInput) ->
        if inputHead == '\n'
        then Right (State (line + 1) 0 restOfInput, inputHead)
        else Right (State line (column + 1) restOfInput, inputHead)
      [] ->
        Left $ parseError state "Encountered end of input."

parseCharacter :: Char -> Parser Char
parseCharacter target =
  Parser $ \state ->
    runParser consume state >>=
      \(state', character) ->
        if character == target
        then Right (state', character)
        else Left $ parseError state'
          "Encountered '" ++ [character] ++ "' instead of '"
          ++ [target] ++ "'."

parseString :: String -> Parser String
parseString = traverse parseCharacter

parseDigits :: Parser Int
parseDigits = stringToInteger <$> parseWhile isDigit

parseWhile :: (Char -> Bool) -> Parser String
parseWhile continue = Parser $ \state ->
  let toParse = takeWhile continue (input state)
  in runParser (parseString toParse) state

toState :: String -> State
toState = State 0 0

parseError :: State -> String -> String
parseError state reason =
  "Parse error at line "
  ++ show (line state)
  ++ ", column "
  ++ show (column state)
  ++ ": "
  ++ reason

stringToInteger :: String -> Int
stringToInteger =
    sum
  . map (uncurry (*))
  . zip [10 ^ e | e <- [0..]]
  . map digitToInt
  . reverse

getValue :: Result a -> Maybe a
getValue result =
  case result of
    Right (_, a) -> Just a
    Left _       -> Nothing

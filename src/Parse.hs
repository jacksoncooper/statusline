{-# LANGUAGE InstanceSigs #-}

module Parse
  ( parseInteger
  , runParser
  , runParserVerbose
  )
where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, digitToInt, isSpace)

-- According to 'Haskell Programming from First Principles', these sorts of
-- parsers have fallen out of style for newer designs. I'm using this
-- representation because of its similarity to the State type synonym, defined
-- in Control.Monad.Trans.State.

data State = State
  { line :: Int
  , column :: Int
  , input :: String
  } deriving Show

type Result a = Either String (State, a)

newtype Parser a =
  Parser { runParser' :: State -> Result a }

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
          runParser' (f a) state'

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \state ->
    Left "Alternative identity."

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser parser) <|> (Parser parser') =
    Parser $ \state ->
         (parser state)
      <> (parser' state)

consumeCharacter :: Parser Char
consumeCharacter =
  Parser $ \state@(State line column input) ->
    case input of
      (inputHead : restOfInput) ->
        if inputHead == '\n'
        then Right (State (line + 1) 0 restOfInput, inputHead)
        else Right (State line (column + 1) restOfInput, inputHead)
      [] ->
        Left $
          parseError state "Encountered end of input."

parseCharacter :: Char -> Parser Char
parseCharacter target =
  parseCharacterIf (target ==) $
    \character ->
         "Expected '"
      ++ [target]
      ++ "' but found '"
      ++ [character] ++ "'."

parseCharacterIf :: (Char -> Bool) -> (Char -> String) -> Parser Char
parseCharacterIf predicate explain =
  Parser $ \state ->
    let result = runParser' consumeCharacter state in
      case result of
        Right (state', character) ->
          if predicate character
          then result
          else Left $ parseError state (explain character)
        Left _ -> result

parseString :: String -> Parser String
parseString = traverse parseCharacter

parseWhitespace :: Parser String
parseWhitespace =
  some $
    parseCharacterIf isSpace $
      \character ->
           "Expected whitespace but found '"
        ++ [character]
        ++ "."

parseDigit :: Parser Int
parseDigit =
  fmap digitToInt $
    parseCharacterIf isDigit $
      \character ->
         "Expected a digit but found '"
      ++ [character]
      ++ "'."

parseInteger :: Parser Int
parseInteger = concatIntegers <$> some parseDigit

parseError :: State -> String -> String
parseError state reason =
     "Parse error at line "
  ++ show (line state)
  ++ ", column "
  ++ show (column state)
  ++ ": "
  ++ reason

toState :: String -> State
toState = State 0 0

getValue :: Result a -> Maybe a
getValue result =
  case result of
    Right (_, a) -> Just a
    Left _       -> Nothing

runParserVerbose :: Parser a -> String -> Result a
runParserVerbose parser = runParser' parser . toState

runParser :: Parser a -> String -> Maybe a
runParser parser = getValue . runParserVerbose parser

concatIntegers :: [Int] -> Int
concatIntegers =
    sum
  . map (uncurry (*))
  . zip [10 ^ e | e <- [0..]]
  . reverse

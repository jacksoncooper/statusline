{-# LANGUAGE InstanceSigs #-}

module Parse
  ( parseDigits
  , parseWhile
  , runParser
  , runParserVerbose
  )
where

import Data.Char (isDigit, digitToInt)

-- According to 'Haskell Programming from First Principles', these sorts of
-- parsers have fallen out of style for newer designs. I'm using this
-- representation because of its similarity to the State type synonym, defined
-- in Control.Monad.Trans.State.

data State = State
  { line :: Int
  , column :: Int
  , input :: String
  } deriving Show

type ParseResult a = Either String (State, a)

newtype Parser a =
  Parser { runParser' :: State -> ParseResult a }

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

parseCharacter :: Char -> Parser Char
parseCharacter character =
  Parser $ \state@(State line column input) ->
    case input of
      (inputHead : restOfInput)
        | inputHead == character ->
            if inputHead == '\n'
            then Right (State (line + 1) 0 restOfInput, inputHead)
            else Right (State line (column + 1) restOfInput, inputHead)
        | otherwise ->
            Left $ parseError state $
                 "Expected '" ++ [character]
              ++ "' but found '" ++ [inputHead]
              ++ "'."
      [] ->
        Left $ parseError state "Encountered end of input."

parseString :: String -> Parser String
parseString = traverse parseCharacter

parseDigits :: Parser Int
parseDigits = stringToInteger <$> parseWhile isDigit

parseWhile :: (Char -> Bool) -> Parser String
parseWhile predicate = Parser $ \state ->
  let string = takeWhile predicate (input state)
  in runParser' (parseString string) state

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

getValue :: ParseResult a -> Maybe a
getValue result =
  case result of
    Right (_, a) -> Just a
    Left _       -> Nothing

runParserVerbose :: Parser a -> String -> ParseResult a
runParserVerbose parser = runParser' parser . toState

runParser :: Parser a -> String -> Maybe a
runParser parser = getValue . runParserVerbose parser

stringToInteger :: String -> Int
stringToInteger =
    sum
  . map (uncurry (*))
  . zip [10 ^ e | e <- [0..]]
  . map digitToInt
  . reverse

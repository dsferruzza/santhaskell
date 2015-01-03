module Parsers (parseInput) where

import Text.Parsec
import Santhaskell

parseInput :: String -> Either ParseError [Person]
parseInput input = parse inputParser "???" input

inputParser :: Parsec String st [Person]
inputParser = sepEndBy1 name endOfLine

name :: Parsec String st Person
name = fmap Person $ many1 alphaNum

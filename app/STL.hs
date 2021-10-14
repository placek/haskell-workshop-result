-- add parsec library to cabal file

module STL (parseSTLString, Name(..), Number(..), Normal(..), Vertex(..), Facet(..), STL(..)) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char

parseSTLString :: String -> Either ParseError STL
parseSTLString stl = parse parseSTL "(stdin)" stl

newtype Name   = Name String deriving (Show)
newtype Number = Number { _n :: Double } deriving (Show)
data Normal    = Normal Number Number Number deriving (Show)
data Vertex    = Vertex Number Number Number deriving (Show)
data Facet     = Facet Normal (Vertex, Vertex, Vertex) deriving (Show)
data STL       = STL Name [Facet] deriving (Show)

(<++>) a b = (++) <$> a <*> b
(<:>) a b  = (:) <$> a <*> b

whitespace = space <|> tab

newLine = do
            endOfLine
            many whitespace

float = integer <++> decimal <++> exponent
  where number   = many1 digit
        plus     = char '+' *> number
        minus    = char '-' <:> number
        integer  = plus <|> minus <|> number
        decimal  = option "" $ char '.' <:> number
        exponent = option "" $ oneOf "eE" <:> integer

parseName :: Parser Name
parseName = do
              x <- many1 (letter <|> char '_')
              return $ Name x

parseNumber :: Parser Number
parseNumber = Number . read <$> float

parseNormal :: Parser Normal
parseNormal = do
                string "normal"
                many1 whitespace
                x <- parseNumber
                many1 whitespace
                y <- parseNumber
                many1 whitespace
                Normal x y <$> parseNumber

parseVertex :: Parser Vertex
parseVertex = do
                string "vertex"
                many1 whitespace
                x <- parseNumber
                many1 whitespace
                y <- parseNumber
                many1 whitespace
                Vertex x y <$> parseNumber

parseFacet :: Parser Facet
parseFacet = do
               string "facet"
               many1 whitespace
               n <- parseNormal
               newLine
               string "outer loop"
               newLine
               x <- parseVertex
               newLine
               y <- parseVertex
               newLine
               z <- parseVertex
               newLine
               string "endloop"
               newLine
               string "endfacet"
               newLine
               return $ Facet n (x, y,z)

parseSTL :: Parser STL
parseSTL = do
             string "solid"
             many1 whitespace
             n <- parseName
             newLine
             fs <- manyTill parseFacet (string "endsolid")
             many endOfLine
             eof
             return $ STL n fs

{-# LANGUAGE ViewPatterns #-}

module Nock.Parse (
    parse
) where 

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Control.Applicative ((<|>))

import Nock.Spec

instance Show Noun where
    show (Atom a) = show a
    show x = "[" ++ showCell x ++ "]"
        where 
            showCell (a :- b) = show a ++ " " ++ showCell b
            showCell a = show a

parse :: String -> Noun
parse (readP_to_S noun -> [(x,_)]) = x
parse _ = error "parse error"

noun :: ReadP Noun
noun = atom <|> cell

atom = Atom . read <$> munch1 isDigit
cell = foldr1 (:-) <$ char '[' <* skipSpaces <*> noun `sepBy1` skipSpaces <* skipSpaces <* char ']'


module Funcons.Operations.Characters where

import Funcons.Operations.Internal

import Data.Char (ord,chr,isAscii)

library :: (HasValues t, Ord t) => Library t
library = libFromList [
    ("ascii-characters", NullaryExpr ascii_characters)
  , ("ascii-character", UnaryExpr ascii_character)
  , ("unicode-character", UnaryExpr unicode_character)
  , ("unicode-point", UnaryExpr unicode_point)
  , ("characters", NullaryExpr characters)
  , ("unicode-characters", NullaryExpr unicode_characters)
  , ("iso-latin-1-characters", NullaryExpr iso_latin_characters)
  , ("latin-1-chars", NullaryExpr iso_latin_characters)
  , ("basic-plane-multilingual-characters", NullaryExpr bmp_characters)
  , ("bmp-chars", NullaryExpr bmp_characters)
  , ("unicode-chars", NullaryExpr unicode_characters)
  , ("unicode-points", NullaryExpr unicode_points)
  , ("basic-multilingual-plane-points", NullaryExpr bmp_points)
  ]

characters_ :: HasValues t => [OpExpr t] -> OpExpr t
characters_ = nullaryOp characters
characters :: HasValues t => OpExpr t
characters = vNullaryOp "characters" (Normal $ injectT $ Characters)

unicode_characters_ :: HasValues t => [OpExpr t] -> OpExpr t
unicode_characters_ = nullaryOp unicode_characters
unicode_characters :: HasValues t => OpExpr t
unicode_characters = vNullaryOp "unicode-characters" (Normal $ injectT $ UnicodeCharacters)

unicode_points_ :: HasValues t => [OpExpr t] -> OpExpr t
unicode_points_ = nullaryOp unicode_points
unicode_points :: HasValues t => OpExpr t
unicode_points = vNullaryOp "unicode-points" (Normal $ injectT (Intersection (IntegersFrom 0) (IntegersUpTo numUnicodeCodes)))

bmp_points_ :: HasValues t => [OpExpr t] -> OpExpr t
bmp_points_ = nullaryOp bmp_points
bmp_points :: HasValues t => OpExpr t
bmp_points = vNullaryOp "basic-multilingual-plane-points" (Normal $ injectT (Intersection (IntegersFrom 0) (IntegersUpTo numBMPCodes)))

ascii_characters_ :: HasValues t => [OpExpr t] -> OpExpr t
ascii_characters_ = nullaryOp ascii_characters
ascii_characters :: HasValues t => OpExpr t
ascii_characters = vNullaryOp "ascii-characters" (Normal $ injectT $ AsciiCharacters)

iso_latin_characters_ :: HasValues t => [OpExpr t] -> OpExpr t
iso_latin_characters_ = nullaryOp iso_latin_characters
iso_latin_characters :: HasValues t => OpExpr t
iso_latin_characters = vNullaryOp "iso-latin-1-characters" (Normal $ injectT $ ISOLatinCharacters)

bmp_characters_ :: HasValues t => [OpExpr t] -> OpExpr t
bmp_characters_ = nullaryOp bmp_characters 
bmp_characters :: HasValues t => OpExpr t
bmp_characters = vNullaryOp "basic-multilingual-plane-characters" (Normal $ injectT $ BMPCharacters)

ascii_character_ :: HasValues t => [OpExpr t] -> OpExpr t
ascii_character_ = unaryOp ascii_character 
ascii_character :: HasValues t => OpExpr t -> OpExpr t
ascii_character = vUnaryOp "ascii-character" op
  where op v | isString_ v, [c] <- unString v, isAscii c
                = Normal $ inject $ downcast_unicode_characters c
             | otherwise = SortErr "ascii-character not applied to a string (of 1 ascii character long)"

unicode_character_ :: HasValues t => [OpExpr t] -> OpExpr t
unicode_character_ = unaryOp unicode_character
unicode_character :: HasValues t => OpExpr t -> OpExpr t
unicode_character = vUnaryOp "unicode-character" op
  where op v | Int i <- upcastIntegers v, i < numUnicodeCodes, i >= 0 = 
                Normal $ inject $ mk_unicode_characters (chr $ fromInteger i)
             | otherwise = SortErr "unicode-character not applied to an integer in the right range"

numUnicodeCodes,numBMPCodes :: Integer
numUnicodeCodes = (2^21)-1
numBMPCodes = (2^17)-1


unicode_point_ :: HasValues t => [OpExpr t] -> OpExpr t
unicode_point_ = unaryOp unicode_point 
unicode_point :: HasValues t => OpExpr t -> OpExpr t
unicode_point = vUnaryOp "unicode-point" op
  where op v | Just c <- upcastCharacter v = 
                  Normal $ inject $ mk_integers (toInteger $ ord c)
             | otherwise = SortErr "unicode-point not applied to a unicode-character"

{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Number where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Decimal as Decimal
import qualified Text.Parsec as Parsec

newtype Number = MkNumber
  { unwrap :: Decimal.Decimal
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Number
decode = do
  sign <- Parsec.option id $ negate <$ Parsec.char '-'
  int <- integerPart
  frac <- Parsec.option [] fractionPart
  ex <- Parsec.option 0 exponentPart
  let mantissa = sign . fromDigits $ int <> frac
      scale = ex - toInteger (length frac)
  pure . MkNumber $ Decimal.mkDecimal mantissa scale

integerPart :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m [Integer]
integerPart =
  Parsec.choice
    [ [0] <$ Parsec.char '0' <* Parsec.notFollowedBy digit,
      (:) <$> nonZeroDigit <*> Parsec.many digit
    ]

fractionPart :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m [Integer]
fractionPart = Parsec.char '.' *> Parsec.many1 digit

exponentPart :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
exponentPart =
  Parsec.oneOf "eE" *> do
    sign <-
      Parsec.option id $
        Parsec.choice
          [ negate <$ Parsec.char '-',
            id <$ Parsec.char '+'
          ]
    ds <- Parsec.many1 digit
    pure . sign $ fromDigits ds

digit :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
digit = digitOf digits

nonZeroDigit :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Integer
nonZeroDigit = digitOf $ drop 1 digits

digitOf :: (Parsec.Stream s m Char) => [(Char, Integer)] -> Parsec.ParsecT s u m Integer
digitOf = Parsec.choice . fmap (\(c, n) -> n <$ Parsec.char c)

digits :: [(Char, Integer)]
digits =
  [ ('0', 0),
    ('1', 1),
    ('2', 2),
    ('3', 3),
    ('4', 4),
    ('5', 5),
    ('6', 6),
    ('7', 7),
    ('8', 8),
    ('9', 9)
  ]

fromDigits :: [Integer] -> Integer
fromDigits = foldl' (\n -> ((10 * n) +)) 0

encode :: Number -> Builder.Builder
encode n =
  let d = unwrap n
   in Builder.integerDec (Decimal.mantissa d)
        <> if Decimal.exponent d == 0
          then mempty
          else Builder.charUtf8 'e' <> Builder.integerDec (Decimal.exponent d)

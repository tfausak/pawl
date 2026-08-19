{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.PairSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Pair" $ do
  let pair :: String -> a -> Pair.Pair a
      pair = Pair.MkPair . String.MkString . Text.pack

  Spec.describe s "decode" $ do
    let p :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m String
        p = Parsec.many1 Parsec.digit

    Spec.it s "succeeds with simple pair" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) "\"a\":1") . Just $ pair "a" "1"

    Spec.it s "succeeds with blank space after name" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) "\"a\" :1") . Just $ pair "a" "1"

    Spec.it s "succeeds with blank space after separator" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) "\"a\": 1") . Just $ pair "a" "1"

    Spec.it s "fails with missing name" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) ":1") Nothing

    Spec.it s "fails with missing separator" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) "\"a\" 1") Nothing

    Spec.it s "fails with extra separator" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) "\"a\"::1") Nothing

    Spec.it s "fails with missing value" $ do
      Spec.assertEq s (Parsec.parseString (Pair.decode p) ":1") Nothing

  Spec.describe s "encode" $ do
    let b = Builder.integerDec

    Spec.it s "encodes simple pair" $ do
      Spec.assertEq s (Builder.toString . Pair.encode b $ pair "a" 1) "\"a\":1"

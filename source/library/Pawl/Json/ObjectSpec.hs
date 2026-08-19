{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.ObjectSpec where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Object" $ do
  let object :: [(String, a)] -> Object.Object a
      object = Object.MkObject . fmap (\(n, v) -> Pair.MkPair (String.MkString $ Text.pack n) v)

  Spec.describe s "decode" $ do
    let p :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m Prelude.String
        p = Parsec.many1 Parsec.digit

    Spec.it s "succeeds with an empty object" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{}") . Just $ object []

    Spec.it s "succeeds with an empty object with blank space" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{ }") . Just $ object []

    Spec.it s "succeeds with a single pair" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{\"a\":1}") . Just $ object [("a", "1")]

    Spec.it s "succeeds with a single pair with blank space" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{ \"a\":1 }") . Just $ object [("a", "1")]

    Spec.it s "succeeds with multiple pairs" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{\"a\":1,\"b\":2}") . Just $ object [("a", "1"), ("b", "2")]

    Spec.it s "succeeds with multiple pairs with blank space" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{ \"a\":1 , \"b\":2 }") . Just $ object [("a", "1"), ("b", "2")]

    Spec.it s "fails with leading comma" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{,\"a\":1}") Nothing

    Spec.it s "fails with trailing comma" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{\"a\":1,}") Nothing

    Spec.it s "fails with extra comma" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{\"a\":1,,\"b\":2}") Nothing

    Spec.it s "fails with only comma" $ do
      Spec.assertEq s (Parsec.parseString (Object.decode p) "{,}") Nothing

  Spec.describe s "encode" $ do
    let b = Builder.integerDec

    Spec.it s "encodes empty object" $ do
      Spec.assertEq s (Builder.toString . Object.encode b $ object []) "{}"

    Spec.it s "encodes single pair" $ do
      Spec.assertEq s (Builder.toString . Object.encode b $ object [("a", 1)]) "{\"a\":1}"

    Spec.it s "encodes multiple pairs" $ do
      Spec.assertEq s (Builder.toString . Object.encode b $ object [("a", 1), ("b", 2)]) "{\"a\":1,\"b\":2}"

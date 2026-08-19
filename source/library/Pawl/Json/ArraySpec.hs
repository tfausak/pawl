{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.ArraySpec where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Array as Array
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Array" $ do
  Spec.describe s "decode" $ do
    let p :: (Parsec.Stream t m Char) => Parsec.ParsecT t u m String
        p = Parsec.many1 Parsec.digit

    Spec.it s "succeeds with an empty array" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[]") . Just $ Array.MkArray []

    Spec.it s "succeeds with an empty array with blank space" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[ ]") . Just $ Array.MkArray []

    Spec.it s "succeeds with a single element" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[1]") . Just $ Array.MkArray ["1"]

    Spec.it s "succeeds with a single element with blank space" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[ 1 ]") . Just $ Array.MkArray ["1"]

    Spec.it s "succeeds with multiple elements" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[1,2]") . Just $ Array.MkArray ["1", "2"]

    Spec.it s "succeeds with multiple elements with blank space" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[ 1 , 2 ]") . Just $ Array.MkArray ["1", "2"]

    Spec.it s "fails with leading comma" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[,1]") Nothing

    Spec.it s "fails with trailing comma" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[1,]") Nothing

    Spec.it s "fails with extra comma" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[1,,2]") Nothing

    Spec.it s "fails with only comma" $ do
      Spec.assertEq s (Parsec.parseString (Array.decode p) "[,]") Nothing

  Spec.describe s "encode" $ do
    let b = Builder.integerDec

    Spec.it s "encodes empty array" $ do
      Spec.assertEq s (Builder.toString . Array.encode b $ Array.MkArray []) "[]"

    Spec.it s "encodes single element" $ do
      Spec.assertEq s (Builder.toString . Array.encode b $ Array.MkArray [1]) "[1]"

    Spec.it s "encodes multiple elements" $ do
      Spec.assertEq s (Builder.toString . Array.encode b $ Array.MkArray [1, 2]) "[1,2]"

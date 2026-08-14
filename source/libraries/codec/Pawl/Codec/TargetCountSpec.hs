{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TargetCountSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.TargetCount as TargetCount
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TargetCount as TargetCount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetCount" $ do
  Spec.it s "exactly one" $
    Common.assertCodec
      s
      TargetCount.codec
      TargetCount.one
      """ {"least":1,"most":1} """
  -- CR 115.6's "up to one", and CR 601.2c's larger version of it.
  Spec.it s "up to two" $
    Common.assertCodec
      s
      TargetCount.codec
      (TargetCount.upTo 2)
      """ {"least":0,"most":2} """
  -- Hearts on Fire's "one or two target creatures": a range whose minimum is
  -- neither zero nor its maximum.
  Spec.it s "one or two" $
    Common.assertCodec
      s
      TargetCount.codec
      TargetCount.MkTargetCount {TargetCount.least = 1, TargetCount.most = Just 2}
      """ {"least":1,"most":2} """
  -- CR 601.2c's "any number of target ..." (Soulfire Eruption): no printed
  -- maximum, so the key is absent both ways.
  Spec.it s "any number" $
    Common.assertCodec
      s
      TargetCount.codec
      TargetCount.anyNumber
      """ {"least":0} """
  -- An explicit null says the same thing, and re-encodes as the absent key.
  Spec.it s "reads a null maximum as any number" $
    Spec.assertEqWith
      s
      "null most decodes as unbounded"
      (Common.parse (Text.pack """ {"least":0,"most":null} """) >>= Codec.decode TargetCount.codec)
      (Right TargetCount.anyNumber)
  -- The two invariants the type states and the decoder keeps.
  Spec.it s "rejects a slot that takes no target" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"least":0,"most":0} """) >>= Codec.decode TargetCount.codec))
      "expected a decode failure"
  Spec.it s "rejects a minimum above the maximum" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"least":2,"most":1} """) >>= Codec.decode TargetCount.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $
    Common.assertHasSchema s TargetCount.codec

{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TargetCountSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.TargetCount as TargetCount
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TargetCount as TargetCount

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetCount" $ do
  Spec.it s "exactly one" $
    Common.assertJsonCodec
      s
      TargetCount.toJson
      TargetCount.fromJson
      TargetCount.one
      """ {"least":1,"most":1} """
  -- CR 115.6's "up to one", and CR 601.2c's larger version of it.
  Spec.it s "up to two" $
    Common.assertJsonCodec
      s
      TargetCount.toJson
      TargetCount.fromJson
      (TargetCount.upTo 2)
      """ {"least":0,"most":2} """
  -- Hearts on Fire's "one or two target creatures": a range whose minimum is
  -- neither zero nor its maximum.
  Spec.it s "one or two" $
    Common.assertJsonCodec
      s
      TargetCount.toJson
      TargetCount.fromJson
      TargetCount.MkTargetCount {TargetCount.least = 1, TargetCount.most = 2}
      """ {"least":1,"most":2} """
  -- The two invariants the type states and the decoder keeps.
  Spec.it s "rejects a slot that takes no target" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"least":0,"most":0} """) >>= TargetCount.fromJson))
      "expected a decode failure"
  Spec.it s "rejects a minimum above the maximum" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"least":2,"most":1} """) >>= TargetCount.fromJson))
      "expected a decode failure"

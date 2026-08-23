{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.IgnoredAbility where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility

codec :: Codec.Codec IgnoredAbility.IgnoredAbility
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec IgnoredAbility.player
  source <- Fields.required "source" ObjectId.codec IgnoredAbility.source
  expiry <- Fields.required "expiry" Expiry.codec IgnoredAbility.expiry
  pure
    IgnoredAbility.MkIgnoredAbility
      { IgnoredAbility.player = player,
        IgnoredAbility.source = source,
        IgnoredAbility.expiry = expiry
      }

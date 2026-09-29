{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockersAre where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockersAre as BlockersAre

-- | @"blockers": null@ is an unblocked attacker, and @[]@ one blocked by nothing.
codec :: Codec.Codec BlockersAre.BlockersAre
codec = Fields.object $ do
  attacker <- Fields.required "attacker" Reference.codec BlockersAre.attacker
  blockers <- Fields.required "blockers" (Common.maybe (Common.set Reference.codec)) BlockersAre.blockers
  pure BlockersAre.MkBlockersAre {BlockersAre.attacker = attacker, BlockersAre.blockers = blockers}

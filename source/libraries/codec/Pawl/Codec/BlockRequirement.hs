{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockRequirement where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockRequirement as BlockRequirement

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec BlockRequirement.BlockRequirement
codec = Fields.object $ do
  attacker <- Fields.required "attacker" Affected.codec BlockRequirement.attacker
  pure BlockRequirement.MkBlockRequirement {BlockRequirement.attacker = attacker}

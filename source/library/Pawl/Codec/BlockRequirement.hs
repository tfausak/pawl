{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockRequirement where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockRequirement as BlockRequirement

-- | Both of CR 509.1c's axes default to absent, which is "unrestricted on that
-- axis" -- so Lure's `attacker` alone and Razorgrass Screen's `subject` alone are
-- each a whole requirement, and neither card writes the other key.
codec :: Codec.Codec BlockRequirement.BlockRequirement
codec = Fields.object $ do
  subject <- Fields.defaulted "subject" Nothing (Common.maybe Affected.codec) BlockRequirement.subject
  attacker <- Fields.defaulted "attacker" Nothing (Common.maybe Affected.codec) BlockRequirement.attacker
  pure
    BlockRequirement.MkBlockRequirement
      { BlockRequirement.subject = subject,
        BlockRequirement.attacker = attacker
      }

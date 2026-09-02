{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AimedAt where

import qualified Pawl.Codec.AttackTargetKind as AttackTargetKind
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AimedAt as AimedAt

-- | Pawl.Codec.CantAttackPlayer's two aimed-at keys, and "kinds" required for
-- that codec's reason: which of CR 506.3's three things a printing names is the
-- sentence itself.
codec :: Codec.Codec AimedAt.AimedAt
codec = Fields.object $ do
  defenders <- Fields.required "defenders" PlayerScope.codec AimedAt.defenders
  kinds <- Fields.required "kinds" (Common.set AttackTargetKind.codec) AimedAt.kinds
  pure
    AimedAt.MkAimedAt
      { AimedAt.defenders = defenders,
        AimedAt.kinds = kinds
      }

{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackCost where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.AttackCostScope as AttackCostScope
import qualified Pawl.Codec.PerCreature as PerCreature
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackCost as AttackCost

-- | "subject" is Pawl.Codec.AttackRequirement's key and names the same axis:
-- the creatures the effect is ABOUT, never Pawl.Codec.BlockRequirement's
-- "attacker", which is the object CR 509.1c's requirements carry instead.
-- "perAttacker" is one attacker's share, not the card's whole cost -- a "for
-- each" repeats it per taxed attacker before CR 508.1h totals the declaration.
--
-- "scope" is Fields.required and not defaulted to the narrow arm, though the
-- narrow arm IS the default reading of an unqualified "you". A defaulted key
-- would let a card transcribed a clause short play as a Ghostly Prison -- a
-- weaker card than printed, in the attacking player's favour -- so every card
-- states which family it belongs to.
codec :: Codec.Codec AttackCost.AttackCost
codec = Fields.object $ do
  subject <- Fields.required "subject" Affected.codec AttackCost.subject
  perAttacker <- Fields.required "perAttacker" PerCreature.codec AttackCost.perAttacker
  scope <- Fields.required "scope" AttackCostScope.codec AttackCost.scope
  pure
    AttackCost.MkAttackCost
      { AttackCost.subject = subject,
        AttackCost.perAttacker = perAttacker,
        AttackCost.scope = scope
      }

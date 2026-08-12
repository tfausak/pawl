{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AttackRequirementSpec where

import qualified Pawl.Codec.AttackRequirement as AttackRequirement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- Curse of the Nightly Hunt's shape (CR 303.4m read through the enchanted
  -- player): AttachedPlayerControls, unlike BlockRequirement's Attached/Matching
  -- pair, since an attacking requirement's subject can be a whole controlled set.
  Spec.describe s "Pawl.Codec.AttackRequirement" . Spec.it s "MkAttackRequirement" $
    Common.assertJsonCodec
      s
      AttackRequirement.toJson
      AttackRequirement.fromJson
      (AttackRequirement.MkAttackRequirement (Affected.AttachedPlayerControls (Filter.HasCardType CardType.Creature)))
      """ {"subject":{"type":"AttachedPlayerControls","value":{"type":"HasCardType","value":{"type":"Creature"}}}} """

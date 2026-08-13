{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivatedAbility where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ActivationRestriction as ActivationRestriction
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (ActivatedAbility.ActivatedAbility card)
codec cardCodec = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) ActivatedAbility.cost
  modal <- Fields.required "modal" (Modal.codec cardCodec) ActivatedAbility.modal
  -- CR 602.5: emitted only for a restricted ability, so the absence of the key
  -- is CR 602.2's default -- no "activate only ..." rider at all.
  restrictions <- Fields.defaulted "restrictions" [] (Common.list ActivationRestriction.codec) ActivatedAbility.restrictions
  -- CR 702.178a: emitted only for a GRANTED ability, so the absence of the key
  -- means the object simply has this ability.
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) ActivatedAbility.condition
  pure
    ActivatedAbility.MkActivatedAbility
      { ActivatedAbility.cost = cost,
        ActivatedAbility.modal = modal,
        ActivatedAbility.restrictions = restrictions,
        ActivatedAbility.condition = condition
      }

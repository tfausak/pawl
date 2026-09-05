{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivatedAbility where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.ActivationRestriction as ActivationRestriction
import qualified Pawl.Codec.Activator as Activator
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Activator as Activator.Type

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card, Typeable.Typeable ability, Eq ability) => Codec.Codec card -> Codec.Codec ability -> Codec.Codec (ActivatedAbility.ActivatedAbility card ability)
codec cardCodec abilityCodec = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) ActivatedAbility.cost
  -- CR 101.1: the ceilings this ability's own words put on CR 602.2b's announced
  -- X (Pawl.Types.ActivatedAbility), emitted only for an ability that states one.
  maximumX <- Fields.defaulted "maximumX" [] (Common.list Quantity.codec) ActivatedAbility.maximumX
  modal <- Fields.required "modal" (Modal.codec cardCodec abilityCodec) ActivatedAbility.modal
  -- CR 602.5: emitted only for a restricted ability, so the absence of the key
  -- is CR 602.2's default -- no "activate only ..." rider at all.
  restrictions <- Fields.defaulted "restrictions" [] (Common.list ActivationRestriction.codec) ActivatedAbility.restrictions
  -- CR 602.1b: emitted only for an ability that names its activators, so the
  -- absence of the key is CR 602.2's default -- the controller alone.
  activator <- Fields.defaulted "activator" Activator.Type.Controller Activator.codec ActivatedAbility.activator
  -- CR 702.178a: emitted only for a GRANTED ability, so the absence of the key
  -- means the object simply has this ability.
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) ActivatedAbility.condition
  -- CR 613.1f: emitted only for an ability another clause of the same card refers
  -- to, so the absence of the key means nothing names it.
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) ActivatedAbility.name
  -- CR 702: the keyword whose rules the ability is under, which only
  -- Pawl.Engine.Keyword's minters set -- so the key is absent from every ability
  -- in data/cards/, and Pawl.CardSpec's "CR 702 no card claims a keyword minted
  -- an ability it printed" is the lint that keeps it that way. It is on the wire
  -- at all because Pawl.Codec.GameState reaches this codec through
  -- Pawl.Codec.Object: an ability already on the stack carries its stamp, and a
  -- state that round-tripped without the key would come back a printed ability.
  keyword <- Fields.defaulted "keyword" Nothing (Common.maybe Keyword.codec) ActivatedAbility.keyword
  pure
    ActivatedAbility.MkActivatedAbility
      { ActivatedAbility.cost = cost,
        ActivatedAbility.maximumX = maximumX,
        ActivatedAbility.modal = modal,
        ActivatedAbility.restrictions = restrictions,
        ActivatedAbility.activator = activator,
        ActivatedAbility.condition = condition,
        ActivatedAbility.name = name,
        ActivatedAbility.keyword = keyword
      }

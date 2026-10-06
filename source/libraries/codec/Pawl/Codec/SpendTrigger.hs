{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpendTrigger where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpendTrigger as SpendTrigger

-- | Game state, so every key is required, Pawl.Codec.ManaUnit's reason.
codec :: Codec.Codec SpendTrigger.SpendTrigger
codec = Fields.object $ do
  casts <- Fields.required "casts" (Filter.codec Keyword.codec) SpendTrigger.casts
  ability <- Fields.required "ability" (TriggeredAbility.codec Card.codec (GrantedAbility.codec Card.codec)) SpendTrigger.ability
  source <- Fields.required "source" ObjectId.codec SpendTrigger.source
  controller <- Fields.required "controller" PlayerId.codec SpendTrigger.controller
  pure
    SpendTrigger.MkSpendTrigger
      { SpendTrigger.casts = casts,
        SpendTrigger.ability = ability,
        SpendTrigger.source = source,
        SpendTrigger.controller = controller
      }

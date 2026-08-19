{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Create where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Create as Create

-- | A bare object keyed by the record's field names, replacing the positional
-- payload this arm used to write.
--
-- That payload was FOUR shapes: two, three or four elements, and the
-- three-element form was itself two shapes told apart by JSON TYPE, since a slot
-- name is a string and riders are an object. Two independently elided keys say
-- all of it with nothing to disambiguate.
--
-- The card codec is a PARAMETER, the posture Pawl.Codec.Filter takes with its
-- keyword: this arm is where card data nests inside card data, and
-- Pawl.Codec.Card is what ties the knot.
codec :: (Typeable.Typeable card) => Codec.Codec card -> Codec.Codec (Create.Create card)
codec cardCodec = Fields.object $ do
  quantity <- Fields.required "quantity" Quantity.codec Create.quantity
  card <- Fields.required "card" cardCodec Create.card
  riders <- Fields.defaulted "riders" EntryRiders.defaultValue EntryRiders.codec Create.riders
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) Create.slot
  pure
    Create.MkCreate
      { Create.quantity = quantity,
        Create.card = card,
        Create.riders = riders,
        Create.slot = slot
      }

{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Prototype where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Prototype as Prototype

-- | A bare object keyed by the record's field names, Pawl.Codec.Equip's shape.
-- No keyword parameter, unlike that one: CR 718.1's inset frame holds a mana
-- cost and a printed box, neither of which can name a Filter or a Cost.
codec :: Codec.Codec Prototype.Prototype
codec = Fields.object $ do
  cost <- Fields.required "cost" ManaCost.codec Prototype.cost
  power <- Fields.required "power" Common.integer Prototype.power
  toughness <- Fields.required "toughness" Common.integer Prototype.toughness
  pure Prototype.MkPrototype {Prototype.cost = cost, Prototype.power = power, Prototype.toughness = toughness}

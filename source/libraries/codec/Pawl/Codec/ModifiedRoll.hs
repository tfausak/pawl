{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ModifiedRoll where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.RollModifier as RollModifier
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ModifiedRoll as ModifiedRoll

-- | A bare object keyed by the record's field names. Both narrowings and the CR
-- 706.2a cost default to Nothing, so a card writes only the ones its sentence
-- states.
codec :: Codec.Codec ModifiedRoll.ModifiedRoll
codec = Fields.object $ do
  sides <- Fields.defaulted "sides" Nothing (Common.maybe Common.natural) ModifiedRoll.sides
  natural <- Fields.defaulted "natural" Nothing (Common.maybe Common.natural) ModifiedRoll.natural
  modifier <- Fields.required "modifier" RollModifier.codec ModifiedRoll.modifier
  cost <- Fields.defaulted "cost" Nothing (Common.maybe (Cost.codec Keyword.codec)) ModifiedRoll.cost
  pure
    ModifiedRoll.MkModifiedRoll
      { ModifiedRoll.sides = sides,
        ModifiedRoll.natural = natural,
        ModifiedRoll.modifier = modifier,
        ModifiedRoll.cost = cost
      }

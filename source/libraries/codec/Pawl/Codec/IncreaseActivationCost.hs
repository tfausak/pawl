{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.IncreaseActivationCost where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.IncreaseActivationCost as IncreaseActivationCost

-- | A bare object keyed by the record's field names, the shape
-- Pawl.Codec.IncreaseSpellCost's own haddock settled for the spell half.
codec :: Codec.Codec IncreaseActivationCost.IncreaseActivationCost
codec = Fields.object $ do
  whichAbilities <- Fields.required "whichAbilities" (Filter.codec Keyword.codec) IncreaseActivationCost.whichAbilities
  amount <- Fields.required "amount" Common.natural IncreaseActivationCost.amount
  pure
    IncreaseActivationCost.MkIncreaseActivationCost
      { IncreaseActivationCost.whichAbilities = whichAbilities,
        IncreaseActivationCost.amount = amount
      }

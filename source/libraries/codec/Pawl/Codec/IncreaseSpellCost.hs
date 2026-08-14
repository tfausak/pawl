{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.IncreaseSpellCost where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec IncreaseSpellCost.IncreaseSpellCost
codec = Fields.object $ do
  whichSpells <- Fields.required "whichSpells" (Filter.codec Keyword.codec) IncreaseSpellCost.whichSpells
  amount <- Fields.required "amount" Common.natural IncreaseSpellCost.amount
  pure
    IncreaseSpellCost.MkIncreaseSpellCost
      { IncreaseSpellCost.whichSpells = whichSpells,
        IncreaseSpellCost.amount = amount
      }

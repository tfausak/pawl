{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ReduceSpellCost where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec ReduceSpellCost.ReduceSpellCost
codec = Fields.object $ do
  whichSpells <- Fields.required "whichSpells" (Filter.codec Keyword.codec) ReduceSpellCost.whichSpells
  reduction <- Fields.required "reduction" ManaCost.codec ReduceSpellCost.reduction
  pure
    ReduceSpellCost.MkReduceSpellCost
      { ReduceSpellCost.whichSpells = whichSpells,
        ReduceSpellCost.reduction = reduction
      }

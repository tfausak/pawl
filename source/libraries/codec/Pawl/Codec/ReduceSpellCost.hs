{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ReduceSpellCost where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec ReduceSpellCost.ReduceSpellCost
codec = Fields.object $ do
  whichSpells <- Fields.required "whichSpells" (Filter.codec Keyword.codec) ReduceSpellCost.whichSpells
  reduction <- Fields.required "reduction" ManaCost.codec ReduceSpellCost.reduction
  -- DEFAULTED to False, which is CR 118.7b-d's spill: only a card printing
  -- Edgewalker's restricting sentence writes the key.
  coloredOnly <- Fields.defaulted "coloredOnly" False Common.boolean ReduceSpellCost.coloredOnly
  pure
    ReduceSpellCost.MkReduceSpellCost
      { ReduceSpellCost.whichSpells = whichSpells,
        ReduceSpellCost.reduction = reduction,
        ReduceSpellCost.coloredOnly = coloredOnly
      }

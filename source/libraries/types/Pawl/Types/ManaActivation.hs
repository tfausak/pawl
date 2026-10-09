module Pawl.Types.ManaActivation where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ObjectId as ObjectId

-- | One mana ability a payer activated in a CR 605.3a mana window, as CR 733.1
-- offers it back: its sources, and the mana it took from and gave to the
-- payer's pool, which the rule's "unless" clause reads.
--
-- Deliberately no codec: a payment in flight is never serialised.
data ManaActivation = MkManaActivation
  { -- | The source whose ability this was -- or, where re-based activations
    -- could not be told apart, every one of their sources.
    sources :: NonEmpty.NonEmpty ObjectId.ObjectId,
    -- | The mana its cost took from the payer's pool (CR 602.2b).
    spent :: [ManaUnit.ManaUnit],
    -- | The mana it, and the triggered mana abilities it caused, added to the
    -- payer's pool (CR 106.4).
    added :: [ManaUnit.ManaUnit]
  }

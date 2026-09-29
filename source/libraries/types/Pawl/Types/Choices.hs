module Pawl.Types.Choices where

import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Reference as Reference

-- | The answers a cast or an activation's own prompts take (CR 601.2b-h, CR
-- 602.2b), carried by that move so two at one moment cannot take each other's.
-- Nothing, or an empty sequence, answers no prompt.
data Choices = MkChoices
  { targets :: Maybe [Reference.Reference],
    modes :: Maybe (Seq.Seq ModeIndex.ModeIndex),
    x :: Maybe Natural,
    cost :: Maybe ManaCost.ManaCost,
    -- | CR 601.2h's permutation of the non-mana cost components' printed
    -- indices.
    costOrder :: Maybe [Natural],
    -- | Each source in the order asked, Nothing declining one.
    manaSources :: Seq.Seq (Maybe Reference.Reference),
    manaYields :: Seq.Seq Mana.Mana
  }
  deriving (Eq, Ord, Show)

none :: Choices
none =
  MkChoices
    { targets = Nothing,
      modes = Nothing,
      x = Nothing,
      cost = Nothing,
      costOrder = Nothing,
      manaSources = Seq.empty,
      manaYields = Seq.empty
    }

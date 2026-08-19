module Pawl.Types.ManaOption where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana

-- | CR 106.12: ONE way to tap a permanent for mana -- the cost CR 602.2b makes
-- that activation pay, paired with the whole yield it adds.
--
-- Both halves, because either alone loses a distinction the player is entitled
-- to make. The yield alone cannot separate an Urborg'd Mana Confluence's free
-- {B} from the {B} it charges 1 life for (#1117); the cost alone cannot separate
-- Birds of Paradise's five colours (CR 105.4).
--
-- The cost VALUE rather than an ordinal into the source's mana abilities, for
-- Pawl.Types.ReplacementEntry's reason: two abilities alike in cost and yield
-- have to stay interchangeable, which value equality gives and an ordinal does
-- not -- and there is no one list to index into, CR 305.6's intrinsic ability
-- being printed on no card and a member of nothing.
data ManaOption = MkManaOption
  { cost :: Cost.Cost Keyword.Keyword,
    yield :: Mana.Mana
  }
  deriving (Eq, Ord, Show)

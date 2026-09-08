module Pawl.Types.PermanentsBecomeTargeted where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.StackObjectKind as StackObjectKind

-- | CR 603.2c's batch reading of CR 601.2c from a BYSTANDER's side: which
-- permanents becoming targets fire the ability, and which announcement road the
-- targeting object came by. Professor Hojo's "whenever one or more creatures you
-- control become the target of an activated ability".
--
-- Two fields rather than two constructors, Pawl.Types.ControllerBecomesTarget's
-- shape: both narrow one rule 601.2c announcement and differ only in how far.
data PermanentsBecomeTargeted = MkPermanentsBecomeTargeted
  { -- | The permanents the condition watches, read live off the battlefield --
    -- CR 601.2c makes them targets while they are still there.
    --
    -- Read at the CR 117.5 boundary the batch is gathered on rather than at the
    -- announcement's own moment, which diverges on a board Rune-Brand Juggler
    -- reaches (gap #3418).
    filter :: Filter.Filter Keyword.Keyword,
    -- | Which of CR 601.2c's announcement roads, the sibling
    -- Pawl.Types.ControllerBecomesTarget's field one recipient over. Nothing is
    -- "a spell or ability"; Just ActivatedAbility is Professor Hojo's.
    kind :: Maybe StackObjectKind.StackObjectKind
  }
  deriving (Eq, Ord, Show)

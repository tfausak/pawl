module Pawl.Types.PermanentsBecomeTargeted where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.StackObjectKind as StackObjectKind

-- | CR 601.2c from a BYSTANDER's side: which permanents becoming targets fire
-- the ability, and which announcement road the targeting object came by.
-- Professor Hojo's "whenever one or more creatures you control become the target
-- of an activated ability" (CR 603.2c's batch) and Venerated Rotpriest's
-- per-creature "whenever a creature you control becomes the target of a spell".
--
-- Two fields rather than two constructors, Pawl.Types.ControllerBecomesTarget's
-- shape: both narrow one rule 601.2c announcement and differ only in how far.
data PermanentsBecomeTargeted = MkPermanentsBecomeTargeted
  { -- | The permanents the condition watches, read off CR 603.10's sample of
    -- the battlefield at the announcement (Event.becameTarget).
    filter :: Filter.Filter Keyword.Keyword,
    -- | Which of CR 601.2c's announcement roads, the sibling
    -- Pawl.Types.ControllerBecomesTarget's field one recipient over. Nothing is
    -- "a spell or ability"; Just ActivatedAbility is Professor Hojo's.
    kind :: Maybe StackObjectKind.StackObjectKind
  }
  deriving (Eq, Ord, Show)

module Pawl.Types.MergeComponent where

import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 730.2: one component of a merged permanent -- "the card or copy that
-- represented that object in addition to any other components that were
-- representing it". What represents the component, plus which KIND of component
-- it is.
--
-- The kind is carried because CR 730.2d asks it -- "if a merged permanent
-- contains a token, the resulting permanent is a token only if the topmost
-- component is a token" -- and CR 730.3's departure asks it again: a card
-- component is put into a zone and a token component ceases to exist there (CR
-- 111.7). A bare printing cannot answer either, tokens being interned as
-- printings like everything else.
--
-- A SEPARATE type rather than Pawl.Types.Source itself, which would make Source
-- recursive and admit an ability or an emblem as a component. These are the
-- shapes rule 730.2's sentence names, and no more.
--
-- POSITIONAL rather than a shared `printing` field, which the meld arm cannot
-- carry: two cards represent that component and neither is the printing its
-- characteristics come off (CR 712.8g). Pawl.Engine.Game.printingOfComponent is
-- the total read that field used to be, beside the other classifiers over this
-- type.
data MergeComponent
  = -- | CR 108.2: a card component, named by its entry in GameState.printings.
    OfCard PrintingId.PrintingId
  | -- | CR 111.3: a token component, named by the printing its effect-defined
    -- characteristics were interned as.
    OfToken PrintingId.PrintingId
  | -- | CR 701.42a: a melded component -- one component that two cards represent,
    -- which Pawl.Types.MeldSource carries and documents.
    --
    -- ONE component rather than one per card, which is the only reading under
    -- which CR 730.2a is answerable: rule 701.42a puts the two cards down as "a
    -- single object", giving them no order between them, so a model that made
    -- each its own component would leave "its topmost component" with two heads
    -- and no tie-break. CR 730.2c is what keeps the meld intact across the merge
    -- -- "a merged permanent is the same object that it was before" -- so CR
    -- 712.8g still gives that component only the combined back face, which is
    -- what Pawl.Engine.Game.printingOfComponent answers with.
    --
    -- CR 730.3's departure still puts down two CARDS, not a melded permanent:
    -- Pawl.Engine.Game.componentsOf expands this arm into rule 712.21's two
    -- cards, so every reader of that classifier keeps quantifying over cards.
    OfMeld MeldSource.MeldSource
  | -- | CR 730.2's "or copy": a copy of a spell as a component, which CR 702.140c
    -- puts here when a copy of a mutating creature spell resolves.
    --
    -- NEITHER of the two arms above, and CR 608.3f is why: a copy of a permanent
    -- spell "will become a token permanent as it is put onto the battlefield",
    -- and rule 702.140c's merge is the one resolution that puts it nowhere -- so
    -- the copy is still a copy here, and it is no card either (CR 707.10, "even
    -- though it has no spell card associated with it"). Pawl.Engine.Game's
    -- componentIsToken answers False for it under CR 730.2d and componentIsCard
    -- answers False under CR 108.2, which is what keeps CR 903.9c from finding a
    -- commander in it.
    OfSpellCopy PrintingId.PrintingId
  deriving (Eq, Ord, Show)

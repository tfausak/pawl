module Pawl.Types.MergeComponent where

import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 730.2: one component of a merged permanent -- "the card or copy that
-- represented that object in addition to any other components that were
-- representing it". The printing it is represented by, plus which KIND of
-- component it is.
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
-- Not implemented: CR 707.10's copy of a mutating creature spell, which rule
-- 730.2's "or copy" admits as a component (#3431).
data MergeComponent
  = -- | CR 108.2: a card component, named by its entry in GameState.printings.
    OfCard {printing :: PrintingId.PrintingId}
  | -- | CR 111.3: a token component, named by the printing its effect-defined
    -- characteristics were interned as.
    OfToken {printing :: PrintingId.PrintingId}
  deriving (Eq, Ord, Show)

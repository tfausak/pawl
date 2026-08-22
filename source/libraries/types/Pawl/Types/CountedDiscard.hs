module Pawl.Types.CountedDiscard where

import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.9b's discard: the ONE player the slot names discards this many cards,
-- choosing which ones. Mind Rot's "target player discards two cards".
--
-- Not implemented: a slot naming several players discards nothing, so "each
-- player discards a card" has no spelling (#1965). Its sibling
-- Pawl.Types.PlayerSacrifices does fold over every seat the slot holds.
data CountedDiscard = MkCountedDiscard
  { slot :: SlotName.SlotName,
    quantity :: Quantity.Quantity,
    -- | Where the CARDS this discard moved are written, for a later effect of the
    -- same resolution to look back at -- Psychic Miasma's "if a land card is
    -- discarded this way", which is CR 701.70a's "if you discarded a nonland card
    -- this way" read from the other side. Pawl.Types.Destroy's `buried` is the
    -- shape and the precedent.
    --
    -- The CR 400.7 incarnations the discard funnel MINTED, never the cards that
    -- were in the hand: rule 701.9a moves the card, and CR 400.7 gives what
    -- arrives a fresh id, so a slot holding the hand id would name an object no
    -- reader of the destination zone can see.
    --
    -- Bound wherever the card wound up rather than only in a graveyard, which is
    -- CR 701.9c: a discarded card a replacement sends elsewhere is discarded all
    -- the same. Whether a READER can find it there is the reader's question, and
    -- data/cards/psychic-miasma.json carries that note.
    --
    -- Absent for a discard nothing looks back at, which is every discard in the
    -- pool but the one that does. Pawl.Types.Discard's These arm has no such
    -- field: no printed card names what an "all nonland cards" discard moved, so
    -- a slot there would be a bind position no card exercises.
    discarded :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)

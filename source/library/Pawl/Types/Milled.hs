module Pawl.Types.Milled where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 701.17a: a player milled cards -- who milled, and WHICH cards were
-- milled, as the incarnations CR 400.7 minted where each one landed.
--
-- The RESULTING ids, not the ones the cards had in the library, because CR
-- 701.17c is what a reader of this event wants: "an effect that refers to a
-- milled card can find that card in the zone it moved to from the library".
-- The zone is not always a graveyard -- a CR 614 replacement can send a milled
-- card elsewhere -- and the card was milled either way, so the destination is
-- not filtered here.
--
-- A record of its own rather than a read of the GameEvent.Moved entries the
-- same mill appends, and The Master, Transcendent's own ruling is why: a card
-- put into a graveyard from a library WITHOUT the word "mill" was not milled,
-- so it is not a legal target for that card's ability. Surveil (CR 701.25a) and
-- explore (CR 701.44a) both move a card from the top of a library to a
-- graveyard and neither is a mill, and a Moved entry cannot tell the three
-- apart -- library to graveyard is all it records.
--
-- ONE entry per mill instruction per player, holding every card that player
-- milled: CR 701.17a mills them all at once, which is what an ability
-- triggering on "one or more cards are milled" reads as a single event.
data Milled = MkMilled
  { player :: PlayerId.PlayerId,
    cards :: Seq.Seq ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)

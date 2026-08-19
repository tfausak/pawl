module Pawl.Types.Revealed where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.RevealCause as RevealCause

-- | CR 701.20a: a player revealed a card, what the reveal was, and the card's
-- characteristics as of the showing.

-- The card's id is routinely dead by the time anything reads this -- a search's
-- "reveal it, and put it into your hand" moves the card one step later and CR
-- 400.7 mints a new object -- so a reader needing a live object must check, as
-- Discarded's does. The id is carried all the same because CR 603.11's linked
-- ability is borne by the very card the reveal showed, which the snapshot alone
-- could not name.
data Revealed = MkRevealed
  { player :: PlayerId.PlayerId,
    card :: ObjectId.ObjectId,
    cause :: RevealCause.RevealCause,
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)

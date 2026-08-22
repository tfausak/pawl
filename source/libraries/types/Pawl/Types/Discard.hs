module Pawl.Types.Discard where

import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's Discard arm: WHICH cards CR 701.9a moves
-- from their owner's hand to their graveyard.
--
-- Two arms because CR 701.9b's default choice -- "effects that cause a player to
-- discard a card allow the affected player to choose which card to discard" --
-- only arises where the effect states a NUMBER and leaves the cards unsaid. A
-- card that names the SET instead, Amnesia's "discards all nonland cards", has
-- nothing left to choose, so the engine must not prompt there.
--
-- One Pawl.Types.Effect arm over both, not two, for the reason #1743 declined a
-- RevealAtRandom: one rule, one arm. Every consumer that CLASSIFIES an effect --
-- Pawl.Engine.EffectZone, Pawl.Engine.ManaAbility, Pawl.Engine.Projection's
-- rewrite -- would otherwise have to learn rule 701.9 twice.
data Discard
  = -- | CR 701.9b: the slot names one player, who picks. CR 609.3 caps the count
    -- at the hand's size, and a full hand is forced rather than asked.
    Counted CountedDiscard.CountedDiscard
  | -- | The card names the set, so CR 701.9b's choice does not arise. The
    -- discarding player is per card: rule 701.9a moves each one from its OWNER's
    -- hand, and a ref reaching several hands (Pawl.Types.EachCardInHand) can
    -- name cards belonging to several owners at once.
    These ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)

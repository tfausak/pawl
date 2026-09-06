module Pawl.Types.OfferCast where

import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 608.2g: offer a player the cast of the objects a reference names, under
-- CR 310.12b's riders.
data OfferCast = MkOfferCast
  { -- | WHICH cards are on offer. An ObjectRef and not a SlotName because CR
    -- 601.3's offer ranges over a SET as often as over one card -- Shell of the
    -- Last Kappa's "a spell from among cards exiled with Shell of the Last
    -- Kappa" is ObjectRef.EachCardExiledWithSource, CR 607.2a's linked set,
    -- where Tinybones, the Pickpocket's one target is ObjectRef.InSlot.
    --
    -- Several cards is a CHOICE, not several casts: Pawl.Engine.Resolve.Effect's
    -- offerCast puts the whole set to the caster as one Prompt.ChooseOfferedCastSpell
    -- and casts at most one.
    ref :: ObjectRef.ObjectRef,
    -- | WHO casts. Rule 608.2g says "a player" rather than the resolving
    -- controller, and CR 601.2's announcements then belong to whoever that is --
    -- Wild Evocation's "that player casts it" is the upkeep player, not the
    -- enchantment's controller. Defaulted to the resolving controller, which is
    -- what CR 310.12b's and CR 702.94a's offers mean.
    caster :: PlayerRef.PlayerRef,
    -- | Whether the cast is CR 608.2g's "instructs" or its "allows" -- the rule
    -- carries both postures in one sentence. Wild Evocation's "the player casts
    -- it ... if able" is the mandatory one; every other producer prints a "may".
    --
    -- Mandatory does NOT mean the cast always happens: rule 601.3's prohibitions
    -- and an unpayable cost still stop it, which is what "if able" says out
    -- loud. Nor does it always remove the question -- CR 118.8c hands it back
    -- where the mandatory additional cost names cards of a stated quality in a
    -- hidden zone, which Pawl.Engine.Cost.statesHiddenQuality classifies.
    optionality :: CastObligation.CastObligation,
    -- | Elided when the offer carries neither rider, which is an ordinary cast
    -- of the card.
    offer :: CastOffer.CastOffer
  }
  deriving (Eq, Ord, Show)

module Pawl.Types.OfferCast where

import qualified Pawl.Types.CastObligation as CastObligation
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.CastRepetition as CastRepetition
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
    -- Several cards is a CHOICE first: Pawl.Engine.Resolve.Effect's offerCast puts
    -- the whole set to the caster as one Prompt.ChooseOfferedCastSpell, and the
    -- `repetition` field below says whether that choice is asked once or over
    -- again.
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
    -- Not implemented: a printed "plays that card" reaches CR 116.2a's land
    -- drop as well as CR 601's cast, and this opcode offers only the cast, so
    -- Word of Command cannot make a player play a land (#3347).
    --
    -- Mandatory does NOT mean the cast always happens: rule 601.3's prohibitions
    -- and an unpayable cost still stop it, which is what "if able" says out
    -- loud. Nor does it always remove the question -- CR 118.8c hands it back
    -- where the mandatory additional cost names cards of a stated quality in a
    -- hidden zone, which Pawl.Engine.Cost.statesHiddenQuality classifies.
    optionality :: CastObligation.CastObligation,
    -- | Elided when the offer carries neither rider, which is an ordinary cast
    -- of the card.
    offer :: CastOffer.CastOffer,
    -- | Whether the offer admits ONE of the cards `ref` names or any number of
    -- them -- the axis `optionality` above is not. Fevered Suspicion's "you may
    -- cast any number of spells from among those nonland cards" writes both: a
    -- may, repeated.
    repetition :: CastRepetition.CastRepetition,
    -- | CR 707.12: whether what is offered is a COPY of each object `ref` names,
    -- created in the zone that object is in, rather than the object itself.
    -- Mizzix's Mastery's "copy it, and you may cast the copy" sets it; every
    -- other producer offers the card where it lies.
    --
    -- A field of THIS type and not of CastOffer, whose riders all describe the
    -- cast -- which face, what it costs, how mana may be spent toward it. This
    -- one changes WHICH OBJECTS the offer ranges over, which is `ref`'s axis, so
    -- Pawl.Engine.Resolve.Effect's offerCast mints the copies before CR 601.3's
    -- choice rather than inside the per-face proposal CastOffer feeds.
    --
    -- CR 707.12a is why one copy is minted per named object and `repetition`
    -- above then asks per copy: "an effect that creates multiple copies and says
    -- a player 'may cast' those objects allows that player to choose
    -- individually, for each of those objects, whether or not to cast it".
    copied :: Bool
  }
  deriving (Eq, Ord, Show)

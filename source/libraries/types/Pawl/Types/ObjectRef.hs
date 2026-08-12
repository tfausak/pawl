module Pawl.Types.ObjectRef where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | WHICH OBJECTS an object-affecting effect names -- the object-side counterpart
-- of Pawl.Types.PlayerRef.
--
-- InSlot stands apart from the other arms: its objects were named BEFORE the
-- effect runs -- a slot, filled at cast or as the ability was placed -- where
-- every other arm's are found AS it runs. That is the distinction CR 115.10a
-- draws: only InSlot can name a target; no other arm ever does.
data ObjectRef
  = -- | The objects bound in a slot (CR 601.2c filled it by targeting, or the
    -- engine reserved it -- Binding.triggerSource). Usually ONE, the slot's
    -- single Recipient, subject to CR 608.2b's illegal-target check when the slot
    -- was a target.
    --
    -- But a slot a Create bound to a whole minted GROUP names every one of them
    -- -- Salt Road Skirmish's "they gain haste until end of turn" over the two
    -- tokens the sentence before it made. A group is a definition and never a
    -- target (CR 115.10a), so it owes CR 608.2b nothing; the arm still reads at
    -- most one object per slot for every OTHER kind of binding, which is why
    -- "target creature" cannot become two.
    InSlot SlotName.SlotName
  | -- | Every PERMANENT ON THE BATTLEFIELD matching the Filter -- Day of
    -- Judgment's "all creatures". The battlefield is where CR 109.2 puts it; a
    -- set drawn from any other zone has no card in the pool (#376).
    --
    -- Not a target and never one (CR 115.10a), so CR 608.2b has nothing to
    -- fizzle. The set is swept when the effect executes (CR 608.2c) and is then
    -- fixed for that instruction; judging which swept objects are affected
    -- before any of them is (CR 608.2f) belongs to the opcode's funnel rather
    -- than to this type.
    --
    -- A CONTINUOUS effect over a set must additionally freeze the swept set into
    -- the effect itself (CR 611.2c), storing Affected.TheseObjects; the one-shots
    -- that take this type store nothing.
    EachMatching (Filter.Filter Keyword.Keyword)
  | -- | Every PLAYER in the game -- Molten Disaster's "and each player". The one
    -- arm that names no object at all, and it is here rather than on
    -- Pawl.Types.PlayerRef because the opcode that needs it takes an ObjectRef:
    -- CR 120.3a makes a player a damage recipient, and Effect.DealDamage already
    -- reaches one through the InSlot arm. Every OTHER ObjectRef-taking opcode
    -- reads objects only, and this arm drops out of the sweep there rather than
    -- being rejected -- the posture Pawl.Engine.Resolve.objectRefObjects already
    -- takes for a player bound in a slot.
    --
    -- Payload-free rather than carrying a Pawl.Types.PlayerRef: "each player" is
    -- what the card says, and a PlayerRef would make InSlot sayable twice over.
    --
    -- Not a target (CR 115.10a) and swept when the effect executes, the two
    -- properties EachMatching above has.
    --
    -- Not implemented, recorded here because the card's JSON cannot carry a
    -- comment: a sentence naming creatures AND players ("each creature without
    -- flying and each player") has to be written as two DealDamage
    -- instructions, so its one CR 608.2f batch becomes two (#1285).
    EachPlayer
  | -- | The ONE card on top of a library -- Count on Luck's "exile the top card of
    -- your library". A library is a per-player zone (CR 400.1) kept as an ordered
    -- pile (CR 401.2), so "the top card" is a position rather than a property, and
    -- that is what no Filter can say: EachMatching sweeps the battlefield (CR
    -- 109.2) and a Filter matches characteristics, neither of which can pick the
    -- head of a hidden pile (CR 400.2).
    --
    -- The PlayerRef is WHOSE library, so "the top card of target player's library"
    -- is the same arm through its InSlot. One card per library named and no depth:
    -- a printing wanting the top THREE has no spelling here (#1299).
    --
    -- Not a target and never one (CR 115.10a) -- the player may be targeted, the
    -- card is not -- so CR 608.2b has nothing to fizzle. Read when the effect
    -- executes (CR 608.2c), which is what makes an empty library a no-op rather
    -- than an error: there is no top card, so the arm names nothing.
    TopOfLibrary PlayerRef.PlayerRef
  deriving (Eq, Ord, Show)

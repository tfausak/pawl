module Pawl.Types.ForEach where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2f's per-object loop: the objects and players the ObjectRef names,
-- each taken in turn with the body run once for it -- Soulfire Eruption's "for
-- each of them, exile the top card of your library, then ... deals damage equal
-- to that card's mana value to that permanent or player", and Rampage of the
-- Clans' "for each permanent destroyed this way, its controller creates a 3/3
-- green Centaur creature token", whose members are the permanents a destruction
-- already removed and so are read through CR 608.2h.
--
-- Parametric in the EFFECT for Pawl.Types.PreventNextDamage's reason: the
-- record holds effects and Pawl.Types.Effect holds the record, so naming Effect
-- here would need an hs-boot file. Instantiated at `Effect card` where the arm
-- is declared.
data ForEach effect = MkForEach
  { -- | The set swept ONCE, before the first iteration -- CR 608.2f's "which
    -- objects" half, the same read every other ObjectRef-taking opcode makes.
    -- Nothing the body does adds to or removes from it.
    ref :: ObjectRef.ObjectRef,
    -- | The name this iteration's member is bound under, for the body to read
    -- as an ObjectRef.InSlot or a PlayerRef.InSlot. A DEFINITION, never a
    -- target (CR 115.10a): the ref above may well have been filled by targeting
    -- and carries CR 608.2b's re-validation, but this name is the loop's, not
    -- the card's announcement.
    slot :: SlotName.SlotName,
    -- | The instructions run once per member, in written order (CR 608.2c). A
    -- SEQUENCE is the whole point: an opcode naming a set applies ITSELF across
    -- it, where this applies a list of them to each member in turn, so a later
    -- instruction can act on what an earlier one produced FOR THAT MEMBER.
    --
    -- What the body BINDS is scoped to its own iteration and no further, which
    -- Pawl.Engine.Resolve.Effect's arm states; once the loop is over those names
    -- hold the UNION across every member, so an instruction after it names the
    -- whole batch -- Mirror Match's "exile those tokens", ONE delayed ability
    -- rather than one per member. Pawl.CombatCostSpec's
    -- PutOntoBattlefieldBlocking group proves it.
    body :: Seq.Seq effect,
    -- | CR 608.2f's SECOND sentence: True where the loop's action "can't be
    -- processed simultaneously", so each member is considered individually and
    -- its events stand apart from the next member's. False -- rule 608.2f's "in
    -- most cases", and what a card that says nothing gets -- runs the whole loop
    -- inside one Pawl.Types.EventGroup, so CR 603.6a's newcomers see each other
    -- enter.
    --
    -- Rule 608.2f's own example is the True: Soulfire Eruption's player "can't
    -- exile the top card of their library multiple times at the same time".
    -- Mutalith Vortex Beast is the other one in the pool, by CR 121.2 -- cards
    -- are drawn one at a time.
    --
    -- Not implemented: rule 608.2f's unit is the ACTION, and CR 608.2e says the
    -- same for the order, so a body of two instructions owes an answer per
    -- instruction where this field gives one for the whole loop (#3886).
    --
    -- Proved by Pawl.CombatCostSpec's "CR 603.6a / 608.2f: every Mirror Match
    -- token sees every other one enter", which is the False. The True has no
    -- gameplay observer -- no printing in the pool watches a batch of library
    -- exiles or of draws -- and is held instead by Pawl.CardSpec's "a ForEach
    -- whose body draws or reads a library top says it is not simultaneous",
    -- which is where CR 121.2 and rule 608.2f's Soulfire Eruption example are
    -- kept as rules rather than as a card's default.
    individually :: Bool
  }
  deriving (Eq, Ord, Show)

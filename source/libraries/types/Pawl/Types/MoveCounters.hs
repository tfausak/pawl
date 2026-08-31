module Pawl.Types.MoveCounters where

import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's MoveCounters arm (CR 122.5).
--
-- An ObjectRef on EACH side. Rule 122.5 defines a move between an object and "a
-- second object" and states its impossibilities about that pair, so a pair is
-- what the arm performs -- but the printed sentence may name a whole GROUP on
-- either side, and every pair it makes is then its own. A group on the FIRST
-- side gathers counters in, Spike Cannibal's "move all +1\/+1 counters from all
-- creatures onto it", every member being its own pair against the one
-- destination. A group on the SECOND spreads them out, Forgotten Ancient's "move
-- any number of +1\/+1 counters from this creature onto other creatures", where
-- HOW MANY land on each is the player's answer
-- (Pawl.Types.Prompt's ChooseDistributedMovedCounters) rather than the card's.
--
-- Not implemented: a group destination under any `kinds` arm but
-- MovedKinds.AnyNumber and MovedKinds.AnyNumberOfKind, whose count is the
-- player's and whose whole answer the distribution prompt can therefore carry.
-- An arm the CARD settles a count on would have to ask where a fixed batch goes
-- rather than what crosses, and MovedKinds.AtLeastOne -- whose count IS the
-- player's -- would have to repair an answer moving nothing by choosing a
-- recipient as well as a kind, which under one destination it never has to do.
-- No printing writes either pairing: the sweep on Pawl.Types.MovedKinds names
-- Forgotten Ancient as the only group destination there is, and Goldberry,
-- River-Daughter's floor as the only "one or more", whose destination is one
-- targeted permanent (#2784).
--
-- `kinds` is WHICH counters cross and how many of each; Pawl.Types.MovedKinds is
-- where every spelling the printed text uses for that is set out.
--
-- ONE batch per kind is what crosses -- one REMOVAL of it per first object, and
-- one PLACEMENT of it per DESTINATION, CR 608.2f processing an action taken on
-- several objects simultaneously. Those are the same thing wherever `to` names a
-- single object, which is every printing but Forgotten Ancient: one placement for
-- the whole instruction however many first objects it was gathered off. Where
-- `to` names a group they part, an arrival being on one object -- the placements
-- cannot batch across recipients, and CR 614.16 and CR 122.7 see one apiece. Rule 122.5 never speaks of more
-- than one counter, so it does not settle the batch on its own; what does is that
-- each of its four impossibilities is an object-level or kind-level property (the
-- two objects being one, the appropriate kind absent, the destination refusing
-- counters, the wrong zone) and NONE of them varies with the count, so the
-- all-or-nothing it states about one counter carries to a batch of one kind
-- between one pair with nothing further to decide. CR 614.16 then makes the
-- placement half a single replaceable event -- "if an effect would put one or
-- more counters on a permanent" -- and CR 122.7 reads a batch put the same way,
-- which is the call Pawl.Types.PutCounters' own `quantity` already makes. CR
-- 609.3 covers the one count-sensitive shortfall, a count larger than the object
-- has: "it does only as much as possible".
data MoveCounters = MkMoveCounters
  { from :: ObjectRef.ObjectRef,
    kinds :: MovedKinds.MovedKinds,
    -- | How many counters rule 122.5 ACTUALLY moved, summed over every kind that
    -- crossed, over every first object the `from` ref named and over every
    -- destination the `to` ref named, written back for a later
    -- effect of the same resolution to read as Quantity.InSlot -- Black Panther,
    -- Wakandan King's "if one or more +1\/+1 counters are moved this way, you
    -- gain that much life and draw a card". Pawl.Types.Destroy's `slot` in every
    -- respect, including that it is bound even when nothing moved: zero is an
    -- answer, where an unbound slot would leave the rider's gate unevaluable
    -- instead. Nothing for every move nothing looks back at.
    slot :: Maybe SlotName.SlotName,
    to :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)

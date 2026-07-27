module Pawl.Keyword where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.Keyword (Keyword)
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.Quantity as Quantity
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import Pawl.Type.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility

-- Rule 702 in its OTHER voice. Most keywords this pool has are read where they
-- matter -- Projection.hasKeyword for an evasion or combat bit, Pawl.Damage for
-- infect's and toxic's damage riders -- because the rule states them as static
-- abilities that some rules-core reader already asks about. Rule 702.70 does
-- not: it spells poisonous out as a TRIGGERED ability, in the same words a card
-- would print, so it has to be MINTED and handed to the ordinary CR 603
-- machinery rather than merely consulted.
--
-- Casing on Keyword here is legitimate for the reason Pawl.Type.Keyword's own
-- comment gives: a keyword is a numbered rule, not an effect's identity. What
-- this module must never do is grow an arm for a CARD.
--
-- The abilities are derived from a projection's POST-LAYER keyword counts, so
-- Humility (LoseAllAbilities, which empties PC.keywords at layer 6) takes rule
-- 702.70a's ability away for free, and an Aura's layer-6 grant adds it for free
-- -- neither needs an arm here.
--
-- The one caller is Pawl.Event's EVENT scan (eventTriggers). Rule 702 has no
-- state-triggered (CR 603.8) or delayed (CR 603.7) keyword ability, so
-- stateTriggers and delayedPending do not consult this; the first keyword that
-- needs them to is the one that must widen those two scans.

-- CR 702.70b: "If a creature has multiple instances of poisonous, each triggers
-- separately." So this returns one ability PER INSTANCE, which is exactly what
-- the projection's per-keyword count says: `Poisonous 1` twice is two abilities
-- and two poison counters, not one ability for 2. (Contrast CR 702.164b, where
-- toxic's N values are SUMMED into a single rider -- Projection.totalToxic.)
--
-- Order is the Map's, which is Keyword's Ord -- rule-number order, and stable.
-- The CR 603.3b ordering prompt indexes into the scan's canonical order, so this
-- being deterministic is what keeps that prompt reproducible.
triggeredAbilitiesOf :: Map Keyword Natural -> [TriggeredAbility Card]
triggeredAbilitiesOf counts = concatMap (uncurry abilitiesFor) (Map.toAscList counts)

-- The abilities one keyword, held `count` times, contributes.
abilitiesFor :: Keyword -> Natural -> [TriggeredAbility Card]
abilitiesFor keyword count = case keyword of
  Keyword.Poisonous n -> List.genericReplicate count (poisonous n)
  Keyword.Deathtouch -> []
  Keyword.Defender -> []
  Keyword.DoubleStrike -> []
  Keyword.FirstStrike -> []
  Keyword.Flying -> []
  Keyword.Haste -> []
  Keyword.Indestructible -> []
  Keyword.Reach -> []
  Keyword.Trample -> []
  Keyword.Vigilance -> []
  Keyword.Fear -> []
  Keyword.Infect -> []
  Keyword.Devoid -> []
  Keyword.Toxic _ -> []

-- CR 702.70a: "'Poisonous N' means 'Whenever this creature deals combat damage
-- to a player, that player gets N poison counters.'"
--
-- "That player" is the player the trigger's own event named, which
-- Pawl.Event.eventBindings stamps under the reserved Binding.triggerPlayer slot
-- as the trigger is gathered -- so the payload is an ordinary slot read and this
-- ability needs no opcode of its own. NOT the ability's controller: CR 603.3a
-- makes that the creature's controller, and the poison goes to their victim.
--
-- Single mode, no targets, ChooseExactly 1 -- so nothing is asked as the ability
-- is placed, which is right because rule 702.70a leaves nothing to choose. Same
-- shape Pawl.Monarch.oneEffect builds for rule 725's inherent abilities.
-- intervening = Nothing: rule 702.70a has no "if" clause.
poisonous :: Natural -> TriggeredAbility Card
poisonous n =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfDealsCombatDamageToPlayer,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton effect) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    effect =
      Effect.GainPlayerCounters
        (PlayerRef.InSlot Binding.triggerPlayer)
        PlayerCounterKind.Poison
        (Quantity.Literal (toInteger n))

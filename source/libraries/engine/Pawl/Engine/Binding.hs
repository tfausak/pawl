module Pawl.Engine.Binding where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.Types.Binding (Binding)
import qualified Pawl.Types.Binding as Binding
import Pawl.Types.ModeIndex (ModeIndex)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.ProjectedCharacteristics (ProjectedCharacteristics)
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.SlotName as SlotName

-- CR 601.2b: the reserved slot under which a spell's single chosen X is stored.
-- No card's targetSlots may name it (lint-enforced): X is not a target, so it
-- needs a key the target namespace cannot collide with.
variableX :: SlotName
variableX = SlotName.MkSlotName (Text.pack "X")

-- CR 700.2: the reserved slot under which a modal spell's chosen modes are
-- stored. No card's targetSlots may name it (lint-enforced): a mode is not a
-- target. Distinct from variableX.
chosenModes :: SlotName
chosenModes = SlotName.MkSlotName (Text.pack "modes")

-- CR 707.5: the reserved slot under which an object's copy snapshot is stored.
-- No card's targetSlots may name it: a copy source is not a target.
copySource :: SlotName
copySource = SlotName.MkSlotName (Text.pack "copySource")

-- CR 113.7: the reserved slot under which a triggered ability's SOURCE object
-- (the object whose ability triggered) is bound as the ability is placed, so
-- "this creature" / "this enchantment" is a slot read rather than a
-- self-referential opcode. No card's targetSlots may name it (lint-enforced): a
-- source is not a target.
--
-- Hazard this comment used to only predict, and which #1043 then resolved.
-- CardSpec.hs's "declared slots == read slots" equality lint now walks a card's
-- ABILITY modes as well as its spell's, and it would indeed be unsatisfiable
-- stated naively -- Resolve.slotsOf returns this slot for an effect that reads
-- it, while the rule above forbids the matching targetSlots entry. What makes it
-- statable is the discipline this comment named: CardSpec.modalSlotsOffend
-- SUBTRACTS the reserved names its carrier binds (this one, variableX,
-- chosenModes, copySource, you, thatPlayer, became, thatSpell) from the READ side
-- before comparing, rather than adding them to the declared side. Loosening it
-- back to a subset check would silently retire its "declared but never read"
-- half, which is the shape the ability lints carried until #1043.
triggerSource :: SlotName
triggerSource = SlotName.MkSlotName (Text.pack "self")

-- CR 109.5: the reserved slot under which a spell's or an ability's CONTROLLER
-- is bound ("you"), so a targetless self-referential clause -- Sarcomancy's
-- "deals 1 damage to you" -- is a slot read rather than a new opcode.
--
-- Stamped on ALL THREE carriers, because rule 109.5 defines the word for all
-- three. "The words 'you' and 'your' on an object refer to the object's
-- controller, its would-be controller (if a player is attempting to play, cast,
-- or activate it)" covers a spell, and Pawl.Engine.Cast.castSpell answers it at
-- CR 601.2i with the caster, who is the spell's controller. "For an activated
-- ability, this is the player who activated the ability. For a triggered
-- ability, this is the controller of the object when the ability triggered"
-- covers the other two: Pawl.Engine.Activate.activateAbility answers the first,
-- Pawl.Engine.Engine.placeBorne the second. CR 725.2's monarch pair is the fourth
-- stamp site (Pawl.Engine.Monarch.placeInherent): those abilities have no object
-- for CR 109.5's triggered-ability sentence to name a controller of, and CR 725.2
-- supplies one itself -- "controlled by the player who was the monarch at the
-- time the abilities triggered".
--
-- Most spells that say "you" never touch this slot: they say it through an
-- opcode carrying a PlayerRef, which Pawl.Engine.Resolve answers from the
-- resolving controller with no slot involved. The shape that needs the slot is
-- damage, whose recipient is an ObjectRef whose only player-reaching arm is a
-- bound one -- Char's "and 2 damage to you" (Pawl.CastSpec's Char case).
--
-- "No card's targetSlots may name it" is lint-enforced as for the names above,
-- and reaches spells and abilities alike: a card declaring a "you" target slot
-- would be prompted for a target and have the answer clobbered by setYou.
you :: SlotName
you = SlotName.MkSlotName (Text.pack "you")

-- CR 603.2: the reserved slot under which the PLAYER an event trigger's event
-- names is bound -- "that player" in CR 702.70a's poisonous. Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, so the ability's
-- payload reads an ordinary slot rather than a "the damaged player" opcode.
--
-- Distinct from `you` (CR 109.5, the ability's CONTROLLER): the player the
-- event names is generally an opponent, and in a multiplayer game which
-- opponent is not derivable from the controller at all.
--
-- Not a target (nothing was chosen), so CR 608.2b has nothing to re-validate --
-- Resolve's legalSlot answers True for any slot that declares no target, which
-- is how this stays readable at resolution. `you`'s "no card's targetSlots may
-- name it" rule applies here too, under the same sweep. That an effect reading
-- this slot sits under a condition that binds it is enforced by
-- Pawl.Engine.Event.eventBindingSlots: only the combat-damage-to-a-player, the
-- CR 701.9a discard, the CR 119.3 life-loss, the CR 119.9 life-GAIN, the CR
-- 601.2i cast, the CR 508.3a attack and the CR 725.1 crowning conditions stamp
-- it, so reading it under any other is a failing test.
triggerPlayer :: SlotName
triggerPlayer = SlotName.MkSlotName (Text.pack "thatPlayer")

-- CR 400.7e / CR 603.6c: the reserved slot under which a zone-change trigger's
-- ARRIVING incarnation is bound -- and, since CR 708.7's readers took it, the
-- slot for the object an event trigger's event NAMES more generally.
--
-- ONE slot for both directions of a zone change, because CR 400.7e is one rule
-- about whatever moved, not a rule about the ability's bearer:
--
--   * a DEPARTURE, where the mover is the bearer -- Endless Cockroaches.
--   * an ENTRY, where the mover is generally NOT the bearer -- Aether Flash.
--     Here `triggerSource` is the enchantment and this slot is the entrant, two
--     unrelated objects. The entrant may be gone by resolution (CR 608.2h),
--     which is why an effect reading this slot must tolerate a dead id.
--
-- For the departure direction it is a SECOND name for what one printed word
-- calls "it", and the two are not interchangeable, because CR 400.7 mints a
-- fresh id on every zone change:
--
--   * `triggerSource` is CR 113.7a's SOURCE, the permanent as it was on the
--     battlefield, read from CR 608.2h last known information by
--     Projection.viewWithLastKnown. Everything the ability says ABOUT itself.
--   * this slot is the CARD, wherever the move put it. Everything the ability
--     DOES to itself, because the other no longer exists to be moved.
--
-- A THIRD reader, and not a zone change at all: CR 708.7's "whenever a permanent
-- is turned face up", whose subject Pine Walker untaps. CR 708.8 leaves one
-- permanent with one id there, so this slot and `triggerSource` name two
-- unrelated OBJECTS rather than two incarnations of one card -- the Aether Flash
-- shape above, not the Endless Cockroaches one. It is this slot and not a fresh
-- one because CR 400.7e's slot is the printed word "it", the thing the EVENT
-- names, and Pawl.Engine.Resolve never learns which condition placed the ability
-- it is reading a slot for.
--
-- Collapsing them either way is a silent wrong answer, not a type error:
-- binding only the source makes every such effect a no-op on a dead id, and
-- rebinding the source to the arrival redirects every viewWithLastKnown
-- quantity read onto the graveyard card's printed characteristics.
--
-- A SECOND writer, and the same notion of "it" from the other side: an ability
-- whose own effect performs the move binds the arrival here too, through
-- Effect.MoveToZone's CR 400.7 slot. Rule 310.12b's "exile it, then you may cast
-- it" is that shape (Pawl.Engine.Battle.siegeDefeat), and the two writers cannot
-- collide -- eventBindings stamps this slot only for the conditions
-- Event.eventBindingSlots names, and a counter-removal condition is not one.
-- No CARD may bind it, which Pawl.CardSpec's reservedBindings sweep enforces.
--
-- Stamped by Pawl.Engine.Event.eventBindings alongside `triggerPlayer`, and not
-- a target -- same CR 608.2b posture as that slot, including the "no card's
-- targetSlots may name it" sweep and the eventBindingSlots check on reads. A
-- condition that binds it only SOMETIMES is rejected by that same lint: CR
-- 400.7e withholds this slot when the destination is hidden (CR 400.2), so the
-- wider leaves-the-battlefield condition binds it for a death and not for a
-- bounce, and no card may read it under that condition yet (#505).
became :: SlotName
became = SlotName.MkSlotName (Text.pack "became")

-- CR 603.2: the reserved slot under which the AMOUNT an event trigger's event
-- names is bound -- the printed words "that much". Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, so the payload
-- reads an ordinary Quantity.InSlot rather than a "how much was prevented" or
-- "how much was gained" opcode.
--
-- ONE slot for every event that supplies a number, the way `became` is one slot
-- for both directions of a zone change, and for the same reason: the printed word
-- is the same word, and which rule produced the number is a fact about the
-- CONDITION rather than about the slot. The conditions that stamp it today:
--
--   * CR 615.13's prevention -- "put that many +1/+1 counters" on Selfless
--     Squire, "you gain that much life" on the same family's other cards.
--   * CR 119.9's life gain -- Sanguine Bond's "target opponent loses that much
--     life".
--   * Its mirror, a player losing life -- Exquisite Blood's "you gain that much
--     life".
--   * CR 510.2's combat damage to a player, in both its bearer-scoped and its
--     filtered form -- Questing Beast's "it deals that much damage to target
--     planeswalker that player controls", Shroofus Sproutsire's "create that many
--     1/1 green Saproling creature tokens".
--   * CR 120.3's damage to the bearer -- Coalhauler Swine's "it deals that much
--     damage to each player".
--
-- No ability bears two conditions, so they can never collide on one object, and
-- Pawl.Engine.Event.eventBindingSlots is what tells the card lint which of them
-- makes the slot available.
--
-- A NUMBER, where triggerPlayer and became are references, so this is the first
-- reserved slot read through Quantity rather than through a Ref. That is why
-- Pawl.Engine.Quantity.evaluateFor's InSlot arm looks on the stack object as well
-- as on the effect's source: an amount an EFFECT bound mid-resolution lives on
-- the source, and one the EVENT supplied lives where every other trigger binding
-- does.
--
-- Read out of Pawl.Types.GameState.ambientAmounts too, and written there by
-- Pawl.Engine.Resolve.runPreventionRider, which is CR 615.5's own channel: a
-- rider whose shielded recipient is a player has no object at all to be bound
-- on. Nothing writes THIS name onto a battlefield permanent any more: the
-- prevention stamp was the one such writer and it is gone, and
-- Resolve.bindAmountSlot -- the writer that can reach a permanent -- only ever
-- writes a slot the CARD authored, which the reserved-name sweeps forbid being
-- this one. So the ambient read, coming last, shadows nothing.
--
-- Not a target (nothing was chosen), so the same CR 608.2b posture and the same
-- "no card's targetSlots may name it" sweep as `you`, `thatPlayer` and `became`.
-- Swept on the BINDING side too, which matters here more than for any of those
-- three: a card naming this one in an effect's bound SlotName -- Destroy's
-- count, MoveToZone's incarnation -- would write it to the source, which
-- evaluateFor's InSlot arm reads FIRST, shadowing the event's amount with the
-- card's own. Pawl.CardSpec's reservedBindings is that sweep.
eventAmount :: SlotName
eventAmount = SlotName.MkSlotName (Text.pack "thatMuch")

-- CR 614.1c: the reserved slot under which an as-enters sacrifice records HOW
-- MANY permanents were sacrificed -- Wood Elemental's "the number of Forests
-- sacrificed as it entered". Stamped by Pawl.Engine.Event's
-- EntryRewrite.SacrificeAnyNumber arm on the entering permanent itself, so the
-- card reads an ordinary Quantity.InSlot rather than a "how many did I eat"
-- opcode. Second of eventAmount's kind, and read the same way.
--
-- Stamped on the PERMANENT and not on the spell that made it: CR 400.7 mints a
-- new object on every zone change, so the count belongs to the incarnation that
-- entered. That is also what makes CR 208.2a bite, since a Wood Elemental that
-- has not entered -- one on the stack, or in any other zone, where CR 604.3 still
-- runs its characteristic-defining ability -- carries no such binding, and the
-- number of Forests it sacrificed as it entered genuinely cannot be determined.
--
-- Stamped even when NONE were sacrificed. A permanent that entered having
-- sacrificed nothing has a determinable count of 0, which is a different fact
-- from a Wood Elemental that never entered at all, and only the arithmetic
-- coincides.
--
-- Not a target, so the same CR 608.2b posture and the same "no card's
-- targetSlots may name it" sweep as eventAmount.
sacrificedCount :: SlotName
sacrificedCount = SlotName.MkSlotName (Text.pack "thatMany")

-- CR 601.2i: the reserved slot under which a cast trigger's WATCHED SPELL is
-- bound -- the printed "it" in Presence of the Master's "whenever a player casts
-- an enchantment spell, counter it", and "that spell" wherever a card spells the
-- word out. Stamped by Pawl.Engine.Event.eventBindings as the trigger is
-- gathered, alongside `triggerPlayer` and `became`, so the payload reads an
-- ordinary slot rather than a "the spell that was cast" opcode.
--
-- NOT `became`, though both name an object an event produced, and the reason is
-- the word itself: CR 400.7e's `became` is the incarnation a card BECAME on
-- changing zones, and a spell being cast is not a zone change at all. CR 601.2a
-- moved the card to the stack earlier in the casting process and CR 601.2i's
-- event is the spell becoming CAST, at which point the object has become
-- nothing -- it is the same stack object it already was. Reusing the name would
-- also have to argue away a collision that is real rather than theoretical: an
-- ability whose condition is a zone change and whose payload names the cast
-- spell is not writable today, but the two writers are independent, and one slot
-- for two unrelated rules is what forces such an argument every time a reader is
-- added.
--
-- Distinct from `triggerSource` (CR 113.7a) for the same reason `became` is:
-- under this condition the bearer is a permanent watching the stack while the
-- spell is a separate object on it, controlled by another player as often as
-- not.
--
-- Not a target -- nothing was chosen -- so the same CR 608.2b posture as
-- `triggerPlayer` and `became`: Resolve's legalSlot answers True for a slot with
-- no target slot, which is how CR 701.6a's countering reaches it at resolution.
-- The "no card's targetSlots may name it" rule applies here too, under the same
-- Pawl.CardSpec sweep, and the binding side of that sweep matters just as much:
-- a card writing this name into an effect's bound SlotName would shadow the
-- event's spell with its own object.
--
-- A dead id is possible and is the payload's problem, exactly as it is for
-- `became`: the spell can leave the stack before the trigger resolves (another
-- counterspell, or a second Presence of the Master), which is the case CR 608.2h
-- is about, and CR 701.6a's funnel no-ops on an id that names nothing.
castSpell :: SlotName
castSpell = SlotName.MkSlotName (Text.pack "thatSpell")

-- CR 601.2c: the reserved slot under which the SPELL OR ABILITY THAT DID THE
-- TARGETING is bound -- rule 702.21a's "that spell or ability", which ward
-- counters and whose controller ward offers the cost to. Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, alongside
-- `castSpell` and the rest.
--
-- For BOTH sides of rule 601.2c's announcement, which is why it is worded over
-- the targeting object rather than over what was targeted: the bearer becoming a
-- target (TriggerCondition.SelfBecomesTargeted, ward's) and the bearer's
-- CONTROLLER becoming one (TriggerCondition.ControllerBecomesTarget, Amulet of
-- Safekeeping's "counter that spell or ability"). The object bound is the same
-- field of the same event either way.
--
-- Distinct from `castSpell` even though both name a stack object: that one is the
-- spell a CAST event named, and an activated ability targeting a warded permanent
-- records no cast event at all. A payload naming the wrong one would still
-- typecheck, so two slots make the mismatch a dead name instead.
--
-- Distinct from `triggerSource` (CR 113.7a) for `castSpell`'s reason: the bearer
-- is a permanent watching the stack, and the targeting object is another object
-- entirely -- one an opponent controls under the relation both readers' printings
-- narrow to.
--
-- ONE object, never a group: rule 601.2c makes each chosen target its own
-- becoming, so a spell naming the bearer twice is two events and two abilities,
-- each holding one id -- which is also what keeps two ward triggers from
-- countering one spell twice (CR 701.6a's funnel no-ops on the second).
--
-- Not a target (nothing was chosen), so the same CR 608.2b posture and the same
-- "no card's targetSlots may name it" sweep as `castSpell`. A dead id is possible
-- and is the payload's problem: the targeting spell can be countered by something
-- else before the trigger reading this slot resolves.
targetingObject :: SlotName
targetingObject = SlotName.MkSlotName (Text.pack "thatTargetingObject")

-- CR 509.3d: the reserved slot under which the CREATURE THAT BLOCKED the bearer
-- is bound -- rule 702.25a's "the blocking creature". Stamped by
-- Pawl.Engine.Event.eventBindings as the trigger is gathered, alongside
-- `triggerPlayer`, `became` and `castSpell`, so flanking's payload is an
-- ordinary slot read rather than a "the creature that blocked me" opcode.
--
-- Distinct from `triggerSource` (CR 113.7a) for the reason `castSpell` is: under
-- this condition the bearer is the ATTACKING creature and the blocker is another
-- object, controlled by the defending player. Distinct from `became` because no
-- zone change happened at all -- CR 509.1g makes a creature a blocking creature
-- where it stands.
--
-- One slot per blocker, never a group: CR 509.3d triggers once for each creature
-- that blocks, so two blockers are two abilities each naming one object, and
-- nothing here holds a set.
--
-- Not a target (nothing was chosen), so the same CR 608.2b posture and the same
-- "no card's targetSlots may name it" sweep as `triggerPlayer` and `became`. A
-- dead id is possible and is the payload's problem, as it is for those: the
-- blocker can leave the battlefield before the trigger resolves.
blockingCreature :: SlotName
blockingCreature = SlotName.MkSlotName (Text.pack "thatBlocker")

-- CR 509.3b: `blockingCreature`'s mirror -- the reserved slot under which the
-- CREATURE THE BEARER BLOCKED is bound, Loyal Sentry's "that creature". Stamped
-- by Pawl.Engine.Event.eventBindings off the same GameEvent.BlockerDeclared,
-- read from the BLOCKING side, so the bearer is the blocker and this names the
-- attacker.
--
-- Distinct from `blockingCreature` rather than one "the other creature in this
-- declaration" slot: a card reads whichever of the two it is not, and a payload
-- that named the wrong one would still typecheck. Two slots make the mismatch a
-- dead name instead.
--
-- One slot, never a group, and not a target: the same posture as
-- `blockingCreature`, for the same reasons.
blockedCreature :: SlotName
blockedCreature = SlotName.MkSlotName (Text.pack "thatAttacker")

-- CR 506.5: the reserved slot under which the creature that was declared as an
-- attacker is bound -- rule 702.83a's "that creature", the one exalted pumps.
-- Stamped by Pawl.Engine.Event.eventBindings off GameEvent.AttackerDeclared --
-- the same event whose CR 508.5 defending player annihilator reads through
-- `triggerPlayer`, though this condition binds only the creature: rule 702.83a
-- names no player.
--
-- NOT `triggerSource` (CR 113.7a), and that is the whole reason it exists: rule
-- 702.83a's condition is "a creature YOU CONTROL attacks alone", so the bearer
-- watches the declaration and is routinely a different permanent from the
-- attacker -- an untapped Aven Squire held back while another creature attacks
-- is the ordinary case, not the corner one.
--
-- Distinct from `blockedCreature` even though both name an attacking creature:
-- that one is the attacker a BLOCK named (CR 509.3b), read from the blocker's
-- side, and a payload naming the wrong one would still typecheck. Two slots make
-- the mismatch a dead name instead.
--
-- One object, never a group: the slot is stamped per declaration event, and the
-- only condition that reads it is one that fires when the declaration named
-- exactly one creature. Not a target (nothing was chosen), so the same CR 608.2b
-- posture and the same "no card's targetSlots may name it" sweep as
-- `blockingCreature`; a dead id by resolution is the payload's problem, as it is
-- there.
attackingCreature :: SlotName
attackingCreature = SlotName.MkSlotName (Text.pack "thatAttackingCreature")

-- CR 120.1: the reserved slot under which the OBJECT THAT DEALT the damage --
-- its source -- is bound: Aragorn, Hornburg Hero's "double the number of +1/+1
-- counters on IT". Stamped by Pawl.Engine.Event.eventBindings off the same
-- GameEvent.DamageDealt the condition matched, so the payload is an ordinary slot
-- read rather than a "the creature that hit them" opcode.
--
-- The name is combat's but the slot is not: SelfIsDealtDamage stamps it for CR
-- 120.3's noncombat damage too, which is Belltower Sphinx's "that source's
-- controller". The wire spelling is `thatDamager` and cards already write it, so
-- the name stays.
--
-- Distinct from `triggerSource` (CR 113.7a) for the reason `attackingCreature`
-- is: the bystander form of the condition watches every permanent its controller
-- has, so the bearer is routinely a different permanent from the damager, and
-- Aragorn watching a Wolf of his own is the ordinary case rather than the corner
-- one. The self-scoped SelfDealsCombatDamageToPlayer needs no such slot, its
-- damager BEING the bearer.
--
-- One object per event, never a group: CR 510.2 deals all the assigned damage at
-- once, but Pawl.Engine.Damage records one DamageEvent per source-and-recipient
-- pair, so two creatures connecting are two events and two abilities each naming
-- one object.
--
-- Not a target (nothing was chosen), so the same CR 608.2b posture and the same
-- "no card's targetSlots may name it" sweep as `blockingCreature`. A dead id is
-- possible and is the payload's problem: a trampler can die to its blocker in the
-- same CR 510.2 event.
combatDamager :: SlotName
combatDamager = SlotName.MkSlotName (Text.pack "thatDamager")

-- CR 702.134c: the reserved slot under which the creature that WAS MENTORED is
-- bound -- Aegis of the Legion's "put a shield counter on that creature". Stamped
-- by Pawl.Engine.Event.eventBindings off GameEvent.Mentored, alongside
-- `blockingCreature` and the rest, so the payload is an ordinary slot read rather
-- than a "the creature my equipped creature mentored" opcode.
--
-- The MENTORED creature and not the mentor, which is the pair's other half: rule
-- 702.134c names both, and the printed sentence acts on the second. The first
-- needs no slot -- Aegis' condition already reaches it through the source's
-- attachment, and no printed payload names it.
--
-- Distinct from `triggerSource` (CR 113.7a) for `blockingCreature`'s reason and
-- one more: the bearer here is an Equipment, so the mentor is not the bearer
-- either, and the mentored creature is a third object again.
--
-- One object, never a group: rule 702.134a's ability has one target, so each
-- resolution mentors exactly one creature and two mentors are two events. Not a
-- target (nothing was chosen), so the same CR 608.2b posture and the same "no
-- card's targetSlots may name it" sweep as `blockingCreature`; a dead id by
-- resolution is the payload's problem, as it is there.
mentoredCreature :: SlotName
mentoredCreature = SlotName.MkSlotName (Text.pack "thatMentoredCreature")

-- A binding that names one object and nothing else -- what a token bound by a
-- Create (CR 603.7c) or a trigger's source slot holds.
toObject :: ObjectId -> Binding
toObject oid = toRecipients (Set.singleton (Recipient.ToObject oid))

-- The general form of toObject and toPlayer below: a binding that names these
-- recipients and nothing else. CR 601.2c's answer for one slot, which is a set
-- because one instance of the word "target" may take several.
toRecipients :: Set Recipient -> Binding
toRecipients rs = Binding.empty {Binding.targets = if Set.null rs then Nothing else Just rs}

-- A binding that names SEVERAL objects and nothing else -- what a Create binds
-- for a card that refers back to every token it made at once, Thatcher Revolt's
-- "those tokens". toObject's plural, and a distinct field rather than a list of
-- Recipients: this is a definition and never a target (CR 115.10a), so it is not
-- subject to CR 608.2b.
toObjects :: Seq ObjectId -> Binding
toObjects oids = Binding.empty {Binding.objects = Just oids}

-- A binding that names one player and nothing else -- CR 729.1b's subgame winner
-- and CR 608.2d's chosen opponent, both bound by Pawl.Engine.Resolve's
-- bindPlayerSlot. Mirrors toObject, but the recipient is a player (ToPlayer),
-- not an object.
toPlayer :: PlayerId -> Binding
toPlayer pid = toRecipients (Set.singleton (Recipient.ToPlayer pid))

-- A binding that names one NUMBER and nothing else -- what a Destroy that
-- counts what it destroyed binds for a later "for each ... destroyed this way"
-- to read, and what CR 615.13's prevented amount rides
-- (Quantity.InSlot). Mirrors toObject and toPlayer, but the value is an
-- amount rather than a recipient, so it rides the same field CR 601.2b's chosen
-- X does.
toAmount :: Natural -> Binding
toAmount n = Binding.empty {Binding.amount = Just n}

-- Bind an object under the reserved triggerSource slot. Dedicated and
-- single-purpose, so the insert cannot clobber another binding -- as below.
setTriggerSource :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setTriggerSource oid = Map.insert triggerSource (toObject oid)

-- Bind a player under the reserved you slot.
setYou :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setYou pid = Map.insert you (toPlayer pid)

-- Bind a player under the reserved triggerPlayer slot.
setTriggerPlayer :: PlayerId -> Map SlotName Binding -> Map SlotName Binding
setTriggerPlayer pid = Map.insert triggerPlayer (toPlayer pid)

-- Bind an object under the reserved became slot (CR 400.7e).
setBecame :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setBecame oid = Map.insert became (toObject oid)

-- Bind an object under the reserved castSpell slot (CR 601.2i).
setCastSpell :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setCastSpell oid = Map.insert castSpell (toObject oid)

-- Bind an object under the reserved targetingObject slot (CR 601.2c).
setTargetingObject :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setTargetingObject oid = Map.insert targetingObject (toObject oid)

-- Bind an object under the reserved blockingCreature slot (CR 509.3d).
setBlockingCreature :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setBlockingCreature oid = Map.insert blockingCreature (toObject oid)

-- Bind an object under the reserved blockedCreature slot (CR 509.3b).
setBlockedCreature :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setBlockedCreature oid = Map.insert blockedCreature (toObject oid)

-- Bind an object under the reserved attackingCreature slot (CR 506.5).
setAttackingCreature :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setAttackingCreature oid = Map.insert attackingCreature (toObject oid)

-- Bind an object under the reserved combatDamager slot (CR 510.2).
setCombatDamager :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setCombatDamager oid = Map.insert combatDamager (toObject oid)

-- Bind an object under the reserved mentoredCreature slot (CR 702.134c).
setMentoredCreature :: ObjectId -> Map SlotName Binding -> Map SlotName Binding
setMentoredCreature oid = Map.insert mentoredCreature (toObject oid)

-- Bind a number under the reserved eventAmount slot (CR 603.2).
setEventAmount :: Natural -> Map SlotName Binding -> Map SlotName Binding
setEventAmount n = Map.insert eventAmount (toAmount n)

-- The modes chosen for a spell, read from its binding environment. Empty when
-- absent (defensive; cast always stamps it, forced or prompted).
modesOf :: Map SlotName Binding -> Seq ModeIndex
modesOf m = Maybe.fromMaybe Seq.empty (Binding.modes =<< Map.lookup chosenModes m)

-- Project the chosen targets (CR 601.2c) out of a binding environment, dropping
-- slots with no target. An empty set is dropped with them: a slot whose count
-- was announced as zero (CR 115.6) is not a filled slot, and CR 608.2b's fizzle
-- measures the filled ones.
targetsOf :: Map SlotName Binding -> Map SlotName (Set Recipient)
targetsOf = Map.filter (not . Set.null) . Map.mapMaybe Binding.targets

-- The OBJECTS a binding environment names, one slot at a time: targetsOf with
-- the player recipients dropped, which is what Pawl.Engine.Filter.Context's
-- slotObjects holds so a Quantity.AgainstSlot can aim at one.
--
-- No CR 608.2b legality filter, unlike Pawl.Engine.Resolve.effectContext's
-- version: the callers here are CR 603.4's two intervening-"if" checks, and what
-- they aim at is a slot the EVENT bound (Binding.became), which was never chosen
-- and so was never a target to become illegal.
objectSlots :: Map SlotName Binding -> Map SlotName ObjectId
objectSlots = Map.mapMaybe (Recipient.objectOf Monad.<=< onlyOne) . targetsOf

-- The PLAYERS a binding environment names, one slot at a time: objectSlots' twin
-- on the other kind of Recipient, and what Pawl.Engine.Filter.bakeBound
-- substitutes into a target slot's CR 603.2 "that player" atom.
--
-- No CR 608.2b legality filter, objectSlots' reason unchanged and sharper here:
-- the slot this is read for is `triggerPlayer`, which the EVENT bound and which
-- was never a target to become illegal.
playerSlots :: Map SlotName Binding -> Map SlotName PlayerId
playerSlots = playersIn . targetsOf

-- playerSlots' inner half, over the PROJECTED targets a resolution already holds
-- rather than over a whole environment. Pawl.Engine.Resolve reads it that way:
-- its arms carry CR 601.2c's chosen recipients (already filtered by CR 608.2b)
-- rather than the object's bindings, and that is what a CR 611.2b duration's
-- condition is baked against at Pawl.Engine.Expiry.arm.
playersIn :: Map SlotName (Set Recipient) -> Map SlotName PlayerId
playersIn = Map.mapMaybe (Recipient.playerOf Monad.<=< onlyOne)

-- The ONE recipient a slot names, or Nothing when it names none or several. What
-- every reader that can point at one object and no more asks of a slot -- CR
-- 601.2c lets a slot hold several, and a reader that cannot take them must not
-- silently take one of them. Which readers those are is Resolve.pluralSlots, and
-- Pawl.CardSpec rejects a card that aims a multi-target slot at one of them.
onlyOne :: Set Recipient -> Maybe Recipient
onlyOne rs = case Set.toList rs of
  [r] -> Just r
  _ -> Nothing

-- The amount (X) bound at a slot, if any.
amountOf :: SlotName -> Map SlotName Binding -> Maybe Natural
amountOf slot m = Binding.amount =<< Map.lookup slot m

-- The objects bound as a GROUP at a slot, if any. Nothing when the slot holds a
-- single target instead, or nothing at all -- so a reader that offers both shapes
-- can tell "them" from "it" without a tag.
objectsOf :: SlotName -> Map SlotName Binding -> Maybe (Seq ObjectId)
objectsOf slot m = Binding.objects =<< Map.lookup slot m

-- The copy snapshot stored on an object, if any (CR 707.2).
copyOf :: Map SlotName Binding -> Maybe ProjectedCharacteristics
copyOf m = Binding.copy =<< Map.lookup copySource m

-- Store a copy snapshot under the reserved copySource slot. Nothing else is
-- ever stored there, so overwriting it wholesale is lossless.
setCopy :: ProjectedCharacteristics -> Map SlotName Binding -> Map SlotName Binding
setCopy pc = Map.insert copySource (Binding.empty {Binding.copy = Just pc})

-- Build the binding environment stamped on a stack object at cast: the chosen
-- targets, the chosen X under variableX, and any chosen modes under
-- chosenModes. The reserved names cannot collide with a target slot
-- (lint-enforced), so the merges below never actually merge; they are
-- insertWith rather than insert so a future binding kind sharing a slot keeps
-- both choices instead of clobbering one.
fromChoices ::
  Map SlotName (Set Recipient) ->
  Maybe Natural ->
  Seq ModeIndex ->
  Map SlotName Binding
fromChoices targets mAmount mModes =
  let fromTargets = Map.filter (Maybe.isJust . Binding.targets) (fmap toRecipients targets)
      withX = case mAmount of
        Nothing -> fromTargets
        Just n ->
          Map.insertWith mergeBinding variableX (Binding.empty {Binding.amount = Just n}) fromTargets
   in if Seq.null mModes
        then withX
        else Map.insertWith mergeBinding chosenModes (Binding.empty {Binding.modes = Just mModes}) withX

-- Combine two bindings for the same slot, preferring the left's present choice
-- in each field. Inputs are disjoint per field by construction, so this is a
-- total, order-independent merge.
mergeBinding :: Binding -> Binding -> Binding
mergeBinding a b =
  Binding.MkBinding
    { Binding.targets = Binding.targets a <|> Binding.targets b,
      Binding.amount = Binding.amount a <|> Binding.amount b,
      Binding.modes = Binding.modes a <|> Binding.modes b,
      Binding.copy = Binding.copy a <|> Binding.copy b,
      Binding.objects = Binding.objects a <|> Binding.objects b
    }

module Pawl.Types.Effect where

import qualified Data.Set as Set
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- | The ISA (design.md section 1): first-order, non-recursive (in CONTROL FLOW --
-- no loops, branches, or recursive calls; design.md line 38), no functions in any
-- field. The ONLY module that may case on a constructor is Pawl.Engine.Resolve -- the
-- rules core asks classifications, never identities.
--
-- The `card` parameter lets an opcode embed a card's characteristics (a created
-- token, a future copy) WITHOUT a module cycle: Card embeds [Effect Card], so a
-- concrete `Effect Card` reference here would make Card and Effect mutually
-- import each other. Parameterizing instead keeps this module Card-free; Card ties
-- the knot by instantiating `Effect Card` (and likewise the two ability types).
-- The resulting data nesting (a token-maker holds a card that could hold effects)
-- is structural, not a recursive CALL -- resolving a maker never evaluates the
-- embedded card's effects, so the control-flow non-recursion above still holds.
data Effect card
  = -- | CR 120.1: "Objects can deal damage to battles, creatures, planeswalkers,
    -- and players." Deal this much of it to what the ObjectRef names.
    --
    -- ObjectRef rather than a bare SlotName for the reason Destroy's comment
    -- gives at length -- one opcode for both the chosen recipient (Lightning
    -- Bolt's InSlot, filled by targeting) and the named set (Corrosive Gale's
    -- "each creature with flying", an EachMatching swept at resolution) rather
    -- than a sibling DamageAll to keep in step with it -- and with one wrinkle
    -- the other ObjectRef-taking opcodes do not have: CR 120.1a lets damage go to
    -- a PLAYER as well ("any target", CR 115.4), which no ObjectRef can name.
    -- That asymmetry is Resolve's to reconcile, and it is why the InSlot arm
    -- still reads a Recipient rather than an ObjectId.
    --
    -- A one-shot under CR 608.2c/608.2f: nothing is stored, so unlike
    -- ModifyTarget and GainControl this arm owes CR 611.2c no frozen set.
    DealDamage ObjectRef.ObjectRef Quantity.Quantity
  | -- | CR 611: create a continuous effect on the objects the ObjectRef names, for
    -- a duration. Giant Growth and Serpent's Gift are this one opcode, differing
    -- only in the Modification (layer 7c vs 6). Resolve stores it -- or, when a
    -- quantity inside it has no answer at resolution (CR 608.2h), stores nothing
    -- rather than a value it would have to re-read later. Either way Resolve
    -- never cases on the Modification.
    --
    -- ObjectRef rather than a bare SlotName for the reason Destroy's comment
    -- gives at length: one opcode serving both the chosen permanent (Giant
    -- Growth's InSlot, filled by targeting) and the named set (Trumpet Blast's
    -- "attacking creatures", an EachMatching swept at resolution), rather than a
    -- sibling ModifyAll to keep in step with it. Only InSlot is a target; CR
    -- 115.10a is the reason (ObjectRef's own comment).
    --
    -- CR 611.2c is the constraint the set arm has to meet, and it is what makes
    -- this widening more than mechanical: "the set of objects it affects is
    -- determined when that continuous effect begins. After that point, the set
    -- won't change." So Resolve sweeps ONCE, at resolution, and freezes the
    -- RESULT into the stored effect as Affected.TheseObjects -- never the Filter,
    -- which would be re-evaluated each projection and would then pump a creature
    -- that became attacking later. The one-shot opcodes that take an ObjectRef
    -- (Destroy, Untap) are under CR 608.2c/608.2f instead, and store nothing.
    ModifyTarget Duration.Duration Modification.Modification ObjectRef.ObjectRef
  | -- | CR 612: rewrite subtype words in the target spell or permanent. The
    -- SubtypeFamily is which words the card's own text names -- Magical Hack's
    -- "one basic land type", Artificial Evolution's "one creature type" -- and
    -- so which words the player is asked for; the Set is the words the NEW one
    -- may not be, Artificial Evolution's "The new creature type can't be Wall"
    -- (empty for a card that restricts nothing); the SlotName is the target
    -- slot. The two words are announced as this effect is applied (CR 608.2d,
    -- Prompt.ChooseLandTypeSwap / Prompt.ChooseCreatureTypeSwap) and baked into
    -- a stored ChangeSubtypeWord continuous effect. Resolve asks and stores;
    -- Projection applies.
    --
    -- The family is not stored alongside the pair: CR 612.2's gate reads the
    -- family of the word being REPLACED, and Pawl.Engine.Subtype answers that
    -- from the word itself.
    ChangeText SubtypeFamily.SubtypeFamily (Set.Set Subtype.Subtype) SlotName.SlotName
  | -- | CR 605: add one unit of mana, of the type the ManaProduction names -- one
    -- fixed type, or one colour its controller chooses (CR 105.4). ONE unit, so a
    -- mode adding more says so by holding the opcode more than once: Sol Ring's
    -- "{T}: Add {C}{C}" is two of these, and Mana.manaRoutesOfGiven reads a
    -- mode's whole list as one activation's yield. Executed by Mana.tapForMana at
    -- payment (CR 605.3b: a mana ability never uses the stack);
    -- Resolve.applyEffect never runs it. Read by ManaAbility.manaProduced (the
    -- "produces mana?" ABI bit).
    AddMana ManaProduction.ManaProduction
  | -- | CR 701.23: search the controller's library for a card matching the Filter,
    -- put it where the SearchDestination says, then shuffle. The Filter is
    -- evaluated over the PRINTED-card view (Projection.viewOfCard) -- a card in a
    -- library has no projection. Evolving Wilds' "basic land card" (CR 701.23a /
    -- 205.4c) is `And [HasCardType Land, HasSupertype Basic]`, and CR 702.29e's
    -- basic landcycling is the same filter with the other destination.
    --
    -- Finds at most one card, always: no card in the pool searches for two
    -- (#283).
    Search (Filter.Filter Keyword.Keyword) SearchDestination.SearchDestination
  | -- | CR 701.13 / Rest in Peace: exile every card in every graveyard. Targetless
    -- and bulk (Rest in Peace's exact shape); a general exile-from-zone is future.
    ExileAllGraveyards
  | -- | CR 727.1/727.1a: restart the game. Targetless and game-wide (the
    -- ExileAllGraveyards / BecomeMonarch shape); the starting player of the new
    -- game is the resolving controller, so no target slot is needed.
    RestartGame
  | -- | CR 723.1: "you control target player during that player's next turn."
    -- Installs pending control keyed to the slot's chosen player, with the
    -- ability's controller as the decider. Mindslaver's exact shape.
    ControlPlayerNextTurn SlotName.SlotName
  | -- | CR 701.8 / 702.12b: destroy the permanents the ObjectRef names -- move each
    -- to its owner's graveyard via the changeZone funnel UNLESS it is
    -- indestructible. NOT MoveToZone slot Graveyard: the indestructible check is
    -- why this is its own opcode (Murder vs Darksteel Myr). Indestructible aside,
    -- the destruction is itself interceptable: Pawl.Engine.Event.destroy offers a
    -- WouldBeDestroyed event to the CR 616.1 replacement loop before the
    -- graveyard move, which is how a regeneration shield (CR 701.19a) intercepts
    -- it.
    -- The Regenerability is CR 701.19c's "It can't be regenerated" rider, carried
    -- by the destruction rather than looked up on the victim -- Terror has it and
    -- the state-based action of CR 704.5g does not, for the same creature.
    --
    -- ObjectRef rather than a SlotName is what lets ONE opcode be both Murder's
    -- "destroy target creature" (InSlot, chosen at cast) and Day of Judgment's
    -- "destroy all creatures" (EachMatching, swept at resolution). A sibling
    -- DestroyAll opcode was the alternative, and it would have had to carry its
    -- own copy of the CR 702.12b gate, the CR 616.1 funnel and the CR 701.19c
    -- rider -- the duplication PlayerRef already exists to avoid on the player
    -- side (Draw's comment). Tap, Untap, ModifyTarget, GainControl and DealDamage
    -- have since taken the same parameter for the same reason -- ModifyTarget and
    -- GainControl additionally owing CR 611.2c a frozen set, since they STORE
    -- what they build; the other object-affecting opcodes still take a bare
    -- SlotName, none of them having a card that names a set (#378).
    --
    -- The Maybe SlotName BINDS how many permanents this destruction ACTUALLY
    -- destroyed into the effect SOURCE's live bindings -- the resolving spell
    -- itself for a spell, the source permanent for an ability, the same holder
    -- Create's minted-token slot uses -- so a later effect of the same resolution
    -- can read it back as Quantity.InSlot. That is Bane of Progress' "destroy all
    -- artifacts and enchantments. Put a +1/+1 counter on this creature for each
    -- permanent destroyed this way", where the two sentences are two ordinary
    -- opcodes joined by the slot rather than one fused opcode.
    --
    -- A DEFINITION, not a read: it is not a target and never appears in
    -- targetSpecs, exactly like Create's minted-token slot and PlaySubgame's
    -- loser slot.
    --
    -- ACTUALLY destroyed, which is not "matched by the ObjectRef": CR 702.12b's
    -- indestructible permanent and CR 701.19a's regenerated one are both swept at
    -- and neither is destroyed, and CR 701.8b says a permanent put into a
    -- graveyard any other way "hasn't been 'destroyed'". So the number comes back
    -- out of the destruction funnel (Event.destroyReturning) rather than from the
    -- length of the swept list.
    --
    -- A COUNT, not the set: a rider that acts on each destroyed permanent rather
    -- than on how many there were is not implemented (#463).
    Destroy ObjectRef.ObjectRef Regenerability.Regenerability (Maybe SlotName.SlotName)
  | -- | CR 701.21/701.21a: the slot's target permanent is sacrificed -- its
    -- CONTROLLER moves it to its OWNER's graveyard. NOT a destruction: CR 701.21a
    -- says so explicitly, so this consults neither indestructible (CR 702.12b) nor
    -- a regeneration shield (CR 701.19a), and is therefore not a reuse of Destroy.
    --
    -- One opcode, not a targetless SacrificeSelf plus a slotted variant: "this
    -- creature" is expressible because Engine.placeOne binds the trigger's
    -- SOURCE into the reserved Pawl.Engine.Binding.triggerSource slot, and "this
    -- creature" recurs far too often to pay for a second opcode.
    Sacrifice SlotName.SlotName
  | -- | CR 701.3 / 702.6a: attach THIS permanent (the effect's source) to the
    -- slot's target. "Equip [cost]" means "[Cost]: Attach this permanent to
    -- target creature you control", so the equipment is the source and the only
    -- slot is what it attaches TO -- the opcode carries one slot, not two.
    --
    -- CR 701.3a: if the source is already attached to something else, attaching
    -- it here moves it, and CR 701.3c restamps it. CR 701.3b: if it cannot legally
    -- be attached to the target it does not move at all, and attaching it to what
    -- it already holds does nothing.
    Attach SlotName.SlotName
  | -- | CR 701.3 / 303.4j: attach the SLOT'S TARGET -- an Aura, Equipment or
    -- Fortification already on the battlefield -- to an object chosen as this
    -- resolves. Crown of the Ages' "Attach target Aura attached to a creature to
    -- another creature".
    --
    -- The mirror image of Attach above, and a separate opcode rather than a
    -- field on it because the two differ in WHAT MOVES: equip attaches its own
    -- source and targets the destination, and this targets the thing that moves
    -- and does not target the destination at all. Crown of the Ages' Gatherer
    -- ruling is explicit -- "This only targets the Aura and not either creature"
    -- -- so the destination is a CHOICE on resolution, outside CR 608.2b's
    -- illegal-target check, which is why it is a bare Filter here and not a
    -- TargetSpec.
    --
    -- The Filter is the destination's card text ("another CREATURE" is
    -- `HasCardType Creature`; Aura Graft's "another permanent IT CAN ENCHANT" is
    -- `Filter.CanHostSubject`, the one atom that asks about the SUBJECT rather
    -- than about the candidate); the candidates it narrows are the permanents on
    -- the battlefield. The "another" is NOT in the Filter: CR 701.3b's second
    -- sentence makes attaching a permanent to what it already holds do nothing
    -- whatever the card says, so the opcode always excludes the current host and
    -- a card that omitted the word would behave identically.
    --
    -- CR 303.4j / 701.3b's FIRST sentence is the failure mode, and it is not a
    -- fizzle: a destination the subject cannot legally be attached to leaves it
    -- exactly where it was -- unmoved and unrestamped -- while the rest of the
    -- ability resolves normally. Only a card whose text does NOT already exclude
    -- such a destination can reach it -- Crown of the Ages can, Aura Graft cannot
    -- -- which is why the rule and the atom are not the same thing.
    AttachTarget SlotName.SlotName (Filter.Filter Keyword.Keyword)
  | -- | CR 400.7: move the slot's target object to a zone through the changeZone
    -- funnel. Bounce = MoveToZone slot Hand (owner-relative -- changeZone carries
    -- Object.owner); targeted exile = MoveToZone slot Exile. The destination is
    -- data; one opcode for every targeted single-object move. Distinct from
    -- Destroy (unconditional move, no indestructible check).
    --
    -- The EntryRiders are what the effect says about the object AS IT ENTERS the
    -- battlefield, beyond its own text -- Meandering Towershell's "return it to
    -- the battlefield tapped and attacking" -- and are shared with Create for
    -- the reason that type's own comment gives (CR 109.3: neither is a
    -- characteristic). They mean nothing for any other destination, and Resolve
    -- applies neither rider there. Resolve reads them; it never cases on them.
    --
    -- The Maybe SlotName BINDS the incarnation CR 400.7 mints at the
    -- DESTINATION into the resolving object's live bindings, exactly as Create's
    -- minted-token slot does and for the same rule: a delayed ability this same
    -- resolution arms (CR 603.7c's "it") must be able to name the object, and
    -- after a zone change the id it was named by is gone. Meandering Towershell
    -- is the producer -- "exile it. Return IT to the battlefield ..." -- where
    -- the two "it"s are two incarnations of one card. A DEFINITION, not a read:
    -- it is not a target and never appears in targetSpecs.
    MoveToZone SlotName.SlotName Zone.Zone EntryRiders.EntryRiders (Maybe SlotName.SlotName)
  | -- | CR 121.1: the players the PlayerRef names each draw this many cards, one
    -- at a time (CR 121.2). Divination is `Relative You`; Ancestral Recall's
    -- "target player draws three cards" is `InSlot`, reading a slot that
    -- TARGETING filled (CR 601.2c). Empty-library draw is a loss (CR 104.3c),
    -- unlike Mill -- the semantic asymmetry that keeps Draw and Mill separate.
    --
    -- PlayerRef rather than a `Draw SlotName Quantity` sibling: the sibling
    -- keeps this opcode's arm count at one but leaves two draw opcodes to keep
    -- in step, which the effect DSL otherwise avoids (cf. MoveToZone's one
    -- opcode for every targeted single-object move).
    Draw PlayerRef.PlayerRef Quantity.Quantity
  | -- | CR 701.17: the slot's target player mills this many (top N of their library
    -- to their graveyard). Milling a short/empty library mills fewer, no penalty
    -- (CR 701.17b) -- unlike Draw, which loses on empty.
    Mill SlotName.SlotName Quantity.Quantity
  | -- | CR 701.9: the slot's target player discards this many. The DISCARDING player
    -- chooses which (CR 701.9b) via Prompt.ChooseDiscard, routed through
    -- Decide.deciderFor. A hand smaller than the count discards all of it (CR
    -- 609.3, "does only as much as possible"), forced -- so it is not prompted.
    Discard SlotName.SlotName Quantity.Quantity
  | -- | CR 119.3: "If an effect causes a player to gain life or lose life, that
    -- player's life total is adjusted accordingly." The players the PlayerRef
    -- names each lose this much. Sign in Blood's "target player ... loses 2
    -- life" is `InSlot`, reading a slot that TARGETING filled (CR 601.2c); a
    -- "you lose N life" drawback is `Relative You`. PlayerRef rather than the
    -- SlotName Mill and Discard take, for the reason Draw's own comment gives:
    -- a slot-only recipient needs a sibling opcode the first time a card says
    -- "you lose N life", and one opcode is easier to keep correct than two. The
    -- CR 704.5a state-based action that may follow is the existing one in
    -- Pawl.Engine.Sba.
    --
    -- NOT a DealDamage aimed at a player. CR 119.2's "damage dealt to a player
    -- normally causes that player to lose that much life" runs one way only, so
    -- routing life loss through the damage funnel would wrongly subject it to
    -- CR 614/615's damage replacement and prevention, to infect's CR 120.3b
    -- diversion (which turns the whole amount into poison counters, losing NO
    -- life), and to toxic's CR 120.3g rider -- and it would append a
    -- GameEvent.DamageDealt for CR 704.5h's deathtouch scan and every
    -- damage-history reader to consume.
    --
    -- GainLife below is the sibling, and is a SEPARATE opcode rather than a
    -- signed amount on this one: CR 119.3 states both in one sentence, but they
    -- are distinct game events for triggers ("whenever you gain life"), which a
    -- signed amount would fuse into one.
    LoseLife PlayerRef.PlayerRef Quantity.Quantity
  | -- | CR 119.3: "If an effect causes a player to gain life or lose life, that
    -- player's life total is adjusted accordingly." The players the PlayerRef
    -- names each gain this much -- Soul Warden's "you gain 1 life" is
    -- `Relative You`. LoseLife's mirror in every respect but the sign, and
    -- separate from it for the reason LoseLife's own comment gives.
    --
    -- No state-based action follows a gain (CR 704.5a is about a life total of 0
    -- or less), so unlike LoseLife this one can never kill anybody.
    GainLife PlayerRef.PlayerRef Quantity.Quantity
  | -- | CR 111: create this many tokens with the given effect-defined characteristics
    -- (CR 111.3). The `card` is the token's "text", embedded literally in the card
    -- data (a nested card, tied to Card by Card's own instantiation; the codec and
    -- round-trip cover it). Quantity is how many (reused from M4a as
    -- Draw/Mill/Discard do); Create (Literal 2) mints two distinct objects.
    -- Targetless and unprompted -- creating a token is never a choice. Executed by
    -- Resolve.applyEffect via Event.createTokens. NOT a copy-token (CR 707) and NOT a
    -- predefined token (CR 111.10): given, not derived.
    --
    -- The EntryRiders is what the effect says about the tokens beyond their text
    -- -- Hanweir Garrison's "that are tapped and attacking" -- and is not part of
    -- the embedded card for the reason that type's own comment gives (CR 109.3:
    -- neither is a characteristic). Resolve reads it; it never cases on it.
    --
    -- The Maybe SlotName BINDS the minted token into the resolving object's LIVE
    -- Object.bindings, so a delayed ability armed by this same resolution (CR
    -- 603.7c's "it") -- which re-reads that live state when ArmDelayedTrigger
    -- captures it -- can name the token. It does NOT make the token visible to a
    -- LATER EFFECT in the same resolution: applyEffect's `chosen` map is computed
    -- once before the effect fold begins, so a later Sacrifice/Destroy/etc. in the
    -- same list still reads the pre-Create snapshot. A DEFINITION, not a read: it
    -- is not a target and never appears in targetSpecs. Defined only for a
    -- single-token create; a Create that binds a slot while making several tokens
    -- is rejected by the Pawl.CardSpec lint family rather than guessed at (#53).
    Create Quantity.Quantity card EntryRiders.EntryRiders (Maybe SlotName.SlotName)
  | -- | CR 614.3 / 615.3: install a floating replacement effect for a duration, with
    -- a use count, an origin and an optional condition. Fog is
    -- `Replace UntilEndOfTurn Unlimited Other Nothing (DamageR (MkDamagePattern (Just Combat) AnySource) PreventAll)`;
    -- Drudge Skeletons' ability is
    -- `Replace UntilEndOfTurn Once Other Nothing (DestructionR Regenerate)`.
    --
    -- ONE opcode for all of them, where M3f/M4d had two separate opcodes -- a
    -- `Prevent` and a one-shot self-regenerate -- because the difference between
    -- a Fog and a regeneration shield is which event class the payload names,
    -- which is data. Targetless (a floating replacement watches a CLASS of
    -- events, not a chosen object) and unprompted. Resolve stores it into
    -- GameState.replacements with this effect's SOURCE (CR 113.7) and a fresh
    -- timestamp; Pawl.Engine.Replacement applies it.
    --
    -- The ReplacementOrigin is CR 614.15's self-replacement bit, and it is on
    -- this opcode for the same reason: a self-replacement is created by "a
    -- resolving spell or ability", which is precisely what installs one of these,
    -- and nothing in the ReplacementEffect payload could say it (see
    -- Pawl.Types.ReplacementOrigin). Galvanic Blast's metalcraft clause is the
    -- one SelfReplacement in the pool.
    --
    -- The Condition is the "if" a replacement-creating clause may carry --
    -- Galvanic Blast's "if you control three or more artifacts". Nothing is the
    -- unconditional case, which is every other card. Resolve checks it as this
    -- effect resolves and installs nothing when it fails, and that is exact for
    -- the pool rather than a shortcut: the only producer is a CR 614.15
    -- self-replacement, whose installation and whose application both happen
    -- inside one resolution with no window between them for the board to change.
    -- A conditional replacement that OUTLIVES its resolution -- where a
    -- check-on-install and a check-on-apply would visibly differ -- has no
    -- producer (#587).
    --
    -- A field on this opcode rather than a general conditional Effect arm, and
    -- the difference is not cosmetic. This gates ONE opcode's own creation of
    -- ONE object, so the effect list is still a straight-line sequence a static
    -- analysis can read end to end -- Duration.ForAsLongAs is already gated the
    -- same way, by CR 611.2b, and Resolve's arm below handles both in one
    -- expression. An `If condition [Effect] [Effect]` arm would instead put a
    -- BRANCH between two effect lists, which is the control flow design.md
    -- section 1 keeps out of the ISA. What the card asks for is the narrow
    -- shape: a base effect plus a conditional replacement of that effect, never
    -- one effect or another.
    Replace Duration.Duration Uses.Uses ReplacementOrigin.ReplacementOrigin (Maybe Condition.Condition) ReplacementEffect.ReplacementEffect
  | -- | CR 614.10a: each player the PlayerRef names skips their NEXT occurrence of
    -- this step or phase. Fatigue is
    -- `SkipNextPhase (InSlot "target") (Step (Beginning DrawStep))`; Stonehorn
    -- Dignitary is `SkipNextPhase (InSlot "target") CombatPhase`, naming a phase
    -- rather than one of its steps (CR 500.1).
    --
    -- NOT a Replace carrying a PhaseR, and not for want of trying: CR 614.1b
    -- makes this a replacement effect and Replace already installs floating ones,
    -- but the pattern has to name a player who is only known at resolution, and a
    -- ReplacementEffect written on a card cannot. Exactly the reason GainControl
    -- is its own opcode rather than a ModifyTarget carrying SetController --
    -- Resolve bakes the PlayerId, and the alternative (a slot name inside the
    -- pattern, resolved later) would make Pawl.Engine.Resolve case on a
    -- ReplacementEffect's identity, which Pawl.Engine.Replacement's header reserves to
    -- itself.
    --
    -- No Duration and no Uses, unlike Replace: CR 614.10a's "next" IS the use
    -- count (one occurrence, then gone), and Fatigue states no duration, so CR
    -- 614.3's "until they're used up" is the whole of the lifetime. Resolve
    -- installs one floating replacement PER NAMED PLAYER with Uses.Once and
    -- Expiry.Never.
    --
    -- Targetless in itself, like GainPlayerCounters: the slot a PlayerRef reads
    -- may have been filled by targeting (CR 601.2c), which is how Fatigue writes
    -- "target player", but nothing here demands it.
    SkipNextPhase PlayerRef.PlayerRef PhaseSelector.PhaseSelector
  | -- | CR 615.7: install a prevention SHIELD over the recipients an ObjectRef
    -- names -- "Prevent the next 4 damage that would be dealt to any target this
    -- turn" (Mending Hands) -- for a duration. The Quantity is the shield's
    -- printed size; the shield then counts DAMAGE down from it (see
    -- Pawl.Types.DamageRewrite.PreventNext).
    --
    -- An ObjectRef rather than a slot, and the same one DealDamage takes, because
    -- CR 115.4's "any target" reaches a PLAYER as well as a permanent and only
    -- Resolve.objectRefRecipients answers in that vocabulary. One shield per
    -- recipient the ref names (CR 615.11's shape for free, though no card in the
    -- pool names more than one).
    --
    -- NOT a Replace carrying a DamageR, and for exactly the reason SkipNextPhase
    -- is not a Replace carrying a PhaseR: the pattern has to name the shielded
    -- permanent or player, which is only known at resolution, and a
    -- ReplacementEffect written on a card cannot name one. Resolve bakes the
    -- Recipient into DamagePattern.whichRecipient, and the alternative -- a slot
    -- name inside the pattern, resolved later -- would make Pawl.Engine.Resolve
    -- case on a ReplacementEffect's identity, which Pawl.Engine.Replacement's
    -- header reserves to itself.
    --
    -- A Duration, unlike SkipNextPhase and like Replace: CR 615.3 gives a
    -- prevention effect two terminators ("until they're used up or their
    -- duration has expired"), the shield's own count is the first, and Mending
    -- Hands' "this turn" is the second, printed on the card rather than assumed
    -- by the engine.
    --
    -- No Uses field: CR 615.7's shield is spent in damage, not in applications,
    -- so Resolve installs it Unlimited and the count terminates it.
    PreventNextDamage Duration.Duration ObjectRef.ObjectRef Quantity.Quantity
  | -- | CR 701.6: counter the slot's target -- "cancel it, removing it from the
    -- stack. It doesn't resolve and none of its effects occur" (CR 701.6a) --
    -- via the Event.counter funnel. ONE opcode for both of rule 701.6a's
    -- subjects: Cancel's slot is a Pool.Spells one and Stifle's a
    -- Pool.Abilities one, and which ending the countering has (the owner's
    -- graveyard for a spell, CR 608.2n's cease for an ability) is the funnel's
    -- own classification of the object it is handed, not a second opcode. CR
    -- 113.9 keeps the two apart where it belongs, in the target pool: an effect
    -- that counters only spells cannot reach an ability.
    --
    -- Distinct from MoveToZone slot Graveyard the way Destroy is (M4b): Counter
    -- is a keyword action on rule 701's list, it carries the CR 113.6g "can't be
    -- countered" gate, and countering a SPELL records a distinct "was countered"
    -- event that the zone change alone could not be told apart from.
    Counter SlotName.SlotName
  | -- | CR 122.6: put this many counters of this kind on the slot's target permanent.
    -- Battlegrowth = PutCounters PlusOnePlusOne (Literal 1) slot; Instill Infection
    -- = PutCounters MinusOneMinusOne (Literal 1) slot. A counter is persistent
    -- object state, NOT a zone change -- Resolve.applyEffect edits Object.counters
    -- in place (Map.insertWith (+)), never through Event.changeZone. Quantity is how
    -- many (reused from M4a; a future X-counter card rides ChooseX). The counter's
    -- P/T effect is applied by the projection (CR 122.1a / 613.4c), not here.
    PutCounters CounterKind.CounterKind Quantity.Quantity SlotName.SlotName
  | -- | CR 122 / 107.14: the players the PlayerRef names each get N counters of a
    -- player-counter kind. Subsumes any self-scoped player counter (energy,
    -- experience, rad) as `Relative You` -- Longtusk Cub's "you get {E}{E}" --
    -- without a new opcode.
    --
    -- The PlayerRef is what lets a player OTHER than the resolving controller
    -- receive them: CR 702.70a's "that player gets N poison counters" is
    -- `InSlot Binding.triggerPlayer`, reading the player the trigger's own event
    -- named. PlayerRef, not PlayerScope, for the reason PlayerRef's own comment
    -- gives -- only PlayerRef can name a binding slot.
    --
    -- Still targetless in itself: a slot this reads may have been filled by
    -- TARGETING (CR 601.2c), which is how a future "target player gets two
    -- poison counters" is written, but nothing here demands it (#120).
    GainPlayerCounters PlayerRef.PlayerRef PlayerCounterKind.PlayerCounterKind Quantity.Quantity
  | -- | CR 701.26a: "To tap a permanent, turn it sideways from an upright
    -- position. Only untapped permanents can be tapped." The exact mirror of
    -- Untap below, down to the ObjectRef -- Dream's Grip's "tap target
    -- permanent" is `InSlot`, and an "all creatures your opponents control" tap
    -- would be `EachMatching`.
    --
    -- A permanent that is ALREADY tapped is left alone rather than being an
    -- error, which is rule 701.26a's own second sentence: there is no such
    -- event, so nothing happens to it. That falls out of the resolution being an
    -- assignment to TapState.Tapped and needs no arm of its own.
    Tap ObjectRef.ObjectRef
  | -- | CR 701.26b: untap the permanents the ObjectRef names. Act of Treason's
    -- "untap that creature" is `InSlot`; Aggravated Assault's "untap all
    -- creatures you control" and Relentless Assault's "untap all creatures that
    -- attacked this turn" are both `EachMatching`, swept at resolution.
    --
    -- ObjectRef rather than a bare SlotName for the reason Destroy's comment
    -- gives at length: one opcode serving both the chosen permanent and the
    -- named set, rather than a sibling UntapAll to keep in step with it.
    Untap ObjectRef.ObjectRef
  | -- | CR 506.4: "A permanent is removed from combat if ... an effect
    -- specifically removes it from combat." THIS is that effect -- the rule's one
    -- clause that a card ASKS for rather than a condition the engine has to
    -- notice, which is why it is an opcode and not a sampler like
    -- Combat.removeChanged. Labyrinth of Skophos' "{4}, {T}: Remove target
    -- attacking or blocking creature from combat" is the card text it exists
    -- for; the slot's target is what leaves.
    --
    -- Removal ONLY. CR 506.4's second sentence -- "a creature that's removed from
    -- combat stops being an attacking, blocking, blocked, and/or unblocked
    -- creature" -- is the whole of the effect, and nothing in rule 506 puts a
    -- creature back, so there is no inverse opcode and no duration to carry.
    --
    -- A bare SlotName rather than Destroy's ObjectRef: the printed card names one
    -- target, and the "each matching" half of ObjectRef exists for a card that
    -- sweeps a set (Day of Judgment). None does here, and the narrower parameter
    -- is what the pool exercises.
    --
    -- CR 506.4a and CR 506.4b bound what removal is NOT, and neither reaches this
    -- opcode: 506.4a keeps a spell that "would have kept that creature from
    -- attacking or blocking" from removing an already-declared creature, and
    -- 506.4b says tapping or untapping does not remove one either. Both are about
    -- effects that do something ELSE; this one says "remove from combat" in as
    -- many words, which is precisely what the rule's own clause names.
    RemoveFromCombat SlotName.SlotName
  | -- | CR 500.8: "Some effects can add phases to a turn. They do this by adding
    -- the phases directly after the specified phase." The payload says which
    -- phases, in written order: Aggravated Assault and Relentless Assault are
    -- `[ExtraCombat, ExtraMain]`, Aurelia, the Warleader is `[ExtraCombat]`, and
    -- Full Throttle is `[ExtraCombat, ExtraCombat]`.
    --
    -- A payload rather than a sibling opcode per shape, because CR 500.8 does not
    -- fix which phases are added and the printed cards genuinely differ. The
    -- list may be empty in the type; no card writes one, and an empty splice is
    -- a no-op rather than a case to guard.
    --
    -- Targetless and unprompted -- CR 500.8 leaves nothing to choose. Executed
    -- by Resolve.applyEffect via Turn.splicePhases, which is where both the CR
    -- 505.1a/506.1 detail of WHAT is inserted and the CR 511.3 question of WHERE
    -- live.
    AddPhases [ExtraPhase.ExtraPhase]
  | -- | CR 613.1b / 611.2c: install a layer-2 control effect on the objects the
    -- ObjectRef names, for a duration. The new controller is THIS effect's
    -- source's controller (the `controller` passed to applyEffect), baked into a
    -- stored SetController continuous effect -- derived, never chosen. Also
    -- re-Sicks each object whose controller actually changed (CR 302.6: the new
    -- controller has not controlled it continuously). NOT a reuse of
    -- ModifyTarget, whose Modification is static card data and cannot carry a
    -- resolution-time PlayerId. Permanent control (CR 613), distinct from
    -- Mindslaver's player-control (CR 723, ControlPlayerNextTurn).
    --
    -- ObjectRef for the reason Destroy's comment gives: Act of Treason's control
    -- clause is the InSlot arm, and Aura Thief's "you gain control of all
    -- enchantments" is the EachMatching one. Like ModifyTarget, and unlike the
    -- one-shots, the swept set has to be FROZEN into the stored effect (CR
    -- 611.2c names controller changes in as many words), so an enchantment that
    -- enters afterwards is not stolen.
    GainControl Duration.Duration ObjectRef.ObjectRef
  | -- | CR 603.7: create a delayed triggered ability -- the one this card declares
    -- under this name (Card.delayedAbilities). First-order: the payload is card
    -- data joined by a name, so this opcode carries no nested ability and adds no
    -- type parameter. The resolving object's binding environment is captured as
    -- the ability is armed, which is how "it" / "that card" (CR 603.7c) is
    -- remembered after this resolution ends.
    --
    -- The Duration is CR 603.7b's "stated duration, such as 'this turn'" --
    -- Full Throttle's "at the beginning of each combat this turn". Nothing is
    -- that rule's default, "only once, the next time its trigger event occurs"
    -- (Tidal Wave), and it is Nothing rather than a Duration arm meaning "once"
    -- because the rule words once-ness as the ABSENCE of a duration.
    --
    -- The Onset is the same envelope's other end -- when the ability becomes
    -- armed rather than when it stops being. Immediately for everything but
    -- Meandering Towershell's "on your next turn"; see Pawl.Types.Onset for why
    -- a total field rather than a second Maybe, and why the gate cannot live in
    -- the ability's own trigger condition.
    ArmDelayedTrigger AbilityName.AbilityName Onset.Onset (Maybe Duration.Duration)
  | -- | CR 611.1 / 613.11: install a stored PLAYER or RULES-modifying continuous
    -- effect on a class of players for a duration. Silence is
    -- `AffectPlayers UntilEndOfTurn Opponents CantCastSpells`.
    --
    -- Targetless, mirroring Replace: a rules-modifying effect watches a CLASS,
    -- not a chosen object, so there is nothing to target and nothing to prompt.
    -- Resolve stores it into GameState.playerEffects with this effect's source,
    -- its controller (CR 109.5, baked in -- the source may be in a graveyard by
    -- the time anyone asks), a fresh timestamp, and Expiry.arm's answer.
    AffectPlayers Duration.Duration PlayerScope.PlayerScope PlayerEffect.PlayerEffect
  | -- | CR 114.2: "[you] get an emblem with [abilities]." Puts an emblem owned and
    -- controlled by the resolving controller into the command zone. Targetless
    -- (the beneficiary is always the resolving controller); the abilities ride a
    -- Card so the emblem reuses the whole ability pipeline. First-order: a data
    -- Card, tied to Card by Card's own instantiation, exactly as Create's is.
    CreateEmblem card
  | -- | CR 725: a player becomes the monarch. Targetless; the beneficiary is named
    -- by the MonarchTarget (the resolving controller, or the controller of the
    -- ability's bound source). Setting the monarch emits GameEvent.BecameMonarch.
    BecomeMonarch MonarchTarget.MonarchTarget
  | -- | CR 725 (Palace Jailer): exile the slot's target UNTIL an opponent of the
    -- effect's controller becomes the monarch. The exile is the usual targeted
    -- move; the DURATION is the novelty -- the exiled incarnation is registered in
    -- GameState.exiledUntilMonarch and returned by Pawl.Engine.Monarch's settle-loop
    -- sweep. NOT MoveToZone: that has no duration and schedules no return.
    ExileUntilMonarch SlotName.SlotName
  | -- | CR 729.1/729.1b: play a Magic subgame, then bind its outcome (the derived
    -- loser) into this slot for a later effect to read. This slot is DEFINED here
    -- (like Create's minted-token slot), not a cast-time target -- the loser is
    -- determined only when the subgame ends, so the following effect reads it
    -- through the per-effect binding re-read in resolveSpellWith. Generic: the
    -- engine reaches subgames through this opcode, never Shahrazad's identity.
    PlaySubgame SlotName.SlotName
  | -- | CR 103.5b (Serum Powder): exile every card in the resolving controller's
    -- hand, then draw that many cards. Targetless and controller-scoped, the
    -- ExileAllGraveyards shape -- unlike Draw, which names its recipient.
    --
    -- ONE opcode rather than an exile composed with a Draw: "that many" is the
    -- hand size BEFORE the exile, so a following Draw would read a hand that is
    -- already empty. Splitting it needs a Count that reads a value produced
    -- earlier in the same resolution, which nothing else wants.
    --
    -- The card granting the action is itself in the hand and is exiled with the
    -- rest: CR 103.5b's action is not a cost, and nothing sets it aside.
    ExileHandThenDraw
  | -- | CR 701.34a: choose any number of permanents and/or players that have a
    -- counter, then give each one additional counter of each kind it already has.
    --
    -- CHOOSE, not target (CR 701.34a says "choose"): there is no target spec, the
    -- set is picked on RESOLUTION via Prompt.ChooseProliferate, and nothing here
    -- is subject to CR 608.2b's illegal-target check. That is why this carries no
    -- SlotName.
    --
    -- Nullary: rule 701.34a fixes the count at one per kind, with no quantity,
    -- kind or scope left to vary. Object counters ride Replacement.putCounters, so CR
    -- 614's counter replacements (Hardened Scales, Doubling Season) get their
    -- opportunity; player counters are added directly, matching
    -- GainPlayerCounters and gapped for the same reason (#122).
    Proliferate
  | -- | CR 701.21a: the slot's target PLAYER sacrifices this many permanents
    -- matching the Filter, chosen by that player. Diabolic Edict's exact shape.
    --
    -- Distinct from Sacrifice, which names a PERMANENT and is "this creature".
    -- The difference is who decides: there the effect picks the victim, here the
    -- sacrificing player does, which is why this one prompts and that one does
    -- not. The sibling of Mill and Discard, which likewise name a player and a
    -- count.
    --
    -- The Filter carries "a creature of their choice" rather than baking creatures
    -- in: edicts differ only in what they name (a creature, a permanent, a land),
    -- and Filter already says all three.
    --
    -- CR 609.3: a player with fewer matching permanents than the count sacrifices
    -- all of them, and one with none sacrifices nothing -- "as much as possible",
    -- and forced, so neither case is prompted.
    PlayerSacrifices SlotName.SlotName (Filter.Filter Keyword.Keyword) Quantity.Quantity
  | -- | CR 500.7: "Some effects can give a player extra turns. They do this by
    -- adding the turns directly after the specified turn." The players the
    -- PlayerRef names each get one extra turn, added directly after the turn
    -- this resolves in. Time Warp's "target player takes an extra turn after
    -- this one" is `InSlot`, reading a slot that TARGETING filled (CR 601.2c).
    --
    -- PlayerRef rather than the bare SlotName Mill and Discard take, for the
    -- reason Draw's own comment gives: a card whose extra turn is its caster's
    -- writes `Relative You` and needs no sibling opcode to say so. Savor the
    -- Moment's "take an extra turn after this one" is that card.
    --
    -- No count and no "which turn": every printed extra-turn card adds ONE turn
    -- directly after the current one, and CR 500.7's "if a player is given
    -- multiple extra turns, the extra turns are added one at a time" is about
    -- several such effects rather than one effect adding several. WHERE the
    -- turns go and in what order they are taken is Engine.handoffTurn's
    -- question, which reads GameState.extraTurns as the stack CR 500.7's last
    -- sentence describes.
    --
    -- CR 500.11 / 614.1b: the PhaseSelectors are the steps and phases the
    -- created turn SKIPS -- Savor the Moment's "skip the untap step of that
    -- turn" pairs `Relative You` with the singleton `Step (Beginning Untap)`.
    -- Empty for Time Warp, which skips nothing.
    --
    -- One opcode carrying a payload, rather than the card's two sentences
    -- becoming two opcodes, because "that turn" has to name the turn this same
    -- resolution just created. A separate skip opcode would need a reference to
    -- it, and CR 500.7's "the most recently created turn will be taken first"
    -- means the obvious spelling of that reference -- SkipNextPhase's CR 614.10a
    -- "next" -- names a DIFFERENT turn as soon as another extra-turn effect
    -- resolves afterwards. Carried on the turn, the scoping cannot be got wrong:
    -- the skips go wherever CR 500.7's stack puts the turn.
    TakeExtraTurn PlayerRef.PlayerRef (Set.Set PhaseSelector.PhaseSelector)
  | -- | CR 701.24: the slot's target object is shuffled into its OWNER's library --
    -- Riftsweeper's "choose target face-up exiled card. Its owner shuffles it
    -- into their library." The move goes through the changeZone funnel (CR
    -- 400.7's new incarnation), landing in the OWNER's library by CR 400.3 --
    -- "if an object would go to any library, graveyard, or hand other than its
    -- owner's, it goes to its owner's corresponding zone" -- which is the rule
    -- the card's own "its owner" is restating. Then that library is shuffled
    -- (CR 701.24a: "randomize the cards within it so that no player knows their
    -- order").
    --
    -- NOT MoveToZone slot Library, and Counter's own comment gives the template
    -- -- the three reasons it is not MoveToZone slot Graveyard line up one for
    -- one here:
    --
    --   * Shuffle is a KEYWORD ACTION in its own right, on rule 701's list
    --     (701.24), beside counter (701.6) and destroy (701.8).
    --   * It carries a gate the bare move does not. CR 701.24c: "if an effect
    --     would cause a player to shuffle one or more specific objects into a
    --     library, that library is shuffled EVEN IF none of those objects are in
    --     the zone they're expected to be in or an effect causes all of those
    --     objects to be moved to another zone or remain in their current zone."
    --     So a CR 616.1 replacement that cancels the move must not cancel the
    --     shuffle -- which a rider read off the move's own result would.
    --   * A shuffle is its own observable event, the way "was countered" is: CR
    --     701.24e and CR 701.24f are both about "abilities that trigger when a
    --     library is shuffled", which a library move alone does not fire.
    --
    -- No PlayerRef saying whose library, and none is expressible: the answer is
    -- the OWNER of the object the slot names, and PlayerRef's three arms are
    -- every player, a relation to the perspective, and a player bound in a slot
    -- -- none of which can read an owner off a bound OBJECT. Derived rather than
    -- named is also what the card says ("its owner"), and it is what makes this
    -- one opcode rather than a move plus a shuffle: Resolve.resolveModes fixes a
    -- resolving ABILITY's bindings before its effect fold begins, so a later
    -- effect could not read an incarnation this one had just bound anyway.
    ShuffleIntoLibrary SlotName.SlotName
  deriving (Eq, Ord, Show)

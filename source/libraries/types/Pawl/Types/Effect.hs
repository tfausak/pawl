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

-- | The ISA (design.md section 1): first-order, non-recursive in CONTROL FLOW --
-- no loops, branches, or recursive calls -- and with no functions in any field.
-- The ONLY module that may case on a constructor is Pawl.Engine.Resolve; the
-- rules core asks classifications, never identities.
--
-- The `card` parameter lets an opcode embed a card's characteristics (a created
-- token, a future copy) WITHOUT a module cycle: Card embeds [Effect Card], so a
-- concrete `Effect Card` here would make the two mutually importing. Card ties
-- the knot by instantiating `Effect Card`. The resulting data nesting is
-- structural, not a recursive CALL -- resolving a token-maker never evaluates
-- the embedded card's effects.
data Effect card
  = -- | CR 120.1: deal this much damage to what the ObjectRef names.
    --
    -- ObjectRef rather than a SlotName for Destroy's reason -- one opcode for
    -- Lightning Bolt's chosen recipient and Corrosive Gale's swept set alike --
    -- with one wrinkle the other ObjectRef opcodes lack: CR 120.1a / 115.4 let
    -- damage reach a PLAYER, which no ObjectRef can name, so the InSlot arm
    -- reads a Recipient rather than an ObjectId and Resolve reconciles it.
    --
    -- A one-shot under CR 608.2c/608.2f: nothing is stored, so unlike
    -- ModifyTarget and GainControl it owes CR 611.2c no frozen set.
    DealDamage ObjectRef.ObjectRef Quantity.Quantity
  | -- | CR 611: create a continuous effect on the objects the ObjectRef names,
    -- for a duration. Giant Growth and Serpent's Gift are this one opcode,
    -- differing only in the Modification (layer 7c vs 6), which Resolve stores
    -- without ever casing on it -- or, when a quantity inside it has no answer at
    -- resolution (CR 608.2h), stores nothing rather than a value it would have to
    -- re-read later.
    --
    -- ObjectRef for Destroy's reason; only InSlot is a target (CR 115.10a). CR
    -- 611.2c is what makes the set arm more than mechanical: the affected set is
    -- fixed when the effect begins, so Resolve sweeps ONCE and freezes the result
    -- as Affected.TheseObjects -- never the Filter, which re-evaluated per
    -- projection would pump a creature that became attacking later. The one-shot
    -- ObjectRef opcodes (Destroy, Untap) are under CR 608.2c/608.2f and store
    -- nothing.
    ModifyTarget Duration.Duration Modification.Modification ObjectRef.ObjectRef
  | -- | CR 612: rewrite subtype words in the target spell or permanent. The
    -- SubtypeFamily is which words the card's own text names -- Magical Hack's
    -- "one basic land type", Artificial Evolution's "one creature type" -- and so
    -- which words the player is asked for; the Set is what the NEW word may not
    -- be, Artificial Evolution's "can't be Wall" (empty for a card that restricts
    -- nothing). Resolve announces the pair as the effect applies (CR 608.2d) and
    -- bakes it into a stored ChangeSubtypeWord effect; Projection applies it.
    --
    -- The family is not stored alongside the pair: CR 612.2's gate reads the
    -- family of the word being REPLACED, which Pawl.Engine.Subtype answers from
    -- the word itself.
    ChangeText SubtypeFamily.SubtypeFamily (Set.Set Subtype.Subtype) SlotName.SlotName
  | -- | CR 605: add one unit of mana, of the type the ManaProduction names -- one
    -- fixed type, or one colour its controller chooses (CR 105.4). ONE unit, so a
    -- mode adding more holds the opcode more than once: Sol Ring's "{T}: Add
    -- {C}{C}" is two of these, and Mana.manaRoutesOfGiven reads a mode's whole
    -- list as one activation's yield. Executed by Mana.tapForMana at payment (CR
    -- 605.3b: a mana ability never uses the stack), never by Resolve.applyEffect.
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
    -- and bulk; a general exile-from-zone is future.
    ExileAllGraveyards
  | -- | CR 727.1/727.1a: restart the game. Targetless and game-wide; the starting
    -- player of the new game is the resolving controller, so no slot is needed.
    RestartGame
  | -- | CR 723.1: you control target player during that player's next turn.
    -- Installs pending control keyed to the slot's chosen player, with the
    -- ability's controller as the decider. Mindslaver's exact shape.
    ControlPlayerNextTurn SlotName.SlotName
  | -- | CR 701.8 / 702.12b: destroy the permanents the ObjectRef names -- move each
    -- to its owner's graveyard via the changeZone funnel UNLESS it is
    -- indestructible. NOT MoveToZone slot Graveyard: the indestructible check is
    -- why this is its own opcode (Murder vs Darksteel Myr), and the destruction is
    -- itself interceptable -- Pawl.Engine.Event.destroy offers a WouldBeDestroyed
    -- event to the CR 616.1 loop before the move, which is how a regeneration
    -- shield (CR 701.19a) intercepts it. The Regenerability is CR 701.19c's
    -- can't-be-regenerated rider, carried by the destruction rather than looked up
    -- on the victim -- Terror has it and CR 704.5g's state-based action does not,
    -- for the same creature.
    --
    -- ObjectRef is what lets ONE opcode be both Murder's "destroy target creature"
    -- and Day of Judgment's "destroy all creatures"; a sibling DestroyAll would
    -- have needed its own copy of the CR 702.12b gate, the CR 616.1 funnel and the
    -- CR 701.19c rider. Tap, Untap, ModifyTarget, GainControl and DealDamage have
    -- since taken the parameter for that reason, the two storing opcodes
    -- additionally owing CR 611.2c a frozen set; the rest still take a bare
    -- SlotName, none of them having a card that names a set (#378).
    --
    -- The Maybe SlotName BINDS how many permanents this destruction ACTUALLY
    -- destroyed into the effect source's live bindings, for a later effect of the
    -- same resolution to read back as Quantity.InSlot -- Bane of Progress' two
    -- sentences are two ordinary opcodes joined by the slot. A DEFINITION, not a
    -- read: never a target, never in targetSpecs. ACTUALLY destroyed is not
    -- "matched by the ObjectRef", since CR 702.12b's indestructible permanent and
    -- CR 701.19a's regenerated one are swept at and survive and CR 701.8b denies
    -- the word to any other graveyard move, so the number comes out of
    -- Event.destroyReturning rather than off the swept list. A COUNT, not the set:
    -- a rider acting on each destroyed permanent is not implemented (#463).
    Destroy ObjectRef.ObjectRef Regenerability.Regenerability (Maybe SlotName.SlotName)
  | -- | CR 701.21/701.21a: the slot's target permanent is sacrificed -- its
    -- CONTROLLER moves it to its OWNER's graveyard. NOT a destruction, which CR
    -- 701.21a says outright, so this consults neither indestructible (CR 702.12b)
    -- nor a regeneration shield (CR 701.19a) and is not a reuse of Destroy.
    --
    -- One opcode, not a targetless SacrificeSelf plus a slotted variant: "this
    -- creature" is expressible because Engine.placeOne binds the trigger's SOURCE
    -- into the reserved Pawl.Engine.Binding.triggerSource slot.
    Sacrifice SlotName.SlotName
  | -- | CR 701.3 / 702.6a: attach THIS permanent (the effect's source) to the
    -- slot's target -- so the equipment is the source and the only slot is what it
    -- attaches TO. CR 701.3a moves an already-attached source and CR 701.3c
    -- restamps it; CR 701.3b leaves it put if it cannot legally be attached to the
    -- target, or if the target is what it already holds.
    Attach SlotName.SlotName
  | -- | CR 701.3 / 303.4j: attach the SLOT'S TARGET -- an Aura, Equipment or
    -- Fortification already on the battlefield -- to an object chosen as this
    -- resolves. Crown of the Ages' "Attach target Aura attached to a creature to
    -- another creature".
    --
    -- The mirror of Attach above, and a separate opcode because the two differ in
    -- WHAT MOVES: equip attaches its own source and targets the destination, and
    -- this targets the thing that moves and does not target the destination at all
    -- (Crown of the Ages' Gatherer ruling is explicit). So the destination is a
    -- CHOICE on resolution, outside CR 608.2b's illegal-target check, and hence a
    -- bare Filter rather than a TargetSpec.
    --
    -- The Filter is the destination's card text; Aura Graft's "another permanent
    -- IT CAN ENCHANT" is `Filter.CanHostSubject`, the one atom that asks about the
    -- SUBJECT. The "another" is NOT in it: CR 701.3b makes attaching a permanent
    -- to what it already holds do nothing whatever the card says, so the opcode
    -- always excludes the current host. CR 303.4j / 701.3b is also the failure
    -- mode, and it is not a fizzle -- an illegal destination leaves the subject
    -- exactly where it was, unrestamped, while the rest of the ability resolves.
    -- Only a card whose text does NOT already exclude such a destination can reach
    -- it (Crown of the Ages can, Aura Graft cannot), which is why the rule and the
    -- atom are not the same thing.
    AttachTarget SlotName.SlotName (Filter.Filter Keyword.Keyword)
  | -- | CR 400.7: move the slot's target object to a zone through the changeZone
    -- funnel. Bounce is Hand (owner-relative -- changeZone carries Object.owner),
    -- targeted exile is Exile; the destination is data, so this is one opcode for
    -- every targeted single-object move. Distinct from Destroy, which checks
    -- indestructible.
    --
    -- The EntryRiders are what the effect says about the object AS IT ENTERS the
    -- battlefield, beyond its own text -- Meandering Towershell's "tapped and
    -- attacking" -- shared with Create for the reason that type's comment gives
    -- (CR 109.3: neither is a characteristic). They mean nothing for any other
    -- destination. Resolve reads them; it never cases on them.
    --
    -- The Maybe SlotName BINDS the incarnation CR 400.7 mints at the DESTINATION,
    -- as Create's minted-token slot does and for the same rule: a delayed ability
    -- this resolution arms (CR 603.7c's "it") must name the object, and after a
    -- zone change the old id is gone. Meandering Towershell is the producer, its
    -- two "it"s two incarnations of one card. A DEFINITION, not a read: never a
    -- target, never in targetSpecs.
    MoveToZone SlotName.SlotName Zone.Zone EntryRiders.EntryRiders (Maybe SlotName.SlotName)
  | -- | CR 121.1: the players the PlayerRef names each draw this many cards, one at
    -- a time (CR 121.2). Divination is `Relative You`; Ancestral Recall is
    -- `InSlot`, reading a slot TARGETING filled (CR 601.2c). Empty-library draw is
    -- a loss (CR 104.3c), unlike Mill -- the asymmetry that keeps the two separate.
    --
    -- PlayerRef rather than a `Draw SlotName Quantity` sibling: the sibling leaves
    -- two draw opcodes to keep in step, which the effect DSL otherwise avoids.
    Draw PlayerRef.PlayerRef Quantity.Quantity
  | -- | CR 701.17: the slot's target player mills this many. A short or empty
    -- library mills fewer, no penalty (CR 701.17b) -- unlike Draw, which loses.
    Mill SlotName.SlotName Quantity.Quantity
  | -- | CR 701.9: the slot's target player discards this many. The DISCARDING
    -- player chooses which (CR 701.9b) via Prompt.ChooseDiscard, routed through
    -- Decide.deciderFor. A hand smaller than the count discards all of it (CR
    -- 609.3), forced -- so it is not prompted.
    Discard SlotName.SlotName Quantity.Quantity
  | -- | CR 119.3: the players the PlayerRef names each lose this much life. Sign in
    -- Blood is `InSlot`, reading a slot TARGETING filled (CR 601.2c); a "you lose
    -- N life" drawback is `Relative You`. PlayerRef rather than Mill's and
    -- Discard's SlotName, for the reason Draw's comment gives. CR 704.5a's
    -- state-based action may follow, from Pawl.Engine.Sba.
    --
    -- NOT a DealDamage aimed at a player. CR 119.2 runs one way only, so the damage
    -- funnel would wrongly subject life loss to CR 614/615's replacement and
    -- prevention, to infect's CR 120.3b diversion (which turns the whole amount
    -- into poison counters, losing NO life) and to toxic's CR 120.3g rider -- and
    -- would append a GameEvent.DamageDealt for CR 704.5h's deathtouch scan and
    -- every damage-history reader.
    --
    -- GainLife is a SEPARATE opcode rather than a signed amount: CR 119.3 states
    -- both in one sentence, but they are distinct events for triggers ("whenever
    -- you gain life"), which a sign would fuse.
    LoseLife PlayerRef.PlayerRef Quantity.Quantity
  | -- | CR 119.3: the players the PlayerRef names each gain this much life -- Soul
    -- Warden's "you gain 1 life" is `Relative You`. LoseLife's mirror but for the
    -- sign, and separate from it for the reason that comment gives. No state-based
    -- action follows a gain (CR 704.5a is about a total of 0 or less), so this one
    -- can never kill anybody.
    GainLife PlayerRef.PlayerRef Quantity.Quantity
  | -- | CR 111: create this many tokens with the given effect-defined
    -- characteristics (CR 111.3). The `card` is the token's text, embedded
    -- literally (tied to Card by Card's own instantiation); Create (Literal 2)
    -- mints two distinct objects. Targetless and unprompted -- creating a token is
    -- never a choice. NOT a copy-token (CR 707) and NOT a predefined token (CR
    -- 111.10): given, not derived.
    --
    -- The EntryRiders is what the effect says about the tokens beyond their text
    -- -- Hanweir Garrison's "tapped and attacking" -- and is not part of the
    -- embedded card for the reason that type's comment gives (CR 109.3: neither is
    -- a characteristic). Resolve reads it; it never cases on it.
    --
    -- The Maybe SlotName BINDS the minted token into the resolving object's LIVE
    -- bindings, so a delayed ability armed by this same resolution (CR 603.7c's
    -- "it") can name the token. It does NOT make the token visible to a LATER
    -- EFFECT in the same resolution: applyEffect's `chosen` map is computed once
    -- before the fold begins, so a later Sacrifice or Destroy still reads the
    -- pre-Create snapshot. A DEFINITION, not a read: never a target, never in
    -- targetSpecs. Defined only for a single-token create; one that binds a slot
    -- while making several is rejected by the Pawl.CardSpec lint family (#53).
    Create Quantity.Quantity card EntryRiders.EntryRiders (Maybe SlotName.SlotName)
  | -- | CR 614.3 / 615.3: install a floating replacement effect for a duration,
    -- with a use count, an origin and an optional condition. Fog and Drudge
    -- Skeletons' regeneration are both this opcode, differing only in the
    -- payload's event class -- which is why they are not two. Targetless (a
    -- floating replacement watches a CLASS of events) and unprompted. Resolve
    -- stores it into GameState.replacements with this effect's SOURCE (CR 113.7)
    -- and a fresh timestamp; Pawl.Engine.Replacement applies it.
    --
    -- The ReplacementOrigin is CR 614.15's self-replacement bit, and it belongs
    -- here because such an effect is created by a resolving spell or ability --
    -- precisely what installs one of these -- and nothing in the payload could say
    -- it. Galvanic Blast's metalcraft clause is the one in the pool, and its "if
    -- you control three or more artifacts" is the Condition; Nothing is the
    -- unconditional case. Resolve checks it on resolution and installs nothing when
    -- it fails, which is exact for the pool rather than a shortcut: a CR 614.15
    -- self-replacement is installed and applied inside one resolution, with no
    -- window for the board to change. A conditional replacement that OUTLIVES its
    -- resolution has no producer (#587).
    --
    -- A field rather than a general conditional Effect arm, and not cosmetically:
    -- this gates ONE opcode's creation of ONE object, so the effect list stays a
    -- straight-line sequence a static analysis can read end to end (CR 611.2b
    -- already gates Duration.ForAsLongAs the same way). An `If condition [Effect]
    -- [Effect]` arm would put a BRANCH between two effect lists, which is the
    -- control flow design.md section 1 keeps out of the ISA.
    Replace Duration.Duration Uses.Uses ReplacementOrigin.ReplacementOrigin (Maybe Condition.Condition) ReplacementEffect.ReplacementEffect
  | -- | CR 614.10a: each player the PlayerRef names skips their NEXT occurrence of
    -- this step or phase. Fatigue names a step; Stonehorn Dignitary names a whole
    -- phase (CR 500.1).
    --
    -- NOT a Replace carrying a PhaseR, though CR 614.1b makes this a replacement
    -- effect: the pattern would have to name a player known only at resolution,
    -- and a ReplacementEffect written on a card cannot. Exactly why GainControl is
    -- its own opcode rather than a ModifyTarget carrying SetController -- the
    -- alternative, a slot name inside the pattern, would make Pawl.Engine.Resolve
    -- case on a ReplacementEffect's identity.
    --
    -- No Duration and no Uses, unlike Replace: CR 614.10a's "next" IS the use
    -- count, and Fatigue states no duration, so CR 614.3's used-up clause is the
    -- whole lifetime. Resolve installs one floating replacement PER NAMED PLAYER
    -- with Uses.Once and Expiry.Never.
    --
    -- Targetless in itself, like GainPlayerCounters: the slot a PlayerRef reads
    -- may have been filled by targeting (CR 601.2c), which is how Fatigue writes
    -- "target player", but nothing here demands it.
    SkipNextPhase PlayerRef.PlayerRef PhaseSelector.PhaseSelector
  | -- | CR 615.7: install a prevention SHIELD over the recipients an ObjectRef
    -- names, for a duration -- Mending Hands' "Prevent the next 4 damage that
    -- would be dealt to any target this turn". The Quantity is the shield's
    -- printed size, which then counts DAMAGE down (see
    -- Pawl.Types.DamageRewrite.PreventNext).
    --
    -- An ObjectRef, the same one DealDamage takes, because CR 115.4's "any target"
    -- reaches a PLAYER and only Resolve.objectRefRecipients answers in that
    -- vocabulary. One shield per recipient (CR 615.11's shape for free, though no
    -- card in the pool names more than one).
    --
    -- NOT a Replace carrying a DamageR, for exactly SkipNextPhase's reason: the
    -- pattern must name the shielded permanent or player, known only at
    -- resolution, so Resolve bakes the Recipient into
    -- DamagePattern.whichRecipient.
    --
    -- A Duration, unlike SkipNextPhase and like Replace: CR 615.3 gives a
    -- prevention effect two terminators, the count being the first and Mending
    -- Hands' "this turn" the second, printed rather than assumed. No Uses field:
    -- CR 615.7's shield is spent in damage, not in applications, so Resolve
    -- installs it Unlimited.
    PreventNextDamage Duration.Duration ObjectRef.ObjectRef Quantity.Quantity
  | -- | CR 701.6/701.6a: counter the slot's target via the Event.counter funnel.
    -- ONE opcode for both of that rule's subjects -- Cancel's slot is a
    -- Pool.Spells one and Stifle's a Pool.Abilities one -- because which ending
    -- the countering has (the owner's graveyard for a spell, CR 608.2n's cease for
    -- an ability) is the funnel's own classification of what it is handed. CR
    -- 113.9 keeps the two apart where it belongs, in the target pool.
    --
    -- Distinct from MoveToZone slot Graveyard the way Destroy is: a keyword action
    -- on rule 701's list, carrying the CR 113.6g can't-be-countered gate, and
    -- recording for a SPELL a distinct "was countered" event the zone change alone
    -- could not be told apart from.
    Counter SlotName.SlotName
  | -- | CR 122.6: put this many counters of this kind on the slot's target
    -- permanent. A counter is persistent object state, NOT a zone change --
    -- Resolve.applyEffect edits Object.counters in place, never through
    -- Event.changeZone. The counter's P/T effect is the projection's (CR 122.1a /
    -- 613.4c), not this opcode's.
    PutCounters CounterKind.CounterKind Quantity.Quantity SlotName.SlotName
  | -- | CR 122 / 107.14: the players the PlayerRef names each get N counters of a
    -- player-counter kind. Subsumes any self-scoped player counter (energy,
    -- experience, rad) as `Relative You` -- Longtusk Cub's "you get {E}{E}".
    --
    -- The PlayerRef is what lets a player OTHER than the resolving controller
    -- receive them: CR 702.70a's poison counters go to
    -- `InSlot Binding.triggerPlayer`, the player the trigger's own event named.
    -- PlayerRef and not PlayerScope, since only PlayerRef can name a binding slot.
    --
    -- Still targetless in itself: a slot this reads may have been filled by
    -- TARGETING (CR 601.2c), which is how a future "target player gets two poison
    -- counters" is written, but nothing here demands it (#120).
    GainPlayerCounters PlayerRef.PlayerRef PlayerCounterKind.PlayerCounterKind Quantity.Quantity
  | -- | CR 701.26a: tap the permanents the ObjectRef names -- Untap's exact mirror,
    -- down to the ObjectRef. A permanent that is ALREADY tapped is left alone
    -- rather than being an error, which is that rule's own second sentence and
    -- falls out of the resolution being an assignment to TapState.Tapped.
    Tap ObjectRef.ObjectRef
  | -- | CR 701.26b: untap the permanents the ObjectRef names. Act of Treason's
    -- "untap that creature" is `InSlot`; Aggravated Assault's and Relentless
    -- Assault's sweeps are `EachMatching`. ObjectRef for Destroy's reason -- one
    -- opcode rather than a sibling UntapAll to keep in step with it.
    Untap ObjectRef.ObjectRef
  | -- | CR 506.4: an effect that specifically removes a permanent from combat --
    -- the rule's one clause a card ASKS for rather than a condition the engine has
    -- to notice, which is why it is an opcode and not a sampler like
    -- Combat.removeChanged. Labyrinth of Skophos is the card text it exists for.
    --
    -- Removal ONLY: CR 506.4's second sentence is the whole effect, nothing in
    -- rule 506 puts a creature back, so there is no inverse opcode and no
    -- duration. A bare SlotName rather than Destroy's ObjectRef, since no card
    -- here sweeps a set. CR 506.4a and CR 506.4b bound what removal is NOT and
    -- neither reaches this opcode: both are about effects that do something ELSE,
    -- where this one says "remove from combat" in as many words.
    RemoveFromCombat SlotName.SlotName
  | -- | CR 500.8: add phases to a turn, directly after the specified phase, in
    -- written order -- Aggravated Assault is `[ExtraCombat, ExtraMain]`, Full
    -- Throttle `[ExtraCombat, ExtraCombat]`. A payload rather than a sibling
    -- opcode per shape, because CR 500.8 does not fix which phases are added and
    -- the printed cards genuinely differ. The list may be empty in the type; no
    -- card writes one, and an empty splice is a no-op rather than a case to guard.
    --
    -- Targetless and unprompted -- CR 500.8 leaves nothing to choose. Executed via
    -- Turn.splicePhases, where both the CR 505.1a/506.1 detail of WHAT is inserted
    -- and the CR 511.3 question of WHERE live.
    AddPhases [ExtraPhase.ExtraPhase]
  | -- | CR 613.1b / 611.2c: install a layer-2 control effect on the objects the
    -- ObjectRef names, for a duration. The new controller is this effect's
    -- source's controller, baked into a stored SetController effect -- derived,
    -- never chosen -- and each object whose controller actually changed is
    -- re-Sicked (CR 302.6). NOT a reuse of ModifyTarget, whose Modification is
    -- static card data and cannot carry a resolution-time PlayerId. Permanent
    -- control (CR 613), distinct from Mindslaver's player-control (CR 723).
    --
    -- ObjectRef for Destroy's reason: Act of Treason is InSlot, Aura Thief's "all
    -- enchantments" EachMatching. Like ModifyTarget and unlike the one-shots, the
    -- swept set is FROZEN into the stored effect (CR 611.2c names controller
    -- changes in as many words), so an enchantment entering afterwards is safe.
    GainControl Duration.Duration ObjectRef.ObjectRef
  | -- | CR 603.7: create the delayed triggered ability this card declares under
    -- this name (Face.delayedAbilities). First-order: the payload is card data
    -- joined by a name, so this opcode carries no nested ability and adds no type
    -- parameter. The resolving object's binding environment is captured as the
    -- ability is armed, which is how "it" / "that card" (CR 603.7c) survives the
    -- end of this resolution.
    --
    -- The Duration is CR 603.7b's stated duration -- Full Throttle's "this turn".
    -- Nothing is that rule's default, once only at the next trigger event (Tidal
    -- Wave), spelled as an absence because the rule words it that way.
    --
    -- The Onset is the envelope's other end -- when the ability becomes armed.
    -- Immediately for everything but Meandering Towershell's "on your next turn";
    -- see Pawl.Types.Onset for why a total field rather than a second Maybe, and
    -- why the gate cannot live in the ability's own trigger condition.
    ArmDelayedTrigger AbilityName.AbilityName Onset.Onset (Maybe Duration.Duration)
  | -- | CR 611.1 / 613.11: install a stored PLAYER or RULES-modifying continuous
    -- effect on a class of players for a duration. Silence is
    -- `AffectPlayers UntilEndOfTurn Opponents CantCastSpells`.
    --
    -- Targetless, mirroring Replace: a rules-modifying effect watches a CLASS, so
    -- there is nothing to target and nothing to prompt. Resolve stores it into
    -- GameState.playerEffects with this effect's source, its controller (CR 109.5,
    -- baked in -- the source may be in a graveyard by the time anyone asks), a
    -- fresh timestamp and Expiry.arm's answer.
    AffectPlayers Duration.Duration PlayerScope.PlayerScope PlayerEffect.PlayerEffect
  | -- | CR 114.2: the resolving controller gets an emblem with the given abilities,
    -- put into the command zone. Targetless; the abilities ride a Card so the
    -- emblem reuses the whole ability pipeline, first-order and tied to Card by
    -- Card's own instantiation exactly as Create's is.
    CreateEmblem card
  | -- | CR 725: a player becomes the monarch. Targetless; the beneficiary is named
    -- by the MonarchTarget (the resolving controller, or the controller of the
    -- ability's bound source). Emits GameEvent.BecameMonarch.
    BecomeMonarch MonarchTarget.MonarchTarget
  | -- | CR 725 (Palace Jailer): exile the slot's target UNTIL an opponent of the
    -- effect's controller becomes the monarch. The DURATION is the novelty -- the
    -- exiled incarnation is registered in GameState.exiledUntilMonarch and
    -- returned by Pawl.Engine.Monarch's settle-loop sweep. NOT MoveToZone, which
    -- has no duration and schedules no return.
    ExileUntilMonarch SlotName.SlotName
  | -- | CR 729.1/729.1b: play a Magic subgame, then bind its outcome (the derived
    -- loser) into this slot for a later effect to read. DEFINED here, like
    -- Create's minted-token slot, not a cast-time target -- the loser is known
    -- only when the subgame ends, so the following effect reads it through the
    -- per-effect binding re-read in resolveSpellWith. Generic: the engine reaches
    -- subgames through this opcode, never Shahrazad's identity.
    PlaySubgame SlotName.SlotName
  | -- | CR 103.5b (Serum Powder): exile every card in the resolving controller's
    -- hand, then draw that many. Targetless and controller-scoped, unlike Draw.
    --
    -- ONE opcode rather than an exile composed with a Draw: "that many" is the
    -- hand size BEFORE the exile, so a following Draw would read an empty hand.
    -- Splitting it needs a Count reading a value produced earlier in the same
    -- resolution, which nothing else wants. The card granting the action is itself
    -- exiled with the rest: CR 103.5b's action is not a cost, and nothing sets it
    -- aside.
    ExileHandThenDraw
  | -- | CR 701.34a: choose any number of permanents and/or players that have a
    -- counter, then give each one additional counter of each kind it already has.
    --
    -- CHOOSE, not target (the rule's own word): no target spec, the set is picked
    -- on RESOLUTION via Prompt.ChooseProliferate, and nothing is subject to CR
    -- 608.2b's illegal-target check -- which is why this carries no SlotName.
    --
    -- Nullary: rule 701.34a fixes the count at one per kind, leaving no quantity,
    -- kind or scope to vary. Object counters ride Replacement.putCounters so CR
    -- 614's counter replacements (Hardened Scales, Doubling Season) get their
    -- opportunity; player counters are added directly, matching GainPlayerCounters
    -- and gapped for the same reason (#122).
    Proliferate
  | -- | CR 701.21a: the slot's target PLAYER sacrifices this many permanents
    -- matching the Filter, chosen by that player. Diabolic Edict's exact shape.
    --
    -- Distinct from Sacrifice, which names a PERMANENT and is "this creature".
    -- The difference is who decides -- there the effect picks the victim, here the
    -- sacrificing player does, which is why this one prompts. The sibling of Mill
    -- and Discard, which likewise name a player and a count. The Filter carries
    -- what the edict names (a creature, a permanent, a land) rather than baking
    -- creatures in.
    --
    -- CR 609.3: a player with fewer matching permanents sacrifices all of them and
    -- one with none sacrifices nothing -- forced, so neither case is prompted.
    PlayerSacrifices SlotName.SlotName (Filter.Filter Keyword.Keyword) Quantity.Quantity
  | -- | CR 500.7: the players the PlayerRef names each get one extra turn, added
    -- directly after the turn this resolves in. Time Warp is `InSlot`, reading a
    -- slot TARGETING filled (CR 601.2c); PlayerRef rather than a bare SlotName for
    -- the reason Draw's comment gives, so Savor the Moment's own-caster turn writes
    -- `Relative You` without a sibling opcode.
    --
    -- No count and no "which turn": every printed extra-turn card adds ONE turn
    -- directly after the current one, and CR 500.7's clause about multiple extra
    -- turns is about several such effects rather than one adding several. WHERE
    -- they go and in what order they are taken is Engine.handoffTurn's question,
    -- reading GameState.extraTurns as the stack CR 500.7 describes.
    --
    -- CR 500.11 / 614.1b: the PhaseSelectors are the steps and phases the created
    -- turn SKIPS -- Savor the Moment pairs `Relative You` with the singleton
    -- `Step (Beginning Untap)`; empty for Time Warp. They ride this opcode rather
    -- than a second one because "that turn" has to name the turn this same
    -- resolution just created, and CR 500.7's most-recently-created-first ordering
    -- means the obvious reference -- SkipNextPhase's CR 614.10a "next" -- names a
    -- DIFFERENT turn as soon as another extra-turn effect resolves afterwards.
    TakeExtraTurn PlayerRef.PlayerRef (Set.Set PhaseSelector.PhaseSelector)
  | -- | CR 701.24: the slot's target object is shuffled into its OWNER's library --
    -- Riftsweeper's "choose target face-up exiled card. Its owner shuffles it into
    -- their library." The move goes through the changeZone funnel (CR 400.7's new
    -- incarnation), landing in the OWNER's library by CR 400.3 -- the rule the
    -- card's own "its owner" restates -- and that library is then shuffled (CR
    -- 701.24a).
    --
    -- NOT MoveToZone slot Library: Counter's three reasons line up one for one.
    -- Shuffle is a KEYWORD ACTION in its own right (CR 701.24), beside counter
    -- (701.6) and destroy (701.8). It carries a gate the bare move does not -- CR
    -- 701.24c shuffles the library even if the named objects are not where they
    -- were expected or are moved elsewhere, so a CR 616.1 replacement cancelling
    -- the move must not cancel the shuffle, which a rider read off the move's own
    -- result would. And a shuffle is its own observable event, the way "was
    -- countered" is: CR 701.24e and CR 701.24f are both about abilities that
    -- trigger when a library is shuffled, which a library move alone does not fire.
    --
    -- No PlayerRef saying whose library, and none is expressible: the answer is
    -- the OWNER of the object the slot names, which PlayerRef's three arms cannot
    -- read off a bound OBJECT. Derived rather than named is what the card says,
    -- and it is what makes this one opcode rather than a move plus a shuffle:
    -- Resolve.resolveModes fixes a resolving ABILITY's bindings before its effect
    -- fold begins, so a later effect could not read an incarnation this one had
    -- just bound anyway.
    ShuffleIntoLibrary SlotName.SlotName
  deriving (Eq, Ord, Show)

module Pawl.Types.Effect where

import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.ChoosePlayer as ChoosePlayer
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.ExileHaunting as ExileHaunting
import qualified Pawl.Types.ExtraPhase as ExtraPhase
import qualified Pawl.Types.Fight as Fight
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SkipNextPhase as SkipNextPhase
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown

-- | The ISA (design.md section 1): first-order, non-recursive in CONTROL FLOW
-- -- no branches and no recursive calls -- and with no functions in any field.
-- The ONLY module that may case on a constructor is Pawl.Engine.Resolve; the
-- rules core asks classifications, never identities.
--
-- The one iteration in it is ForEach's, and it is not a loop in that sense: CR
-- 608.2f's per-object processing runs a body over a set SWEPT ONCE before the
-- first pass, so the count is data an analysis can read off the instruction.
--
-- The `card` parameter lets an opcode embed a card's characteristics WITHOUT a
-- module cycle: Card embeds [Effect Card], and ties the knot by instantiating
-- `Effect Card`. The resulting data nesting is structural, not a recursive CALL.
--
-- Two field conventions recur. An opcode takes an ObjectRef rather than a bare
-- SlotName once a card names a SET (Murder beside Day of Judgment); only
-- ObjectRef.InSlot is ever a target (CR 115.10a), and the ones that STORE a
-- continuous effect additionally owe CR 611.2c a frozen sweep, where the
-- one-shots under CR 608.2c/608.2f store nothing. An opcode takes a PlayerRef
-- rather than a SlotName once a rule reaches players the card did not target
-- (Draw's `Relative You` beside Ancestral Recall's `InSlot`).
data Effect card ability
  = -- | CR 120.1: deal damage to what the payload's clauses name, each clause
    -- carrying its own amount and all of them one CR 608.2f action.
    DealDamage DealDamage.DealDamage
  | -- | CR 701.14: the two slots' creatures fight, which CR 701.14b makes a
    -- condition on the pair rather than two independent damage instructions.
    Fight Fight.Fight
  | -- | CR 611: create a continuous effect on the objects the ObjectRef names,
    -- for a duration, over the set CR 611.2c freezes as the effect begins.
    ModifyTarget (ModifyTarget.ModifyTarget ability)
  | -- | CR 612: rewrite subtype words in the target spell or permanent, the
    -- pair announced as the effect applies (CR 608.2d).
    ChangeText ChangeText.ChangeText
  | -- | CR 605: one player adds one unit of mana, of the type the payload's
    -- ManaProduction names -- one fixed type, or one colour its controller
    -- chooses (CR 105.4).
    --
    -- Not implemented: a mana ability's payment route (Cost.tapForMana, CR
    -- 605.3b) reads the production, the retention and both of CR 106.6's
    -- clauses, and ignores the recipient (#1673); everything else resolves
    -- through Resolve.applyEffect, which reads the whole record.
    AddMana ManaAddition.ManaAddition
  | -- | CR 701.23: the players Search.searcher names each search the
    -- Search.zones of each player Search.owner names for cards matching
    -- Search.filter, put them where Search.destination says, then shuffle any
    -- library searched.
    Search Search.Search
  | -- | CR 701.13 / Rest in Peace: exile every card in every graveyard.
    -- Targetless and bulk; a general exile-from-zone is future.
    ExileAllGraveyards
  | -- | CR 727.1 / 727.1a: restart the game, with the resolving controller as
    -- the new game's starting player; the ObjectRef is CR 727.5's exemption.
    RestartGame (Maybe ObjectRef.ObjectRef)
  | -- | CR 723.1: you control target player during that player's next turn
    -- (Mindslaver).
    ControlPlayerNextTurn SlotName.SlotName
  | -- | CR 701.8 / 702.12b: destroy the permanents the ObjectRef names, through
    -- the CR 616.1 loop so a regeneration shield (CR 701.19a) can catch it; the
    -- Regenerability is CR 701.19c's rider.
    Destroy Destroy.Destroy
  | -- | CR 701.21 / 701.21a: a player is instructed to sacrifice the permanents
    -- the ObjectRef names, moving each to its owner's graveyard and consulting neither
    -- indestructible nor a regeneration shield. WHICH player is the payload's
    -- Sacrificer, because rule 701.21a lets only a permanent's controller
    -- sacrifice it and the two printed templates address different seats: an
    -- ordinary "sacrifice it" instructs this effect's controller, so a permanent
    -- that has changed hands since is not sacrificed at all, where CR 701.54c's
    -- "the blocking creature's controller sacrifices it" instructs whoever holds
    -- it now.
    Sacrifice SacrificeEffect.SacrificeEffect
  | -- | CR 701.3 / 702.6a: attach this permanent (the effect's source) to the
    -- slot's target.
    Attach SlotName.SlotName
  | -- | CR 701.3 / 303.4j: attach the slot's target -- an attachment already on
    -- the battlefield -- to an object the Filter admits, chosen as this resolves
    -- (Crown of the Ages).
    AttachTarget AttachTarget.AttachTarget
  | -- | CR 303.4d / 301.5c: attach the slot's target to each permanent the
    -- Filter matches, which Pawl.Engine.Attach.arbitrate reduces to the one the
    -- attachment's controller picks.
    AttachTargetToEach AttachTarget.AttachTarget
  | -- | CR 701.3a, the third arrangement: a bound object moves, to a targeted
    -- destination (Sigarda's Aid) -- the only attach opcode whose destination is
    -- chosen at CR 603.3d.
    AttachBound AttachBound.AttachBound
  | -- | CR 400.7: move the objects the ObjectRef names to a zone through the
    -- changeZone funnel; the destination, the entry riders, the binding for CR
    -- 400.7j's new incarnations, CR 113.6m's stated origin zone and CR 401.2's
    -- library placement are all payload fields.
    MoveToZone MoveToZone.MoveToZone
  | -- | CR 121.1: the players the PlayerRef names each draw this many cards, one
    -- at a time (CR 121.2), an empty library being a loss (CR 104.3c). The slot
    -- remembers which cards, for a later clause of the same resolution (#1899).
    Draw Draw.Draw
  | -- | CR 701.17: the players the PlayerRef names each mill this many, a short
    -- library milling fewer with no penalty (CR 701.17b). The MillTally
    -- remembers how many counted, read back as Quantity.InSlot.
    Mill Mill.Mill
  | -- | CR 701.20a: the cards the ObjectRef names are revealed -- shown to all
    -- players -- and nothing moves (CR 701.20b).
    --
    -- Not implemented: rule 701.20a keeps a revealed card revealed "for as long
    -- as necessary", which pawl has nowhere to store (#1408).
    Reveal Reveal.Reveal
  | -- | CR 400.11c: the resolving controller puts a card they own from outside
    -- the game into their hand, showing it first where the card prints CR
    -- 701.20a's reveal (Burning Wish). The Filter is evaluated against the
    -- printed face, no object existing out there to project.
    --
    -- Not implemented: a destination other than the hand, and a count other than
    -- one -- the two axes Pawl.Types.FromOutsideTheGame does not carry
    -- (gap #2449).
    --
    -- Not implemented: where the reveal is printed it happens as the card
    -- arrives in the hand rather than before the move, GameEvent.Revealed being
    -- keyed on an ObjectId and outside the game having none to key it on
    -- (#2450).
    FromOutsideTheGame FromOutsideTheGame.FromOutsideTheGame
  | -- | CR 608.2n: this spell goes to exile as this instruction runs, rather
    -- than to its owner's graveyard at the end of its resolution (Burning Wish).
    -- A spell only, an ability's resolving object being no card (CR 113.7a).
    ExileThisSpell
  | -- | CR 701.20e: the cards the ObjectRef names are looked at -- shown to one
    -- player rather than all -- and bound into the slot, so a later clause of
    -- the same resolution can act on what was seen (Into the Wilds).
    --
    -- Not implemented: no seat is shown anything, there being no per-player view
    -- of the state (#1412), so the binding is the whole of the look and WHO
    -- looks is not carried.
    LookAt LookAt.LookAt
  | -- | CR 701.22a: the players the PlayerRef names each scry this many, the
    -- ordered partition being theirs (Prompt.ChooseScry). One library rewrite
    -- and no zone change.
    Scry PlayerQuantity.PlayerQuantity
  | -- | CR 701.25a: the players the PlayerRef names each surveil this many, the
    -- unwanted cards crossing into their graveyard where Scry's go to the bottom
    -- of the library.
    --
    -- Not implemented: CR 701.25b's "additional cards" rider (#1343).
    Surveil PlayerQuantity.PlayerQuantity
  | -- | CR 701.29a: the players the PlayerRef names each fateseal this many --
    -- Scry's library rewrite over an opponent's library, that opponent chosen as
    -- the effect applies.
    Fateseal PlayerQuantity.PlayerQuantity
  | -- | CR 701.44a: the permanents the ObjectRef names each explore, ordered
    -- across seats by CR 701.44d.
    Explore ObjectRef.ObjectRef
  | -- | CR 701.9: the slot's target player discards this many, choosing which
    -- (CR 701.9b); a hand smaller than the count discards all of it (CR 609.3).
    Discard Discard.Discard
  | -- | CR 119.3: the players the PlayerRef names each lose this much life --
    -- not damage aimed at a player, CR 119.2 running one way only.
    LoseLife PlayerQuantity.PlayerQuantity
  | -- | CR 119.3: the players the PlayerRef names each gain this much life,
    -- LoseLife's mirror but for the sign.
    GainLife PlayerQuantity.PlayerQuantity
  | -- | CR 701.12c: the two players the ExchangeSides names exchange life
    -- totals, each reaching the other's previous total.
    ExchangeLifeTotals ExchangeSides.ExchangeSides
  | -- | CR 119.5: the players the PlayerRef names each gain or lose the
    -- necessary amount to end up with this life total (Magister Sphinx).
    SetLifeTotal PlayerQuantity.PlayerQuantity
  | -- | Reverse the Sands' "redistribute any number of players' life totals":
    -- CR 119.7 / 119.8's assignment, each new total a CR 119.5 gain or loss.
    -- Choose, not target, so the permutation is picked on resolution.
    RedistributeLifeTotals
  | -- | CR 702.179c: the players the PlayerRef names each have their speed
    -- increased by this much, which for a player with no speed sets it to that
    -- value.
    IncreaseSpeed PlayerQuantity.PlayerQuantity
  | -- | The other direction: the named players each have their speed reduced by
    -- this much, never below the card's printed floor (Spikeshell Harrier).
    --
    -- Whether an effect may push speed past 4 is IncreaseSpeed's question;
    -- Pawl.Engine.Speed.maxSpeed states pawl's answer.
    DecreaseSpeed SpeedDecrease.SpeedDecrease
  | -- | CR 111: create this many tokens with the given effect-defined
    -- characteristics (CR 111.3), the `card` being the token's text embedded
    -- literally. Create.slot binds what was minted; see Resolve.namesEveryToken
    -- for which tokens CR 603.7c's "it" names.
    Create (Create.Create card)
  | -- | Alchemy's conjure keyword action: create a card that was in nobody's
    -- deck and put it into a zone (Emporium Thopterist). Digital-only, so there
    -- is no rule to cite; unlike CR 111.1's token, the result is a card.
    Conjure (Conjure.Conjure card)
  | -- | CR 707.1 / 111.3: create this many tokens per named object that are
    -- copies of it (Cackling Counterpart), entering simultaneously.
    --
    -- Not implemented: the rest of the EntryRiders record beyond CR 122.6's
    -- counters, and a bound slot for CR 603.7c's "it" -- Kiki-Jiki's hasty token
    -- and the delayed sacrifice that names it (#2302).
    CreateCopy CreateCopy.CreateCopy
  | -- | CR 707.4 / 613.1a: make a permanent already on the battlefield a copy of
    -- another object (Unstable Shapeshifter), by writing CR 707.2's copiable
    -- values themselves.
    --
    -- Not implemented: the CR 707.9a exception every printed producer carries
    -- ("except it has this ability"), so pawl's Shapeshifter loses its own
    -- trigger as it copies and cannot copy again (#1292); and a stated duration
    -- (#1753).
    BecomeCopy BecomeCopy.BecomeCopy
  | -- | CR 707.10: put a copy of a spell or of an activated or triggered ability
    -- on the stack onto the stack (Twincast, Lithoform Engine), cloning the
    -- original stack object so CR 707.10's "all decisions made for it" carries
    -- over.
    CopyStackObject CopyStackObject.CopyStackObject
  | -- | CR 614.3 / 615.3: install a floating replacement effect for a duration,
    -- with a use count, an origin and an optional condition asked as the event
    -- would happen (CR 614.1). Targetless.
    Replace (Replace.Replace card (Effect card ability))
  | -- | CR 614.10a: each player the PlayerRef names skips their next occurrence
    -- of this step or phase (Fatigue, Stonehorn Dignitary).
    SkipNextPhase SkipNextPhase.SkipNextPhase
  | -- | CR 615.7: install a prevention shield of the printed size over the
    -- recipients an ObjectRef or a described recipient side names, for a
    -- duration (Mending Hands). PreventNextDamage.riders is CR 615.5's
    -- additional effect.
    PreventNextDamage (PreventNextDamage.PreventNextDamage (Effect card ability))
  | -- | CR 615.1 / 615.3: install an unbounded prevention shield over the same
    -- recipients (Selfless Squire) -- PreventNextDamage with the Quantity
    -- removed, so it ends only when its duration does.
    PreventAllDamage (PreventAllDamage.PreventAllDamage (Effect card ability))
  | -- | CR 614.9: install a floating redirection effect (Turn the Tables); the
    -- Maybe DamageKind is printed rather than assumed.
    RedirectDamage RedirectDamage.RedirectDamage
  | -- | CR 701.6 / 701.6a: counter the objects the ObjectRef names via the
    -- Event.counter funnel, which carries the can't-be-countered gates and
    -- decides each subject's ending. The optional slot is how many it countered.
    Counter Counter.Counter
  | -- | CR 122.6: put this many counters of this kind on the permanents the
    -- ObjectRef names, one call to Event.putCounters apiece since CR 614.16
    -- replaces one placement at a time.
    PutCounters PutCounters.PutCounters
  | -- | CR 122: remove this many counters of this kind from the slot's target
    -- permanent; asking for more than are present removes what is there.
    RemoveCounters RemoveCounters.RemoveCounters
  | -- | CR 122.5: move counters from the permanents an ObjectRef names onto the
    -- one a slot does, atomically -- the rule's four impossibilities are checked
    -- before either half runs.
    MoveCounters MoveCounters.MoveCounters
  | -- | CR 122.8: put the counters the slot's object had onto the permanents the
    -- ObjectRef names (Iron Apprentice). A put and not a move, CR 122.2 having
    -- already made the first object's counters cease.
    PutCountersFrom PutCountersFrom.PutCountersFrom
  | -- | CR 122 / 107.14: the players the PlayerRef names each get N counters of
    -- a player-counter kind (energy, experience, poison, rad).
    GainPlayerCounters PlayerCounters.PlayerCounters
  | -- | CR 122: the players the PlayerRef names each lose N counters of a
    -- player-counter kind (CR 728.1); removing more than they have removes what
    -- they have.
    RemovePlayerCounters PlayerCounters.PlayerCounters
  | -- | CR 107.14: "you may pay any amount of {E}" -- the resolving controller
    -- names an amount, removes that many energy counters, and the amount is
    -- bound to this SlotName for a later effect to read (Harnessed Lightning).
    -- Not a cost, nothing on the card being gated on the payment.
    --
    -- Pawl.VariableEffectSpec's "CR 118.12 paying nothing declines the offer" is
    -- what proves paying zero does not take a gate hung off this opcode.
    PayAnyEnergy SlotName.SlotName
  | -- | CR 701.26a: tap the permanents the ObjectRef names, leaving an already
    -- tapped one alone.
    Tap ObjectRef.ObjectRef
  | -- | CR 701.26b: untap the permanents the ObjectRef names.
    Untap ObjectRef.ObjectRef
  | -- | CR 701.35a: detain the permanents the ObjectRef names, that rule fixing
    -- the duration and all three limbs.
    Detain ObjectRef.ObjectRef
  | -- | CR 701.15a: goad the permanents the ObjectRef names, until the next turn
    -- of this resolution's controller; CR 701.15b's two requirements are one
    -- opcode.
    Goad ObjectRef.ObjectRef
  | -- | CR 502.3 / 611.2: the permanents the ObjectRef names don't untap during
    -- their controller's next untap step (Elvish Hunter). CR 701.43a's exert is
    -- a different clause, riding Object.exertedBy.
    DoesNotUntapNext ObjectRef.ObjectRef
  | -- | CR 701.27a: turn the permanents the ObjectRef names over, so each shows
    -- its other face. The transform wording only; CR 701.28's convert is Convert
    -- below.
    Transform ObjectRef.ObjectRef
  | -- | CR 701.28a: the Transformers cards' word for Transform above, kept a
    -- second opcode because CR 701.28c-f restate the gates in convert's own
    -- words, and sharing one implementation so the two cannot drift.
    Convert ObjectRef.ObjectRef
  | -- | CR 701.42a: meld the cards the payload's ObjectRef names -- put them
    -- onto the battlefield with their back faces up and combined, as one
    -- permanent (CR 712.14c).
    Meld (Meld.Meld card)
  | -- | CR 702.26b: the permanents the ObjectRef names phase out. Not a zone
    -- change (CR 702.26d), and CR 702.26a fixes when it ends.
    PhaseOut ObjectRef.ObjectRef
  | -- | CR 708.2: turn the named permanents face down with the copiable values
    -- the effect lists for them, CR 708.2a's 2/2 supplying them where it lists
    -- none.
    TurnFaceDown TurnFaceDown.TurnFaceDown
  | -- | CR 708: turn the slot's target permanent face up, which CR 708.8 makes
    -- argumentless -- not CR 116.2b's special action, so no cost is paid.
    TurnFaceUp SlotName.SlotName
  | -- | CR 506.4: an effect that specifically removes the named permanents from
    -- combat (Labyrinth of Skophos). Removal only, that rule's second sentence
    -- being the whole effect.
    RemoveFromCombat ObjectRef.ObjectRef
  | -- | CR 509.1h's escape clause: an effect says an attacking creature becomes
    -- blocked (Curtain of Light), blocked by nothing.
    --
    -- Not implemented: CR 509.1h's other direction, an effect saying a creature
    -- becomes unblocked (Scryfall `oracle:"becomes unblocked"`, 2026-08-14, no
    -- hit).
    BecomesBlocked SlotName.SlotName
  | -- | CR 500.8: add phases to a turn, directly after the specified phase, in
    -- written order (Aggravated Assault). Targetless.
    AddPhases [ExtraPhase.ExtraPhase]
  | -- | CR 724.1: end the turn (Time Stop). Nullary, that rule's six steps
    -- fixing the whole procedure, including CR 724.1f's suppression of priority.
    EndTurn
  | -- | CR 724.2: end the combat phase (Mandate of Peace, which rule 724.2 says
    -- is the only card that does). EndTurn's shape, differing in what CR 724.2d
    -- leaves at the head of the schedule.
    EndCombatPhase
  | -- | CR 613.1b / 611.2c: install a layer-2 control effect on the objects the
    -- ObjectRef names, for a duration; the new controller is derived from this
    -- effect's source, and each object whose controller changed is re-Sicked (CR
    -- 302.6).
    GainControl DurationRef.DurationRef
  | -- | CR 603.7: create the delayed triggered ability this card declares under
    -- this name (Face.delayedAbilities), capturing the resolving object's
    -- bindings so CR 603.7c's "it" survives this resolution.
    ArmDelayedTrigger ArmDelayedTrigger.ArmDelayedTrigger
  | -- | CR 611.1 / 613.11: install a stored player- or rules-modifying
    -- continuous effect on some players for a duration (Silence, Cease-Fire),
    -- with its controller baked in (CR 109.5).
    AffectPlayers AffectPlayers.AffectPlayers
  | -- | CR 509.1c / 613.11: install a stored blocking requirement for a
    -- duration, one instance per (blocker, attacker) pair -- provoke (CR
    -- 702.39a) is this opcode.
    RequireBlock RequireBlock.RequireBlock
  | -- | CR 701.19c / 611.1: install a stored regeneration prohibition over the
    -- permanents the ref names, for a duration (Hurr Jackal); the printed
    -- one-destruction form is Pawl.Types.Regenerability instead.
    CantBeRegenerated CantBeRegenerated.CantBeRegenerated
  | -- | CR 508.1d / 613.11: install a stored attacking requirement for a
    -- duration (Alluring Siren), one instance per (attacker, defender) pair --
    -- rule 508.1d's two axes being an object and a player.
    RequireAttack RequireAttack.RequireAttack
  | -- | CR 509.1b / 613.11: install a stored blocking restriction for a duration
    -- (Zirda, the Dawnwaker). A rules modification (CR 613.11) rather than a
    -- Modification, so Pawl.Engine.Projection never sees it.
    ForbidBlock ForbidBlock.ForbidBlock
  | -- | CR 508.1c / 613.11: install a stored attacking restriction for a
    -- duration (Netter en-Dal), ForbidBlock's twin one rule over.
    ForbidAttack ForbidAttack.ForbidAttack
  | -- | CR 114.2: the resolving controller gets an emblem with the given
    -- abilities, put into the command zone. Targetless; the abilities ride a
    -- Card so the emblem reuses the whole ability pipeline.
    CreateEmblem card
  | -- | CR 725: a player the MonarchTarget names becomes the monarch.
    BecomeMonarch MonarchTarget.MonarchTarget
  | -- | CR 726.1: a player the InitiativeTarget names takes the initiative.
    TakeTheInitiative InitiativeTarget.InitiativeTarget
  | -- | The permanent in the slot gains this designation -- CR 702.112a's
    -- renown, CR 701.37a's monstrous, CR 701.60a's suspect and CR 719.3a's
    -- solved. Writes Object.designations, which CR 613 could not carry, and is
    -- idempotent (CR 702.112c, CR 701.60d).
    Designate Designate.Designate
  | -- | CR 716.2a's first half: "[Cost]: This Class's level becomes N." Writes
    -- Object.classLevel, CR 716.2b making a level a designation; the "only if
    -- this Class is level N-1" half rides the ability's own condition.
    SetClassLevel SetClassLevel.SetClassLevel
  | -- | CR 701.60a's other ending: the named permanents are no longer suspected.
    -- Not a designation-parameterised inverse of Designate, that ending
    -- belonging to `Suspected` alone.
    Unsuspect ObjectRef.ObjectRef
  | -- | CR 709.5f and CR 709.5g: lock or unlock halves of the slot's permanent
    -- (Keys to the House). One opcode over a setting, the two rules being one
    -- sentence with two words swapped; which half is asked at resolution (CR
    -- 608.2d) and how many is the payload. General, never Room-shaped.
    SetHalfLocked SetHalfLocked.SetHalfLocked
  | -- | CR 702.100a and CR 702.100b together: put a +1/+1 counter on the slot's
    -- permanent, and if one or more actually land, that permanent evolves. One
    -- opcode because rule 702.100b makes the marker conditional on the placement.
    Evolve SlotName.SlotName
  | -- | CR 702.134a and CR 702.134c together: put a +1/+1 counter on the slot's
    -- creature, and record that the source mentored it. Evolve's shape one rule
    -- over.
    Mentor SlotName.SlotName
  | -- | CR 702.149a and CR 702.149c together: put a +1/+1 counter on the slot's
    -- creature, and record that it trained. Evolve's shape, over
    -- Binding.triggerSource rather than a chosen target.
    Train SlotName.SlotName
  | -- | CR 731.1: "it becomes day" / "it becomes night" -- the game gains that
    -- designation, which CR 702.145c and CR 702.145f make daybound and
    -- nightbound permanents transform for.
    ItBecomes Daytime.Daytime
  | -- | CR 725 (Palace Jailer): exile the slot's target until an opponent of the
    -- effect's controller becomes the monarch. Not MoveToZone, which schedules
    -- no return.
    ExileUntilMonarch SlotName.SlotName
  | -- | CR 702.55a: exile the object the ObjectRef names, haunting the creature
    -- the SlotName's target names; the link is filed in GameState.haunting,
    -- which CR 702.55b reads.
    ExileHaunting ExileHaunting.ExileHaunting
  | -- | CR 729.1 / 729.1b: play a Magic subgame, then bind its winner into this
    -- slot; nothing is bound for a draw. A definition, never a target.
    PlaySubgame SlotName.SlotName
  | -- | CR 608.2d: the resolving controller chooses one player from the payload's
    -- scope, and the player chosen is bound into its slot for a later effect of
    -- the same resolution to name -- Skullwinder's "choose an opponent", Stadium
    -- Vendors' "choose a player". Choose, not target; elided at one candidate.
    ChoosePlayer ChoosePlayer.ChoosePlayer
  | -- | ChoosePlayer's twin with the decision replaced by randomness (Ruhan of
    -- the Fomori), so Prompt's RandomOpponent carries no Decider. Elided at one
    -- candidate, CR 102.2 leaving a two-player game exactly one opponent.
    --
    -- Not implemented: a scope beside this slot, so "choose a player at random"
    -- (Scrambleverse, Wildfire Devils) has no spelling (#3230).
    ChooseOpponentAtRandom SlotName.SlotName
  | -- | CR 706.1: roll a die of the stated kind, and bind the result as an
    -- amount at the payload's slot for a later effect to read as
    -- Quantity.InSlot (Ancient Copper Dragon).
    --
    -- CR 706.3's results table needs no arm here: a striation is one Clause of
    -- the same mode gated on a Condition.Compares over this slot, which CR
    -- 706.3b's "all part of one ability" is exactly, and Djinni Windseer in
    -- Pawl.DiceSpec is what proves it. Not implemented: CR 706.3c's "Roll again"
    -- (#2124).
    RollDie RollDie.RollDie
  | -- | CR 705.1: flip a coin, and bind CR 705.2's outcome at the payload's slot
    -- for a later clause of the same resolution to gate on (Winter Sky). The
    -- call is a choice and the outcome is not, so the two prompts differ in
    -- whether they carry a Decider.
    FlipCoin FlipCoin.FlipCoin
  | -- | CR 103.5b (Serum Powder): exile every card in the resolving controller's
    -- hand, then draw that many -- "that many" being the hand size before the
    -- exile, which is why it is one opcode.
    ExileHandThenDraw
  | -- | CR 701.34a: choose any number of permanents and/or players that have a
    -- counter, then give each one additional counter of each kind it already
    -- has. Choose, not target, so the set is picked on resolution.
    Proliferate
  | -- | CR 201.4 via CR 608.2c: the resolving controller chooses a card name,
    -- written to Object.chosenNames on the resolving object (Ancient Vendetta).
    -- The Filter is CR 201.4a's restriction, passed to the prompt unchecked
    -- (#663).
    --
    -- Not implemented: a chooser other than CR 109.5's "you" -- Petra Sphinx's
    -- "target player chooses a card name" (#2233).
    ChooseCardName (Filter.Filter Keyword.Keyword)
  | -- | CR 701.39a: "bolster N" -- choose a creature the resolving controller
    -- controls with the least toughness, or tied for least, then put that many
    -- +1\/+1 counters on it. Choose, not target.
    Bolster Quantity.Quantity
  | -- | CR 701.47a: "amass [subtype] N", performed by Pawl.Engine.Amass.amass as
    -- one procedure that cannot stop early (CR 701.47b). Choose, not target.
    Amass Amass.Amass
  | -- | CR 701.68a: "blight N" -- the players the PlayerRef names each put N
    -- -1\/-1 counters on a creature they control, each asked separately. Choose,
    -- not target.
    Blight PlayerQuantity.PlayerQuantity
  | -- | CR 701.54a: the Ring tempts the resolving controller, performed by
    -- Pawl.Engine.Ring.tempt as one procedure that cannot stop early (CR
    -- 701.54d). Nullary, rule 701.54a fixing everything.
    TemptWithTheRing
  | -- | CR 701.49: the resolving controller ventures into the dungeon,
    -- performed by Pawl.Engine.Dungeon.venture. The payload is CR 701.49d's
    -- "[quality]", a CR 205.3p dungeon type rather than a Filter.
    Venture (Maybe Subtype.Subtype)
  | -- | CR 701.21a: the players the slot names each sacrifice this many
    -- permanents matching the Filter, each chosen by that player (Diabolic
    -- Edict) in APNAP order (CR 101.4b) before anything leaves the battlefield.
    PlayerSacrifices PlayerSacrifices.PlayerSacrifices
  | -- | CR 500.7: the players the PlayerRef names each get the payload's count
    -- of extra turns (Ral Zarek's "for each coin that comes up heads"), added
    -- directly after the turn this resolves in. CR 500.11 / 614.1b: the
    -- PhaseSelectors are the steps and phases each such turn skips (Savor the
    -- Moment).
    TakeExtraTurn TakeExtraTurn.TakeExtraTurn
  | -- | CR 701.24: the referenced objects are shuffled into their owners'
    -- libraries (Riftsweeper), and CR 701.24c shuffles even where they are gone;
    -- ShuffleIntoLibrary.library names the library where the card's words do not
    -- derive it.
    ShuffleIntoLibrary ShuffleIntoLibrary.ShuffleIntoLibrary
  | -- | CR 701.24a on its own: the libraries the PlayerRef names are randomized,
    -- and nothing moves. A second arm rather than an empty ref on the one above,
    -- which would send cards through the CR 400.7 funnel.
    Shuffle PlayerRef.PlayerRef
  | -- | CR 608.2g: offer a player the cast of the object the slot names (CR
    -- 310.12b). The slot is a read, not a definition; CR 601.3's permission
    -- comes from the offer itself, and an offer is never a cast.
    OfferCast OfferCast.OfferCast
  | -- | CR 601.3: grant the permission to play the objects the ObjectRef names,
    -- for a duration (Victor Mancha, Runaway) -- a standing permission where
    -- OfferCast is one cast now. The `spending` rider is CR 118.14's.
    --
    -- Not implemented: a beneficiary other than CR 109.5's "you". The owner-side
    -- grants (Release to the Wind, Soul Partition) each carry a second clause
    -- pawl cannot yet spell.
    GrantPlayFromExile GrantPlayFromExile.GrantPlayFromExile
  | -- | CR 702.170c: the objects the ObjectRef names each become plotted
    -- (Kellan Joins Up). Not named Plot, CR 702.170e reserving that verb for CR
    -- 116.2k's special action; CR 702.170d fixes the beneficiary and the timing.
    MakePlotted ObjectRef.ObjectRef
  | -- | CR 608.2f: take the swept set one member at a time and run the body for
    -- each, with that member bound under the payload's slot for that iteration.
    -- The one arm that runs a sequence per member; every other set-naming opcode
    -- applies itself across the set, so reach for those first. Ordered APNAP,
    -- then by the resolving controller's choice.
    ForEach (ForEach.ForEach (Effect card ability))
  deriving (Eq, Ord, Show)

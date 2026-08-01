module Pawl.Engine.Activate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. Reads projected creature-ness -- a land (Evolving Wilds) is never sick-
-- gated. Asks Pawl.Engine.Cost for the CLASSIFICATION rather than matching a component
-- constructor here.
--
-- Keyed to `pid`, the player trying to activate: CR 302.6 asks whether the
-- creature has been under THEIR control since THEIR most recent turn began, so a
-- settle recorded for anyone else does not answer it (#198). CR 702.10c's haste
-- exemption comes with it, from the shared predicate.
--
-- BOTH of CR 302.6's symbols reach here: requiresSicknessCheck answers for the
-- tap symbol (CR 107.5) and the untap symbol (CR 107.6) alike, so this function
-- never learns which one a cost carries.
sicknessOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
sicknessOk = sicknessOkGiven Map.empty

sicknessOkGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
sicknessOkGiven pcs pid srcId ability gs =
  let needsCheck = Cost.requiresSicknessCheck (ActivatedAbility.cost ability)
      isCreature = Set.member CardType.Creature (Projection.cardTypesGiven pcs srcId gs)
   in not (needsCheck && isCreature && not (Summoning.settledOrHastyGiven pcs pid srcId gs))

-- The abilities to consider activating, which depends on WHERE the object is --
-- the one place that zone question is asked, so no caller repeats it.
--
-- On the battlefield: the PROJECTION's, so Humility (layer 6) strips them.
--
-- In a hand: the ones rule 702 mints for the card's printed keywords, which is
-- cycling (CR 702.29a) and nothing else today. Read off the PRINTED card rather
-- than a projection for the reason Keyword.handAbilitiesOf's own haddock gives:
-- CR 613's layer system does not reach a hand, so there is nothing there to
-- project and no Humility to respect. CR 113.6b is the rule that lets an ability
-- name its own zone this way.
--
-- Anywhere else: nothing. Flashback is not a counterexample -- CR 702.34a makes
-- it a CASTING permission, so a card in a graveyard reaches Pawl.Engine.Cast and never
-- this function. Rule 702's other zone abilities (madness, retrace, escape) are
-- casting permissions too; the first ability ACTIVATED from somewhere other than
-- the battlefield or a hand adds an arm here.
--
-- CR 702.29b is why this gates ACTIVATION and not existence: "Although the
-- cycling ability can be activated only if the card is in a player's hand, it
-- continues to exist while the object is on the battlefield and in all other
-- zones. Therefore objects with cycling will be affected by effects that depend
-- on objects having one or more activated abilities." Nothing in the pool asks
-- that second question yet; when something does, it must ask the CARD rather
-- than this function, which deliberately answers a narrower one.
--
-- INLINE, and not for the usual reason. This body reaches Projection.project,
-- which sicknessOk above and activatable's membership check also compute FOR THE
-- SAME OBJECT. Inlined, GHC shares those; opaque, it cannot, and
-- Projection.gather -- the sweep its own haddock warns is superlinear -- runs
-- again per object per enumeration. Measured on "a three-seat lands-only mirror
-- needs TWO deck-outs to find a winner": 10.4s inlined, 29.3s not, which was the
-- whole of a 2x regression in the suite.
--
-- The ENUMERATION path no longer rests on that: Action.legalActions projects the
-- whole board once and threads it through abilitiesForGiven and
-- activatableGiven, so the sharing there is explicit, the way
-- Projection.controllerOfGiven and Sba's one-projection-per-pass make it (#200,
-- and #316's own prescription). The pragma stays for the LONE-QUERY path -- the
-- wrapper here, which precomputes nothing and where GHC's sharing is still all
-- there is.
{-# INLINE abilitiesFor #-}
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesFor = abilitiesForGiven Map.empty

-- The zone question -- asked in exactly ONE place, here, whichever board it is
-- answered against. Only the battlefield arm reads the board at all: CR 613's
-- layer system does not reach a hand, so a hand's abilities are minted from the
-- printed card and a hand object's absence from the board is not a miss (see
-- Projection.projectGiven).
--
-- This carries the pragma too, and is what makes the wrapper's one effective: the
-- wrapper is a partial application of this, so inlining it exposes nothing for
-- GHC to share unless this body comes with it.
{-# INLINE abilitiesForGiven #-}
abilitiesForGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesForGiven pcs oid gs = case fmap Object.zone (Game.lookupObject oid gs) of
  Just Zone.Battlefield -> Projection.abilitiesGiven pcs oid gs
  Just Zone.Hand -> case Game.cardOf oid gs of
    Nothing -> []
    Just card -> Keyword.handAbilitiesOf (Card.keywords card)
  _ -> []

-- CR 602.2: "Only an object's controller (or its owner, if it doesn't have a
-- controller) can activate its activated ability unless the object specifically
-- says otherwise." Both halves of that parenthetical are live here, which is why
-- this cannot simply be Projection.controllerOf.
--
-- On the battlefield the controller is the answer. A card in a hand reaches the
-- rule's OWNER clause: CR 108.4 says "a card doesn't have a controller unless
-- that card represents a permanent or spell", so it has none at all -- and CR
-- 400.3 puts every card in a hand in its owner's, so the owner is also the player
-- whose hand it is in.
--
-- Nothing for every other zone, matching abilitiesFor's silence there: a zone
-- that offers no ability needs no activator.
activatorOf :: ObjectId -> GameState -> Maybe PlayerId
activatorOf oid gs = activatorOfGiven (Projection.controlGrants gs) oid gs

-- activatorOf with the control-grant list PRECOMPUTED, so an enumeration over
-- every permanent walks the battlefield for control-granting statics once rather
-- than once per permanent (#200) -- the reason Projection.controls threads the
-- same list.
activatorOfGiven :: [Projection.ControlGrant] -> ObjectId -> GameState -> Maybe PlayerId
activatorOfGiven grants oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.zone obj of
    Zone.Battlefield -> Projection.controllerOfGiven grants Set.empty oid gs
    Zone.Hand -> Just (Object.owner obj)
    _ -> Nothing

-- CR 307.5: does this ability's timing rider permit activating it right now?
--
-- The window itself is Turn.sorcerySpeedWindow, shared with CR 307.1's casting
-- gate: two rules, the same three conjuncts, one copy. Its haddock carries the
-- rule text and the reason no prohibition may be consulted -- a player under
-- Silence may still equip.
--
-- Priority is not re-checked here: the only caller is Action.legalActions, which
-- the priority loop asks only of the player who has priority.
--
-- CASTING PROHIBITIONS ARE NOT CONSULTED, by any arm. CR 307.5's last two
-- sentences say so for the sorcery-speed rider -- "Effects that would preclude
-- that player from casting a sorcery spell don't affect the player's capability
-- to perform that action" -- and no rule extends Pawl.Engine.Cast's CR 601.3 list to an
-- activation either, since CR 601.3 is about beginning to CAST a spell. That is
-- why this function reads GameState.phase directly rather than reaching for
-- Pawl.Types.CastingRestriction, whose DuringPhase arm is spelled the same way
-- and answers a different question (see Pawl.Types.ActivationTiming).
--
-- This gate makes the ability un-OFFERED. Engine.priorityLoop is what makes that
-- binding: it rejects an action the interpreter was not offered, so a
-- sorcery-speed activation named at instant speed does not happen either (#219).
timingOk :: PlayerId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
timingOk pid ability gs = case ActivatedAbility.timing ability of
  ActivationTiming.AnyTime -> True
  ActivationTiming.SorcerySpeed -> Turn.sorcerySpeedWindow pid gs
  -- CR 500.1's phases and steps, compared for equality: GameState.phase is the
  -- one the game is in, and Pawl.Types.Phase spans both kinds. The same one-line
  -- comparison Pawl.Engine.Cast makes for CastingRestriction.DuringPhase, deliberately
  -- duplicated rather than shared -- the two gates differ in what else they may
  -- read, which is the whole of the paragraph above.
  ActivationTiming.DuringPhase phase -> GameState.phase gs == phase

-- CR 606.3: "A player may activate a loyalty ability of a permanent they control
-- any time they have priority and the stack is empty during a main phase of their
-- turn, but only if no player has previously activated a loyalty ability of that
-- permanent that turn." CR 306.5d says the same thing for planeswalkers.
--
-- Vacuously true for every ability that is not a loyalty ability, which is what
-- makes this a conjunct rather than an arm of timingOk: CR 606.3 is a rule about
-- what a COST contains (CR 606.2), not a timing rider a card prints, so it is
-- derived from the cost through Pawl.Engine.Cost.isLoyaltyCost and never read off
-- Pawl.Types.ActivationTiming. That type documents that it carries ONE rider and
-- never several (#456); CR 606.3 is two clauses, and this is why it needs neither.
--
-- The window is Turn.sorcerySpeedWindow verbatim, not a near-copy: CR 606.3's
-- first clause and CR 307.5's restricted case are the same three facts -- priority
-- (which only the priority holder is asked, so it is not re-checked), a main phase
-- of that player's turn, and an empty stack.
--
-- The once-per-turn clause is a fold over the CR 608.2i log rather than a stamp,
-- and it is keyed on the PERMANENT and not on the player: the rule says "no
-- player has previously activated", so an opponent who somehow activated it first
-- has used the permanent's one activation.
loyaltyOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
loyaltyOk pid srcId ability gs =
  not (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
    || (Turn.sorcerySpeedWindow pid gs && not (loyaltyActivatedThisTurn srcId gs))

loyaltyActivatedThisTurn :: ObjectId -> GameState -> Bool
loyaltyActivatedThisTurn srcId gs = elem (GameEvent.LoyaltyAbilityActivated srcId) (GameState.events gs)

-- CR 602.2/602.5: the ability is a member of the source's abilities
-- (abilitiesFor), it is not a mana ability (mana abilities are handled at
-- payment, not the stack), the whole activation cost is payable (CR 118.3), the
-- {T} sickness gate holds, the ability's timing rider permits it now (CR 307.5),
-- and enough modes are fillable to satisfy the selection (CR 700.2a/602.2b).
--
-- The cost is the PRINTED one: an activated ability's cost is deliberately not
-- routed through Cost.total (#90).
--
-- activatableGiven is the half Action.legalActions wants: `grants` is one
-- control-grant walk and `pcs` one whole-board projection, taken once for the
-- enumeration instead of once per permanent per ability (#200, #316). See
-- Projection.projectGiven for what the board is and why passing Map.empty here
-- is the same answer at a lone query's cost.
--
-- Two of the conjuncts are deliberately NOT given the board: Target.fillableModes
-- and Cost.canPay ask about OTHER objects (the target pool, the mana sources), so
-- each hoists its own board for its own sweep -- Target.legalRecipients and
-- Mana.manaSources both do. #316 draws that same line for Cost.canPay, and calls
-- it a different fold and a separate question.
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatable pid srcId ability gs = activatableGiven (Projection.controlGrants gs) Map.empty pid srcId ability gs

activatableGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatableGiven grants pcs pid srcId ability gs =
  activatorOfGiven grants srcId gs == Just pid
    && elem ability (abilitiesForGiven pcs srcId gs)
    && not (Mana.isManaAbility ability)
    && sicknessOkGiven pcs pid srcId ability gs
    && timingOk pid ability gs
    && loyaltyOk pid srcId ability gs
    && Natural.length (Target.fillableModes (Just pid) srcId Map.empty (ActivatedAbility.modal ability) gs)
      >= Modal.selectionCount (ActivatedAbility.modal ability)
    && Cost.canPay pid srcId (ActivatedAbility.cost ability) gs

-- CR 602.2a: "If an activated ability is being activated from a hidden zone, the
-- card that has that ability is revealed (see rule 701.20a)." Note what the rule
-- does NOT say: there is no qualifier about the cost. Every activation from a
-- hidden zone reveals, and the "cost that can't be paid while the object is on
-- the battlefield" clause people remember is CR 113.6j, which is about where an
-- ability FUNCTIONS, not about revealing.
--
-- The revealer is the activating player. CR 602.2a is worded passively and names
-- nobody, but the hidden zone being read is theirs -- CR 400.3 puts every card in
-- a hand in its owner's, and Activate.activatorOf gives a card in a hand to that
-- owner precisely because CR 108.4 leaves it with no controller.
--
-- Reaches exactly the hand today: abilitiesFor serves the battlefield (public)
-- and the hand, and answers [] for every other zone -- so the library, the other
-- hidden zone, offers nothing to activate. Mana abilities never reach this
-- function -- CR 605.3b keeps them off the stack ("an activated mana ability
-- doesn't go on the stack"), `activatable` excludes them, and Mana.tapForMana
-- handles them by tapping a permanent, which is on the battlefield, so a mana
-- ability could not be a hidden-zone activation in any case.
--
-- CR 701.20a's duration for this case -- "the card remains revealed from the
-- time the spell or ability is announced until the time it leaves the stack" --
-- is not modeled, and is vacuous for every card in the pool: cycling discards
-- the card as a cost (CR 702.29a), so it is in a public graveyard a moment
-- later and there is no window in which "still revealed" differs from "visible
-- anyway". A forecast ability (CR 702.57a) is the shape that keeps the card in
-- hand and makes the duration observable; none is in the pool (#185, #282).
revealIfHidden :: PlayerId -> ObjectId -> Game ()
revealIfHidden pid srcId = do
  gs <- State.get
  case fmap Object.zone (Game.lookupObject srcId gs) of
    Just zone | Game.isHiddenZone zone -> Event.reveal pid srcId
    _ -> pure ()

-- CR 602.2: announce the activation, revealing the card if it is coming from a
-- hidden zone (602.2a), put the ability on the stack (a fresh OfAbility object),
-- choose modes (602.2b) then stamp targets, pay the additional costs, keep
-- priority (117.3c). Reject-not-repair on an illegal mode or target answer;
-- enumeration guarantees costs are payable, so payment cannot fail after the
-- prompt.
--
-- `before` is the pre-announcement state and is the ONLY thing the rejection
-- paths restore to, which is what puts CR 602.2a's reveal inside the rollback:
-- an activation the engine refused revealed nothing, and must leave no reveal
-- in the log claiming otherwise. Everything else reads `gs`, the state as of the
-- announcement.
activateAbility :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> Game ()
activateAbility pid srcId ability = do
  before <- State.get
  -- CR 602.2a's own order: the reveal is part of announcing, and so precedes
  -- the ability becoming an object on the stack (the rest of that same rule).
  revealIfHidden pid srcId
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = ts
          }
      onStack =
        gs2
          { GameState.objects = Map.insert abilId obj (GameState.objects gs2),
            GameState.stack = abilId : GameState.stack gs2
          }
      decider = Decide.deciderFor pid gs
      -- CR 602.2b/700.2a, mirroring Cast.castSpell's mode block: as many legal
      -- modes as the selection demands (or fewer) is FORCED, unprompted -- every
      -- existing single-mode ability is exactly {ModeIndex 0}, so this stamp
      -- stays behavior-identical to the pre-modal ability. A real choice (more
      -- legal modes than the selection demands) issues ChooseModes (M4h Task 4).
      legal = Target.fillableModes (Just pid) srcId Map.empty (ActivatedAbility.modal ability) gs
      count = Modal.selectionCount (ActivatedAbility.modal ability)
  State.put onStack
  chosenModes <-
    if Natural.length legal <= count
      then pure legal
      else Trans.lift (Program.prompt (Prompt.ChooseModes decider pid abilId legal count))
  -- Reject-not-repair: an answer that is not a size-`count` subset of the legal
  -- modes makes the whole activation a no-op, guarding every step below.
  if not (Set.isSubsetOf chosenModes legal && Natural.length chosenModes == count)
    then State.put before
    else do
      -- CR 602.2b: "the remainder of the process for activating an ability is
      -- identical to the process for casting a spell listed in rules 601.2b-i",
      -- so CR 118.13a's announcement -- which names "the activation cost of an
      -- activated ability" in its own words -- happens here, at 601.2b's
      -- position, and not when the cost is paid. Moltensteel Dragon's "{R/P}: This
      -- creature gets +1/+0 until end of turn" is what exercises it, and the rule's
      -- other two clauses -- a cost paid during a resolution, or for a special
      -- action -- are the ones still unreached (#373).
      --
      -- `id` rather than Cost.totalMana, and that is #90 rather than an oversight:
      -- an activation cost is not routed through Cost.total anywhere, so
      -- `activatable` above checked the PRINTED cost and the printed cost is what
      -- will be paid. Measuring the announcement through anything else would
      -- offer routes against a total this engine never computes.
      announcedCost <- Cost.announce pid srcId id (ActivatedAbility.cost ability)
      let sets = Target.legalSets (Just pid) srcId (Modal.modesTargetSpecs chosenModes (ActivatedAbility.modal ability)) gs
      chosen <-
        if Map.null sets
          then pure Map.empty
          else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid abilId sets))
      let keysAgree = Map.keysSet chosen == Map.keysSet sets
          eachLegal = and (Map.intersectionWith Set.member chosen sets)
      if not (keysAgree && eachLegal)
        then State.put before -- reject: the whole activation is a no-op
        else do
          -- CR 113.7: bind the source permanent under the reserved self slot, so
          -- an activated ability that refers to "this creature" (e.g. Longtusk
          -- Cub's "put a +1/+1 counter on Longtusk Cub") resolves the reference
          -- as a slot read -- exactly as Engine.placeOne does for a TRIGGERED
          -- ability's source. srcId is the source permanent (Source.OfAbility),
          -- which is what "this permanent" names. Additive: no existing activated
          -- ability reads the self slot, so this cannot disturb them.
          State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setTriggerSource srcId (Binding.fromChoices chosen Map.empty Nothing chosenModes)}) abilId (GameState.objects g)})
          -- CR 601.2g/h via Pawl.Engine.Cost.pay: the mana window, then the components.
          -- activatable pre-checks payability (Cost.canPay, which is pure), so an
          -- ability offered here is one SOME sequence of choices pays for -- but
          -- Unpaid is reachable all the same, because the mana window then asks the
          -- player to make those choices and a mis-tapped colour is a choice the
          -- engine must honour (Mana.payCost's haddock, and ManaSpec's "a Birds
          -- tapped for green does not pay {B}"). Reject-not-repair restores the
          -- whole activation -- including the ability object this function put on
          -- the stack -- when it happens.
          payment <- Cost.pay pid srcId announcedCost
          case payment of
            -- CR 606.3: record that a loyalty ability of THIS PERMANENT was
            -- activated, which is the whole of the once-per-turn limit's storage
            -- (see loyaltyOk above). Every path that rejects the activation
            -- restores `before`, and the log lives in that state, so no rejected
            -- activation can leave a record behind wherever this sits.
            Payment.Paid ->
              Monad.when
                (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
                (State.modify' (Event.recordEvent (GameEvent.LoyaltyAbilityActivated srcId)))
            Payment.Unpaid -> State.put before

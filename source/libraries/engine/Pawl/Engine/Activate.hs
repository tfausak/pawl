module Pawl.Engine.Activate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
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
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
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
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. Asks Pawl.Engine.Cost for the CLASSIFICATION rather than matching a
-- component constructor here, and covers both of CR 302.6's symbols -- the tap
-- symbol (CR 107.5) and the untap symbol (CR 107.6) alike.
--
-- Keyed to `pid`, the player trying to activate: CR 302.6 asks about THEIR
-- control since THEIR most recent turn began, so a settle recorded for anyone
-- else does not answer it (#198). CR 702.10c's haste exemption comes with it,
-- from the shared predicate.
--
-- Reads PROJECTED creature-ness, so a plain land is never sick-gated and an
-- animated one is.
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
-- On the battlefield: the PROJECTION's, so Humility (layer 6) strips them. In a
-- hand: the ones rule 702 mints for the card's printed keywords, which is
-- cycling (CR 702.29a) and nothing else today, read off the PRINTED card
-- because pawl's projection does not reach a hand (#160); CR 113.6b is the rule
-- that lets an ability name its own zone. Anywhere else: nothing -- flashback
-- and rule 702's other zone abilities are CASTING permissions (CR 702.34a), so
-- they reach Pawl.Engine.Cast instead. The first ability ACTIVATED from a third
-- zone adds an arm here.
--
-- CR 702.29b is why this gates ACTIVATION and not existence: a cycling ability
-- keeps existing in every zone, so an effect counting activated abilities sees
-- it. Nothing in the pool asks that second question yet; whatever does must ask
-- the CARD rather than this function, which answers a narrower one.
--
-- This is the LONE-QUERY convenience wrapper: it precomputes nothing, so it
-- reaches Projection.project for itself, as do sicknessOk above and
-- activatable's membership check when called the same way. The ENUMERATION path
-- goes through abilitiesForGiven instead.
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesFor = abilitiesForGiven Map.empty

-- The ...Given half of the pair, and the one the enumeration calls:
-- Action.legalActions hands it the board it projected once, so nothing here
-- re-derives a projection per object. Only the battlefield arm reads that board
-- at all -- a hand object's absence from it is not a miss (#160; see
-- Projection.projectGiven).
abilitiesForGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesForGiven pcs oid gs = case fmap Object.zone (Game.lookupObject oid gs) of
  Just Zone.Battlefield -> Projection.abilitiesGiven pcs oid gs
  Just Zone.Hand -> case Game.faceOf oid gs of
    Nothing -> []
    Just face -> Keyword.handAbilitiesOf (Face.keywords face)
  _ -> []

-- CR 602.2: only an object's controller, or its owner if it has no controller,
-- may activate its activated ability. Both halves of that parenthetical are live
-- here, which is why this cannot simply be Projection.controllerOf: a card in a
-- hand has no controller at all (CR 108.4), and CR 400.3 puts every card in a
-- hand in its owner's.
--
-- Nothing for every other zone, matching abilitiesFor's silence there.
activatorOf :: ObjectId -> GameState -> Maybe PlayerId
activatorOf oid gs = activatorOfGiven (Projection.controlGrants gs) oid gs

-- activatorOf with the control-grant list PRECOMPUTED, so an enumeration over
-- every permanent walks the battlefield for control-granting statics once rather
-- than once per permanent (#200).
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
-- gate: two rules, the same three conjuncts, one copy. Priority is not
-- re-checked here: the only caller is Action.legalActions, which the priority
-- loop asks only of the player who has priority.
--
-- CASTING PROHIBITIONS ARE NOT CONSULTED, by any arm. CR 307.5 says so for the
-- sorcery-speed rider, and no rule extends Pawl.Engine.Cast's CR 601.3 list to
-- an activation either, since CR 601.3 is about beginning to CAST a spell. That
-- is why this reads GameState.phase and GameState.activePlayer directly rather
-- than reaching for Pawl.Types.CastingRestriction, whose DuringPhase arm is
-- spelled the same way and answers a different question.
--
-- This gate makes the ability un-OFFERED. Engine.priorityLoop is what makes that
-- binding: it rejects an action the interpreter was not offered (#219).
timingOk :: PlayerId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
timingOk pid ability gs = case ActivatedAbility.timing ability of
  ActivationTiming.AnyTime -> True
  ActivationTiming.SorcerySpeed -> Turn.sorcerySpeedWindow pid gs
  -- CR 500.1's phases and steps: Turn.inWindow asks whether GameState.phase
  -- falls inside the window the rider names. CONTAINMENT rather than equality,
  -- because a rider may name a phase that has steps -- Jade Statue's "only
  -- during combat" is live in all five of CR 506.1's combat steps, while
  -- Desert's names one of them. Pawl.Engine.Cast makes the equality comparison
  -- for CastingRestriction.DuringPhase, whose arm still carries a bare Phase
  -- (#527); deliberately duplicated rather than shared, since the two gates
  -- differ in what else they may read.
  --
  -- CR 102.1 supplies the second conjunct, a genuinely separate fact: Desert's
  -- rider names no turn (EachTurn, and the step alone decides), while Llanowar
  -- Augur's "only during your upkeep" names alice's upkeep and not bob's. CR
  -- 109.5 is why `pid` answers "your" -- for an activated ability that is the
  -- player who activated it, which `activatable` has already pinned to
  -- activatorOf -- so a stolen permanent's rider follows the thief.
  ActivationTiming.DuringPhase window scope ->
    Turn.inWindow window (GameState.phase gs)
      && case scope of
        TurnScope.EachTurn -> True
        TurnScope.ControllersTurn -> GameState.activePlayer gs == pid

-- CR 606.3 (CR 306.5d says the same for planeswalkers): a loyalty ability may be
-- activated only with priority and an empty stack during a main phase of its
-- controller's turn, and only if no player has already activated a loyalty
-- ability of that permanent this turn.
--
-- Vacuously true for every ability that is not a loyalty ability, which is what
-- makes this a conjunct rather than an arm of timingOk: CR 606.3 is a rule about
-- what a COST contains (CR 606.2), not a timing rider a card prints, so it is
-- derived through Pawl.Engine.Cost.isLoyaltyCost and never read off
-- Pawl.Types.ActivationTiming, which carries ONE rider and never several (#456).
--
-- The window is Turn.sorcerySpeedWindow verbatim, not a near-copy: CR 606.3's
-- first clause and CR 307.5's restricted case are the same three facts, and
-- priority is not re-checked because only the priority holder is asked.
--
-- The once-per-turn clause is a fold over the CR 608.2i log rather than a stamp,
-- and it is keyed on the PERMANENT and not on the player: an opponent who
-- somehow activated it first has used the permanent's one activation.
loyaltyOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
loyaltyOk pid srcId ability gs =
  not (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
    || (Turn.sorcerySpeedWindow pid gs && not (loyaltyActivatedThisTurn srcId gs))

loyaltyActivatedThisTurn :: ObjectId -> GameState -> Bool
loyaltyActivatedThisTurn srcId gs = elem (GameEvent.LoyaltyAbilityActivated srcId) (GameState.events gs)

-- CR 602.2b's routing of an activation cost through CR 601.2b, at the X=0 FLOOR:
-- an ability is affordable when its activation cost is payable with X=0, since
-- the activating player may always choose 0. `activatable` conjoins this
-- predicate and `affordableX` climbs it, so what activatability measures and
-- what the bound reports cannot drift apart.
--
-- The substitution is not decoration. A ManaSymbol.Variable that reaches payment
-- demands nothing at all (Mana.waysOf), so leaving it in place would answer the
-- same as X=0 by accident rather than by rule -- the accident that made the {X}
-- free (#544).
payableCost :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCost = payableCostAt 0

-- The same question asked at some OTHER value of X -- `payableCost` is this at
-- the floor and `affordableX` is this climbed.
--
-- NO CR 601.2f TOTALLING, which is the one place this parts company with the
-- spell's version: an activation cost is deliberately not routed through
-- Cost.total anywhere (#90), so the printed cost is what is measured and what
-- will be paid. When #90 lands, this is the site that changes.
payableCostAt :: Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAt x pid srcId gs cost = Cost.canPay pid srcId (Cost.substituteX x cost) gs

-- CR 601.2b via 602.2b: the greatest X this player could actually pay for, which
-- is what Prompt.ChooseX carries. The climb itself is Cost.greatestPayableX,
-- shared with Cast.affordableX; only the predicate differs, and only by CR
-- 601.2f's totalling (#90). Advisory, never a clamp -- see Prompt.ChooseX.
affordableX :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Natural
affordableX pid srcId gs cost = Cost.greatestPayableX (\x -> payableCostAt x pid srcId gs cost) cost

-- CR 602.2/602.5: the ability is a member of the source's abilities
-- (abilitiesFor), it is not a mana ability (those are handled at payment, not
-- the stack), the whole activation cost is payable at CR 601.2b's X=0 floor (CR
-- 118.3), the {T} sickness gate holds, the ability's timing rider permits it now
-- (CR 307.5), and enough modes are fillable to satisfy the selection (CR
-- 700.2a/602.2b). The cost is the PRINTED one (#90).
--
-- activatableGiven is the half Action.legalActions wants: `grants` is one
-- control-grant walk and `pcs` one whole-board projection, taken once for the
-- enumeration instead of once per permanent per ability (#200, #316). Two of the
-- conjuncts are deliberately NOT given that board: Target.fillableModes and
-- Cost.canPay ask about OTHER objects (the target pool, the mana sources), so
-- each hoists its own board for its own sweep (#316).
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
    && payableCost pid srcId gs (ActivatedAbility.cost ability)

-- CR 602.2a: an ability activated from a hidden zone reveals the card that has
-- it (CR 701.20a). Note what the rule does NOT say: there is no qualifier about
-- the cost. The "cost that can't be paid while the object is on the battlefield"
-- clause people remember is CR 113.6j, which is about where an ability FUNCTIONS.
--
-- The revealer is the activating player. CR 602.2a is worded passively and names
-- nobody, but the hidden zone being read is theirs -- CR 400.3 puts every card in
-- a hand in its owner's, and Activate.activatorOf gives a card in a hand to that
-- owner precisely because CR 108.4 leaves it with no controller.
--
-- Reaches exactly the hand today: abilitiesFor serves the battlefield and the
-- hand and answers [] elsewhere, so the library offers nothing to activate, and
-- mana abilities never reach this function (CR 605.3b keeps them off the stack).
--
-- CR 701.20a's duration -- revealed until the ability leaves the stack -- is not
-- modeled, and is vacuous for every card in the pool: cycling discards the card
-- as a cost (CR 702.29a), so it is in a public graveyard a moment later. A
-- forecast ability (CR 702.57a) is the shape that would make the duration
-- observable; none is in the pool (#185, #282).
revealIfHidden :: PlayerId -> ObjectId -> Game ()
revealIfHidden pid srcId = do
  gs <- State.get
  case fmap Object.zone (Game.lookupObject srcId gs) of
    Just zone | Game.isHiddenZone zone -> Event.reveal pid srcId
    _ -> pure ()

-- CR 602.2: announce the activation, revealing the card if it is coming from a
-- hidden zone (602.2a), put the ability on the stack (a fresh OfAbility object),
-- then walk CR 601.2b-i as CR 602.2b sends it -- choose modes, announce the value
-- of X, announce the Phyrexian symbols (CR 118.13a), stamp targets, pay -- and
-- keep priority (117.3c). Reject-not-repair on an illegal mode or target answer.
--
-- An announced X can lose the activation all by itself: enumeration measures the
-- cost at CR 601.2b's X=0 floor, and the value the player names is theirs to
-- name freely. See the gate below.
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
            Object.enteredUnder = Nothing,
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.playableFromExileBy = Nothing
          }
      onStack =
        gs2
          { GameState.objects = Map.insert abilId obj (GameState.objects gs2),
            GameState.stack = abilId : GameState.stack gs2
          }
      decider = Decide.deciderFor pid gs
      -- CR 602.2b/700.2a, mirroring Cast.castSpell's mode block: as many legal
      -- modes as the selection demands (or fewer) is FORCED, unprompted -- every
      -- single-mode ability is exactly {ModeIndex 0}. A real choice (more legal
      -- modes than the selection demands) issues ChooseModes.
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
      -- CR 602.2b routes the rest of the activation through CR 601.2b-i, so CR
      -- 601.2b's announcement of a variable cost (CR 107.3) governs an activation
      -- cost's {X} too, and it is asked HERE -- after the modes, before the Phyrexian
      -- announcement and before CR 601.2c's targets, which is 601.2b's own order.
      --
      -- Cinder Elemental exercises it. Not asking was not a missing question but
      -- a free {X}: a ManaSymbol.Variable that survives to payment demands
      -- nothing (Mana.waysOf), so the engine was announcing 0 on the player's
      -- behalf (#544).
      --
      -- The bound rides the PRINTED cost, which for an activation is the cost
      -- `activatable` gated on and the cost that will be paid (#90); nothing
      -- filters the answer against it (see Prompt.ChooseX).
      let printedCost = ActivatedAbility.cost ability
      mAmount <-
        if Cost.hasVariable printedCost
          then fmap Just (Trans.lift (Program.prompt (Prompt.ChooseX decider pid abilId (affordableX pid srcId gs printedCost))))
          else pure Nothing
      let announcedAtX = maybe printedCost (\x -> Cost.substituteX x printedCost) mAmount
      -- CR 602.2: an activation a player cannot comply with is illegal, and the
      -- game returns to the moment before it started. The X just named is where
      -- that can first become true: `activatable` measured the cost at CR
      -- 601.2b's X=0 floor, the only value it can know before an announcement
      -- exists.
      --
      -- Asked with the same predicate that floor was asked with, so a gate and an
      -- announcement cannot disagree about what a cost is. That matters beyond
      -- tidiness: CR 118.13a's Phyrexian announcement below runs on this cost, and
      -- an X large enough to leave neither of CR 107.4f's routes payable would
      -- leave Mana.announcePhyrexian with no offer to make. This gate is what
      -- keeps that arm out of reach from here (#417).
      --
      -- Reject-not-repair, the posture every other step here takes: the
      -- announcement is NOT clamped to affordableX -- CR 601.2b lets the player
      -- announce the value freely -- it is honoured and then loses the ability.
      -- Asked unconditionally rather than only when there is an {X}, which buys
      -- one predicate over one cost instead of two spellings of when the gate
      -- applies.
      if not (payableCost pid srcId gs announcedAtX)
        then State.put before -- reject: the whole activation is a no-op
        else do
          -- CR 118.13a's announcement, which names an activated ability's
          -- activation cost, happens here at 601.2b's position and not when the
          -- cost is paid. Moltensteel Dragon exercises it; the rule's other two
          -- clauses -- a cost paid during a resolution, or for a special action --
          -- are still unreached (#373).
          --
          -- `id` rather than Cost.totalMana, and that is #90 rather than an
          -- oversight: an activation cost is not routed through Cost.total
          -- anywhere, so measuring the announcement through anything else would
          -- offer routes against a total this engine never computes.
          --
          -- Run on the cost carrying the ANNOUNCED value, which is CR 601.2b's own
          -- order (the value of X precedes the Phyrexian announcement).
          announcedCost <- Cost.announce pid srcId id announcedAtX
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
              -- an activated ability that refers to "this creature" (Longtusk Cub)
              -- resolves the reference as a slot read -- exactly as
              -- Engine.placeBorne does for a TRIGGERED ability's source.
              --
              -- CR 109.5 binds the controller under the reserved you slot in the
              -- same breath: "The words 'you' and 'your' on an object refer to the
              -- object's controller ... For an activated ability, this is the
              -- player who activated the ability." That player is `pid`, the one
              -- CR 602.2 lets activate this ability at all -- so Brothers of Fire's
              -- "and 1 damage to you" reaches a player, exactly as
              -- Engine.placeBorne does for a TRIGGERED ability's controller.
              --
              -- CR 601.2b's announced X is stamped alongside, onto the ABILITY object
              -- and not the source permanent -- Cinder Elemental sacrifices that
              -- permanent to pay, so the ability is the only holder still there to
              -- read at resolution (Quantity.evaluateFor). CR 113.7a is why the
              -- ability keeps all three once its source is gone: "Once activated or
              -- triggered, an ability exists on the stack independently of its
              -- source."
              State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setYou pid (Binding.setTriggerSource srcId (Binding.fromChoices chosen mAmount chosenModes))}) abilId (GameState.objects g)})
              -- CR 601.2g/h via Pawl.Engine.Cost.pay: the mana window, then the
              -- components. The gates above prove SOME sequence of choices pays for
              -- this ability -- but Unpaid is reachable all the same, because the
              -- mana window then asks the player to make those choices and a
              -- mis-tapped colour is a choice the engine must honour (Mana.payCost).
              -- Reject-not-repair restores the whole activation, including the
              -- ability object this function put on the stack.
              payment <- Cost.pay pid srcId announcedCost
              case payment of
                -- CR 606.3: record that a loyalty ability of THIS PERMANENT was
                -- activated, which is the whole of the once-per-turn limit's storage
                -- (see loyaltyOk above). Every path that rejects the activation
                -- restores `before`, and the log lives in that state, so no rejected
                -- activation can leave a record behind.
                Payment.Paid ->
                  Monad.when
                    (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
                    (State.modify' (Event.recordEvent (GameEvent.LoyaltyAbilityActivated srcId)))
                Payment.Unpaid -> State.put before

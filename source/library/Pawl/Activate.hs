module Pawl.Activate where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Cost as Cost
import qualified Pawl.Decide as Decide
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Summoning as Summoning
import qualified Pawl.Target as Target
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivationTiming as ActivationTiming
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Payment as Payment
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. Reads projected creature-ness -- a land (Evolving Wilds) is never sick-
-- gated. Asks Pawl.Cost for the CLASSIFICATION rather than matching a component
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
sicknessOk pid srcId ability gs =
  let needsCheck = Cost.requiresSicknessCheck (ActivatedAbility.cost ability)
      isCreature = Set.member CardType.Creature (Projection.cardTypesOf srcId gs)
   in not (needsCheck && isCreature && not (Summoning.settledOrHasty pid srcId gs))

-- The abilities to consider activating. Task 5: the card's PRINTED abilities.
-- Task 9 switches the body to `Projection.abilitiesOf srcId gs` so Humility
-- (layer 6) strips them -- the single switch point, the keywordsOf pattern.
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesFor = Projection.abilitiesOf

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
-- This gate makes the ability un-OFFERED. Engine.priorityLoop is what makes that
-- binding: it rejects an action the interpreter was not offered, so a
-- sorcery-speed activation named at instant speed does not happen either (#219).
timingOk :: PlayerId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
timingOk pid ability gs = case ActivatedAbility.timing ability of
  ActivationTiming.AnyTime -> True
  ActivationTiming.SorcerySpeed -> Turn.sorcerySpeedWindow pid gs

-- CR 602.2/602.5: the ability is a member of the source's abilities
-- (abilitiesFor), it is not a mana ability (mana abilities are handled at
-- payment, not the stack), the whole activation cost is payable (CR 118.3), the
-- {T} sickness gate holds, the ability's timing rider permits it now (CR 307.5),
-- and enough modes are fillable to satisfy the selection (CR 700.2a/602.2b).
--
-- The cost is the PRINTED one: an activated ability's cost is deliberately not
-- routed through Cost.total (#90).
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatable pid srcId ability gs =
  Projection.controllerOf srcId gs == Just pid
    && elem ability (abilitiesFor srcId gs)
    && not (Mana.isManaAbility ability)
    && sicknessOk pid srcId ability gs
    && timingOk pid ability gs
    && Natural.length (Target.fillableModes (Just pid) srcId Map.empty (ActivatedAbility.modal ability) gs)
      >= Modal.selectionCount (ActivatedAbility.modal ability)
    && Cost.canPay pid srcId (ActivatedAbility.cost ability) gs

-- CR 602.2: put the ability on the stack (a fresh OfAbility object), choose
-- modes (602.2b) then stamp targets, pay the additional costs, keep priority
-- (117.3c). Reject-not-repair on an illegal mode or target answer; enumeration
-- guarantees costs are payable, so payment cannot fail after the prompt.
activateAbility :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> Game ()
activateAbility pid srcId ability = do
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
    then State.put gs
    else do
      let sets = Target.legalSets (Just pid) srcId (Modal.modesTargetSpecs chosenModes (ActivatedAbility.modal ability)) gs
      chosen <-
        if Map.null sets
          then pure Map.empty
          else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid abilId sets))
      let keysAgree = Map.keysSet chosen == Map.keysSet sets
          eachLegal = and (Map.intersectionWith Set.member chosen sets)
      if not (keysAgree && eachLegal)
        then State.put gs -- reject: the whole activation is a no-op
        else do
          -- CR 113.7: bind the source permanent under the reserved self slot, so
          -- an activated ability that refers to "this creature" (e.g. Longtusk
          -- Cub's "put a +1/+1 counter on Longtusk Cub") resolves the reference
          -- as a slot read -- exactly as Engine.placeOne does for a TRIGGERED
          -- ability's source. srcId is the source permanent (Source.OfAbility),
          -- which is what "this permanent" names. Additive: no existing activated
          -- ability reads the self slot, so this cannot disturb them.
          State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setTriggerSource srcId (Binding.fromChoices chosen Map.empty Nothing chosenModes)}) abilId (GameState.objects g)})
          -- CR 601.2g/h via Pawl.Cost.pay: the mana window, then the components.
          -- activatable pre-checks payability (Cost.canPay, which is pure), so
          -- Unpaid is unreachable; reject-not-repair restores the whole
          -- activation -- including the ability object this function put on the
          -- stack -- if it ever is not.
          payment <- Cost.pay pid srcId (ActivatedAbility.cost ability)
          case payment of
            Payment.Paid -> pure ()
            Payment.Unpaid -> State.put gs

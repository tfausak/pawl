module Pawl.Activate where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Target as Target
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.AdditionalCost as AdditionalCost
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardType as CardType
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. Reads projected creature-ness -- a land (Evolving Wilds) is never sick-
-- gated. Only {T} (TapSelf) is affected.
tapSicknessOk :: ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
tapSicknessOk srcId ability gs =
  let needsTap = elem AdditionalCost.TapSelf (AbilityCost.additional (ActivatedAbility.cost ability))
      isCreature = Set.member CardType.Creature (Projection.cardTypesOf srcId gs)
      settled = case Game.lookupObject srcId gs of
        Just obj -> Object.sickness obj == Sickness.Settled
        Nothing -> False
   in not (needsTap && isCreature && not settled)

-- Can this additional cost be paid right now?
canPayAdditional :: ObjectId -> GameState -> AdditionalCost.AdditionalCost -> Bool
canPayAdditional srcId gs c = case c of
  AdditionalCost.TapSelf -> case Game.lookupObject srcId gs of
    Just obj -> Object.tapped obj == TapState.Untapped
    Nothing -> False
  AdditionalCost.SacrificeSelf -> Set.member srcId (GameState.battlefield gs)

-- The abilities to consider activating. Task 5: the card's PRINTED abilities.
-- Task 9 switches the body to `Projection.abilitiesOf srcId gs` so Humility
-- (layer 6) strips them -- the single switch point, the keywordsOf pattern.
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesFor = Projection.abilitiesOf

-- CR 602.2/602.5: the ability is a member of the source's abilities (abilitiesFor),
-- it is not a mana ability (mana abilities are handled at payment, not the
-- stack), every additional cost is payable, the {T} sickness gate holds, and
-- every target slot has a legal recipient (CR 602.2b).
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatable pid srcId ability gs =
  Game.controllerOf srcId gs == Just pid
    && elem ability (abilitiesFor srcId gs)
    && not (Mana.isManaAbility ability)
    && all (canPayAdditional srcId gs) (AbilityCost.additional (ActivatedAbility.cost ability))
    && tapSicknessOk srcId ability gs
    && not (any Set.null (Map.elems (Target.legalSets (ActivatedAbility.targetSpecs ability) gs)))
    && maybe True (\c -> Mana.canPay pid c gs) (AbilityCost.mana (ActivatedAbility.cost ability))

-- CR 602.2: put the ability on the stack (a fresh OfAbility object), choose and
-- stamp targets (602.2b), pay the additional costs, keep priority (117.3c).
-- Reject-not-repair on an illegal target answer; enumeration guarantees costs are
-- payable, so payment cannot fail after the prompt.
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
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
      onStack =
        gs2
          { GameState.objects = Map.insert abilId obj (GameState.objects gs2),
            GameState.stack = abilId : GameState.stack gs2
          }
      decider = Decide.deciderFor pid gs
      sets = Target.legalSets (ActivatedAbility.targetSpecs ability) gs
  State.put onStack
  chosen <-
    if Map.null sets
      then pure Map.empty
      else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid abilId sets))
  let keysAgree = Map.keysSet chosen == Map.keysSet sets
      eachLegal = and (Map.intersectionWith Set.member chosen sets)
  if not (keysAgree && eachLegal)
    then State.put gs -- reject: the whole activation is a no-op
    else do
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing}) abilId (GameState.objects g)})
      let additional = AbilityCost.additional (ActivatedAbility.cost ability)
          payAll g = List.foldl' (payAdditional srcId) g additional
      case AbilityCost.mana (ActivatedAbility.cost ability) of
        Nothing -> State.modify' payAll
        Just cost -> do
          g1 <- State.get
          case Mana.payCost pid cost g1 of
            -- activatable pre-checks canPay, so within the source elision this is
            -- unreachable; reject-not-repair if a distinguishable source ever makes
            -- payment fail (git-bug 65ce714).
            Nothing -> State.put gs
            Just paid -> State.put (payAll paid)

-- Pay one additional cost against the source permanent.
payAdditional :: ObjectId -> GameState -> AdditionalCost.AdditionalCost -> GameState
payAdditional srcId gs c = case c of
  AdditionalCost.TapSelf ->
    gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) srcId (GameState.objects gs)}
  AdditionalCost.SacrificeSelf -> Event.changeZone srcId Zone.Graveyard gs

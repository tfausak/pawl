module Pawl.Resolve where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Target as Target
import Pawl.Type.AbilityName (AbilityName)
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivePrevention as ActivePrevention
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardCriterion as CardCriterion
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import qualified Pawl.Type.Duration as Duration
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import Pawl.Type.ManaType (ManaType)
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Sickness as Sickness
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Subtype (Subtype)
import qualified Pawl.Type.Supertype as Supertype
import qualified Pawl.Type.TapState as TapState
import Pawl.Type.TargetSpec (TargetSpec)
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone

-- THE ONE LEGITIMATE HOME of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1). Everything else asks classifications. The
-- executor itself arrives with resolution; slotsOf is the read half of the
-- dataflow lint.
slotsOf :: Effect Card.Type.Card -> Set SlotName
slotsOf effect = case effect of
  Effect.DealDamage slot _ -> Set.singleton slot
  Effect.ModifyTarget _ _ slot -> Set.singleton slot
  Effect.ChangeText slot -> Set.singleton slot
  Effect.AddMana _ -> Set.empty
  Effect.Search _ -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.ControlPlayerNextTurn slot -> Set.singleton slot
  Effect.Destroy slot -> Set.singleton slot
  Effect.Sacrifice slot -> Set.singleton slot
  Effect.MoveToZone slot _ -> Set.singleton slot
  Effect.Draw _ -> Set.empty
  Effect.Mill slot _ -> Set.singleton slot
  Effect.Discard slot _ -> Set.singleton slot
  -- Create's slot is a DEFINITION, not a read: it is not a target, so the D4
  -- lint must not see it here.
  Effect.Create {} -> Set.empty
  Effect.Prevent _ _ -> Set.empty
  Effect.RegenerateSelf -> Set.empty
  Effect.Counter slot -> Set.singleton slot
  Effect.PutCounters _ _ slot -> Set.singleton slot
  Effect.Untap slot -> Set.singleton slot
  Effect.GainControl _ slot -> Set.singleton slot
  Effect.ArmDelayedTrigger _ -> Set.empty

-- D4 (the value half): does any of these effects read X? A card that reads X
-- must declare {X} in its cost (the lint), the same reads-equal-declares contract
-- slotsOf draws for target slots. Casing on Effect/Quantity is this module's
-- charter. NOTE: when an opcode gains a Quantity field, add its arm here -- the
-- compiler will not force it, since Quantity is compared by ==.
readsX :: [Effect Card.Type.Card] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = case effect of
      Effect.DealDamage _ quantity -> quantity == Quantity.Type.X
      Effect.ModifyTarget {} -> False
      Effect.ChangeText _ -> False
      Effect.AddMana _ -> False
      Effect.Search _ -> False
      Effect.ExileAllGraveyards -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy _ -> False
      Effect.Sacrifice _ -> False
      Effect.MoveToZone {} -> False
      Effect.Draw quantity -> quantity == Quantity.Type.X
      Effect.Mill _ quantity -> quantity == Quantity.Type.X
      Effect.Discard _ quantity -> quantity == Quantity.Type.X
      Effect.Create quantity _ _ -> quantity == Quantity.Type.X
      Effect.Prevent _ _ -> False
      Effect.RegenerateSelf -> False
      Effect.Counter _ -> False
      Effect.PutCounters _ quantity _ -> quantity == Quantity.Type.X
      Effect.Untap _ -> False
      Effect.GainControl _ _ -> False
      Effect.ArmDelayedTrigger _ -> False

-- CR 605: does this effect add mana, and which type? The "produces mana?" ABI
-- classification (design.md risk register). Read by Mana.isManaAbility to keep
-- mana abilities off the stack. Casing on Effect is Resolve's charter.
manaProduced :: Effect Card.Type.Card -> Maybe ManaType
manaProduced effect = case effect of
  Effect.AddMana mt -> Just mt
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText _ -> Nothing
  Effect.Search _ -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy _ -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw _ -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.Prevent _ _ -> Nothing
  Effect.RegenerateSelf -> Nothing
  Effect.Counter _ -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.Untap _ -> Nothing
  Effect.GainControl _ _ -> Nothing
  Effect.ArmDelayedTrigger _ -> Nothing

-- CR 601.3 (Panglacial): does this effect search a library? The classification
-- Stack asks before resolving, to offer the cast-while-searching opportunity.
-- Search searches the controller's own library; every other effect does not.
searchesLibrary :: Effect Card.Type.Card -> Bool
searchesLibrary effect = case effect of
  Effect.Search _ -> True
  Effect.DealDamage _ _ -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText _ -> False
  Effect.AddMana _ -> False
  Effect.ExileAllGraveyards -> False
  Effect.ControlPlayerNextTurn _ -> False
  Effect.Destroy _ -> False
  Effect.Sacrifice _ -> False
  Effect.MoveToZone {} -> False
  Effect.Draw _ -> False
  Effect.Mill {} -> False
  Effect.Discard {} -> False
  Effect.Create {} -> False
  Effect.Prevent _ _ -> False
  Effect.RegenerateSelf -> False
  Effect.Counter _ -> False
  Effect.PutCounters {} -> False
  Effect.Untap _ -> False
  Effect.GainControl _ _ -> False
  Effect.ArmDelayedTrigger _ -> False

-- CR 701.23a / 205.4c: does this card match the search criterion? BasicLandCard =
-- a Land with the Basic supertype.
matchesCriterion :: CardCriterion.CardCriterion -> Card.Type.Card -> Bool
matchesCriterion crit card = case crit of
  CardCriterion.BasicLandCard ->
    Set.member CardType.Land (TypeLine.types (Card.Type.typeLine card))
      && Set.member Supertype.Basic (TypeLine.supertypes (Card.Type.typeLine card))

-- The target slots of ChangeText effects: the slots whose land-type pair Cast
-- must bind at cast (CR 612). Casing on Effect is Resolve's charter; Cast asks
-- this classification rather than casing on Effect itself.
textChangeSlots :: Card.Type.Card -> [SlotName]
textChangeSlots card =
  let slotOf effect = case effect of
        Effect.ChangeText slot -> Just slot
        _ -> Nothing
   in Maybe.mapMaybe slotOf (Card.allEffects card)

-- CR 603.7: the delayed abilities an effect list ARMS, by name. The read half of
-- the AbilityName dataflow lint, exactly as slotsOf is for target slots.
armedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger name -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- The slots an effect list DEFINES rather than reads: a Create that names the
-- token it mints (CR 603.7c's "it"). The write half of the same lint.
definedSlots :: [Effect Card.Type.Card] -> Set SlotName
definedSlots effects =
  let bound effect = case effect of
        Effect.Create _ _ mSlot -> mSlot
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe bound effects)

-- Does any Create bind a slot while minting more than one token? CR 603.7c's "it"
-- names ONE object; binding one of several would be the engine choosing. A named
-- deferral (the P4 spec, section 8), rejected by the lint until a card needs
-- "sacrifice THEM".
bindsSeveralTokens :: [Effect Card.Type.Card] -> Bool
bindsSeveralTokens effects =
  let offends effect = case effect of
        Effect.Create quantity _ (Just _) -> quantity /= Quantity.Type.Literal 1
        _ -> False
   in any offends effects

-- Rewrite basic-land-type words in an effect's AST (CR 612). Cases on Effect
-- (Resolve's charter); delegates the inner modification of ModifyTarget to
-- Projection.rewriteModification, so neither module touches the other's
-- constructors. DealDamage and ChangeText carry no rewritable land-type word.
rewriteEffect :: [(Subtype, Subtype)] -> Effect Card.Type.Card -> Effect Card.Type.Card
rewriteEffect pairs effect = case effect of
  Effect.ModifyTarget duration modification slot ->
    Effect.ModifyTarget duration (Projection.rewriteModification pairs modification) slot
  Effect.DealDamage _ _ -> effect
  Effect.ChangeText _ -> effect
  Effect.AddMana _ -> effect
  Effect.Search _ -> effect
  Effect.ExileAllGraveyards -> effect
  Effect.ControlPlayerNextTurn _ -> effect
  Effect.Destroy _ -> effect
  Effect.Sacrifice _ -> effect
  Effect.MoveToZone {} -> effect
  Effect.Draw _ -> effect
  Effect.Mill {} -> effect
  Effect.Discard {} -> effect
  -- A text-changer does not reach a token's embedded card here (spec section 8).
  Effect.Create {} -> effect
  Effect.Prevent _ _ -> effect
  Effect.RegenerateSelf -> effect
  -- No rewritable land-type word.
  Effect.Counter _ -> effect
  Effect.PutCounters {} -> effect
  Effect.Untap _ -> effect
  Effect.GainControl _ _ -> effect
  Effect.ArmDelayedTrigger _ -> effect

-- A resolving spell's PROJECTED effects: ONLY its chosen modes' effects (CR
-- 608.2c/700.2 -- an unchosen mode's effects never resolve), with every
-- text-change affecting it applied (CR 612). This is read-point 3 of the
-- rewritable AST -- the resolver honors a spell hacked on the stack. A
-- non-modal card has one mode, always chosen, so this is unchanged for it.
effectsOf :: ObjectId -> GameState -> [Effect Card.Type.Card]
effectsOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Game.cardOf oid gs of
    Nothing -> []
    Just card ->
      let chosen = Binding.modesOf (Object.bindings obj)
       in map (rewriteEffect (Projection.textChangesAffecting oid gs)) (Card.modesEffects chosen card)

-- CR 608.2b then CR 608.2: re-validate every filled slot against its spec; if
-- the spell has slots and ALL are now illegal, it does not resolve -- it moves
-- to the graveyard with no effect applied (the fizzle). Otherwise the effects
-- run in order (CR 608.2c), each skipping a slot whose target is illegal
-- (illegal targets are unaffected; other parts still happen), and the spell
-- goes to its owner's graveyard as the final part of resolution (CR 608.2n).
resolveSpell :: ObjectId -> Game ()
resolveSpell oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.cardOf oid gs of
      Nothing -> pure ()
      Just card ->
        -- CR 608.2b/700.2c: re-validate only the CHOSEN modes' slots -- an
        -- unchosen mode's slot was never filled and is not part of this
        -- resolution's legality question.
        let specs = Card.modesTargetSpecs (Binding.modesOf (Object.bindings obj)) card
            chosen = Binding.targetsOf (Object.bindings obj)
            legalSlot slot recipient = case Map.lookup slot specs of
              -- CR 608.2b is about TARGETS. A slot with no target spec is a
              -- RESERVED binding -- the trigger's source (Pawl.Binding.triggerSource),
              -- a token this resolution minted -- and was never targeted, so it can
              -- never have become an illegal target.
              Nothing -> True
              Just spec -> Target.stillLegal recipient spec gs
            legality = Map.mapWithKey legalSlot chosen
            -- CR 608.2b's fizzle asks about the TARGETED slots only, so the
            -- reserved slots above cannot rescue a spell whose every target is gone.
            targeted = Map.restrictKeys legality (Map.keysSet specs)
            fizzles = not (Map.null specs) && not (or (Map.elems targeted))
         in if fizzles
              then State.modify' (Event.changeZone oid Zone.Graveyard)
              else do
                -- CR 613 / 608.2c: the resolving spell's controller, projected --
                -- a spell has no control effect, so this is Object.owner obj
                -- unchanged, but a controlled permanent's later ability (below)
                -- needs this same projection to resolve under the thief.
                let effectController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
                Monad.mapM_ (applyEffect oid effectController (Binding.subtypesOf (Object.bindings obj)) legality chosen) (effectsOf oid gs)
                State.modify' (Event.changeZone oid Zone.Graveyard)

-- CR 608.2: the executor shared by an activated ability (M3e) and a triggered
-- ability (M3f) on the stack. Re-validate filled slots (CR 608.2b), fold
-- applyEffect over the effects with `srcId` (the source permanent) as the effect
-- source (CR 608.2g), then the ability ceases (CR 608.2n). `stackId` is the
-- ability object's own id.
resolveEffects :: ObjectId -> ObjectId -> [Effect Card.Type.Card] -> Map.Map SlotName TargetSpec -> Game ()
resolveEffects stackId srcId effects specs = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipient = case Map.lookup slot specs of
            -- CR 608.2b is about TARGETS. A slot with no target spec is a
            -- RESERVED binding -- the trigger's source (Pawl.Binding.triggerSource),
            -- a token this resolution minted -- and was never targeted, so it can
            -- never have become an illegal target.
            Nothing -> True
            Just spec -> Target.stillLegal recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          -- CR 608.2b's fizzle asks about the TARGETED slots only, so the
          -- reserved slots above cannot rescue a spell whose every target is gone.
          targeted = Map.restrictKeys legality (Map.keysSet specs)
          fizzles = not (Map.null specs) && not (or (Map.elems targeted))
          -- CR 613 / 608.2c: the source PERMANENT's controller, projected -- so a
          -- controlled permanent's ability (e.g. a stolen creature's tap ability)
          -- resolves under the thief, not the original owner.
          effectController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf srcId gs)
       in do
            Monad.unless fizzles $
              Monad.mapM_ (applyEffect srcId effectController (Binding.subtypesOf (Object.bindings obj)) legality chosen) effects
            State.modify' (cease stackId)

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (srcId), not the ability object -- so DealDamage comes from Prodigal Sorcerer
-- (CR 608.2g). CR 700.2c/M4g: reads only the ability's CHOSEN modes (stamped at
-- activation, Activate.activateAbility) via Modal.modesEffects/modesTargetSpecs,
-- the same mode-scoping resolveSpell already applies to a modal spell. Reuses
-- applyEffect with the same per-slot legality and CR 608.2b fizzle as a spell.
-- CR 608.2n: the ability then ceases to exist -- removed from the stack and
-- objects, NOT buried (an ability is not a card).
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
          modal = ActivatedAbility.modal ability
       in resolveEffects abilId srcId (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)

-- CR 608.2n: an ability leaves the stack and ceases to exist (no graveyard).
cease :: ObjectId -> GameState -> GameState
cease abilId gs =
  gs
    { GameState.stack = filter (/= abilId) (GameState.stack gs),
      GameState.objects = Map.delete abilId (GameState.objects gs)
    }

-- One effect, applied. The case on the constructor is THIS module's charter.
-- `controller` is the controller of the resolving spell/ability -- who searches
-- their own library (CR 701.23), never the effect `source` (for an ability, the
-- source permanent may already be sacrificed as a cost).
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffect source controller bound legality chosen effect = case effect of
  Effect.DealDamage slot quantity ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case Quantity.evaluate gs source (Just controller) quantity of
          -- An unevaluable quantity is a no-op, the powerOf posture.
          Nothing -> gs
          Just n ->
            if n <= 0
              then gs
              -- The applied effect IS the event (the M3a spec, section 4):
              -- constructing this DamageEvent and funneling it is the whole
              -- application. CR 120.3e / 120.3a live in applyDamage.
              else Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n) (Projection.hasKeyword Keyword.Deathtouch source gs) DamageKind.Noncombat] gs
        _ -> gs
  Effect.ModifyTarget duration modification slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target ->
            -- CR 611.2c: the affected set is locked to this one object now.
            -- CR 608.2h / 611.2d: and so is the VALUE -- "the answer is determined
            -- only once, when the effect is applied". The quantities are frozen to
            -- Literals against the SOURCE (which holds a chosen X) and the source's
            -- CONTROLLER (whose hand a player-scoped count counts), never against
            -- the target. See the P3b spec, section 2.4.
            let (ts, gs1) = Game.freshTimestamp gs
                frozen = Projection.freezeQuantities gs source (Just controller) modification
                eff =
                  ContinuousEffect.MkContinuousEffect
                    { ContinuousEffect.source = source,
                      ContinuousEffect.timestamp = ts,
                      ContinuousEffect.duration = duration,
                      ContinuousEffect.modification = frozen,
                      ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                    }
             in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
        -- A modification cannot land on a player (CreatureTarget/LandTarget name
        -- objects) or an illegal slot (CR 608.2b): no-op.
        _ -> gs
  Effect.ChangeText slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality, Map.lookup slot bound) of
        (Just recipient, True, Just (from, to)) ->
          case recipientObject recipient of
            Nothing -> gs
            Just target ->
              -- CR 611 / 612: an indefinite continuous effect over the one target
              -- (CR 611.2c fixed set). The (from, to) is the caster's binding, baked
              -- in here; Projection rewrites both the target's type line and, at
              -- gather, any static-ability words (Task 7). Resolve CONSTRUCTS the
              -- Modification but never cases on one.
              let (ts, gs1) = Game.freshTimestamp gs
                  eff =
                    ContinuousEffect.MkContinuousEffect
                      { ContinuousEffect.source = source,
                        ContinuousEffect.timestamp = ts,
                        ContinuousEffect.duration = Duration.Indefinite,
                        ContinuousEffect.modification = Modification.ChangeSubtypeWord from to,
                        ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                      }
               in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
        _ -> gs
  -- CR 605.3b: a mana ability never resolves on the stack. AddMana is applied by
  -- Mana.tapForMana at payment, never here. Reaching this arm means a mana ability
  -- was wrongly put on the stack -- an isManaAbility classification bug.
  Effect.AddMana _ -> pure ()
  Effect.Search crit ->
    let matches1 g oid = case Game.cardOf oid g of
          Nothing -> False
          Just card -> matchesCriterion crit card
     in do
          gs <- State.get
          let matches = filter (matches1 gs) (Game.zoneMembers Zone.Library controller gs)
              decider = Decide.deciderFor controller gs
          found <- Trans.lift (Program.prompt (Prompt.SearchLibrary decider controller matches))
          case found of
            Nothing -> pure ()
            Just cardId -> State.modify' (putTapped cardId)
          -- CR 701.23: shuffle the (possibly reduced) library afterward.
          lib <- State.gets (Game.zoneMembers Zone.Library controller)
          shuffled <- Trans.lift (Program.prompt (Prompt.Shuffle lib))
          State.modify' (reorderLibrary controller shuffled)
  -- Rest in Peace's ETB: exile every card in every graveyard (CR 400.7 each move
  -- funnels through changeZone). A graveyard->exile move matches no M3f
  -- replacement or trigger, so no cascade.
  Effect.ExileAllGraveyards -> do
    gs <- State.get
    let gyCards = concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) (Map.keys (GameState.players gs))
    State.modify' (\g -> List.foldl' (\g1 c -> Event.changeZone c Zone.Exile g1) g gyCards)
  Effect.ControlPlayerNextTurn slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just (Recipient.ToPlayer target), True) ->
          -- CR 723.1: schedule control of `target` by this ability's controller
          -- (CR 723.5). Map.insert overwrites a prior pending control (CR 723.1a).
          gs {GameState.pendingControl = Map.insert target (Decider.MkDecider controller) (GameState.pendingControl gs)}
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs
  Effect.Destroy slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          -- CR 701.8: destroy through the single funnel -- indestructible (CR
          -- 702.12b) and regeneration (CR 701.19a) are Event.destroy's to decide.
          Just target -> Event.destroy target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
  Effect.Sacrifice slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient cannot be sacrificed
          -- CR 701.21: through the single funnel, which is NOT Event.destroy --
          -- CR 701.21a: sacrificing is not destroying.
          Just target -> Event.sacrifice target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
        _ -> gs
  Effect.MoveToZone slot zone ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative.
          Just target -> Event.changeZone target zone gs
        _ -> gs
  Effect.Draw quantity -> do
    gs <- State.get
    case Quantity.evaluate gs source (Just controller) quantity of
      Just n
        | n > 0 ->
            -- CR 120: draw n, folding the shared primitive so each draw re-reads the
            -- library top and the CR 121.3 empty-library loss is preserved.
            State.modify' (\g -> List.foldl' (\g1 _ -> Event.drawCard controller g1) g [1 .. n])
      _ -> pure ()
  Effect.Mill slot quantity ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just (Recipient.ToPlayer target), True) ->
          case Quantity.evaluate gs source (Just controller) quantity of
            Just n
              | n > 0 ->
                  -- CR 701.17/701.17b: top min(n, library) of the target's library to
                  -- their graveyard, funnelled so each move mints a new incarnation.
                  let topN = take (fromInteger n) (Game.zoneMembers Zone.Library target gs)
                   in List.foldl' (\g c -> Event.changeZone c Zone.Graveyard g) gs topN
            _ -> gs
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs
  Effect.Discard slot quantity -> do
    gs <- State.get
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer target), True) ->
        case Quantity.evaluate gs source (Just controller) quantity of
          Just n
            | n > 0 -> do
                let held = Game.zoneMembers Zone.Hand target gs
                    bury cs g = List.foldl' (\g1 c -> Event.changeZone c Zone.Graveyard g1) g cs
                if fromInteger n >= length held
                  -- CR 609.3: discarding the whole hand is "as much as possible," so
                  -- it is forced -- no choice, so no prompt.
                  then State.modify' (bury held)
                  else do
                    -- CR 701.9b: the discarding player chooses which cards.
                    let decider = Decide.deciderFor target gs
                    choices <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider target held (fromInteger n)))
                    let toDiscard = take (fromInteger n) (filter (\c -> elem c held) choices)
                    State.modify' (bury toDiscard)
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.Create quantity card mSlot -> do
    gs <- State.get
    case Quantity.evaluate gs source (Just controller) quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2). Each createToken mints a distinct
            -- object.
            let before = GameState.battlefield gs
            State.modify' (\g -> List.foldl' (\g1 _ -> Event.createToken controller card g1) g [1 .. n])
            case mSlot of
              Nothing -> pure ()
              Just slot -> do
                -- CR 603.7c: bind the minted token so a later effect in this same
                -- resolution -- or the delayed ability it arms -- can name it. The
                -- lint guarantees n == 1 here, so the single new battlefield id is
                -- unambiguous.
                after <- State.gets GameState.battlefield
                case Set.toList (Set.difference after before) of
                  newId : _ -> State.modify' (bindSlot source slot newId)
                  [] -> pure ()
      _ -> pure ()
  Effect.ArmDelayedTrigger name -> do
    gs <- State.get
    case Game.cardOf source gs >>= (Map.lookup name . Card.Type.delayedAbilities) of
      -- The dataflow lint makes a dangling name a failing test, never a silent
      -- no-op; this arm only keeps the executor total.
      Nothing -> pure ()
      Just ability ->
        -- CR 603.7d-f: the controller is the player who controlled the spell or
        -- ability AS IT RESOLVED -- `controller`, baked in now. CR 603.7a: an
        -- entry appended here can only ever match events at or after the current
        -- watermark, so it never fires on an event that already happened.
        let captured = maybe Map.empty Object.bindings (Game.lookupObject source gs)
            entry =
              DelayedTrigger.MkDelayedTrigger
                { DelayedTrigger.ability = ability,
                  DelayedTrigger.source = source,
                  DelayedTrigger.controller = controller,
                  DelayedTrigger.bindings = captured
                }
         in State.put gs {GameState.delayedTriggers = GameState.delayedTriggers gs Seq.|> entry}
  Effect.Prevent duration prevention ->
    -- CR 615.3: install the shield; Event.applyPreventions consults it at each
    -- damage funnel until cleanup drops it (CR 514.2). Targetless and unprompted.
    State.modify' $ \gs ->
      gs {GameState.preventions = ActivePrevention.MkActivePrevention prevention duration : GameState.preventions gs}
  Effect.RegenerateSelf ->
    -- CR 701.19a: add one shield to the source permanent. Map.insertWith (+)
    -- stacks a second activation. A shield on a gone/non-battlefield source is
    -- harmless (nothing will destroy it).
    State.modify' $ \gs ->
      gs {GameState.regenerationShields = Map.insertWith (+) source 1 (GameState.regenerationShields gs)}
  Effect.Counter slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        -- CR 701.6a: the slot's target is a spell on the stack; counter it through
        -- the single funnel. A player recipient / illegal slot (CR 608.2b): no-op.
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target -> Event.counter target gs
        _ -> gs
  Effect.PutCounters kind quantity slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient takes no counters
          Just target -> case Quantity.evaluate gs source (Just controller) quantity of
            Nothing -> gs -- unevaluable quantity: no-op (the powerOf posture)
            Just n -> if n <= 0 then gs else putCounters target kind (fromInteger n) gs
        _ -> gs -- illegal slot at resolution (CR 608.2b): no-op
  Effect.Untap slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        -- CR 701.26b: rotate the slot's target back to the upright position.
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient cannot be untapped
          Just target -> gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) target (GameState.objects gs)}
        _ -> gs -- illegal slot at resolution (CR 608.2b): no-op
  Effect.GainControl duration slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient cannot be controlled
          Just target ->
            -- CR 613.1b / 611.2c: the new controller is `controller` (this
            -- effect's source's controller), baked in now -- derived, never
            -- chosen. CR 302.6: the new controller has not controlled the
            -- permanent continuously, so it is re-Sicked.
            let (ts, gs1) = Game.freshTimestamp gs
                eff =
                  ContinuousEffect.MkContinuousEffect
                    { ContinuousEffect.source = source,
                      ContinuousEffect.timestamp = ts,
                      ContinuousEffect.duration = duration,
                      ContinuousEffect.modification = Modification.SetController controller,
                      ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                    }
                sicken o = o {Object.sickness = Sickness.Sick}
             in gs1
                  { GameState.continuousEffects = eff : GameState.continuousEffects gs1,
                    GameState.objects = Map.adjust sicken target (GameState.objects gs1)
                  }
        _ -> gs -- illegal slot at resolution (CR 608.2b): no-op

-- CR 603.7c: bind `target` into `slot` of `holder`'s binding environment, so a
-- later effect of the same resolution -- or a delayed ability armed by it -- can
-- name the object. `holder` is the effect SOURCE, which is the resolving spell
-- itself for a spell and the source permanent for an ability; the same object
-- ArmDelayedTrigger captures from, so the two always agree.
bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState
bindSlot holder slot target gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObject target) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 122.6: add `n` counters of a kind to a permanent's per-incarnation state.
putCounters :: ObjectId -> CounterKind.CounterKind -> Natural -> GameState -> GameState
putCounters oid kind n gs =
  let bump obj = obj {Object.counters = Map.insertWith (+) kind n (Object.counters obj)}
   in gs {GameState.objects = Map.adjust bump oid (GameState.objects gs)}

-- Put a library card onto the battlefield tapped (CR 701.23's Evolving Wilds
-- shape). changeZone mints a new object; tap it by id after the move.
putTapped :: ObjectId -> GameState -> GameState
putTapped cardId gs =
  let moved = Event.changeZone cardId Zone.Battlefield gs
   in case newestBattlefieldOf cardId gs moved of
        Nothing -> moved
        Just newId ->
          moved {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) newId (GameState.objects moved)}

-- The single battlefield id present after a one-object move that was absent
-- before (changeZone mints a fresh id, CR 400.7).
newestBattlefieldOf :: ObjectId -> GameState -> GameState -> Maybe ObjectId
newestBattlefieldOf _ before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    newId : _ -> Just newId
    [] -> Nothing

-- Write the shuffled order back to a player's library.
reorderLibrary :: PlayerId -> [ObjectId] -> GameState -> GameState
reorderLibrary pid order gs =
  gs {GameState.library = Map.insert pid (Seq.fromList order) (GameState.library gs)}

-- The object a recipient names, if any (CR 612 targets a spell or permanent, not
-- a player).
recipientObject :: Recipient -> Maybe ObjectId
recipientObject r = case r of
  Recipient.ToObject oid -> Just oid
  Recipient.ToCreature oid -> Just oid
  Recipient.ToPlayer _ -> Nothing

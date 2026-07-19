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
import qualified Pawl.Binding as Binding
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Target as Target
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.CardCriterion as CardCriterion
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Decider as Decider
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
slotsOf :: Effect -> Set SlotName
slotsOf effect = case effect of
  Effect.DealDamage slot _ -> Set.singleton slot
  Effect.ModifyTarget _ _ slot -> Set.singleton slot
  Effect.ChangeText slot -> Set.singleton slot
  Effect.AddMana _ -> Set.empty
  Effect.Search _ -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.ControlPlayerNextTurn slot -> Set.singleton slot
  Effect.Destroy slot -> Set.singleton slot
  Effect.MoveToZone slot _ -> Set.singleton slot
  Effect.Draw _ -> Set.empty
  Effect.Mill slot _ -> Set.singleton slot

-- D4 (the value half): does any of these effects read X? A card that reads X
-- must declare {X} in its cost (the lint), the same reads-equal-declares contract
-- slotsOf draws for target slots. Casing on Effect/Quantity is this module's
-- charter. NOTE: when an opcode gains a Quantity field, add its arm here -- the
-- compiler will not force it, since Quantity is compared by ==.
readsX :: [Effect] -> Bool
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
      Effect.MoveToZone {} -> False
      Effect.Draw quantity -> quantity == Quantity.Type.X
      Effect.Mill _ quantity -> quantity == Quantity.Type.X

-- CR 605: does this effect add mana, and which type? The "produces mana?" ABI
-- classification (design.md risk register). Read by Mana.isManaAbility to keep
-- mana abilities off the stack. Casing on Effect is Resolve's charter.
manaProduced :: Effect -> Maybe ManaType
manaProduced effect = case effect of
  Effect.AddMana mt -> Just mt
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText _ -> Nothing
  Effect.Search _ -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw _ -> Nothing
  Effect.Mill {} -> Nothing

-- CR 601.3 (Panglacial): does this effect search a library? The classification
-- Stack asks before resolving, to offer the cast-while-searching opportunity.
-- Search searches the controller's own library; every other effect does not.
searchesLibrary :: Effect -> Bool
searchesLibrary effect = case effect of
  Effect.Search _ -> True
  Effect.DealDamage _ _ -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText _ -> False
  Effect.AddMana _ -> False
  Effect.ExileAllGraveyards -> False
  Effect.ControlPlayerNextTurn _ -> False
  Effect.Destroy _ -> False
  Effect.MoveToZone {} -> False
  Effect.Draw _ -> False
  Effect.Mill {} -> False

-- CR 701.23a / 205.4c: does this card match the search criterion? BasicLandCard =
-- a Land with the Basic supertype.
matchesCriterion :: CardCriterion.CardCriterion -> Card.Card -> Bool
matchesCriterion crit card = case crit of
  CardCriterion.BasicLandCard ->
    Set.member CardType.Land (TypeLine.types (Card.typeLine card))
      && Set.member Supertype.Basic (TypeLine.supertypes (Card.typeLine card))

-- The target slots of ChangeText effects: the slots whose land-type pair Cast
-- must bind at cast (CR 612). Casing on Effect is Resolve's charter; Cast asks
-- this classification rather than casing on Effect itself.
textChangeSlots :: Card.Card -> [SlotName]
textChangeSlots card =
  let slotOf effect = case effect of
        Effect.ChangeText slot -> Just slot
        _ -> Nothing
   in Maybe.mapMaybe slotOf (Card.effects card)

-- Rewrite basic-land-type words in an effect's AST (CR 612). Cases on Effect
-- (Resolve's charter); delegates the inner modification of ModifyTarget to
-- Projection.rewriteModification, so neither module touches the other's
-- constructors. DealDamage and ChangeText carry no rewritable land-type word.
rewriteEffect :: [(Subtype, Subtype)] -> Effect -> Effect
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
  Effect.MoveToZone {} -> effect
  Effect.Draw _ -> effect
  Effect.Mill {} -> effect

-- A resolving spell's PROJECTED effects: its printed effects with every
-- text-change affecting it applied (CR 612). This is read-point 3 of the
-- rewritable AST -- the resolver honors a spell hacked on the stack.
effectsOf :: ObjectId -> GameState -> [Effect]
effectsOf oid gs = case Game.cardOf oid gs of
  Nothing -> []
  Just card -> map (rewriteEffect (Projection.textChangesAffecting oid gs)) (Card.effects card)

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
        let specs = Card.targetSpecs card
            chosen = Binding.targetsOf (Object.bindings obj)
            legalSlot slot recipient = case Map.lookup slot specs of
              Nothing -> False
              Just spec -> Target.stillLegal recipient spec gs
            legality = Map.mapWithKey legalSlot chosen
            fizzles = not (Map.null specs) && not (or (Map.elems legality))
         in if fizzles
              then State.modify' (Event.changeZone oid Zone.Graveyard)
              else do
                Monad.mapM_ (applyEffect oid (Object.owner obj) (Binding.subtypesOf (Object.bindings obj)) legality chosen) (effectsOf oid gs)
                State.modify' (Event.changeZone oid Zone.Graveyard)

-- CR 608.2: the executor shared by an activated ability (M3e) and a triggered
-- ability (M3f) on the stack. Re-validate filled slots (CR 608.2b), fold
-- applyEffect over the effects with `srcId` (the source permanent) as the effect
-- source (CR 608.2g), then the ability ceases (CR 608.2n). `stackId` is the
-- ability object's own id.
resolveEffects :: ObjectId -> ObjectId -> [Effect] -> Map.Map SlotName TargetSpec -> Game ()
resolveEffects stackId srcId effects specs = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> False
            Just spec -> Target.stillLegal recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          fizzles = not (Map.null specs) && not (or (Map.elems legality))
       in do
            Monad.unless fizzles $
              Monad.mapM_ (applyEffect srcId (Object.owner obj) (Binding.subtypesOf (Object.bindings obj)) legality chosen) effects
            State.modify' (cease stackId)

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (srcId), not the ability object -- so DealDamage comes from Prodigal Sorcerer
-- (CR 608.2g). Reuses applyEffect with the same per-slot legality and CR 608.2b
-- fizzle as a spell. CR 608.2n: the ability then ceases to exist -- removed from
-- the stack and objects, NOT buried (an ability is not a card).
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility -> Game ()
resolveAbility abilId srcId ability =
  resolveEffects abilId srcId (ActivatedAbility.effects ability) (ActivatedAbility.targetSpecs ability)

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
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect -> Game ()
applyEffect source controller bound legality chosen effect = case effect of
  Effect.DealDamage slot quantity ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case Quantity.evaluate gs source quantity of
          -- An unevaluable quantity is a no-op, the powerOf posture.
          Nothing -> gs
          Just n ->
            if n <= 0
              then gs
              -- The applied effect IS the event (the M3a spec, section 4):
              -- constructing this DamageEvent and funneling it is the whole
              -- application. CR 120.3e / 120.3a live in applyDamage.
              else Damage.applyDamage [DamageEvent.MkDamageEvent source recipient (fromInteger n) (Projection.hasKeyword Keyword.Deathtouch source gs)] gs
        _ -> gs
  Effect.ModifyTarget duration modification slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target ->
            -- CR 611.2c: the affected set is locked to this one object now. The
            -- Modification's quantities are stored as-is (Literals); CR 611.2b's
            -- freeze is a no-op until X exists, at which point evaluate-and-freeze
            -- here. See the M3b spec, section 3.
            let (ts, gs1) = Game.freshTimestamp gs
                eff =
                  ContinuousEffect.MkContinuousEffect
                    { ContinuousEffect.source = source,
                      ContinuousEffect.timestamp = ts,
                      ContinuousEffect.duration = duration,
                      ContinuousEffect.modification = modification,
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
          Just target ->
            -- CR 700.4: an indestructible permanent can't be destroyed -- the
            -- effect does nothing (no move). Read through the projection, so a
            -- Humility'd permanent (keywords stripped) can be destroyed.
            if Projection.hasKeyword Keyword.Indestructible target gs
              then gs
              else Event.changeZone target Zone.Graveyard gs
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
    case Quantity.evaluate gs source quantity of
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
          case Quantity.evaluate gs source quantity of
            Just n
              | n > 0 ->
                  -- CR 701.13/701.13b: top min(n, library) of the target's library to
                  -- their graveyard, funnelled so each move mints a new incarnation.
                  let topN = take (fromInteger n) (Game.zoneMembers Zone.Library target gs)
                   in List.foldl' (\g c -> Event.changeZone c Zone.Graveyard g) gs topN
            _ -> gs
        -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
        _ -> gs

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

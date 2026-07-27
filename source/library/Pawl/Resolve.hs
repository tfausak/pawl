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
import qualified Pawl.Card as Card
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Setup as Setup
import qualified Pawl.Target as Target
import Pawl.Type.AbilityName (AbilityName)
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.DamageKind as DamageKind
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.DelayedTrigger as DelayedTrigger
import qualified Pawl.Type.Duration as Duration
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Effect as Effect
import Pawl.Type.Game (Game)
import qualified Pawl.Type.GameEvent as GameEvent
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.HandActionPerformer as HandActionPerformer
import Pawl.Type.ManaProduction (ManaProduction)
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.MonarchTarget as MonarchTarget
import qualified Pawl.Type.MonarchWatch as MonarchWatch
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.PlayerRef (PlayerRef)
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import Pawl.Type.SlotName (SlotName)
import qualified Pawl.Type.Source as Source
import Pawl.Type.Subtype (Subtype)
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import Pawl.Type.TargetSpec (TargetSpec)
import qualified Pawl.Type.Zone as Zone

-- The slots a PlayerRef reads. Only InSlot names one; EachPlayer and Relative
-- are answered from the evaluation context alone. Factored out of slotsOf below
-- so the recursion into PlayerRef is stated once.
playerRefSlots :: PlayerRef -> Set SlotName
playerRefSlots ref = case ref of
  PlayerRef.EachPlayer -> Set.empty
  PlayerRef.Relative _ -> Set.empty
  PlayerRef.InSlot slot -> Set.singleton slot

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
  Effect.ExileHandThenDraw -> Set.empty
  Effect.RestartGame -> Set.empty
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
  Effect.Replace {} -> Set.empty
  Effect.Counter slot -> Set.singleton slot
  Effect.PutCounters _ _ slot -> Set.singleton slot
  Effect.GainPlayerCounters ref _ _ -> playerRefSlots ref
  Effect.Untap slot -> Set.singleton slot
  Effect.GainControl _ slot -> Set.singleton slot
  Effect.ArmDelayedTrigger _ -> Set.empty
  Effect.AffectPlayers {} -> Set.empty
  Effect.CreateEmblem {} -> Set.empty
  Effect.BecomeMonarch {} -> Set.empty
  Effect.ExileUntilMonarch slot -> Set.singleton slot
  Effect.Attach slot -> Set.singleton slot
  -- CR 729.1/729.1b: PlaySubgame's slot is a DEFINITION (the derived loser,
  -- bound once the subgame ends), not a read -- same shape as Create's slot.
  Effect.PlaySubgame _ -> Set.empty

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
      Effect.ExileHandThenDraw -> False
      Effect.RestartGame -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy _ -> False
      Effect.Sacrifice _ -> False
      Effect.MoveToZone {} -> False
      Effect.Draw quantity -> quantity == Quantity.Type.X
      Effect.Mill _ quantity -> quantity == Quantity.Type.X
      Effect.Discard _ quantity -> quantity == Quantity.Type.X
      Effect.Create quantity _ _ -> quantity == Quantity.Type.X
      Effect.Replace {} -> False
      Effect.Counter _ -> False
      Effect.PutCounters _ quantity _ -> quantity == Quantity.Type.X
      Effect.GainPlayerCounters _ _ quantity -> quantity == Quantity.Type.X
      Effect.Untap _ -> False
      Effect.GainControl _ _ -> False
      Effect.ArmDelayedTrigger _ -> False
      Effect.AffectPlayers {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.Attach _ -> False
      Effect.PlaySubgame _ -> False

-- CR 605: does this effect add mana, and how is its type decided? The "produces
-- mana?" ABI classification (design.md risk register). Read by Mana.isManaAbility
-- to keep mana abilities off the stack, and by Mana.manaTypesOf to enumerate what
-- a source could produce. Casing on Effect is Resolve's charter.
--
-- Returns the ManaProduction rather than a settled ManaType because CR 605.1a
-- asks whether the ability "could add mana", which an unresolved colour choice
-- answers yes to; WHICH colour is Mana.tapForMana's prompt, not a static fact.
manaProduced :: Effect Card.Type.Card -> Maybe ManaProduction
manaProduced effect = case effect of
  Effect.AddMana production -> Just production
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText _ -> Nothing
  Effect.Search _ -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.RestartGame -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy _ -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw _ -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.Replace {} -> Nothing
  Effect.Counter _ -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.Untap _ -> Nothing
  Effect.GainControl _ _ -> Nothing
  Effect.ArmDelayedTrigger _ -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.Attach _ -> Nothing
  Effect.PlaySubgame _ -> Nothing

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
  Effect.ExileHandThenDraw -> False
  Effect.RestartGame -> False
  Effect.ControlPlayerNextTurn _ -> False
  Effect.Destroy _ -> False
  Effect.Sacrifice _ -> False
  Effect.MoveToZone {} -> False
  Effect.Draw _ -> False
  Effect.Mill {} -> False
  Effect.Discard {} -> False
  Effect.Create {} -> False
  Effect.Replace {} -> False
  Effect.Counter _ -> False
  Effect.PutCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.Untap _ -> False
  Effect.GainControl _ _ -> False
  Effect.ArmDelayedTrigger _ -> False
  Effect.AffectPlayers {} -> False
  Effect.CreateEmblem {} -> False
  Effect.BecomeMonarch {} -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.Attach _ -> False
  Effect.PlaySubgame _ -> False

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
        Effect.PlaySubgame slot -> Just slot
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe bound effects)

-- Does any Create bind a slot while minting more than one token? CR 603.7c's "it"
-- names ONE object; binding one of several would be the engine choosing, so the
-- lint rejects it rather than guessing (#53).
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
  Effect.ExileHandThenDraw -> effect
  Effect.RestartGame -> effect
  Effect.ControlPlayerNextTurn _ -> effect
  Effect.Destroy _ -> effect
  Effect.Sacrifice _ -> effect
  Effect.MoveToZone {} -> effect
  Effect.Draw _ -> effect
  Effect.Mill {} -> effect
  Effect.Discard {} -> effect
  -- A text-changer does not reach a token's embedded card here (spec section 8).
  Effect.Create {} -> effect
  Effect.Replace {} -> effect
  -- No rewritable land-type word.
  Effect.Counter _ -> effect
  Effect.PutCounters {} -> effect
  -- No rewritable land-type word.
  Effect.GainPlayerCounters {} -> effect
  Effect.Untap _ -> effect
  Effect.GainControl _ _ -> effect
  Effect.ArmDelayedTrigger _ -> effect
  -- A player effect carries no basic-land-type word for CR 612 to rewrite.
  Effect.AffectPlayers {} -> effect
  -- An emblem's embedded card carries no basic-land-type word here (as Create's
  -- token does not; spec section 8).
  Effect.CreateEmblem {} -> effect
  -- No rewritable land-type word.
  Effect.BecomeMonarch {} -> effect
  -- No rewritable land-type word.
  Effect.ExileUntilMonarch _ -> effect
  Effect.Attach _ -> effect
  -- No rewritable land-type word.
  Effect.PlaySubgame _ -> effect

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
       in fmap (rewriteEffect (Projection.textChangesAffecting oid gs)) (Card.modesEffects chosen card)

-- CR 608.2b: are ALL of this spell's targets illegal? "For every instance of the
-- word 'target'" -- so a spell with no target spec never fizzles, and one with
-- several survives if any one is still legal. Reserved slots (a trigger's source,
-- a token this resolution minted) are not targets and cannot make a spell fizzle;
-- they are vacuously legal.
--
-- Shared by the ordinary spell path (resolveSpellWith) and the Aura path
-- (Pawl.Stack), which is the whole point of it being a function: an Aura spell is
-- the first PERMANENT spell that can be countered on resolution, and a second
-- copy of this logic would drift.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Game.cardOf oid gs of
    Nothing -> False
    Just card ->
      let specs = Card.modesTargetSpecs (Binding.modesOf (Object.bindings obj)) card
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> True
            Just spec -> Target.stillLegal oid recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          targeted = Map.restrictKeys legality (Map.keysSet specs)
       in not (Map.null specs) && not (or (Map.elems targeted))

-- CR 608.2b then CR 608.2: re-validate every filled slot against its spec; if
-- the spell has slots and ALL are now illegal, it does not resolve -- it moves
-- to the graveyard with no effect applied (the fizzle). Otherwise the effects
-- run in order (CR 608.2c), each skipping a slot whose target is illegal
-- (illegal targets are unaffected; other parts still happen), and the spell
-- goes to its owner's graveyard as the final part of resolution (CR 608.2n).
-- CR 608.2b/608.2c, extended for CR 729.1b: resolve a spell, re-reading the
-- resolving object's bindings before EACH effect so a slot DEFINED mid-resolution
-- (PlaySubgame's loser; a Create's minted token) is visible to a later effect.
-- Target-slot legality is still fixed at the START of resolution (the pre-fold
-- `gs`); only newly-defined reserved slots (never targets) newly appear, and a
-- reserved slot is vacuously legal (legalSlot's Nothing branch). `runSubgame` is
-- the injected nested-game runner.
resolveSpellWith :: Game Result -> ObjectId -> Game ()
resolveSpellWith runSubgame oid = do
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
            legalSlot slot recipient = case Map.lookup slot specs of
              -- CR 608.2b is about TARGETS. A slot with no target spec is a
              -- RESERVED binding -- the trigger's source (Pawl.Binding.triggerSource),
              -- a token this resolution minted -- and was never targeted, so it can
              -- never have become an illegal target.
              Nothing -> True
              Just spec -> Target.stillLegal oid recipient spec gs
         in if targetsAllIllegal oid gs
              then Event.changeZone oid Zone.Graveyard
              else do
                -- CR 405.4: a spell's controller is the player who cast it,
                -- fixed once, at cast time -- Object.owner obj already carries
                -- that value (a card's owner never differs from its caster in
                -- this pool: nothing lets a player cast a card they don't
                -- own from hand). This Projection.controllerOf call is a no-op
                -- today: no effect in the pool ever installs a SetController
                -- naming a STACK object, so it always folds back to
                -- Object.owner -- but it re-reads live projected control
                -- rather than trusting the frozen owner outright, the same
                -- shape an ability's controller recompute used to take (#83).
                let effectController = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)
                Monad.forM_ (effectsOf oid gs) $ \eff -> do
                  -- Re-read the live bindings for THIS effect: a prior PlaySubgame
                  -- may have bound its loser slot. Target legality is recomputed
                  -- with the same pre-fold `legalSlot` (targets unchanged; the new
                  -- reserved slot is vacuously legal).
                  bindingsNow <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject oid)
                  let chosenNow = Binding.targetsOf bindingsNow
                      legalityNow = Map.mapWithKey legalSlot chosenNow
                  applyEffectWith runSubgame oid effectController (Binding.subtypesOf bindingsNow) legalityNow chosenNow eff
                Event.changeZone oid Zone.Graveyard

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame

-- CR 608.2: the executor shared by an activated ability (M3e) and a triggered
-- ability (M3f) on the stack. Re-validate filled slots (CR 608.2b), fold
-- applyEffect over the effects with `srcId` (the source permanent) as the effect
-- source (CR 113.7), then the ability ceases (CR 608.2n). `stackId` is the
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
            Just spec -> Target.stillLegal srcId recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          -- CR 608.2b's fizzle asks about the TARGETED slots only, so the
          -- reserved slots above cannot rescue a spell whose every target is gone.
          targeted = Map.restrictKeys legality (Map.keysSet specs)
          fizzles = not (Map.null specs) && not (or (Map.elems targeted))
          -- CR 113.8: the controller of an activated ability on the stack is
          -- the player who activated it; the controller of a triggered
          -- ability on the stack is whoever controlled its source when it
          -- triggered (CR 603.3a). Both are fixed once, at the ability's
          -- creation -- Activate.activateAbility stamps Object.owner = the
          -- activating player, and Engine.placeOne stamps it with
          -- PendingTrigger.controller, the CR 603.3a value -- and never
          -- revisited. `obj` here is the ability object itself (looked up by
          -- `stackId`, not `srcId`), so its stamped owner IS the answer; a
          -- stolen permanent's later controller must not override it.
          effectController = Object.owner obj
       in do
            Monad.unless fizzles $
              Monad.mapM_ (applyEffect srcId effectController (Binding.subtypesOf (Object.bindings obj)) legality chosen) effects
            State.modify' (cease stackId)

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (srcId), not the ability object -- so DealDamage comes from Prodigal Sorcerer
-- (CR 113.7a). CR 700.2c/M4g: reads only the ability's CHOSEN modes (stamped at
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
-- The subgame-runner-aware executor. `runSubgame` is the injected Game Result
-- that PLAYS a nested game (Engine.playSubgame); the bare applyEffect below
-- passes noSubgame. Only the PlaySubgame arm consults it.
-- CR 701.3a/701.3b: may `src` legally be attached to `target` right now?
--
-- CR 301.5 is the only attachment legality this pool can express: "An Equipment
-- can be attached to a creature. It can't legally be attached to anything that
-- isn't a creature", plus CR 301.5c's "An Equipment can't equip itself".
-- Subtype and creature-ness are read through the PROJECTION, so an Equipment
-- that lost the subtype (CR 301.5c's second sentence) and a permanent animated
-- into a creature both answer correctly.
--
-- False for a non-Equipment source. An Aura mover would consult that Aura's own
-- enchant spec instead (CR 303.4j), and no card in the pool moves an Aura, so
-- that branch has no producer to prove it (#187).
attachLegal :: ObjectId -> ObjectId -> GameState -> Bool
attachLegal src target gs =
  Set.member Subtype.Equipment (Projection.subtypesOf src gs)
    && src /= target
    && Projection.isCreatureOf target gs

applyEffectWith :: Game Result -> ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffectWith runSubgame source controller bound legality chosen effect = case effect of
  Effect.DealDamage slot quantity -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case Quantity.evaluate viewOf context gs source quantity of
        -- An unevaluable quantity is a no-op, the powerOf posture.
        Nothing -> pure ()
        Just n ->
          Monad.when (n > 0) $
            -- The applied effect IS the event (the M3a spec, section 4):
            -- constructing this DamageEvent and funneling it is the whole
            -- application. CR 120.3e / 120.3a live in applyDamage.
            Damage.applyDamage [Damage.damageEvent gs DamageKind.Noncombat source recipient (fromInteger n)]
      _ -> pure ()
  Effect.ModifyTarget duration modification slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs
          Just target -> case Expiry.arm controller source duration gs of
            -- CR 611.2b: the duration never started, so the effect does nothing
            -- and is never stored.
            Nothing -> gs
            Just expiry ->
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
                        ContinuousEffect.expiry = expiry,
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
            -- CR 611.2a: the opcode states no duration, so the effect "lasts
            -- until the end of the game" -- Duration.Indefinite, armed through
            -- Pawl.Expiry like the other three storing arms rather than naming a
            -- stored Expiry here. Indefinite always arms, so the Nothing branch
            -- is unreachable; it is written out because arm is total over
            -- Duration and CR 611.2b's "never starts" is its general answer.
            Just target -> case Expiry.arm controller source Duration.Indefinite gs of
              Nothing -> gs
              Just expiry ->
                -- CR 611 / 612: a continuous effect over the one target (CR 611.2c
                -- fixed set). The (from, to) is the caster's binding, baked in here;
                -- Projection rewrites both the target's type line and, at gather, any
                -- static-ability words. Resolve CONSTRUCTS the Modification but never
                -- cases on one.
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          ContinuousEffect.modification = Modification.ChangeSubtypeWord from to,
                          ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                        }
                 in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
        _ -> gs
  -- CR 605.3b: a mana ability never resolves on the stack. AddMana is applied by
  -- Mana.tapForMana at payment, never here. Reaching this arm means a mana ability
  -- was wrongly put on the stack -- an isManaAbility classification bug.
  Effect.AddMana _ -> pure ()
  Effect.Search filter_ ->
    -- CR 701.23a: match each library card through the PRINTED-card view -- a card
    -- in a library has no projection. The context has no perspective (CR 109.5): a
    -- search filter never references a player, so ControlledBy is vacuously False.
    -- No source in scope at this site.
    let searchContext = Filter.MkContext Nothing Nothing
        matches1 g oid = case Game.cardOf oid g of
          Nothing -> False
          Just card -> Filter.matches searchContext (Projection.viewOfCard card) filter_
     in do
          gs <- State.get
          let matches = filter (matches1 gs) (Game.zoneMembers Zone.Library controller gs)
              decider = Decide.deciderFor controller gs
          answer <- Trans.lift (Program.prompt (Prompt.SearchLibrary decider controller matches))
          -- CR 701.23a: the card found is one the search's own filter admits.
          -- Filtered, not trusted (#222): naming a card the filter excluded, or one
          -- that is not in the library at all, finds nothing rather than fetching
          -- it. CR 701.23b makes that a legal outcome in its own right -- "that
          -- player isn't required to find some or all of those cards even if
          -- they're present" -- so rejecting needs no new branch downstream.
          let found = case answer of
                Just oid | List.elem oid matches -> Just oid
                _ -> Nothing
          mapM_ putTapped found
          -- CR 701.23: shuffle the (possibly reduced) library afterward.
          lib <- State.gets (Game.zoneMembers Zone.Library controller)
          shuffleAnswer <- Trans.lift (Program.prompt (Prompt.Shuffle lib))
          State.modify' (reorderLibrary controller (Game.honourShuffle lib shuffleAnswer))
  -- Rest in Peace's ETB: exile every card in every graveyard (CR 400.7 each move
  -- funnels through changeZone). A graveyard->exile move matches no M3f
  -- replacement or trigger, so no cascade.
  --
  -- The loop spans every player in the map, including one who has left the game,
  -- and that is masked rather than correct: CR 800.4a took every object a
  -- departing player owns out of the game, so Game.zoneMembers finds nothing in
  -- their graveyard and the extra iteration moves nothing. Left as-is for the
  -- same reason Count.playersFor is -- see the argument there.
  Effect.ExileAllGraveyards -> do
    gs <- State.get
    let gyCards = concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) (Map.keys (GameState.players gs))
    Monad.mapM_ (\c -> Event.changeZone c Zone.Exile) gyCards
  -- CR 103.5b (Serum Powder): "exile all the cards from your hand, then draw
  -- that many cards." The count is the hand size BEFORE the exile, which is why
  -- this is one opcode and not an exile followed by a Draw.
  --
  -- Both halves go through the usual funnels: Event.changeZone mints each exiled
  -- card a fresh incarnation (CR 400.7), and Event.drawCard flags a draw from an
  -- empty library, so a short deck still loses at the first upkeep (CR 727.3 /
  -- 729.3) exactly as the mulligan redraw already does.
  Effect.ExileHandThenDraw -> do
    gs <- State.get
    let handIds = Game.zoneMembers Zone.Hand controller gs
    Monad.mapM_ (\oid -> Event.changeZone oid Zone.Exile) handIds
    Monad.replicateM_ (length handIds) (Event.drawCard controller)
  -- CR 727.1/727.1a: restart the game. The starting player of the new game is
  -- this ability's controller (CR 727.1a), which applyEffect already holds as
  -- `controller`; the rebuild lives in Setup (game construction). The engine
  -- reaches it through a generic opcode, never Karn's identity.
  -- CR 727.4: this resolves several frames deep -- inside the priority loop,
  -- inside a step -- and the rebuild replaces the game those frames are running.
  -- GameState.restartSignal is how they unwind to the rebuilt turn 1; see
  -- Pawl.Type.RestartSignal.
  -- Not implemented: the CR 727.5/727.5a exemption + put-onto-battlefield rider
  -- of full Karn Liberated (#135), which retires the synthetic-restart gate.
  Effect.RestartGame -> Setup.restartGame performHandAction controller
  -- CR 729.1/729.5: run the nested game to completion (the runner does the
  -- construction, play, funnel-back, and reshuffle); then bind its outcome.
  -- CR 729.1b: the loser is the 2-player derivation from the Result; a Drawn
  -- subgame binds nothing (the follow-on then no-ops). Multi-player "each
  -- player who doesn't win" and a widened Result are deferred (#138); this
  -- arm runs only on the SPELL path -- an ability-driven subgame is deferred
  -- (#137). The fixed follow-on stands in for Shahrazad's half-life rider
  -- (#139), which retires the synthetic gate.
  -- Shahrazad's Oracle text scopes the loser to "each player who doesn't win
  -- the subgame" -- so the roster the loser is drawn from is the players who
  -- were actually seated for the subgame, i.e. Game.stillPlayingInOrder,
  -- not the main game's full seating (GameState.turnOrder). A player who
  -- departed the main game before this effect resolved never played the
  -- subgame -- Setup.subgameStateFrom seats only stillPlayingInOrder -- so
  -- turnOrder can name a non-participant as "the loser" even though nothing
  -- about them ever touched the subgame.
  Effect.PlaySubgame slot -> do
    result <- runSubgame
    order <- State.gets Game.stillPlayingInOrder
    case result of
      Result.Won winner -> case List.find (/= winner) order of
        Just loser -> State.modify' (bindLoserSlot source slot loser)
        Nothing -> pure ()
      Result.Drawn -> pure ()
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
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure ()
        -- CR 701.8: destroy through the single funnel -- indestructible (CR
        -- 702.12b) and regeneration (CR 701.19a) are Event.destroy's to decide.
        Just target -> Event.destroy target
      -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
      _ -> pure ()
  Effect.Sacrifice slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient cannot be sacrificed
        -- CR 701.21: through the single funnel, which is NOT Event.destroy --
        -- CR 701.21a: sacrificing is not destroying.
        Just target -> Event.sacrifice target
      -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
      _ -> pure ()
  Effect.MoveToZone slot zone ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure ()
        -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative.
        Just target -> Event.changeZone target zone
      _ -> pure ()
  Effect.Draw quantity -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 ->
            -- CR 120: draw n, folding the shared primitive so each draw re-reads the
            -- library top and the CR 121.3 empty-library loss is preserved.
            Monad.replicateM_ (fromInteger n) (Event.drawCard controller)
      _ -> pure ()
  Effect.Mill slot quantity -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer target), True) ->
        case Quantity.evaluate viewOf context gs source quantity of
          Just n
            | n > 0 ->
                -- CR 701.17/701.17b: top min(n, library) of the target's library to
                -- their graveyard, funnelled so each move mints a new incarnation.
                let topN = take (fromInteger n) (Game.zoneMembers Zone.Library target gs)
                 in Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) topN
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.Discard slot quantity -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer target), True) ->
        case Quantity.evaluate viewOf context gs source quantity of
          Just n
            | n > 0 -> do
                let held = Game.zoneMembers Zone.Hand target gs
                    bury :: [ObjectId] -> Game ()
                    bury = Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard)
                if fromInteger n >= length held
                  -- CR 609.3: discarding the whole hand is "as much as possible," so
                  -- it is forced -- no choice, so no prompt.
                  then bury held
                  else do
                    -- CR 701.9b: the discarding player chooses which cards.
                    let decider = Decide.deciderFor target gs
                    choices <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider target held (fromInteger n)))
                    let toDiscard = take (fromInteger n) (filter (\c -> elem c held) choices)
                    bury toDiscard
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.Create quantity card mSlot -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2), through the single funnel -- so CR
            -- 614's token replacements (Doubling Season) get their opportunity.
            minted <- Event.createTokens controller card (fromInteger n)
            case (mSlot, minted) of
              (Nothing, _) -> pure ()
              -- Unreachable: createTokens places every token onto the battlefield
              -- (CR 111.2). Total rather than partial: nothing bound matches "the
              -- token was never named" instead of crashing.
              (Just _, []) -> pure ()
              -- CR 603.7c: bind the minted token into live Object.bindings so a
              -- delayed ability THIS SAME resolution arms can name it. The lint
              -- guarantees the PRINTED quantity is 1 here (#53) -- but a
              -- replacement can now make it more (Doubling Season doubling a
              -- delayed ability's named token), in which case "it" names the
              -- first and the rest are unnamed (#77).
              (Just slot, newId : _) -> State.modify' (bindSlot source slot newId)
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
  Effect.Replace duration uses re ->
    -- CR 614.3 / 615.3: install the floating replacement; Pawl.Replacement
    -- consults it at every funnel until cleanup drops it (CR 514.2) or its use is
    -- spent. Targetless and unprompted. CR 113.7: the SOURCE is this effect's
    -- source, which is what CR 615.13's "prevented" triggers will read (#58).
    State.modify' $ \gs -> case Expiry.arm controller source duration gs of
      -- CR 611.2b: the duration never started, so no floating replacement is
      -- installed.
      Nothing -> gs
      Just expiry ->
        let (ts, gs1) = Game.freshTimestamp gs
            active =
              ActiveReplacement.MkActiveReplacement
                { ActiveReplacement.effect = re,
                  ActiveReplacement.source = source,
                  ActiveReplacement.timestamp = ts,
                  ActiveReplacement.expiry = expiry,
                  ActiveReplacement.uses = uses
                }
         in gs1 {GameState.replacements = active : GameState.replacements gs1}
  Effect.AffectPlayers duration scope playerEffect ->
    -- CR 611.1 / 613.11: install the stored player effect. Targetless and
    -- unprompted. CR 109.5: the CONTROLLER is baked in now, because the source
    -- will not have one to project once it leaves the stack (Silence is an
    -- instant). The SCOPE is not: CR 611.2c makes a rules-modifying effect one
    -- that "can affect objects that weren't affected when that continuous effect
    -- began", so it is re-resolved on every read.
    State.modify' $ \gs -> case Expiry.arm controller source duration gs of
      -- CR 611.2b: the duration never started, so nothing is stored.
      Nothing -> gs
      Just expiry ->
        let (ts, gs1) = Game.freshTimestamp gs
            active =
              ActivePlayerEffect.MkActivePlayerEffect
                { ActivePlayerEffect.source = source,
                  ActivePlayerEffect.controller = controller,
                  ActivePlayerEffect.timestamp = ts,
                  ActivePlayerEffect.expiry = expiry,
                  ActivePlayerEffect.scope = scope,
                  ActivePlayerEffect.effect = playerEffect
                }
         in gs1 {GameState.playerEffects = active : GameState.playerEffects gs1}
  Effect.CreateEmblem card -> do
    -- CR 114.2 / 613.7a: the emblem enters the command zone under the resolving
    -- controller; its entry timestamp is what the projection reads when ordering
    -- its static ability's continuous effect. Inert per-incarnation fields (it is
    -- never tapped/damaged/countered): harmless, nothing reads them here.
    let mkObj ts =
          Object.MkObject
            { Object.owner = controller,
              Object.source = Source.OfEmblem card,
              Object.zone = Zone.Command,
              Object.tapped = TapState.Untapped,
              Object.damage = 0,
              Object.sickness = Sickness.Settled controller,
              Object.bindings = Map.empty,
              Object.counters = Map.empty,
              Object.attachedTo = Nothing,
              Object.timestamp = ts
            }
    _ <- Event.placeObject controller mkObj Zone.Command
    pure ()
  Effect.BecomeMonarch target -> do
    gs <- State.get
    let newMonarch = case target of
          -- "you become the monarch."
          MonarchTarget.TheController -> Just controller
          -- CR 725.2: the controller of the ability's bound source (the damaging
          -- creature), read from the reserved trigger-source slot.
          MonarchTarget.ControllerOfSource ->
            Map.lookup Binding.triggerSource chosen
              >>= recipientObject
              >>= (\o -> Projection.controllerOf o gs)
    case newMonarch of
      Nothing -> pure ()
      Just p -> do
        -- CR 725.3: the previous monarch ceases simply because `monarch` is
        -- overwritten (at most one at a time).
        State.modify' (\g -> g {GameState.monarch = Just p})
        State.modify' (Event.recordEvent (GameEvent.BecameMonarch p))
  -- CR 701.3a / 702.6a: "Attach this permanent to target creature you control."
  -- CR 701.3a is the move itself -- "take it from where it currently is and put
  -- it onto that object" -- so this relocates a source that is already attached
  -- elsewhere, which is the whole reason the opcode exists: an Aura attaches once,
  -- as it enters, and nothing could move it afterwards (#187).
  Effect.Attach slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient: CR 702.5d's enchant-player is unrepresentable (#190)
        Just target -> do
          gs <- State.get
          let alreadyThere = case Game.lookupObject source gs of
                Nothing -> False
                Just obj -> Object.attachedTo obj == Just target
          -- CR 701.3b, both sentences: an attach that cannot legally be performed
          -- does not move the permanent at all (it stays where it was rather than
          -- becoming unattached), and attaching it to what it is ALREADY attached
          -- to "does nothing" -- which matters because of the restamp below.
          Monad.when (attachLegal source target gs && not alreadyThere) $ do
            gs1 <- State.get
            -- CR 701.3c: attaching to a DIFFERENT object gives it a new timestamp.
            -- Not cosmetic -- CR 613.7 orders layer effects by it, so two things
            -- modifying one creature apply in attach order.
            let (ts, gs2) = Game.freshTimestamp gs1
                move o = o {Object.attachedTo = Just target, Object.timestamp = ts}
            State.put gs2 {GameState.objects = Map.adjust move source (GameState.objects gs2)}
      _ -> pure ()
  Effect.ExileUntilMonarch slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure ()
        Just target -> do
          -- CR 400.7: exile the target through the funnel; register the resulting
          -- incarnation for return when an opponent of `controller` (CR 102.2)
          -- BECOMES the monarch. The monarch as of right now is stamped into the
          -- watch, so an opponent who already holds the crown at this moment does
          -- not discharge it -- Palace Jailer's ruling is explicit that the
          -- creature "won't immediately return just because an opponent is the
          -- monarch" (#171).
          mNew <- Event.changeZoneReturning target Zone.Exile
          case mNew of
            Nothing -> pure ()
            Just newId -> do
              monarchNow <- State.gets GameState.monarch
              let watch =
                    MonarchWatch.MkMonarchWatch
                      { MonarchWatch.controller = controller,
                        MonarchWatch.lastMonarch = monarchNow
                      }
              State.modify' (\g -> g {GameState.exiledUntilMonarch = Map.insert newId watch (GameState.exiledUntilMonarch g)})
      _ -> pure ()
  Effect.Counter slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      -- CR 701.6a: the slot's target is a spell on the stack; counter it through
      -- the single funnel. A player recipient / illegal slot (CR 608.2b): no-op.
      (Just recipient, True) -> mapM_ Event.counter $ recipientObject recipient
      _ -> pure ()
  Effect.PutCounters kind quantity slot -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient takes no counters
        Just target -> case Quantity.evaluate viewOf context gs source quantity of
          Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
          -- CR 122.6: through the single funnel, so CR 614's counter replacements
          -- (Hardened Scales, Doubling Season) get their opportunity.
          Just n -> Monad.when (n > 0) (Event.putCounters target kind (fromInteger n))
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
  Effect.GainPlayerCounters ref kind quantity -> do
    gs <- State.get
    let viewOf = Projection.fullView gs
        context = Filter.MkContext (Just controller) (Just source)
        -- The recipients. A slot's legality is asked the way every other slot
        -- read asks it (CR 608.2b): a slot filled by targeting that has since
        -- become illegal names nobody, and a RESERVED slot -- the trigger's
        -- "that player" -- has no target spec, so legalSlot already answered
        -- True for it.
        recipients = case ref of
          PlayerRef.InSlot slot -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
            (Just (Recipient.ToPlayer pid), True) -> [pid]
            _ -> [] -- an unfilled, illegal, or non-player slot: no-op
          PlayerRef.Relative PlayerRelation.You -> [controller]
          PlayerRef.Relative PlayerRelation.Opponent -> filter (/= controller) everyone
          PlayerRef.EachPlayer -> everyone
        -- Not filtered to the players still in the game, matching
        -- Count.playersFor's seat list and unobservable for the same reason: no
        -- card in the pool reaches the two arms that use it. See that function's
        -- comment for the one-line change whenever one does.
        everyone = Map.keys (GameState.players gs)
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 ->
            -- CR 122 / 107.14: each recipient gets n counters of kind, directly
            -- on the player record -- no funnel of PutCounters' kind, since CR
            -- 614's counter replacements have no player-counter producer yet
            -- (#122).
            Monad.forM_ recipients $ \pid ->
              State.modify'
                ( \g ->
                    g
                      { GameState.players =
                          Map.adjust
                            (\p -> p {Player.counters = Map.insertWith (+) kind (fromInteger n) (Player.counters p)})
                            pid
                            (GameState.players g)
                      }
                )
      _ -> pure ()
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
          Just target
            -- CR 800.4b: "If an object would change to the control of a player
            -- who has left the game, it doesn't." `controller` is baked at
            -- trigger time (CR 113.8), so a resolution can name a player who has
            -- since left. Nothing would clean up after the change: CR 800.4a's
            -- fourth clause ("Then, if there are any objects still controlled by
            -- that player, those objects are exiled") is not a state-based action
            -- and "happens as soon as the player leaves the game", so it has
            -- already run and does not run again. Without this guard the
            -- permanent would simply sit on the battlefield controlled by a
            -- player who is not in the game.
            | List.notElem controller (Game.stillPlaying gs) -> gs
            | otherwise -> case Expiry.arm controller source duration gs of
                -- CR 611.2b: the duration never started -- no control effect is
                -- stored, and nothing is re-Sicked, because control never changed.
                Nothing -> gs
                Just expiry ->
                  -- CR 613.1b / 611.2c: the new controller is `controller` (this
                  -- effect's source's controller), baked in now -- derived, never
                  -- chosen. CR 302.6: the new controller has not controlled the
                  -- permanent continuously, so it is re-Sicked.
                  --
                  -- Unless control does not actually move. CR 302.6 asks whether
                  -- control was CONTINUOUS, and gaining control of a permanent you
                  -- already control interrupts nothing, so the clock must not
                  -- reset (#206). Act of Treason may legally target your own
                  -- creature -- untapping it is the reason to.
                  --
                  -- Compared against the PROJECTED controller, read before the new
                  -- effect is stored, not against Object.owner: you may already
                  -- control a permanent you do not own (Control Magic), and
                  -- re-gaining that one interrupts nothing either.
                  let (ts, gs1) = Game.freshTimestamp gs
                      eff =
                        ContinuousEffect.MkContinuousEffect
                          { ContinuousEffect.source = source,
                            ContinuousEffect.timestamp = ts,
                            ContinuousEffect.expiry = expiry,
                            ContinuousEffect.modification = Modification.SetController controller,
                            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton target)
                          }
                      alreadyTheirs = Projection.controllerOf target gs == Just controller
                      sicken o = if alreadyTheirs then o else o {Object.sickness = Sickness.Sick}
                   in gs1
                        { GameState.continuousEffects = eff : GameState.continuousEffects gs1,
                          GameState.objects = Map.adjust sicken target (GameState.objects gs1)
                        }
        _ -> gs -- illegal slot at resolution (CR 608.2b): no-op

-- The no-subgame executor (the ability path and every direct caller): a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffect = applyEffectWith noSubgame

-- CR 103.5b / CR 103.6: perform the effects of an action a card grants from a
-- player's hand. Pawl.Mulligan's two window loops reach this through the
-- HandActionPerformer parameter they are handed (see
-- Pawl.Type.HandActionPerformer for why it is a parameter).
--
-- The action does not use the stack -- both rules say the player PERFORMS it,
-- not that they cast or activate anything -- so there is nothing to put on the
-- stack and no modes to bind. The CHOICE of whether to act was already routed
-- through Decide.deciderFor at the prompt (CR 723).
--
-- Stands on the noSubgame floor (applyEffect), exactly as the RestartGame arm
-- does: no hand action starts a subgame.
performHandAction :: HandActionPerformer.HandActionPerformer
performHandAction source player =
  Monad.mapM_
    ( applyEffect
        source
        player
        Map.empty
        -- CR 115.1: the reserved self slot is NOT a target, so there is no CR
        -- 608.2b legality question to answer -- the card is in the acting
        -- player's hand by construction. Binding it is how "this card" is
        -- expressible with no self-variant opcode (see Effect.Sacrifice's
        -- comment, and Engine.placeOne, which binds a trigger's source the same
        -- way). CR 103.6a's "puts that card onto the battlefield" is then just
        -- MoveToZone on this slot.
        (Map.singleton Binding.triggerSource True)
        (Map.singleton Binding.triggerSource (Recipient.ToObject source))
    )

-- CR 603.7c: bind `target` into `slot` of `holder`'s binding environment, so a
-- delayed ability armed later in the SAME resolution can name the object.
-- `holder` is the effect SOURCE, which is the resolving spell itself for a
-- spell and the source PERMANENT for an ability -- the same object
-- ArmDelayedTrigger captures from (`Game.lookupObject source gs` there), so the
-- two always agree. Whether this makes the slot visible to a later effect of
-- the same fold now depends on the path: on the SPELL path, resolveSpellWith
-- re-reads Object.bindings before EACH effect, so a later Sacrifice/Destroy/
-- etc. reading the same slot DOES see the mid-fold value (this is exactly what
-- lets PlaySubgame's derived loser reach a follow-on DealDamage). On the
-- ABILITY path, resolveEffects still folds applyEffect over a `chosen`
-- snapshot taken once before the fold starts, so a later Sacrifice/Destroy/
-- etc. there still sees the pre-Create value (Nothing). Only ArmDelayedTrigger
-- sees it on either path, because it re-reads Object.bindings from LIVE
-- GameState rather than from `chosen`. A spell-mode effect that tried to read
-- a dangling Create slot would be caught loudly by the D4 lint (declared slots
-- == read slots) rather than silently no-op, so this gap is a documentation
-- defect, not a latent one.
--
-- For an ABILITY, `holder`/`source` is the source PERMANENT, not the ability
-- object on the stack -- so a delayed ability armed by a triggered or activated
-- ability captures the PERMANENT's bindings (e.g. an earlier Create on that
-- same permanent), never the arming ability's own chosen targets or its `self`
-- slot. Unexercised today: only Tidal Wave, a spell (whose `source` IS the
-- resolving stack object), arms anything.
bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState
bindSlot holder slot target gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObject target) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- The default runner for every resolution that is NOT a subgame-bearing spell:
-- there is no nested game, so a PlaySubgame effect resolves as a draw and binds
-- nothing. The ability path (resolveEffects) and every direct test caller take
-- this; a subgame played from an ABILITY is deferred (no gate card needs one).
noSubgame :: Game Result
noSubgame = pure Result.Drawn

-- CR 729.1b: bind the subgame's derived loser (a player) into `slot` on the
-- resolving object, so a later effect (DealDamage) can read it. Mirrors bindSlot,
-- but the recipient is a player (ToPlayer), not an object.
bindLoserSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState
bindLoserSlot holder slot loser gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer loser) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- Put a library card onto the battlefield tapped (CR 701.23's Evolving Wilds
-- shape). changeZone mints a new object; tap it by id after the move.
putTapped :: ObjectId -> Game ()
putTapped cardId = do
  before <- State.get
  Event.changeZone cardId Zone.Battlefield
  moved <- State.get
  case newestBattlefieldOf cardId before moved of
    Nothing -> pure ()
    Just newId ->
      State.put moved {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) newId (GameState.objects moved)}

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

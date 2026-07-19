module Pawl.Resolve where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Damage as Damage
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Target as Target
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Card as Card
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.DamageEvent as DamageEvent
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
import Pawl.Type.Recipient (Recipient)
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Subtype (Subtype)
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

-- CR 605: does this effect add mana, and which type? The "produces mana?" ABI
-- classification (design.md risk register). Read by Mana.isManaAbility to keep
-- mana abilities off the stack. Casing on Effect is Resolve's charter.
manaProduced :: Effect -> Maybe ManaType
manaProduced effect = case effect of
  Effect.AddMana mt -> Just mt
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText _ -> Nothing

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
            chosen = Object.targets obj
            legalSlot slot recipient = case Map.lookup slot specs of
              Nothing -> False
              Just spec -> Target.stillLegal recipient spec gs
            legality = Map.mapWithKey legalSlot chosen
            fizzles = not (Map.null specs) && not (or (Map.elems legality))
         in if fizzles
              then State.modify' (Game.changeZone oid Zone.Graveyard)
              else do
                Monad.mapM_ (applyEffect oid (Object.chosenSubtypes obj) legality chosen) (effectsOf oid gs)
                State.modify' (Game.changeZone oid Zone.Graveyard)

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (srcId), not the ability object -- so DealDamage comes from Prodigal Sorcerer
-- (CR 608.2g). Reuses applyEffect with the same per-slot legality and CR 608.2b
-- fizzle as a spell. CR 608.2n: the ability then ceases to exist -- removed from
-- the stack and objects, NOT buried (an ability is not a card).
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let specs = ActivatedAbility.targetSpecs ability
          chosen = Object.targets obj
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> False
            Just spec -> Target.stillLegal recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          fizzles = not (Map.null specs) && not (or (Map.elems legality))
       in do
            Monad.unless fizzles $
              Monad.mapM_ (applyEffect srcId (Object.chosenSubtypes obj) legality chosen) (ActivatedAbility.effects ability)
            State.modify' (cease abilId)

-- CR 608.2n: an ability leaves the stack and ceases to exist (no graveyard).
cease :: ObjectId -> GameState -> GameState
cease abilId gs =
  gs
    { GameState.stack = filter (/= abilId) (GameState.stack gs),
      GameState.objects = Map.delete abilId (GameState.objects gs)
    }

-- One effect, applied. The case on the constructor is THIS module's charter.
applyEffect :: ObjectId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect -> Game ()
applyEffect source bound legality chosen effect = case effect of
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

-- The object a recipient names, if any (CR 612 targets a spell or permanent, not
-- a player).
recipientObject :: Recipient -> Maybe ObjectId
recipientObject r = case r of
  Recipient.ToObject oid -> Just oid
  Recipient.ToCreature oid -> Just oid
  Recipient.ToPlayer _ -> Nothing

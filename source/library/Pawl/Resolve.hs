module Pawl.Resolve where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Binding as Binding
import qualified Pawl.Card as Card
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Decide as Decide
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Setup as Setup
import qualified Pawl.Target as Target
import qualified Pawl.Turn as Turn
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Duration as Duration
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Expiry as Expiry.Type
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HandActionPerformer as HandActionPerformer
import Pawl.Types.ManaProduction (ManaProduction)
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ObjectRef (ObjectRef)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerRef (PlayerRef)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import Pawl.Types.Subtype (Subtype)
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TokenEntry as TokenEntry
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone

-- The slots a PlayerRef reads. Only InSlot names one; EachPlayer and Relative
-- are answered from the evaluation context alone. Factored out of slotsOf below
-- so the recursion into PlayerRef is stated once.
playerRefSlots :: PlayerRef -> Set SlotName
playerRefSlots ref = case ref of
  PlayerRef.EachPlayer -> Set.empty
  PlayerRef.Relative _ -> Set.empty
  PlayerRef.InSlot slot -> Set.singleton slot

-- The slots an ObjectRef reads. Only InSlot names one; EachMatching is swept
-- from the battlefield at resolution and names nothing at cast -- so a card whose
-- only object reference is a set declares no target spec, and CR 608.2b has
-- nothing to fizzle (CR 115.10a: without the word "target" it is not a target).
-- The object-side twin of playerRefSlots above.
objectRefSlots :: ObjectRef -> Set SlotName
objectRefSlots ref = case ref of
  ObjectRef.InSlot slot -> Set.singleton slot
  ObjectRef.EachMatching _ -> Set.empty

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
  Effect.Search _ _ -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.Proliferate -> Set.empty
  Effect.ExileHandThenDraw -> Set.empty
  Effect.PlayerSacrifices slot _ _ -> Set.singleton slot
  Effect.RestartGame -> Set.empty
  Effect.ControlPlayerNextTurn slot -> Set.singleton slot
  Effect.Destroy ref _ -> objectRefSlots ref
  Effect.Sacrifice slot -> Set.singleton slot
  Effect.RemoveFromCombat slot -> Set.singleton slot
  Effect.MoveToZone slot _ -> Set.singleton slot
  Effect.Draw ref _ -> playerRefSlots ref
  Effect.Mill slot _ -> Set.singleton slot
  Effect.Discard slot _ -> Set.singleton slot
  Effect.LoseLife ref _ -> playerRefSlots ref
  Effect.GainLife ref _ -> playerRefSlots ref
  -- Create's slot is a DEFINITION, not a read: it is not a target, so the D4
  -- lint must not see it here.
  Effect.Create {} -> Set.empty
  Effect.Replace {} -> Set.empty
  -- The PlayerRef may name a target slot -- Fatigue's "target player".
  Effect.SkipNextPhase ref _ -> playerRefSlots ref
  Effect.Counter slot -> Set.singleton slot
  Effect.PutCounters _ _ slot -> Set.singleton slot
  Effect.GainPlayerCounters ref _ _ -> playerRefSlots ref
  Effect.Untap ref -> objectRefSlots ref
  Effect.AddPhases _ -> Set.empty
  Effect.GainControl _ slot -> Set.singleton slot
  Effect.ArmDelayedTrigger _ _ -> Set.empty
  Effect.AffectPlayers {} -> Set.empty
  Effect.CreateEmblem {} -> Set.empty
  Effect.BecomeMonarch {} -> Set.empty
  Effect.ExileUntilMonarch slot -> Set.singleton slot
  Effect.Attach slot -> Set.singleton slot
  Effect.AttachTarget slot _ -> Set.singleton slot
  -- CR 729.1/729.1b: PlaySubgame's slot is a DEFINITION (the derived loser,
  -- bound once the subgame ends), not a read -- same shape as Create's slot.
  Effect.PlaySubgame _ -> Set.empty
  -- The PlayerRef may name a target slot -- Time Warp's "target player".
  Effect.TakeExtraTurn ref -> playerRefSlots ref

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
      Effect.Search _ _ -> False
      Effect.ExileAllGraveyards -> False
      Effect.Proliferate -> False
      Effect.ExileHandThenDraw -> False
      Effect.PlayerSacrifices _ _ quantity -> quantity == Quantity.Type.X
      Effect.RestartGame -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy {} -> False
      Effect.Sacrifice _ -> False
      Effect.RemoveFromCombat _ -> False
      Effect.MoveToZone {} -> False
      Effect.Draw _ quantity -> quantity == Quantity.Type.X
      Effect.Mill _ quantity -> quantity == Quantity.Type.X
      Effect.Discard _ quantity -> quantity == Quantity.Type.X
      Effect.LoseLife _ quantity -> quantity == Quantity.Type.X
      Effect.GainLife _ quantity -> quantity == Quantity.Type.X
      Effect.Create quantity _ _ _ -> quantity == Quantity.Type.X
      Effect.Replace {} -> False
      Effect.SkipNextPhase {} -> False
      Effect.Counter _ -> False
      Effect.PutCounters _ quantity _ -> quantity == Quantity.Type.X
      Effect.GainPlayerCounters _ _ quantity -> quantity == Quantity.Type.X
      Effect.Untap _ -> False
      Effect.AddPhases _ -> False
      Effect.GainControl _ _ -> False
      Effect.ArmDelayedTrigger _ _ -> False
      Effect.AffectPlayers {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.Attach _ -> False
      Effect.AttachTarget {} -> False
      Effect.PlaySubgame _ -> False
      Effect.TakeExtraTurn _ -> False

-- CR 605: does this effect add mana, and how is its type decided? The "produces
-- mana?" ABI classification (design.md risk register). Read by Mana.isManaAbility
-- to keep mana abilities off the stack, and by Mana.manaRoutesOfGiven to
-- enumerate what one activation of a source would add. Casing on Effect is
-- Resolve's charter.
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
  Effect.Search _ _ -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.PlayerSacrifices {} -> Nothing
  Effect.RestartGame -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy {} -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.RemoveFromCombat _ -> Nothing
  Effect.MoveToZone {} -> Nothing
  Effect.Draw {} -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.LoseLife {} -> Nothing
  Effect.GainLife {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.Replace {} -> Nothing
  Effect.SkipNextPhase {} -> Nothing
  Effect.Counter _ -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.Untap _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.GainControl _ _ -> Nothing
  Effect.ArmDelayedTrigger _ _ -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.TakeExtraTurn _ -> Nothing

-- CR 601.3 (Panglacial): does this effect search a library? The classification
-- Stack asks before resolving, to offer the cast-while-searching opportunity.
-- Search searches the controller's own library; every other effect does not.
searchesLibrary :: Effect Card.Type.Card -> Bool
searchesLibrary effect = case effect of
  Effect.Search _ _ -> True
  Effect.Proliferate -> False
  Effect.PlayerSacrifices {} -> False
  Effect.DealDamage _ _ -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText _ -> False
  Effect.AddMana _ -> False
  Effect.ExileAllGraveyards -> False
  Effect.ExileHandThenDraw -> False
  Effect.RestartGame -> False
  Effect.ControlPlayerNextTurn _ -> False
  Effect.Destroy {} -> False
  Effect.Sacrifice _ -> False
  Effect.RemoveFromCombat _ -> False
  Effect.MoveToZone {} -> False
  Effect.Draw {} -> False
  Effect.Mill {} -> False
  Effect.Discard {} -> False
  Effect.LoseLife {} -> False
  Effect.GainLife {} -> False
  Effect.Create {} -> False
  Effect.Replace {} -> False
  Effect.SkipNextPhase {} -> False
  Effect.Counter _ -> False
  Effect.PutCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.Untap _ -> False
  Effect.AddPhases _ -> False
  Effect.GainControl _ _ -> False
  Effect.ArmDelayedTrigger _ _ -> False
  Effect.AffectPlayers {} -> False
  Effect.CreateEmblem {} -> False
  Effect.BecomeMonarch {} -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.Attach _ -> False
  Effect.AttachTarget {} -> False
  Effect.PlaySubgame _ -> False
  Effect.TakeExtraTurn _ -> False

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
        Effect.ArmDelayedTrigger name _ -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- The slots an effect list DEFINES rather than reads: a Create that names the
-- token it mints (CR 603.7c's "it"). The write half of the same lint.
definedSlots :: [Effect Card.Type.Card] -> Set SlotName
definedSlots effects =
  let bound effect = case effect of
        Effect.Create _ _ _ mSlot -> mSlot
        Effect.PlaySubgame slot -> Just slot
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe bound effects)

-- Does any Create bind a slot while minting more than one token? CR 603.7c's "it"
-- names ONE object; binding one of several would be the engine choosing, so the
-- lint rejects it rather than guessing (#53).
--
-- The PRINTED quantity is all a lint can see. A CR 614.16 replacement (Doubling
-- Season) multiplies the count at RESOLUTION, past this check; the Create arm
-- below handles that by asking the controller which token "it" names.
bindsSeveralTokens :: [Effect Card.Type.Card] -> Bool
bindsSeveralTokens effects =
  let offends effect = case effect of
        Effect.Create quantity _ _ (Just _) -> quantity /= Quantity.Type.Literal 1
        _ -> False
   in any offends effects

-- CR 612's basic-land-type word swap, over an effect's AST. Cases on Effect
-- (Resolve's charter); delegates the inner Modification of ModifyTarget to
-- Projection.rewriteModification and every carried Filter to Filter.rewrite, so
-- no module touches another's constructors. DealDamage and ChangeText carry no
-- rewritable land-type word.
--
-- CR 612.1 rewrites "any words or symbols printed on that object", so a Filter
-- an effect carries is not exempt: Boil's "destroy all Islands" is a land-type
-- word inside an ObjectRef.EachMatching. Every arm holding one goes through
-- Filter.rewrite or rewriteObjectRef rather than being special-cased here.
-- CR 612.1 through an ObjectRef. InSlot names an object CHOSEN at cast time, not
-- a word on the card, so there is nothing in it to rewrite; EachMatching's
-- Filter is card text like any other. Lives here beside objectRefObjects rather
-- than in a module of its own, which the type does not have.
rewriteObjectRef :: [(Subtype, Subtype)] -> ObjectRef -> ObjectRef
rewriteObjectRef pairs ref = case ref of
  ObjectRef.InSlot _ -> ref
  ObjectRef.EachMatching f -> ObjectRef.EachMatching (Filter.rewrite pairs f)

rewriteEffect :: [(Subtype, Subtype)] -> Effect Card.Type.Card -> Effect Card.Type.Card
rewriteEffect pairs effect = case effect of
  Effect.ModifyTarget duration modification slot ->
    Effect.ModifyTarget duration (Projection.rewriteModification pairs modification) slot
  Effect.DealDamage _ _ -> effect
  Effect.ChangeText _ -> effect
  Effect.AddMana _ -> effect
  Effect.Search filter_ destination -> Effect.Search (Filter.rewrite pairs filter_) destination
  Effect.ExileAllGraveyards -> effect
  Effect.Proliferate -> effect
  Effect.ExileHandThenDraw -> effect
  Effect.PlayerSacrifices slot filter_ quantity -> Effect.PlayerSacrifices slot (Filter.rewrite pairs filter_) quantity
  Effect.RestartGame -> effect
  Effect.ControlPlayerNextTurn _ -> effect
  Effect.Destroy ref regenerability -> Effect.Destroy (rewriteObjectRef pairs ref) regenerability
  Effect.Sacrifice _ -> effect
  Effect.RemoveFromCombat _ -> effect
  Effect.MoveToZone {} -> effect
  Effect.Draw {} -> effect
  Effect.Mill {} -> effect
  Effect.Discard {} -> effect
  -- No rewritable land-type word.
  Effect.LoseLife {} -> effect
  -- No rewritable land-type word.
  Effect.GainLife {} -> effect
  -- A text-changer does not reach a token's embedded card here (spec section 8).
  Effect.Create {} -> effect
  Effect.Replace {} -> effect
  -- A Phase carries no basic-land-type word for CR 612 to rewrite.
  Effect.SkipNextPhase {} -> effect
  -- No rewritable land-type word.
  Effect.Counter _ -> effect
  Effect.PutCounters {} -> effect
  -- No rewritable land-type word.
  Effect.GainPlayerCounters {} -> effect
  Effect.Untap ref -> Effect.Untap (rewriteObjectRef pairs ref)
  -- CR 500.8's added phases carry no basic-land-type word for CR 612 to rewrite.
  Effect.AddPhases _ -> effect
  Effect.GainControl _ _ -> effect
  Effect.ArmDelayedTrigger _ _ -> effect
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
  Effect.AttachTarget slot filter_ -> Effect.AttachTarget slot (Filter.rewrite pairs filter_)
  -- No rewritable land-type word.
  Effect.PlaySubgame _ -> effect
  -- CR 500.7's added turns carry no basic-land-type word for CR 612 to rewrite.
  Effect.TakeExtraTurn _ -> effect

-- A resolving spell's PROJECTED modes: ONLY its chosen ones (CR 608.2c/700.2 --
-- an unchosen mode's effects never resolve), with every text-change affecting it
-- applied (CR 612) to each effect. This is read-point 3 of the rewritable AST --
-- the resolver honors a spell hacked on the stack. A non-modal card has one
-- mode, always chosen, so this is unchanged for it.
--
-- Modes rather than a flat effect list because CR 603.5's "may" is a property of
-- the mode, and resolveSpellWith has to ask about it once per mode. A text
-- change rewrites EFFECTS only -- CR 612's word swap has nothing to say about
-- whether an instruction is optional -- so the optionality passes through.
modesOf :: ObjectId -> GameState -> [(ModeIndex, Mode.Mode Card.Type.Card)]
modesOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Game.cardOf oid gs of
    Nothing -> []
    Just card ->
      let chosen = Binding.modesOf (Object.bindings obj)
          rewrite = rewriteEffect (Projection.textChangesAffecting oid gs)
          rewriteMode m = m {Mode.effects = fmap rewrite (Mode.effects m)}
       in fmap (fmap rewriteMode) (Card.chosenModes chosen card)

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
-- CR 405.4: who controls a SPELL on the stack -- "a spell's controller is the
-- player who cast it", fixed at cast time -- for both CR 608.2b's legality
-- perspective and the effects' own execution.
--
-- One function because those two must name the same player, not because they
-- currently disagree: they do not. controllerOfGiven answers Nothing only for an
-- object that does not exist, and both callers have already matched `Just obj`
-- from a lookup, so the fallback below is unreachable at either. What was wrong
-- was having the same question spelled two ways -- a bare Projection.controllerOf
-- for legality and this expression for execution -- which is a divergence waiting
-- to be introduced rather than one already there.
--
-- The projection read is itself a no-op in this pool: nothing installs a
-- SetController naming a stack object, so it always folds back to the owner. #83
-- argues it should trust Object.owner outright; whichever way that lands, it now
-- lands in one place instead of three.
spellController :: Object.Object -> ObjectId -> GameState -> PlayerId
spellController obj oid gs = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)

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
            -- CR 608.2b's perspective is the SPELL's controller (CR 405.4), read
            -- through the same function resolveSpellWith uses for effect
            -- execution so the two cannot drift apart.
            Just spec -> Target.stillLegal (Just (spellController obj oid gs)) oid recipient spec gs
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
              Just spec -> Target.stillLegal (Just (spellController obj oid gs)) oid recipient spec gs
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
                let effectController = spellController obj oid gs
                Monad.forM_ (modesOf oid gs) $ \(idx, mode) -> do
                  -- CR 603.5 / 608.2d: a printed "may" is answered HERE, between
                  -- the preceding mode's instructions and this one's -- the
                  -- announcement CR 608.2d places "while applying the effect".
                  taken <- exercises oid effectController idx mode
                  let applyOne eff = do
                        -- Re-read the live bindings for THIS effect: a prior PlaySubgame
                        -- may have bound its loser slot. Target legality is recomputed
                        -- with the same pre-fold `legalSlot` (targets unchanged; the new
                        -- reserved slot is vacuously legal).
                        bindingsNow <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject oid)
                        let chosenNow = Binding.targetsOf bindingsNow
                            legalityNow = Map.mapWithKey legalSlot chosenNow
                        applyEffectWith runSubgame oid effectController (Binding.subtypesOf bindingsNow) legalityNow chosenNow eff
                  Monad.when taken (Monad.forM_ (Mode.effects mode) applyOne)
                Event.changeZone oid Zone.Graveyard

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame

-- CR 608.2: the executor shared by an activated ability (M3e) and a triggered
-- ability (M3f) on the stack. Re-validate filled slots (CR 608.2b), then walk
-- the CHOSEN MODES in order (CR 608.2c/700.2c) applying each one's effects with
-- `srcId` (the source permanent) as the effect source (CR 113.7), asking about
-- any printed "may" as it goes (CR 603.5); then the ability ceases (CR 608.2n).
-- `stackId` is the ability object's own id.
--
-- Takes the modes rather than a flat effect list plus a separate spec map: the
-- specs ARE the union of those modes' own (CR 700.2c), so deriving them here is
-- one fewer pair of arguments that could disagree.
resolveModes :: ObjectId -> ObjectId -> [(ModeIndex, Mode.Mode Card.Type.Card)] -> Game ()
resolveModes stackId srcId modes = do
  gs <- State.get
  case Game.lookupObject stackId gs of
    Nothing -> pure ()
    Just obj ->
      let specs = Map.unions (fmap (Mode.targetSpecs . snd) modes)
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipient = case Map.lookup slot specs of
            -- CR 608.2b is about TARGETS. A slot with no target spec is a
            -- RESERVED binding -- the trigger's source (Pawl.Binding.triggerSource),
            -- a token this resolution minted -- and was never targeted, so it can
            -- never have become an illegal target.
            Nothing -> True
            -- CR 608.2b: the perspective is the ABILITY's controller -- literally
            -- the `effectController` bound below, whose own comment explains why
            -- that is Object.owner and not a live projection (CR 113.8: fixed at
            -- the ability's creation, and a stolen permanent's later controller
            -- must not override it). Reading the projection here instead would
            -- have contradicted that rule three lines away.
            --
            -- `srcId` stays the source (CR 113.7) and may well be gone: that is
            -- exactly the case this rule is about, and why the perspective is not
            -- read from it.
            Just spec -> Target.stillLegal (Just effectController) srcId recipient spec gs
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
          resolveOne (idx, mode) = do
            -- CR 603.5 / 608.2d: the printed "may", answered as this mode's
            -- instructions are applied. Run only when `fizzles` is False, which
            -- is what keeps the question from being asked about an ability whose
            -- every target is already gone (CR 608.2b) -- it never resolves, so
            -- there is nothing for the answer to decide.
            taken <- exercises stackId effectController idx mode
            Monad.when taken (Monad.mapM_ (applyEffect srcId effectController (Binding.subtypesOf (Object.bindings obj)) legality chosen) (Mode.effects mode))
       in do
            Monad.unless fizzles (Monad.forM_ modes resolveOne)
            State.modify' (cease stackId)

-- CR 603.5 / 608.2d: does this mode's instruction list happen at all? A
-- mandatory mode always does. An OPTIONAL one -- a printed "may" -- is its
-- controller's call, and it is made HERE, as the effect is applied, never by the
-- engine and never earlier: CR 603.5 says an optional ability goes on the stack
-- "regardless of whether their controller intends to exercise the ability's
-- option or not. The choice is made when the ability resolves."
--
-- `resolving` is the object on the stack -- the spell, or the ability object --
-- which is what the prompt names. `controller` is who "you" means (CR 405.4 for
-- a spell, CR 113.8 for an ability) and therefore who is asked; the ask goes
-- through Decide.deciderFor, so a player controlled under CR 723.1 has their
-- controller answer, exactly like every other choice.
exercises :: ObjectId -> PlayerId -> ModeIndex -> Mode.Mode Card.Type.Card -> Game Bool
exercises resolving controller idx mode = case Mode.optionality mode of
  Optionality.Mandatory -> pure True
  Optionality.Optional -> do
    gs <- State.get
    let decider = Decide.deciderFor controller gs
    decision <- Trans.lift (Program.prompt (Prompt.ChooseOptional decider controller resolving idx))
    pure $ case decision of
      OptionalDecision.Exercises -> True
      OptionalDecision.Declines -> False

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (srcId), not the ability object -- so DealDamage comes from Prodigal Sorcerer
-- (CR 113.7a). CR 700.2c/M4g: reads only the ability's CHOSEN modes (stamped at
-- activation, Activate.activateAbility) via Modal.chosenModes, the same
-- mode-scoping resolveSpell already applies to a modal spell. Reuses applyEffect
-- with the same per-slot legality and CR 608.2b fizzle as a spell.
-- CR 608.2n: the ability then ceases to exist -- removed from the stack and
-- objects, NOT buried (an ability is not a card).
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
       in resolveModes abilId srcId (Modal.chosenModes chosen (ActivatedAbility.modal ability))

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
--
-- That last parenthesis is why every arm below that evaluates a Quantity binds
-- its view as `Projection.viewWithLastKnown source gs` and not
-- `Projection.fullView gs`. CR 608.2h: "if the effect requires information from a
-- specific object, INCLUDING THE SOURCE OF THE ABILITY ITSELF, the effect uses
-- the current information of that object if it's in the public zone it was
-- expected to be in; if it's no longer in that zone … the effect uses the
-- object's last known information." Uniform across the arms rather than
-- special-cased on the one opcode a card exercises today: the rule is about the
-- source, not about which effect is asking, and for a source that still exists
-- the two views are equal by construction.
-- The subgame-runner-aware executor. `runSubgame` is the injected Game Result
-- that PLAYS a nested game (Engine.playSubgame); the bare applyEffect below
-- passes noSubgame. Only the PlaySubgame arm consults it.
-- CR 701.3a/701.3b: may `src` legally be attached to `target` right now?
--
-- CR 701.3a's last sentence is the whole rule: "An Aura, Equipment, or
-- Fortification can't be attached to an object or player it couldn't enchant,
-- equip, or fortify, respectively." So this dispatches on which of those `src`
-- is, read through the PROJECTION, so an Equipment that lost the subtype (CR
-- 301.5c's second sentence) and a permanent animated into a creature both answer
-- correctly.
--
-- Equipment: CR 301.5, "An Equipment can be attached to a creature. It can't
-- legally be attached to anything that isn't a creature."
--
-- Aura: CR 303.4, "what an Aura can be attached to is defined by its enchant
-- keyword ability" -- so this asks the Aura's own enchant spec the question CR
-- 608.2b asks of a target, through Target.legalRecipients rather than a
-- hand-rolled creature test, which is what makes an enchant spec that narrows
-- further (a colour, a controller) honoured here for free. CR 109.5's "you" on
-- that spec is the AURA's controller, not the moving effect's -- proven by
-- Pawl.AuraSpec's "CR 303.4j whole cards", where Crown of the Ages cannot move
-- Setessan Training ("Enchant creature you control") onto an opponent's
-- creature. An Aura with no enchant ability answers False and cannot arise: the
-- Pawl.CardSpec lint family holds the Aura-iff-enchant biconditional in both
-- directions.
--
-- The Aura branch's first conjunct is CR 303.4d's "An Aura that's also a creature
-- can't enchant anything" -- the RESTRICTION half of that rule, whose
-- state-based half is Pawl.Sba.cannotBeAttached. Unreachable in this pool (such
-- an Aura is detached by CR 704.5p and buried by CR 704.5m before any player
-- could target it), written anyway because it costs one comparison. The Equipment
-- branch has no counterpart: CR 301.5c's matching restriction carries a
-- reconfigure exception (CR 702.151b) that nothing here can express, and the
-- Equipment path is not this change's to alter (#193).
--
-- The `src /= target` conjunct is CR 301.5c ("An Equipment can't equip itself")
-- and CR 303.4d ("An Aura can't enchant itself") at once.
--
-- False for a source that is neither, which is CR 701.3b's third sentence: "If
-- an effect tries to attach an object that isn't an Aura, Equipment, or
-- Fortification to another object or player, the effect does nothing and the
-- first object doesn't move." There is no Subtype.Fortification to case on.
attachLegal :: ObjectId -> ObjectId -> GameState -> Bool
attachLegal src target gs
  | src == target = False
  | Set.member Subtype.Equipment subtypes = Projection.isCreatureOf target gs
  | Set.member Subtype.Aura subtypes =
      not (Projection.isCreatureOf src gs)
        && case Game.cardOf src gs >>= Card.Type.enchant of
          Nothing -> False
          Just spec ->
            any
              (\r -> recipientObject r == Just target)
              (Target.legalRecipients (Projection.controllerOf src gs) src spec gs)
  | otherwise = False
  where
    subtypes = Projection.subtypesOf src gs

-- The players a PlayerRef names DURING a resolution, read from the slots this
-- resolution filled rather than from the source's bindings (which is what
-- Count.playersFor reads, for a static count). Shared by applyEffectWith's
-- GainPlayerCounters and Draw arms.
--
-- A slot's legality is asked the way every other slot read asks it (CR 608.2b):
-- a slot filled by targeting that has since become illegal names nobody, and a
-- RESERVED slot -- the trigger's "that player" -- has no target spec, so
-- legalSlot already answered True for it.
--
-- CR 102.1: "A player is one of the people in the game." A player who has left
-- keeps their row in GameState.players -- Player.status turns Departed, the key
-- stays -- so `everyone` is Game.stillPlaying rather than the map's keys, and
-- CR 800.4a's departure takes a seat out of both enumerating arms.
--
-- Only those two arms, because only they read the roster. `Relative You` and
-- `InSlot` name one specific player who arrived from elsewhere -- the
-- resolution's controller, a recipient the caster chose -- so there is no
-- roster here for CR 102.1 to filter. Whether a departed player can still BE
-- one of those is a different question under different clauses: CR 800.4d keeps
-- their triggered ability off the stack to begin with, and CR 800.4i governs an
-- effect that needs information about a specific player who has left. The
-- unbuilt parts of those clauses are #181, not this.
--
-- `everyone` is in the players map's PlayerId order rather than turn order, and
-- that is deliberate: an unordered SET of players is what a PlayerRef names, and
-- a caller with an ordering rule to obey imposes it on this answer -- the Draw
-- arm does, for CR 121.2c.
playerRefPlayers :: Map.Map SlotName Recipient -> Map.Map SlotName Bool -> PlayerId -> GameState -> PlayerRef -> [PlayerId]
playerRefPlayers chosen legality controller gs ref = case ref of
  PlayerRef.InSlot slot -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
    (Just (Recipient.ToPlayer pid), True) -> [pid]
    _ -> [] -- an unfilled, illegal, or non-player slot: no-op
  PlayerRef.Relative PlayerRelation.You -> [controller]
  PlayerRef.Relative PlayerRelation.Opponent -> filter (/= controller) everyone
  PlayerRef.EachPlayer -> everyone
  where
    everyone = Game.stillPlaying gs

-- The objects an ObjectRef names DURING a resolution -- the object-side twin of
-- playerRefPlayers above, and the ONE place a filter-selected set is swept, so
-- every opcode that takes an ObjectRef gets the same answer.
--
-- InSlot asks legality the way every other slot read does (CR 608.2b): a slot
-- filled by targeting that has since become illegal names nobody, and a player
-- recipient names no object.
--
-- EachMatching folds the battlefield (CR 109.2: a description with a card type
-- and no zone "means a permanent of that card type ... on the battlefield")
-- against the projection, so a permanent that is a creature only because of a
-- layer-4 effect is in the set and one whose printed line says Creature but is
-- currently not is out. The filter context is this effect's own -- CR 109.5's
-- "you" is the ability's controller and IsSource is its source -- because the
-- filter IS the ability's card text. That is the footing AttachTarget's
-- destination filter is on, and not PlayerSacrifices', whose filter is read
-- against the victim instead.
--
-- WHEN: at the moment the caller runs, which is when this instruction is
-- reached, CR 608.2c ("follows its instructions in the order written"). The list
-- is then FIXED -- the caller iterates over this answer, so nothing a later
-- element's fate does can add to or remove from it. That is one half of CR
-- 608.2f's "each such action is processed simultaneously": WHICH objects the
-- instruction names.
--
-- The other half is not this function's to keep. Whether each named object is
-- actually AFFECTED has to be judged before any of them is, or "destroy all
-- creatures" degrades into destroying them one at a time with a fresh look in
-- between -- so a caller hands the whole list to its funnel as one batch rather
-- than calling it once per element. Event.destroy's haddock has that half.
--
-- ORDER: APNAP (CR 608.2f's "APNAP order is used to make the primary
-- determination of the order of those actions"), then ascending ObjectId within
-- a controller. That second key is the engine's, not the resolving controller's
-- as CR 608.2f's secondary sentence would have it (#379). The no-controller
-- fallback is unreachable: Projection.controllerOf answers Nothing only for an
-- object that does not exist, and every id here came out of the battlefield.
objectRefObjects :: Map.Map SlotName Bool -> Map.Map SlotName Recipient -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [ObjectId]
objectRefObjects legality chosen controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
    (Just recipient, True) -> Maybe.maybeToList (recipientObject recipient)
    _ -> []
  ObjectRef.EachMatching filter_ ->
    let context = Filter.MkContext (Just controller) (Just source)
        matching =
          filter
            (\oid -> Filter.matches context (Projection.viewOfObject oid gs) filter_)
            (Set.toList (GameState.battlefield gs))
        order = Game.apnapOrder gs
        last_ = length order
        seat oid = case Projection.controllerOf oid gs of
          Nothing -> last_
          Just pid -> Maybe.fromMaybe last_ (List.elemIndex pid order)
     in List.sortOn (\oid -> (seat oid, oid)) matching

applyEffectWith :: Game Result -> ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffectWith runSubgame source controller bound legality chosen effect = case effect of
  Effect.DealDamage slot quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
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
            Damage.applyDamage [Damage.damageEvent gs DamageKind.Noncombat source recipient (Integer.toNaturalSaturating n)]
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
  Effect.Search filter_ destination ->
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
          -- Where the card goes is the CARD's instruction, not rule 701.23's --
          -- that rule says only how to look, and CR 701.23e says the same of the
          -- reveal ("if the effect that contains the search instruction doesn't
          -- also contain instructions to reveal the found card(s), then they're
          -- not revealed"). Evolving Wilds puts it onto the battlefield tapped;
          -- CR 702.29e's typecycling reveals it and puts it into the hand.
          --
          -- The searcher is the revealer: CR 701.20a's "show that card to all
          -- players" is done by the player following the instruction, which for
          -- a search of "your library" is its controller.
          mapM_ (putFound controller destination) found
          -- CR 701.23: shuffle the (possibly reduced) library afterward.
          lib <- State.gets (Game.zoneMembers Zone.Library controller)
          shuffleAnswer <- Trans.lift (Program.prompt (Prompt.Shuffle lib))
          State.modify' (reorderLibrary controller (Game.honourShuffle lib shuffleAnswer))
  -- Rest in Peace's ETB: exile every card in every graveyard (CR 400.7 each move
  -- funnels through changeZone). A graveyard->exile move matches no M3f
  -- replacement or trigger, so no cascade.
  --
  -- "Every graveyard" is every player's, and CR 102.1 makes that the players
  -- still in the game -- Game.stillPlaying, not the keys of GameState.players,
  -- which keep a departed seat's row. Unobservable, and written anyway for the
  -- reason Count.playersFor gives: CR 800.4a took every object a departing
  -- player owned out of the game, so Game.zoneMembers finds nothing in their
  -- graveyard and the extra iteration moved nothing either way.
  Effect.ExileAllGraveyards -> do
    gs <- State.get
    let gyCards = concatMap (\pid -> Game.zoneMembers Zone.Graveyard pid gs) (Game.stillPlaying gs)
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
  -- Pawl.Types.RestartSignal.
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
  Effect.Destroy ref regenerability -> do
    gs <- State.get
    -- CR 701.8: destroy them through the single funnel -- indestructible (CR
    -- 702.12b) and regeneration (CR 701.19a) are Event.destroy's to decide. The
    -- card's own CR 701.19c rider rides along, because whether a shield may
    -- apply is a fact about THIS destruction (Terror's), not the victim.
    --
    -- The whole set goes to the funnel as ONE batch rather than one call per
    -- victim: CR 608.2f's "each such action is processed simultaneously" governs
    -- which permanents are named (objectRefObjects) and when each one's CR
    -- 702.12b gate is judged (Event.destroy) alike. An illegal slot (CR 608.2b),
    -- a non-object recipient, or a set that matched nothing all arrive here as
    -- the empty list and destroy nothing -- one path, not three.
    Event.destroy regenerability (objectRefObjects legality chosen controller source gs ref)
  Effect.Sacrifice slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient cannot be sacrificed
        -- CR 701.21: through the single funnel, which is NOT Event.destroy --
        -- CR 701.21a: sacrificing is not destroying. The sacrificing player is
        -- this effect's controller, which for the "this creature" shape this
        -- opcode serves is the permanent's own controller; the funnel's CR 701.21a
        -- guard turns any other case into a no-op rather than a wrong sacrifice.
        Just target -> Event.sacrifice controller target
      -- Illegal slot (CR 608.2b) or a non-object recipient: no-op.
      _ -> pure ()
  Effect.RemoveFromCombat slot ->
    State.modify' $ \gs ->
      case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case recipientObject recipient of
          Nothing -> gs -- a player recipient is not in combat
          -- CR 506.4: through Game.removeFromCombat, the one performer of every
          -- clause of that rule -- so this clause takes CR 509.1h's asymmetry
          -- with it for free, an attacker losing its whole entry while a blocker
          -- leaves only the set inside a surviving one. Argued in full there.
          --
          -- Unprompted and undirected: CR 506.4's second sentence says what
          -- removal does, and leaves nothing to ask or to choose.
          Just target -> Game.removeFromCombat target gs
        -- Illegal slot (CR 608.2b) or a non-object recipient: no-op. A target
        -- that has already left combat needs no guard either -- removing a
        -- creature that is not in the record is what Game.removeFromCombat
        -- already does to it, which is nothing.
        _ -> gs
  Effect.MoveToZone slot zone ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure ()
        -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative.
        Just target -> Event.changeZone target zone
      _ -> pure ()
  Effect.Draw ref quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        -- Whoever the PlayerRef names draws -- the controller for Divination's
        -- `Relative You`, the targeted player for Ancestral Recall's `InSlot`,
        -- each opponent for Master of the Feast's `Relative Opponent`, the whole
        -- table for Vision Skeins' `EachPlayer`.
        named = playerRefPlayers chosen legality controller gs ref
        -- CR 121.2c: "If more than one player is instructed to draw cards, the
        -- active player performs all of their draws first, then each other
        -- player in turn order does the same." The reorder lives here rather
        -- than in playerRefPlayers because CR 121.2c is a rule about DRAWING;
        -- that helper's other caller, GainPlayerCounters, has no ordering rule
        -- to obey. Observable, not cosmetic: each draw records the drawn card's
        -- zone change in the one turn-scoped log that triggers scan (CR 603.2),
        -- so the order the draws happen in is the order the log reports.
        -- CR 121.2d (shared team turns) has no reader -- pawl has no teams
        -- (#175).
        --
        -- An intersection: apnapOrder supplies the ORDER, `named` the
        -- MEMBERSHIP, and a player in only one of the two does not draw. Both
        -- directions have a case, and the one that bites is a seat apnapOrder
        -- names and `named` does not. A departure does not shorten the seating
        -- roster -- CR 800.4k and CR 800.4m both speak of the turn a departed
        -- player "would have begun", so their seat stays in the order -- while
        -- playerRefPlayers stopped naming them at CR 102.1. Drawing for one was
        -- never a no-op: CR 800.4a took their library out of the game with them,
        -- so Event.drawCard would record them in GameState.drewFromEmpty,
        -- writing engine state for someone who is not in the game.
        drawers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 ->
            -- CR 121.2: draw n one at a time, folding the shared primitive so each
            -- draw re-reads the library top and the CR 104.3c empty-library loss is
            -- preserved.
            Monad.forM_ drawers $ \pid ->
              Monad.replicateM_ (Integer.toIntSaturating n) (Event.drawCard pid)
      _ -> pure ()
  Effect.Mill slot quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer target), True) ->
        case Quantity.evaluate viewOf context gs source quantity of
          Just n
            | n > 0 ->
                -- CR 701.17/701.17b: top min(n, library) of the target's library to
                -- their graveyard, funnelled so each move mints a new incarnation.
                let topN = List.genericTake n (Game.zoneMembers Zone.Library target gs)
                 in Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard) topN
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.Discard slot quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer target), True) ->
        case Quantity.evaluate viewOf context gs source quantity of
          Just n
            | n > 0 -> do
                let held = Game.zoneMembers Zone.Hand target gs
                    bury :: [ObjectId] -> Game ()
                    bury = Monad.mapM_ (\c -> Event.changeZone c Zone.Graveyard)
                    -- The quantity as the count it is. `n > 0` above, so the
                    -- clamp never decides anything here.
                    count = Integer.toNaturalSaturating n
                if count >= Natural.length held
                  -- CR 609.3: discarding the whole hand is "as much as possible," so
                  -- it is forced -- no choice, so no prompt.
                  then bury held
                  else do
                    -- CR 701.9b: the discarding player chooses which cards.
                    let decider = Decide.deciderFor target gs
                    choices <- Trans.lift (Program.prompt (Prompt.ChooseDiscard decider target held count))
                    -- FILTERED AND COMPLETED, the posture PlayerSacrifices takes
                    -- below and for the same reason. Dropping the invalid picks is
                    -- not enough: this branch is reached only when the hand is
                    -- LARGER than the count, so CR 609.3's "as much as possible"
                    -- is not doing any work here and every card the answer omits
                    -- is one the player could have discarded. An interpreter
                    -- answering with too few -- or with nothing -- would otherwise
                    -- discard fewer cards than the effect demands and cheat a Mind
                    -- Rot. Reject-not-repair is the COST path's option, available
                    -- there only because a cost may go unpaid; an effect has no
                    -- such out.
                    --
                    -- Deduplicated as well as filtered, which PlayerSacrifices
                    -- gets for free from its Set-shaped answer: ChooseDiscard is
                    -- answered with a LIST, so a card named twice would otherwise
                    -- fill two of the n slots and discard one card too few.
                    --
                    -- `held` is longer than n and `valid <> filler` is a
                    -- permutation of it, so the take always yields exactly n.
                    let valid = List.nub (filter (\c -> elem c held) choices)
                        filler = filter (\c -> List.notElem c valid) held
                    bury (List.genericTake count (valid <> filler))
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.LoseLife ref quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        -- Whoever the PlayerRef names loses the life -- the targeted player for
        -- Sign in Blood's `InSlot`, the controller for a `Relative You`
        -- drawback. Unordered, on the footing GainPlayerCounters is on rather
        -- than Draw's: there is no CR 121.2c for life, and CR 704.3 checks
        -- state-based actions only as a player would get priority, so no life
        -- total is observable between one adjustment and the next.
        losers = playerRefPlayers chosen legality controller gs ref
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 ->
            -- CR 119.3: the life total is simply adjusted, directly on the
            -- player record. Not through Pawl.Damage: CR 119.2 makes damage a
            -- CAUSE of life loss, not a synonym for it (the opcode's own
            -- comment has the consequences). A direct subtraction is what
            -- CostComponent.PayLife does for CR 119.4's cost side, and the CR
            -- 704.5a state-based action that may follow is the existing one in
            -- Pawl.Sba.
            Monad.forM_ losers $ \pid ->
              State.modify'
                ( \g ->
                    g
                      { GameState.players =
                          Map.adjust
                            (\p -> p {Player.life = Player.life p - n})
                            pid
                            (GameState.players g)
                      }
                )
      _ -> pure ()
  -- CR 119.3's other half, LoseLife's mirror in every respect but the sign. The
  -- comments above apply verbatim: same `viewWithLastKnown` reading, same
  -- unordered adjustment, same direct write to the player record. The one
  -- difference is that nothing in CR 704.5 follows a gain -- CR 704.5a fires on
  -- "0 or less life", which a gain cannot reach.
  Effect.GainLife ref quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        gainers = playerRefPlayers chosen legality controller gs ref
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 ->
            Monad.forM_ gainers $ \pid ->
              State.modify'
                ( \g ->
                    g
                      { GameState.players =
                          Map.adjust
                            (\p -> p {Player.life = Player.life p + n})
                            pid
                            (GameState.players g)
                      }
                )
      _ -> pure ()
  -- CR 701.21a: the slot's target player sacrifices `quantity` permanents
  -- matching the filter, and THAT PLAYER chooses which -- the whole difference
  -- between this and Sacrifice above.
  --
  -- CR 609.3: with no more candidates than the count, every one of them goes and
  -- there is nothing to ask; with none, nothing happens. Only a genuine surplus
  -- raises the prompt, which is the same shape Cost's Sacrifice component takes.
  Effect.PlayerSacrifices slot filter_ quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just (Recipient.ToPlayer victim), True) ->
        case Quantity.evaluate viewOf context gs source quantity of
          Just n
            | n > 0 -> do
                -- Candidates are what the VICTIM controls, ascending, so both the
                -- elision and a short transcript are deterministic. No perspective
                -- on the filter context: an edict's filter names a quality, never
                -- a player.
                let candidates =
                      List.sort
                        ( filter
                            (\oid -> Filter.matches (Filter.MkContext Nothing Nothing) (Projection.viewOfObject oid gs) filter_)
                            (Projection.controls victim gs)
                        )
                    decider = Decide.deciderFor victim gs
                    -- The quantity as the count it is. `n > 0` above, so the
                    -- clamp never decides anything here.
                    count = Integer.toNaturalSaturating n
                picked <-
                  if Natural.length candidates <= count
                    then pure (Set.fromList candidates)
                    else Trans.lift (Program.prompt (Prompt.ChooseSacrifices decider victim source candidates count))
                -- FILTERED AND COMPLETED, not merely filtered. Dropping the
                -- invalid picks is not enough: Diabolic Edict is not "may", so an
                -- interpreter answering with too few -- or with nothing -- would
                -- otherwise sacrifice fewer permanents than the effect demands and
                -- cheat the edict. CR 609.3 caps this at "as much as possible",
                -- which is every candidate, not however many the answer named.
                --
                -- So the valid picks are honoured first and the rest is made up
                -- deterministically from the remaining candidates, in the order
                -- they were offered. That differs from the cost path's
                -- reject-not-repair on purpose: a cost may simply go unpaid, and
                -- an effect has no such out.
                let wanted = min count (Natural.length candidates)
                    valid = filter (\oid -> Set.member oid picked) candidates
                    filler = filter (\oid -> List.notElem oid valid) candidates
                Monad.mapM_ (Event.sacrifice victim) (List.genericTake wanted (valid <> filler))
          _ -> pure ()
      -- Not a player recipient or an illegal slot (CR 608.2b): no-op.
      _ -> pure ()
  Effect.Create quantity card entry mSlot -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
    case Quantity.evaluate viewOf context gs source quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2), through the single funnel -- so CR
            -- 614's token replacements (Doubling Season) get their opportunity.
            -- CR 110.5b: the funnel is handed the entry's tap state, so a token
            -- the effect says is tapped is never untapped for an instant.
            minted <- Event.createTokens controller card (Integer.toNaturalSaturating n) (TokenEntry.tapped entry)
            -- CR 508.4: "if a creature is put onto the battlefield attacking, its
            -- controller chooses which defending player ... it's attacking". The
            -- rules for that live in Pawl.Combat, which is also what keeps this
            -- from looking like a declaration -- CR 508.3a's attack triggers see
            -- nothing (Pawl.Combat.putOntoBattlefieldAttacking's own comment).
            --
            -- After the entry loops rather than inside them: CR 614.16's token
            -- replacement settles the COUNT first, so this joins the tokens that
            -- actually entered, however many that turned out to be.
            Monad.when (TokenEntry.attacking entry) (Monad.mapM_ Combat.putOntoBattlefieldAttacking minted)
            case (mSlot, minted) of
              (Nothing, _) -> pure ()
              -- Unreachable: createTokens places every token onto the battlefield
              -- (CR 111.2). Total rather than partial: nothing bound matches "the
              -- token was never named" instead of crashing.
              (Just _, []) -> pure ()
              -- CR 603.7c: bind the minted token into live Object.bindings so a
              -- delayed ability THIS SAME resolution arms can name it. One token
              -- is the whole candidate set, so there is nothing to ask -- and
              -- where the rules leave nothing to ask, don't prompt. The
              -- Pawl.CardSpec lint keeps the PRINTED quantity at 1 (#53), which is
              -- why this is the ordinary case.
              (Just slot, [only]) -> State.modify' (bindSlot source slot only)
              -- CR 614.16 got there first: a token replacement (Doubling Season)
              -- multiplied the count at RESOLUTION, after the lint had passed, so
              -- several tokens now stand where CR 603.7c's "it" names one
              -- PARTICULAR object. CR 707.10e is the codified analogue -- when a
              -- replacement makes a copy target more than one object, "the copy's
              -- controller chooses one of them" -- so this asks rather than
              -- picking the first, which would be the engine choosing.
              --
              -- FILTERED, NOT TRUSTED, the same posture Sba.chooseLegendVictims
              -- takes: an answer naming something that was not minted falls back
              -- to the first, since the slot must end up bound either way.
              (Just slot, first : second : rest) -> do
                gs1 <- State.get
                let candidates = first NonEmpty.:| (second : rest)
                    decider = Decide.deciderFor controller gs1
                answer <- Trans.lift (Program.prompt (Prompt.ChooseBoundToken decider controller source candidates))
                let named = if List.elem answer (NonEmpty.toList candidates) then answer else first
                State.modify' (bindSlot source slot named)
      _ -> pure ()
  Effect.ArmDelayedTrigger name duration -> do
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
                  DelayedTrigger.bindings = captured,
                  -- CR 603.7b's stated duration, armed the way a continuous
                  -- effect's is. The two Maybes meet here and mean different
                  -- things: the OUTER one is the card printing no duration at
                  -- all, and the inner one is Expiry.arm reporting that a
                  -- printed duration never STARTED (CR 611.2b's "if the 'for as
                  -- long as' duration never starts, the effect does nothing").
                  -- Both collapse to Nothing, and both should: an ability whose
                  -- stated duration never began has no duration to outlive its
                  -- first firing, so CR 603.7b's default is exactly right for it.
                  DelayedTrigger.expiry = duration >>= \d -> Expiry.arm controller source d gs
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
  Effect.SkipNextPhase ref phase -> do
    -- CR 614.1b: "effects that use the word 'skip' are replacement effects", so
    -- this installs one -- floating, because a sorcery's skip outlives the
    -- sorcery (CR 614.3: floating replacements "last until they're used up").
    --
    -- CR 614.10a: one instance PER NAMED PLAYER, each with Uses.Once, and each
    -- PREPENDED as its own row rather than merged into any it finds. That is
    -- where "if two effects each cause a player to skip their next occurrence,
    -- that player must skip the next two" comes from: two Fatigues on one player
    -- are two rows with two timestamps, and Pawl.Replacement.consume spends
    -- exactly the one it applied.
    --
    -- Expiry.Never, and no Duration on the opcode to derive anything else from:
    -- Fatigue states no duration, so CR 614.3's other terminator ("or their
    -- duration has expired") never fires and the skip waits however many turns it
    -- must. That is CR 614.10a's own answer for a "next" that has not come round
    -- yet -- "the other will remain until another occurrence can be skipped".
    --
    -- CR 113.7: the SOURCE is this effect's source, as Replace's is.
    gs <- State.get
    let named = playerRefPlayers chosen legality controller gs ref
        install pid g =
          let (ts, g1) = Game.freshTimestamp g
              active =
                ActiveReplacement.MkActiveReplacement
                  { ActiveReplacement.effect =
                      ReplacementEffect.PhaseR
                        PhasePattern.MkPhasePattern
                          { PhasePattern.whichPhase = phase,
                            -- The player the resolution named, baked now. Card
                            -- data cannot name one (see
                            -- Pawl.Types.PhasePattern).
                            PhasePattern.whosePhase = Just pid
                          },
                    ActiveReplacement.source = source,
                    ActiveReplacement.timestamp = ts,
                    ActiveReplacement.expiry = Expiry.Type.Never,
                    ActiveReplacement.uses = Uses.Once
                  }
           in g1 {GameState.replacements = active : GameState.replacements g1}
    State.modify' (\g -> List.foldl' (flip install) g named)
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
  -- elsewhere. The SOURCE moves here; AttachTarget below is the sibling that
  -- moves the slot's TARGET instead.
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
  -- CR 701.3a, in the other direction from Attach above: the SLOT's target is
  -- what moves, and the destination is chosen now rather than targeted. Crown of
  -- the Ages' "Attach target Aura attached to a creature to another creature",
  -- whose ruling says in as many words that "this only targets the Aura and not
  -- either creature".
  Effect.AttachTarget slot filter_ ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      -- An unfilled slot, or one CR 608.2b has since made illegal: no-op.
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient: nothing on the battlefield moves
        Just subject -> do
          gs <- State.get
          let host = Game.lookupObject subject gs >>= Object.attachedTo
              -- The destinations the card's own TEXT admits: battlefield
              -- permanents matching the Filter, less the one the subject already
              -- holds. That exclusion is CR 701.3b's second sentence -- attaching
              -- a permanent to what it is already attached to "does nothing" --
              -- and it is also how Crown of the Ages' "ANOTHER creature" is
              -- spelled, so a card omitting the word would behave identically.
              --
              -- Deliberately NOT narrowed to destinations the move would be
              -- LEGAL for. "Another creature" is the whole of what the card says;
              -- pre-filtering past it would answer CR 303.4j's question on the
              -- player's behalf, and CR 303.4j exists precisely because the
              -- choice can land somewhere the subject may not go.
              --
              -- Ascending, so both the elision below and a transcript are
              -- deterministic. The filter context is this effect's own -- CR
              -- 109.5's "you" is the ability's controller and IsSource is its
              -- source -- because the destination filter IS the ability's card
              -- text, unlike PlayerSacrifices', which is read against the victim.
              candidates =
                List.sort
                  ( filter
                      (\oid -> Just oid /= host && Filter.matches (Filter.MkContext (Just controller) (Just source)) (Projection.viewOfObject oid gs) filter_)
                      (Set.toList (GameState.battlefield gs))
                  )
          case candidates of
            -- CR 609.3: the effect does as much as it can, and with no
            -- destination the text admits that is nothing.
            [] -> pure ()
            first : rest -> do
              destination <- case rest of
                -- One destination is no choice at all, and the effect is not a
                -- "may" -- where the rules leave nothing to ask, don't prompt.
                -- The elision is not re-derived for an optional attach (#359).
                [] -> pure first
                second : more -> do
                  let offered = first NonEmpty.:| (second : more)
                  -- FILTERED, NOT TRUSTED, the ChooseBoundToken posture: an
                  -- answer naming something that was never offered falls back to
                  -- the first candidate, since the effect is mandatory and must
                  -- pick something.
                  answer <- Trans.lift (Program.prompt (Prompt.ChooseAttachment (Decide.deciderFor controller gs) controller subject offered))
                  pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
              gs1 <- State.get
              -- CR 303.4j, for an Aura -- "the Aura doesn't move" -- and CR
              -- 701.3b's first sentence for the rest. A FAILURE MODE, not a
              -- fizzle: the ability resolved, and the only thing that did not
              -- happen is the move. In particular the subject stays attached to
              -- its old host rather than becoming unattached, so CR 704.5m has
              -- nothing to bury.
              Monad.when (attachLegal subject destination gs1) $ do
                -- CR 701.3c: attaching to a DIFFERENT object gives it a new
                -- timestamp, which CR 613.7 orders layer effects by. Always a
                -- different object here -- the current host was never offered.
                let (ts, gs2) = Game.freshTimestamp gs1
                    move o = o {Object.attachedTo = Just destination, Object.timestamp = ts}
                State.put gs2 {GameState.objects = Map.adjust move subject (GameState.objects gs2)}
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
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case recipientObject recipient of
        Nothing -> pure () -- a player recipient takes no counters
        Just target -> case Quantity.evaluate viewOf context gs source quantity of
          Nothing -> pure () -- unevaluable quantity: no-op (the powerOf posture)
          -- CR 122.6: through the single funnel, so CR 614's counter replacements
          -- (Hardened Scales, Doubling Season) get their opportunity.
          Just n -> Monad.when (n > 0) (Event.putCounters target kind (Integer.toNaturalSaturating n))
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
      -- CR 701.34a: "choose any number of permanents and/or players that have a
      -- counter, then give each one additional counter of each kind that permanent or
      -- player already has."
      --
      -- Every clause of that sentence is load-bearing. "That have a counter" is the
      -- candidate filter, so proliferate never starts anything on its first counter.
      -- "Any number" means the chosen set may be empty, which is why a lone candidate
      -- is still asked about. "Each kind ... already has" means one more of every kind
      -- present, not a doubling and not a kind of the chooser's choosing.
      --
      -- Both the candidates and their kinds are read from the state BEFORE the prompt
      -- and before any counter lands. That keeps the answer to "which kinds does this
      -- have" fixed for the whole action (CR 608.2h's posture), so a CR 614
      -- replacement that scales one placement cannot feed back and widen the set of
      -- kinds still being walked.
      --
      -- Targetless: nothing was targeted, so unlike every slot-reading opcode here
      -- there is no CR 608.2b legality to re-check.
      --
      -- The "players" of CR 701.34a are CR 102.1's -- the people IN the game --
      -- so the roster is Game.stillPlaying, not the keys of GameState.players,
      -- which keep a departed seat's row. This is the observable one, and the
      -- reason #279 was worth closing rather than deferring again: a departed
      -- player's counters stay on their record, because CR 800.4a removes their
      -- OBJECTS and CR 109.1's list of what an object is has no room for a
      -- counter on a player. Their poison is still there for kindsFor to find,
      -- so the map's keys would offer someone who is not in the game as a
      -- choice, and honouring that answer puts a fresh counter on a non-player.
  Effect.Proliferate -> do
    gs <- State.get
    let everyone = Game.stillPlaying gs
        kindsOn oid = foldMap (Map.keys . Map.filter (> 0) . Object.counters) (Game.lookupObject oid gs)
        kindsFor pid = foldMap (Map.keys . Map.filter (> 0) . Player.counters) (Map.lookup pid (GameState.players gs))
        -- The battlefield is shared, and zoneMembers slices it by OWNER, so the
        -- union over every seat is every permanent in play -- not just this
        -- player's. CR 701.34a lets a proliferating player choose anyone's.
        onBattlefield = concatMap (\pid -> Game.zoneMembers Zone.Battlefield pid gs) everyone
        permanents = filter (not . null . kindsOn) onBattlefield
        players = filter (not . null . kindsFor) everyone
    Monad.unless (null permanents && null players) $ do
      (pickedPermanents, pickedPlayers) <-
        Trans.lift (Program.prompt (Prompt.ChooseProliferate (Decide.deciderFor controller gs) controller permanents players))
      -- FILTERED, NOT TRUSTED, the posture every other prompt reader takes: an
      -- answer naming something that was not offered is dropped rather than
      -- honoured, so a bogus id cannot mint a counter on a permanent that had
      -- none -- which is precisely what the candidate filter exists to prevent.
      let keptPermanents = filter (\oid -> Set.member oid pickedPermanents) permanents
          keptPlayers = filter (\pid -> Set.member pid pickedPlayers) players
      -- CR 122.6: object counters go through the single funnel, so CR 614's
      -- counter replacements (Hardened Scales, Doubling Season) apply to a
      -- proliferated counter exactly as they do to a placed one.
      Monad.forM_ keptPermanents $ \oid ->
        Monad.forM_ (kindsOn oid) $ \kind -> Event.putCounters oid kind 1
      -- Player counters are added directly, with no CR 614 opportunity, matching
      -- GainPlayerCounters below and gapped for the same reason (#122).
      Monad.forM_ keptPlayers $ \pid ->
        Monad.forM_ (kindsFor pid) $ \kind ->
          State.modify'
            ( \g ->
                g
                  { GameState.players =
                      Map.adjust
                        (\pl -> pl {Player.counters = Map.insertWith (+) kind 1 (Player.counters pl)})
                        pid
                        (GameState.players g)
                  }
            )
  Effect.GainPlayerCounters ref kind quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        recipients = playerRefPlayers chosen legality controller gs ref
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
                            (\p -> p {Player.counters = Map.insertWith (+) kind (Integer.toNaturalSaturating n) (Player.counters p)})
                            pid
                            (GameState.players g)
                      }
                )
      _ -> pure ()
  Effect.Untap ref ->
    State.modify' $ \gs ->
      -- CR 701.26b: rotate each named permanent back to the upright position.
      -- The victims are enumerated ONCE, for the CR 608.2f simultaneity
      -- objectRefObjects buys; an illegal slot (CR 608.2b), a player recipient
      -- and a set that matched nothing all arrive as the empty list and untap
      -- nothing, so there is one path rather than three.
      let untap o = o {Object.tapped = TapState.Untapped}
       in gs
            { GameState.objects =
                foldr (Map.adjust untap) (GameState.objects gs) (objectRefObjects legality chosen controller source gs ref)
            }
  -- CR 500.8: add the phases, directly after the phase this is resolving in.
  --
  -- Turn.splicePhases is handed GameState.phase because "directly after this
  -- phase" is NOT the head of `remaining` when the resolving phase still has
  -- steps to come: Aurelia, the Warleader's trigger resolves in the declare
  -- attackers step, where this combat phase's own declare blockers, combat
  -- damage and end of combat steps are all still ahead. CR 511.3 is what bounds
  -- the phase, and Turn.thisPhase is where that lives.
  Effect.AddPhases extras ->
    State.modify' $ \gs ->
      gs {GameState.remaining = Turn.splicePhases (GameState.phase gs) extras (GameState.remaining gs)}
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
  Effect.TakeExtraTurn ref -> do
    gs <- State.get
    let named = playerRefPlayers chosen legality controller gs ref
        -- CR 500.7: "If multiple players are given extra turns, the extra turns
        -- are added one at a time, in APNAP order (see rule 101.4)." The
        -- intersection is Draw's, for Draw's reasons: apnapOrder supplies the
        -- ORDER and `named` the MEMBERSHIP, so a seat the rotation still names
        -- but playerRefPlayers does not -- a departed player, who stopped being
        -- one at CR 102.1 while keeping their seat -- gets no turn. A departed
        -- player named through a TARGET slot can still get an entry, since that
        -- arm reads the slot rather than the roster; CR 800.4k catches it at the
        -- handoff, where the turn simply does not begin.
        --
        -- Observable, not cosmetic: the pushes below are what CR 500.7's last
        -- sentence then reverses, so APNAP order is what decides which of two
        -- players takes their extra turn first.
        takers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    -- CR 500.7: "the extra turns are added ONE AT A TIME ... the MOST RECENTLY
    -- CREATED turn will be taken first." So each taker is pushed onto the head
    -- in turn, and the last one pushed is the first one Engine.handoffTurn pops
    -- -- a stack, not a queue. A second TakeExtraTurn resolving later in the
    -- same turn lands in front of this one's entries for the same reason, which
    -- is the half of the rule that two Time Warps exercise.
    State.modify' (\g -> g {GameState.extraTurns = List.foldl' (flip (:)) (GameState.extraTurns g) takers})

-- The no-subgame executor (the ability path and every direct caller): a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> PlayerId -> Map.Map SlotName (Subtype, Subtype) -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffect = applyEffectWith noSubgame

-- CR 103.5b / CR 103.6: perform the effects of an action a card grants from a
-- player's hand. Pawl.Mulligan's two window loops reach this through the
-- HandActionPerformer parameter they are handed (see
-- Pawl.Types.HandActionPerformer for why it is a parameter).
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

-- CR 701.23: do to a found card what the search said -- a move for every
-- destination and, for one of them, a CR 701.20a reveal first. The move goes
-- through the CR 400.7 funnel either way, so a replacement watching the
-- destination composes and the card that lands is a new object.
putFound :: PlayerId -> SearchDestination.SearchDestination -> ObjectId -> Game ()
putFound searcher destination cardId = case destination of
  SearchDestination.BattlefieldTapped -> putTapped cardId
  -- No tapped-ness to set afterwards and so no need for the found id: a card in
  -- a hand has no tap state to speak of (CR 110.5 gives a status only to a
  -- permanent).
  --
  -- The reveal comes FIRST, in the card's own order ("reveal that card, put it
  -- into your hand"), and CR 701.20b is what makes that an order rather than
  -- decoration: revealing does not move the card, so it happens while the card
  -- is still in the library.
  --
  -- Not a stylistic preference -- these two lines do not commute, in two
  -- different ways. Swapped as written, the reveal shows NOTHING: CR 400.7 has
  -- already ceased `cardId`, so Event.reveal finds no object and no-ops.
  -- Revealing the incarnation the move mints instead would record something,
  -- with the same characteristics today, and would still be the wrong act --
  -- what was shown was the card in the library, not the card in the hand.
  SearchDestination.RevealThenHand -> do
    Event.reveal searcher cardId
    Event.changeZone cardId Zone.Hand

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

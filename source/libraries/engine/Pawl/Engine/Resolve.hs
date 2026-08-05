module Pawl.Engine.Resolve where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Mulligan as Mulligan
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Ring as Ring
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.AbilityName (AbilityName)
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Duration as Duration
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.ExtraTurn as ExtraTurn
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HandActionPerformer as HandActionPerformer
import qualified Pawl.Types.Mode as Mode
import Pawl.Types.ModeIndex (ModeIndex)
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MonarchTarget as MonarchTarget
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.ObjectRef (ObjectRef)
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Onset as Onset
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PhasePattern as PhasePattern
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.PlayerRef (PlayerRef)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.SearchDestination as SearchDestination
import qualified Pawl.Types.Sickness as Sickness
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SourceRelation as SourceRelation
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.UnlessPaid as UnlessPaid
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

-- The slots an ObjectRef reads. Only InSlot names one; EachMatching is swept from
-- the battlefield at resolution and names nothing at cast, so a card whose only
-- object reference is a set declares no target spec and CR 608.2b has nothing to
-- fizzle (CR 115.10a).
objectRefSlots :: ObjectRef -> Set SlotName
objectRefSlots ref = case ref of
  ObjectRef.InSlot slot -> Set.singleton slot
  ObjectRef.EachMatching _ -> Set.empty

-- The one legitimate home of `case effect of`: this module is the VM's opcode
-- semantics (design.md section 1), and everything else asks classifications.
-- slotsOf is the read half of the dataflow lint.
--
-- Every arm carrying a Quantity unions Quantity.slots over it, a Quantity.InSlot
-- being a slot read like any other. X is not one of those reads; readsX below is
-- X's own half of the contract.
slotsOf :: Effect Card.Type.Card -> Set SlotName
slotsOf effect = case effect of
  Effect.DealDamage ref quantity -> Set.union (objectRefSlots ref) (Quantity.slots quantity)
  -- The modification's own quantities read slots too, through
  -- Projection.quantitiesOf. No card in the pool reads a slot there, but a
  -- dangling one would otherwise slip past the lint entirely.
  Effect.ModifyTarget duration modification ref ->
    Set.unions [objectRefSlots ref, foldMap Quantity.slots (Projection.quantitiesOf modification), durationSlots duration]
  Effect.ChangeText _ _ slot -> Set.singleton slot
  Effect.AddMana _ -> Set.empty
  Effect.Search _ _ -> Set.empty
  Effect.ExileAllGraveyards -> Set.empty
  Effect.Proliferate -> Set.empty
  Effect.TemptWithTheRing -> Set.empty
  Effect.ExileHandThenDraw -> Set.empty
  Effect.PlayerSacrifices slot _ quantity -> Set.insert slot (Quantity.slots quantity)
  Effect.RestartGame -> Set.empty
  Effect.ControlPlayerNextTurn slot -> Set.singleton slot
  -- The third field is a DEFINITION (how many this sweep destroyed), not a read,
  -- so it belongs to definedSlots below and must not appear here -- Create's and
  -- PlaySubgame's slots take the same posture.
  Effect.Destroy ref _ _ -> objectRefSlots ref
  Effect.Sacrifice slot -> Set.singleton slot
  Effect.RemoveFromCombat slot -> Set.singleton slot
  Effect.MoveToZone slot _ _ _ -> Set.singleton slot
  Effect.Draw ref quantity -> Set.union (playerRefSlots ref) (Quantity.slots quantity)
  Effect.Mill slot quantity -> Set.insert slot (Quantity.slots quantity)
  Effect.Discard slot quantity -> Set.insert slot (Quantity.slots quantity)
  Effect.LoseLife ref quantity -> Set.union (playerRefSlots ref) (Quantity.slots quantity)
  Effect.GainLife ref quantity -> Set.union (playerRefSlots ref) (Quantity.slots quantity)
  -- Create's slot is a DEFINITION, not a read: it is not a target, so the D4
  -- lint must not see it here. Its Quantity is a read like every other.
  Effect.Create quantity _ _ _ -> Quantity.slots quantity
  -- The ReplacementEffect carries no Quantity, but the Duration and Condition each
  -- carry two, and a Quantity.InSlot inside either is a slot read. No card writes
  -- one, but a dangling one would slip past the lint as ModifyTarget's would.
  Effect.Replace duration _ _ condition _ -> Set.union (durationSlots duration) (foldMap conditionSlots condition)
  -- The PlayerRef may name a target slot -- Fatigue's "target player".
  Effect.SkipNextPhase ref _ -> playerRefSlots ref
  -- The ObjectRef names the shielded recipient and the Quantity the shield's size.
  Effect.PreventNextDamage duration ref quantity ->
    Set.unions [durationSlots duration, objectRefSlots ref, Quantity.slots quantity]
  -- The same two reads, minus the shield size this opcode does not carry.
  Effect.PreventAllDamage duration ref -> Set.union (durationSlots duration) (objectRefSlots ref)
  Effect.Counter slot -> Set.singleton slot
  Effect.PutCounters _ quantity slot -> Set.insert slot (Quantity.slots quantity)
  Effect.GainPlayerCounters ref _ quantity -> Set.union (playerRefSlots ref) (Quantity.slots quantity)
  Effect.Tap ref -> objectRefSlots ref
  Effect.Untap ref -> objectRefSlots ref
  Effect.Transform ref -> objectRefSlots ref
  Effect.AddPhases _ -> Set.empty
  Effect.GainControl _ ref -> objectRefSlots ref
  Effect.ArmDelayedTrigger {} -> Set.empty
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
  Effect.TakeExtraTurn ref _ -> playerRefSlots ref
  Effect.ShuffleIntoLibrary slot -> Set.singleton slot

-- CR 611.2b: the only Duration carrying a Quantity is ForAsLongAs, through its
-- Condition.
durationSlots :: Duration.Duration -> Set SlotName
durationSlots duration = case duration of
  Duration.UntilEndOfTurn -> Set.empty
  Duration.Indefinite -> Set.empty
  Duration.UntilYourNextTurn -> Set.empty
  Duration.ForAsLongAs condition -> conditionSlots condition
  Duration.UntilEndOfCombat -> Set.empty

-- Every slot a whole MODE reads: its effect list's, plus the one CR 118.12a's
-- "unless [a player] pays" names its payer by. What the D4 dataflow lint asks,
-- since a payer slot no effect ALSO reads would otherwise dangle unnoticed. Mana
-- Leak's Counter happens to read the very slot its "unless" names, so the lint's
-- answer is the same either way for the one card in the pool; a card whose payer
-- and target differ is what this exists for.
modeSlots :: Mode.Mode Card.Type.Card -> Set SlotName
modeSlots mode =
  Set.union
    (foldMap slotsOf (Mode.effects mode))
    (maybe Set.empty (Set.singleton . UnlessPaid.payer) (Mode.unlessPaid mode))

-- Both sides of a Condition are a Quantity, and either may read a slot.
conditionSlots :: Condition.Type.Condition -> Set SlotName
conditionSlots condition =
  Set.union
    (Quantity.slots $ Condition.Type.measured condition)
    (Quantity.slots $ Condition.Type.threshold condition)

-- Does any of these effects read X? A card that reads X must declare {X} in its
-- cost -- the same reads-equal-declares contract slotsOf draws for target slots.
-- When an opcode gains a Quantity field, add its arm here: the compiler will not
-- force it, Quantity being compared by ==.
--
-- That comparison is shallow, so an X nested inside a Plus or a Count is not
-- detected, unlike slotsOf, which recurses through Quantity.slots (#482).
readsX :: [Effect Card.Type.Card] -> Bool
readsX = any effectReadsX
  where
    effectReadsX effect = case effect of
      Effect.DealDamage _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      -- Untamed Might's "+X/+X" is an X the effect itself does not carry: it sits
      -- inside the Modification, reached through Projection.quantitiesOf.
      Effect.ModifyTarget _ modification _ -> elem (Quantity.Type.InSlot Binding.variableX) (Projection.quantitiesOf modification)
      Effect.ChangeText {} -> False
      Effect.AddMana _ -> False
      Effect.Search _ _ -> False
      Effect.ExileAllGraveyards -> False
      Effect.Proliferate -> False
      Effect.TemptWithTheRing -> False
      Effect.ExileHandThenDraw -> False
      Effect.PlayerSacrifices _ _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.RestartGame -> False
      Effect.ControlPlayerNextTurn _ -> False
      Effect.Destroy {} -> False
      Effect.Sacrifice _ -> False
      Effect.RemoveFromCombat _ -> False
      Effect.MoveToZone {} -> False
      Effect.Draw _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.Mill _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.Discard _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.LoseLife _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.GainLife _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.Create quantity _ _ _ -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.Replace {} -> False
      Effect.SkipNextPhase {} -> False
      Effect.PreventNextDamage _ _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.PreventAllDamage {} -> False
      Effect.Counter _ -> False
      Effect.PutCounters _ quantity _ -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.GainPlayerCounters _ _ quantity -> quantity == Quantity.Type.InSlot Binding.variableX
      Effect.Tap _ -> False
      Effect.Untap _ -> False
      Effect.Transform _ -> False
      Effect.AddPhases _ -> False
      Effect.GainControl _ _ -> False
      Effect.ArmDelayedTrigger {} -> False
      Effect.AffectPlayers {} -> False
      Effect.CreateEmblem {} -> False
      Effect.BecomeMonarch {} -> False
      Effect.ExileUntilMonarch _ -> False
      Effect.Attach _ -> False
      Effect.AttachTarget {} -> False
      Effect.PlaySubgame _ -> False
      Effect.TakeExtraTurn {} -> False
      Effect.ShuffleIntoLibrary _ -> False

-- CR 601.3 (Panglacial): does this effect search a library? The classification
-- Stack asks before resolving, to offer the cast-while-searching opportunity.
-- Search searches the controller's own library; every other effect does not.
searchesLibrary :: Effect Card.Type.Card -> Bool
searchesLibrary effect = case effect of
  Effect.Search _ _ -> True
  Effect.Proliferate -> False
  Effect.TemptWithTheRing -> False
  Effect.PlayerSacrifices {} -> False
  Effect.DealDamage _ _ -> False
  Effect.ModifyTarget {} -> False
  Effect.ChangeText {} -> False
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
  Effect.PreventNextDamage {} -> False
  Effect.PreventAllDamage {} -> False
  Effect.Counter _ -> False
  Effect.PutCounters {} -> False
  Effect.GainPlayerCounters {} -> False
  Effect.Tap _ -> False
  Effect.Untap _ -> False
  Effect.Transform _ -> False
  Effect.AddPhases _ -> False
  Effect.GainControl _ _ -> False
  Effect.ArmDelayedTrigger {} -> False
  Effect.AffectPlayers {} -> False
  Effect.CreateEmblem {} -> False
  Effect.BecomeMonarch {} -> False
  Effect.ExileUntilMonarch _ -> False
  Effect.Attach _ -> False
  Effect.AttachTarget {} -> False
  Effect.PlaySubgame _ -> False
  -- CR 701.24 shuffles a library but never LOOKS at one, which is what CR 701.23a
  -- makes a search, so this opens no window.
  Effect.ShuffleIntoLibrary _ -> False
  Effect.TakeExtraTurn {} -> False

-- CR 603.7: the delayed abilities an effect list ARMS, by name. The read half of
-- the AbilityName dataflow lint, exactly as slotsOf is for target slots.
armedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
armedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger name _ _ -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- CR 603.7: the delayed abilities an effect list arms with an ONSET -- the ones
-- whose firing is gated past the turn that armed them. armedAbilities' sibling,
-- narrowed to the arms that are not Onset.Immediately.
--
-- Its customer is the card lint, which joins these names against the card's own
-- delayedAbilities and asks Event.controllerTurnScoped of each condition.
onsetGatedAbilities :: [Effect Card.Type.Card] -> Set AbilityName
onsetGatedAbilities effects =
  let named effect = case effect of
        Effect.ArmDelayedTrigger _ Onset.Immediately _ -> Nothing
        Effect.ArmDelayedTrigger name _ _ -> Just name
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe named effects)

-- The slots an effect list DEFINES rather than reads: a Create that names what
-- it mints -- one token, or every one of them -- a MoveToZone that names the incarnation CR
-- 400.7 minted at the destination, a PlaySubgame that names its loser, a Destroy
-- that names how many it destroyed. The write half of the same lint.
definedSlots :: [Effect Card.Type.Card] -> Set SlotName
definedSlots effects =
  let bound effect = case effect of
        Effect.Create _ _ _ mSlot -> mSlot
        Effect.MoveToZone _ _ _ mSlot -> mSlot
        Effect.PlaySubgame slot -> Just slot
        Effect.Destroy _ _ mSlot -> mSlot
        _ -> Nothing
   in Set.fromList (Maybe.mapMaybe bound effects)

-- Does a Create's slot name EVERY token it minted ("those tokens") rather than
-- one particular one (CR 603.7c's "it")? Which of the two a card means is its own
-- text, and CR 111 gives the opcode no way to carry the word; the PRINTED quantity
-- is the only thing left that can tell them apart, and the count at RESOLUTION
-- cannot -- CR 614.16 lets a replacement multiply either one.
--
-- Literal 1 is "it": the card printed one token, so a doubled create leaves
-- several candidates for a singular word and the Create arm asks which. Anything
-- else is "them", on the reading that a card printing several tokens and
-- referring back to them has nothing on it to pick ONE by -- Thatcher Revolt (in
-- the pool), Chandra Acolyte of Flame and Orthion (not) all say it in the plural,
-- and Orthion carries both forms with the singular on its ONE-token ability.
--
-- An inference from the quantity, and unlike the rejection it replaces it cannot
-- be linted: nothing in card data records the word, so a future card printing
-- several while naming one would bind them all with no diagnostic. No such card
-- is known.
namesEveryToken :: Quantity.Type.Quantity -> Bool
namesEveryToken quantity = quantity /= Quantity.Type.Literal 1

-- A resolving spell's PROJECTED modes: only its chosen ones (CR 608.2c/700.2),
-- with every text change affecting it applied to each effect (CR 612), so the
-- resolver honors a spell hacked on the stack. A non-modal card has one mode,
-- always chosen.
--
-- Modes rather than a flat effect list because CR 603.5's "may" is a property of
-- the mode. A text change rewrites EFFECTS only, so optionality passes through.
modesOf :: ObjectId -> GameState -> [(ModeIndex, Mode.Mode Card.Type.Card)]
modesOf oid gs = case Game.lookupObject oid gs of
  Nothing -> []
  Just obj -> case Game.faceOf oid gs of
    Nothing -> []
    Just face ->
      let chosen = Binding.modesOf (Object.bindings obj)
          rewrite = Projection.rewriteEffect (Projection.textChangesAffecting oid gs)
          rewriteMode m = m {Mode.effects = fmap rewrite (Mode.effects m)}
       in fmap (fmap rewriteMode) (Card.chosenModes chosen face)

-- CR 405.4: who controls a SPELL on the stack, fixed at cast time -- for both CR
-- 608.2b's legality perspective and the effects' own execution. One function
-- because those two must name the same player, not because they disagree today.
--
-- The projection read is a no-op in this pool: nothing installs a SetController
-- naming a stack object, so it always folds back to the owner (#83).
spellController :: Object.Object -> ObjectId -> GameState -> PlayerId
spellController obj oid gs = Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs)

-- CR 608.2b: are ALL of this spell's targets illegal? A spell with no target spec
-- never fizzles, and one with several survives if any one is still legal. Reserved
-- slots are not targets and are vacuously legal.
--
-- Shared by the ordinary spell path and the Aura path in Pawl.Engine.Stack, which
-- is the point of it being a function: an Aura spell is the first permanent spell
-- that can be countered on resolution, and a second copy would drift.
targetsAllIllegal :: ObjectId -> GameState -> Bool
targetsAllIllegal oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Game.faceOf oid gs of
    Nothing -> False
    Just face ->
      let specs = Card.modesTargetSpecs (Binding.modesOf (Object.bindings obj)) face
          chosen = Binding.targetsOf (Object.bindings obj)
          legalSlot slot recipient = case Map.lookup slot specs of
            Nothing -> True
            -- CR 608.2b's perspective is the SPELL's controller (CR 405.4), read
            -- through the same function resolveSpellWith uses for execution.
            Just spec -> Target.stillLegal (Just (spellController obj oid gs)) oid recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          targeted = Map.restrictKeys legality (Map.keysSet specs)
       in not (Map.null specs) && not (or (Map.elems targeted))

-- CR 608.2b then CR 608.2: re-validate every filled slot against its spec; if the
-- spell has slots and ALL are now illegal it fizzles, moving to the graveyard with
-- no effect applied. Otherwise the effects run in order (CR 608.2c), each skipping
-- a slot whose target is illegal, and the spell goes to its owner's graveyard as
-- the final part of resolution (CR 608.2n).
--
-- Extended for CR 729.1b: the resolving object's bindings are re-read before EACH
-- effect, so a slot DEFINED mid-resolution is visible to a later one. Target-slot
-- legality stays fixed at the start of resolution; only newly-defined reserved
-- slots appear, and those are vacuously legal. `runSubgame` is the injected
-- nested-game runner.
resolveSpellWith :: Game Result -> ObjectId -> Game ()
resolveSpellWith runSubgame oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> case Game.faceOf oid gs of
      Nothing -> pure ()
      Just face ->
        -- CR 608.2b/700.2c: re-validate only the CHOSEN modes' slots -- an
        -- unchosen mode's slot was never filled and is not part of this
        -- resolution's legality question.
        let specs = Card.modesTargetSpecs (Binding.modesOf (Object.bindings obj)) face
            legalSlot slot recipient = case Map.lookup slot specs of
              -- CR 608.2b is about TARGETS. A slot with no target spec is a
              -- RESERVED binding -- a trigger's source, a token this resolution
              -- minted -- and was never targeted.
              Nothing -> True
              Just spec -> Target.stillLegal (Just (spellController obj oid gs)) oid recipient spec gs
         in if targetsAllIllegal oid gs
              then Event.changeZone oid Zone.Graveyard
              else do
                let effectController = spellController obj oid gs
                Monad.forM_ (modesOf oid gs) $ \(idx, mode) -> do
                  -- CR 603.5 / 608.2d: a printed "may" is answered here, between
                  -- the preceding mode's instructions and this one's.
                  taken <- exercises oid effectController idx mode
                  -- CR 118.12a: and then this mode's "unless [a player] pays",
                  -- offered only for a mode that is happening at all. The
                  -- legality and the targets are the START-of-resolution ones,
                  -- matching CR 608.2b's single re-validation; the per-effect
                  -- re-read below adds only slots this resolution DEFINES, and
                  -- an "unless" is offered before any of THIS mode's effects have
                  -- run. A later mode's gate is asked after an earlier mode's
                  -- effects, which no card reaches: the pool's one unlessPaid is
                  -- a single-mode spell.
                  gatePaid <-
                    if taken
                      then
                        let chosenAtStart = Binding.targetsOf (Object.bindings obj)
                         in paid oid oid idx (Map.mapWithKey legalSlot chosenAtStart) chosenAtStart mode
                      else pure False
                  let applyOne eff = do
                        -- Re-read the live bindings for THIS effect: a prior
                        -- PlaySubgame may have bound its loser slot.
                        bindingsNow <- State.gets (maybe (Object.bindings obj) Object.bindings . Game.lookupObject oid)
                        let chosenNow = Binding.targetsOf bindingsNow
                            legalityNow = Map.mapWithKey legalSlot chosenNow
                        applyEffectWith runSubgame oid oid effectController legalityNow chosenNow eff
                  Monad.when (taken && not gatePaid) (Monad.forM_ (Mode.effects mode) applyOne)
                finishSpell oid face effectController

-- CR 608.2n / 715.3d: where the spell goes as the last part of its resolution.
-- Its owner's graveyard, unless it was cast as an Adventure -- then its
-- controller exiles it instead, and CR 715.3d's permission to play it goes onto
-- the exiled card.
--
-- Reached only from the RESOLVING path above. The fizzle a few lines up keeps
-- the graveyard, and must: a spell whose targets have all become illegal does
-- not resolve at all (CR 608.2b), so CR 715.3d's "as it resolves" never applies
-- to it. The mechanic's own ruling says so outright -- an Adventure spell that
-- leaves the stack any other way, "most likely by being countered or by failing
-- to resolve because its targets have all become illegal", is not exiled.
--
-- Written onto the id the move RETURNS and never onto `oid`: CR 400.7 mints a
-- fresh incarnation in exile and deletes the one that was on the stack, so the
-- permission belongs to the new object. Nothing comes back when the move was
-- cancelled, and then there is no exiled card to permit anything about.
finishSpell :: ObjectId -> Face.Face Card.Type.Card -> PlayerId -> Game ()
finishSpell oid face controller =
  if not (Card.isAdventure face)
    then Event.changeZone oid Zone.Graveyard
    else do
      exiled <- Event.changeZoneReturning oid Zone.Exile
      Monad.forM_ exiled $ \newId ->
        State.modify' $ \gs ->
          gs
            { GameState.objects =
                Map.adjust (\o -> o {Object.playableFromExileBy = Just controller}) newId (GameState.objects gs)
            }

-- The no-subgame spell resolver (Stack's default path and every direct caller).
resolveSpell :: ObjectId -> Game ()
resolveSpell = resolveSpellWith noSubgame

-- CR 608.2: the executor shared by an activated and a triggered ability on the
-- stack. Re-validates filled slots (CR 608.2b), walks the CHOSEN modes in order
-- (CR 608.2c/700.2c) applying each one's effects with `srcId` as the effect source
-- (CR 113.7) and asking about any printed "may" (CR 603.5), then the ability
-- ceases (CR 608.2n). `stackId` is the ability object's own id.
--
-- Takes the modes rather than a flat effect list plus a spec map: the specs ARE
-- the union of those modes' own (CR 700.2c).
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
            -- RESERVED binding -- the trigger's source (Pawl.Engine.Binding.triggerSource),
            -- a token this resolution minted -- and was never targeted, so it can
            -- never have become an illegal target.
            Nothing -> True
            -- CR 608.2b: the perspective is the ABILITY's controller, the
            -- `effectController` bound below. `srcId` stays the source (CR 113.7)
            -- and may well be gone -- exactly the case this rule is about, and why
            -- the perspective is not read from it.
            Just spec -> Target.stillLegal (Just effectController) srcId recipient spec gs
          legality = Map.mapWithKey legalSlot chosen
          -- CR 608.2b's fizzle asks about the TARGETED slots only, so the
          -- reserved slots above cannot rescue a spell whose every target is gone.
          targeted = Map.restrictKeys legality (Map.keysSet specs)
          fizzles = not (Map.null specs) && not (or (Map.elems targeted))
          -- CR 113.8 / 603.3a: an activated ability's controller is who activated
          -- it, a triggered ability's is whoever controlled its source when it
          -- triggered. Both are stamped as Object.owner at the ability's creation
          -- and never revisited, and `obj` is the ability object itself, so a
          -- stolen permanent's later controller must not override it.
          effectController = Object.owner obj
          resolveOne (idx, mode) = do
            -- CR 603.5 / 608.2d: the printed "may", answered as this mode's
            -- instructions are applied. Run only when `fizzles` is False, so the
            -- question is never asked about an ability that never resolves.
            taken <- exercises stackId effectController idx mode
            -- CR 118.12a: then the "unless [a player] pays", against the same
            -- slots -- see the spell path for why it follows the "may".
            gatePaid <- if taken then paid stackId srcId idx legality chosen mode else pure False
            Monad.when (taken && not gatePaid) (Monad.mapM_ (applyEffect stackId srcId effectController legality chosen) (Mode.effects mode))
       in do
            Monad.unless fizzles (Monad.forM_ modes resolveOne)
            State.modify' (Game.cease stackId)

-- CR 603.5 / 608.2d: does this mode's instruction list happen at all? A mandatory
-- mode always does; an optional one is its controller's call, made HERE as the
-- effect is applied -- CR 603.5 puts an optional ability on the stack regardless
-- and defers the choice to resolution.
--
-- `resolving` is the object on the stack, which is what the prompt names.
-- `controller` is who "you" means (CR 405.4 for a spell, CR 113.8 for an ability)
-- and therefore who is asked, through Decide.deciderFor so a player controlled
-- under CR 723.1 has their controller answer.
exercises :: ObjectId -> PlayerId -> ModeIndex -> Mode.Mode Card.Type.Card -> Game Bool
exercises resolving controller idx mode = case Mode.optionality mode of
  Optionality.Mandatory -> pure True
  Optionality.Optional -> do
    gs <- State.get
    let decider = Decide.deciderFor controller gs
    decision <- Game.choose (Prompt.ChooseOptional decider controller resolving idx)
    pure $ case decision of
      OptionalDecision.Exercises -> True
      OptionalDecision.Declines -> False

-- CR 118.12a: does this mode's instruction list happen, given the "unless [a
-- player] pays" it may state? A mode stating none always does. One that states
-- one offers the cost to the player its `payer` slot names, and the instructions
-- are the OTHER branch -- that rule reads "'[Do something] unless [a player does
-- something else]' ... means the same thing as '[A player may do something
-- else]. If [that player doesn't], [do something]'", so a refusal is not a
-- failure and the resolution continues either way.
--
-- The branch is keyed on the ANSWER and never on the board afterwards, which is
-- CR 118.12 in as many words: the clause "checks whether the player chose to pay
-- an optional cost or started to pay a mandatory cost, regardless of what events
-- actually occurred". That rule's Dermoplasm example is what a re-check of the
-- world would get wrong -- the payment's intended consequence was replaced and
-- the branch still ran. Nothing here looks at the target, at the payer's board or
-- at the mana that moved; it reads Prompt.ChooseToPay's answer.
--
-- FIVE outcomes. Exactly one skips the instructions -- the payer chose to pay
-- and the payment went through -- and the other four run them:
--
--   * no payer. The slot is unfilled, has become an illegal target (CR 608.2b),
--     or names an object that is gone, so there is nobody to offer the cost to
--     and nobody has paid. Unreachable for Mana Leak, whose single target slot
--     sends the whole spell through CR 608.2b's fizzle before this is asked.
--   * the payer CANNOT pay. CR 118.12a has made this cost a "may", and CR 118.3
--     says "a player can't pay a cost without having the necessary resources to
--     pay it fully" -- so declining is the only possible answer and there is
--     nothing to ask. Not asked; see Prompt.ChooseToPay.
--   * the payer declines.
--   * the payer chose to pay and the payment did not go through.
--
-- A payment that was chosen and then did not go through is read as not paid,
-- which is the branch that leaves the game where it started: Pawl.Engine.Cost.pay
-- restores the entry state, so an Unpaid result is a complete no-op and no cost
-- was paid to read a choice off. Nothing in the pool reaches it, and the reason
-- is Mana Leak's cost specifically: {3} is GENERIC, so every tap pays it. A
-- coloured resolution cost would reach it the way Pawl.Engine.Mana.payCost's own
-- haddock describes -- failure there is reachable, because a player who taps
-- their only Birds of Paradise for green cannot then pay {B} (#417, #56).
--
-- The cost is paid AGAINST `source` rather than the resolving stack object: CR
-- 113.7a keeps the source on the permanent, which is what a component naming
-- "this" must reach. The two are the same object for a spell.
--
-- CR 118.13b's announcement -- how a symbol payable in multiple ways is being
-- paid, chosen immediately before this payment -- is not made (#702).
paid :: ObjectId -> ObjectId -> ModeIndex -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Mode.Mode Card.Type.Card -> Game Bool
paid resolving source idx legality chosen mode = case Mode.unlessPaid mode of
  Nothing -> pure False
  Just gate -> do
    gs <- State.get
    let cost = UnlessPaid.cost gate
    case payerOf (UnlessPaid.payer gate) legality chosen gs of
      Nothing -> pure False
      Just payer ->
        if not (Cost.canPay payer source cost gs)
          then pure False
          else do
            decision <- Game.choose (Prompt.ChooseToPay (Decide.deciderFor payer gs) payer resolving idx cost)
            case decision of
              PaymentDecision.Declines -> pure False
              PaymentDecision.Pays -> do
                outcome <- Cost.pay payer source cost
                pure (outcome == Payment.Paid)

-- Which player a resolution cost is offered to. ONE slot read answering in two
-- ways, since a slot may hold either kind of recipient: a slot bound to a PLAYER
-- names that player, and one bound to an OBJECT names whoever controls it -- CR
-- 109.4, "only objects on the stack or on the battlefield have a controller",
-- and CR 405.4 for a spell in particular. Mana Leak's "its controller", read off
-- the targeted spell. NOT CR 109.5, which is the rule for the words "you" and
-- "your" on an object; this card says "its controller" instead.
--
-- Legality is asked as every other slot read asks it (CR 608.2b): a slot filled
-- by targeting that has since become illegal names nobody, and a reserved slot
-- has no target spec, so legalSlot already answered True.
payerOf :: SlotName -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> GameState -> Maybe PlayerId
payerOf slot legality chosen gs = case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
  (Just (Recipient.ToPlayer pid), True) -> Just pid
  (Just recipient, True) -> Recipient.objectOf recipient >>= \oid -> Projection.controllerOf oid gs
  _ -> Nothing

-- CR 608: resolve an activated ability. The effect SOURCE is the source permanent
-- (CR 113.7a), not the ability object. Reads only the ability's CHOSEN modes (CR
-- 700.2c), stamped at activation, and reuses applyEffect with the same per-slot
-- legality and CR 608.2b fizzle as a spell. The ability then ceases (CR 608.2n)
-- rather than being buried -- an ability is not a card.
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
       in resolveModes abilId srcId (Modal.chosenModes chosen (ActivatedAbility.modal ability))

-- CR 701.27a over ONE object: turn it over, or leave the map exactly as it was.
-- The Transform arm of applyEffectWith folds this over its victims.
--
-- Two ways nothing happens, and neither is an error:
--
--   * the id names nothing on the BATTLEFIELD. CR 701.27a transforms a
--     PERMANENT, which is what CR 110.1 makes an object on the battlefield, so a
--     slot naming a card in another zone turns nothing over. Belt and braces
--     against the common case, where CR 400.7 has already minted a fresh id for
--     the card that left and this id names nothing at all.
--   * Card.turnedOver declines -- CR 701.27c's card that is not double-faced,
--     CR 701.27d's instant or sorcery face.
--   * CR 701.27f's already-turned gate, alreadyTurnedFor below.
--
-- ONE FIELD, in place, because CR 712.18 says the permanent is not a new object:
-- "When a double-faced permanent transforms or converts, it doesn't become a new
-- object. Any effects that applied to that permanent will continue to apply to
-- it." So no id is minted, no timestamp is reissued, and damage, counters and
-- attachments ride through untouched -- the rule's own Example is a +2/+2 that
-- survives the turn. CR 400.7 is the negative half of the same claim: this is not
-- a zone change, so nothing mints an incarnation.
--
-- Reads the object's OWN card (Game.cardOf), never a projected one, which is the
-- footing Object.face is stored on: CR 712.9's first Example turns on a Clone
-- being a one-faced card whatever it copied, and that is the same read.
--
-- `now` is minted ONCE for the whole instruction by the caller rather than per
-- victim, because CR 608.2f processes a swept set simultaneously: two Humans
-- transformed by one Moonmist turned over at the same moment, and a later CR
-- 701.27f comparison must not be able to tell them apart.
turnOver :: ObjectId -> Timestamp.Timestamp -> GameState -> ObjectId -> Map.Map ObjectId Object.Object -> Map.Map ObjectId Object.Object
turnOver resolving now gs oid objects
  | not (Set.member oid (GameState.battlefield gs)) = objects
  | alreadyTurnedFor resolving oid gs = objects
  | otherwise = case (Map.lookup oid objects, Game.cardOf oid gs) of
      (Just object, Just card) -> case Card.turnedOver (Object.face object) card of
        Nothing -> objects
        Just name -> Map.insert oid object {Object.face = Just name, Object.turnedOverAt = Just now} objects
      _ -> objects

-- CR 701.27f, first sentence: "If an activated or triggered ability of a
-- permanent that isn't a delayed triggered ability of that permanent tries to
-- transform it, the permanent does so only if it hasn't transformed or converted
-- since the ability was put onto the stack. ... if the permanent has already
-- transformed or converted, an instruction to do either is ignored."
--
-- True when this resolution must be ignored. Two conditions, and BOTH narrow:
--
--   * the resolving object is an ability whose SOURCE is the very permanent
--     being turned over (CR 113.7a). A spell is not one -- Moonmist transforms a
--     permanent that already transformed this turn, because the rule names only
--     abilities -- and neither is an ability of some OTHER permanent, because the
--     rule says such an ability "tries to transform IT".
--   * the permanent's last turn is later than the moment the ability was put
--     onto the stack, which is that ability object's own CR 613.7d timestamp.
--     Both come from GameState.nextTimestamp, so one `>` decides it and equality
--     cannot arise -- a fresh stamp is never reissued.
--
-- The rule's SECOND sentence measures a delayed triggered ability from when it
-- was CREATED rather than from when it reached the stack, and that half is not
-- implemented: a fired delayed ability arrives as the same Source.OfTrigger an
-- ordinary one does (Engine.placeBorne), and Pawl.Types.DelayedTrigger records
-- no creation moment to compare against, so this measures it from the stack like
-- any other trigger (#694).
alreadyTurnedFor :: ObjectId -> ObjectId -> GameState -> Bool
alreadyTurnedFor resolving victim gs = case Game.lookupObject resolving gs of
  Nothing -> False
  Just ability ->
    abilityOf (Object.source ability)
      && maybe False (> Object.timestamp ability) (Game.lookupObject victim gs >>= Object.turnedOverAt)
  where
    -- A CLASSIFICATION of the resolving object -- which kind of object it is,
    -- never which card it is. CR 725.2's sourceless inherent trigger has no
    -- permanent to be an ability OF, so it falls out with the spells.
    abilityOf source = case source of
      Source.OfAbility srcId _ -> srcId == victim
      Source.OfTrigger srcId _ -> srcId == victim
      Source.OfCard _ -> False
      Source.OfToken _ -> False
      Source.OfEmblem _ -> False
      Source.OfInherentTrigger _ _ -> False

-- CR 701.3a/701.3b: may `src` legally be attached to `destination` right now?
--
-- CR 701.3a's last sentence is the whole rule -- an Aura, Equipment or
-- Fortification can't be attached to something it couldn't enchant, equip or
-- fortify -- so this dispatches on which `src` is, read through the PROJECTION, so
-- that an Equipment that lost the subtype (CR 301.5c) and a permanent animated
-- into a creature both answer correctly.
--
-- Equipment is CR 301.5's creature test. Aura is CR 303.4's enchant ability, asked
-- through Target.admittedRecipients rather than a hand-rolled creature test, which
-- honours an enchant spec that narrows further for free. Admission and not target
-- legality: CR 702.5a gives the enchant ability both jobs and this is the second,
-- so rule 702's targeting restrictions do not reach here. CR 109.5's "you" on that
-- spec is the AURA's controller, not the moving effect's. An Aura with no enchant
-- ability cannot arise -- the CardSpec lint holds the biconditional.
--
-- The Aura branch's first test is CR 303.4d's "an Aura that's also a creature
-- can't enchant anything", whose state-based half is Sba.cannotBeAttached.
-- Unreachable in this pool, written because it costs one comparison. The Equipment
-- branch has no counterpart: CR 301.5c's matching restriction carries a
-- reconfigure exception nothing here can express (#193).
--
-- The first guard -- the destination naming `src` itself -- is CR 301.5c and CR
-- 303.4d at once. Nothing for a source that is neither, per CR 701.3b; there is no
-- Subtype.Fortification to case on.
--
-- Answers with the RECIPIENT to store rather than a Bool, since CR 303.4
-- attachment is to an object or player and Object.attachedTo records which -- and
-- the tag must be the one the moving permanent's OWN rules reference the
-- destination by, not the one the moving effect targeted it with. Taking the
-- answer from the spec's candidate list is what keeps Sba's CR 303.4c re-check
-- able to compare against that same list later, so `destination` is matched by
-- which object or player it names rather than by how the caller tagged it.
attachmentFor :: ObjectId -> Recipient -> GameState -> Maybe Recipient
attachmentFor src destination gs
  | Recipient.objectOf destination == Just src = Nothing
  -- CR 301.5, "it can't legally be attached to anything that isn't a creature" --
  -- which is also why a player destination falls to Nothing here rather than
  -- getting a branch of its own.
  | Set.member Subtype.Equipment subtypes = case Recipient.objectOf destination of
      Just oid | Projection.isCreatureOf oid gs -> Just (Recipient.ToCreature oid)
      _ -> Nothing
  | Set.member Subtype.Aura subtypes =
      if Projection.isCreatureOf src gs
        then Nothing
        else case Game.faceOf src gs >>= Face.enchant of
          Nothing -> Nothing
          Just spec ->
            List.find
              (names destination)
              (Set.toList (Target.admittedRecipients (Projection.controllerOf src gs) src spec gs))
  | otherwise = Nothing
  where
    subtypes = Projection.subtypesOf src gs
    -- Same object, or same player, however either was tagged. Two object tags
    -- (ToCreature / ToObject) name one object; ToPlayer is the only player tag,
    -- so those compare whole.
    names a b = case (Recipient.objectOf a, Recipient.objectOf b) of
      (Just x, Just y) -> x == y
      (Nothing, Nothing) -> a == b
      _ -> False

-- The players a PlayerRef names DURING a resolution, read from the slots this
-- resolution filled rather than the source's bindings (which is what
-- Count.playersFor reads for a static count).
--
-- A slot's legality is asked as every other slot read asks it (CR 608.2b): one
-- filled by targeting that has since become illegal names nobody, and a reserved
-- slot has no target spec, so legalSlot already answered True.
--
-- CR 102.1: a player who has left keeps their row in GameState.players with a
-- Departed status, so `everyone` is Game.stillPlaying rather than the map's keys.
-- Only the two enumerating arms read the roster -- `Relative You` and `InSlot`
-- name one specific player who arrived from elsewhere, and whether a departed
-- player can still be one of those is CR 800.4d/800.4i's question (#181).
--
-- `everyone` is in PlayerId order rather than turn order: a PlayerRef names an
-- unordered SET, and a caller with an ordering rule imposes it on this answer, as
-- the Draw arm does for CR 121.2c.
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
-- The two callers that store a CONTINUOUS effect -- ModifyTarget and GainControl
-- -- take a further obligation from CR 611.2c: "the set of objects it affects is
-- determined when that continuous effect begins. After that point, the set won't
-- change." This answer is that determination, and those arms freeze it into the
-- stored effect as Affected.TheseObjects rather than keeping the Filter around.
-- Nothing here enforces that; it is stated so a third storing caller does not
-- reach for Affected.Matching, which is a STATIC ability's dynamic set.
--
-- ORDER: APNAP (CR 608.2f's "APNAP order is used to make the primary
-- determination of the order of those actions"), then ascending ObjectId within
-- a controller. That second key is the engine's, not the resolving controller's
-- as CR 608.2f's secondary sentence would have it (#379). The no-controller
-- fallback is unreachable: Projection.controllerOf answers Nothing only for an
-- object that does not exist, and every id here came out of the battlefield.
objectRefObjects :: Map.Map SlotName Bool -> Map.Map SlotName Recipient -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [ObjectId]
objectRefObjects legality chosen resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- The slot names every token a Create bound there ("they"), so all of them
    -- are named at once. Ahead of the target read and not subject to `legality`:
    -- a group binding is a definition, never a target (CR 115.10a), so CR 608.2b
    -- has nothing to re-validate and the slot never appears in `chosen`.
    Just group -> Foldable.toList group
    Nothing -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> Maybe.maybeToList (Recipient.objectOf recipient)
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

-- The same sweep as objectRefObjects, one step earlier: what an ObjectRef names as
-- RECIPIENTS, before the objects are picked out of them.
--
-- It exists because CR 115.4's "any target" includes a player and CR 120.1 lets
-- damage go to one, so DealDamage's InSlot arm must be able to name something no
-- ObjectId can. Every other ObjectRef-taking opcode affects permanents only and
-- reads objectRefObjects, which is this answer with the players dropped.
--
-- EachMatching names permanents, so its members arrive as Recipient.ToObject: CR
-- 109.2 draws the set from the battlefield with no claim about card types beyond
-- the Filter's own, and classifying each one is the OPCODE's job.
objectRefRecipients :: Map.Map SlotName Bool -> Map.Map SlotName Recipient -> ObjectId -> PlayerId -> ObjectId -> GameState -> ObjectRef -> [Recipient]
objectRefRecipients legality chosen resolving controller source gs ref = case ref of
  ObjectRef.InSlot slot -> case slotGroup slot resolving gs of
    -- A group is objects, so its members arrive as Recipient.ToObject exactly as
    -- EachMatching's do -- the player a slot can hold is the single-target case
    -- below, and CR 111.1's tokens are never players.
    Just group -> fmap Recipient.ToObject (Foldable.toList group)
    Nothing -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> [recipient]
      _ -> []
  ObjectRef.EachMatching _ -> fmap Recipient.ToObject (objectRefObjects legality chosen resolving controller source gs ref)

-- The objects a Create bound into `slot` as a GROUP, read off the RESOLVING stack
-- object's live bindings -- the same place Effect.Sacrifice and ArmDelayedTrigger
-- read it, and not out of `chosen`, which projects CR 601.2c's targets only.
--
-- Live rather than snapshotted, which is what lets a later effect of the same
-- resolution name what an earlier Create minted: Salt Road Skirmish's "they gain
-- haste" is the sentence after the one that made them.
slotGroup :: SlotName -> ObjectId -> GameState -> Maybe (Seq.Seq ObjectId)
slotGroup slot resolving gs = Binding.objectsOf slot (maybe Map.empty Object.bindings (Game.lookupObject resolving gs))

-- CR 615.3: install one floating prevention shield over `recipient`, for a
-- duration. The shared body of Effect.PreventNextDamage's and
-- Effect.PreventAllDamage's arms, which differ only in the DamageRewrite they
-- hand it -- CR 615.7's countdown or CR 615.1's unbounded one.
--
-- The row's other fields are Replace's: CR 113.7's source, CR 109.5's controller
-- baked at installation, and a fresh timestamp for its CR 614.5 identity.
--
-- One shield PER RECIPIENT, which is why both arms fold this over the set their
-- ObjectRef names rather than installing a single row: CR 615.11 says an effect
-- covering several recipients "creates a prevention shield for each applicable
-- creature when the spell or ability that generates that effect resolves". Every
-- producer in the pool names exactly one, so the fold is over a singleton today.
--
-- Not implemented: CR 615.5's additional effect. The row carries a
-- ReplacementEffect and no payload, so a prevention that also does something --
-- Test of Faith's "for each 1 damage prevented this way, put a +1/+1 counter on
-- that creature" -- cannot be written (#689).
installShield :: PlayerId -> ObjectId -> Duration.Duration -> DamageRewrite.DamageRewrite -> GameState -> Recipient -> GameState
installShield controller source duration rewrite g recipient = case Expiry.arm controller source duration g of
  -- CR 611.2b: the duration never started, so no shield is installed.
  Nothing -> g
  Just expiry ->
    let (ts, g1) = Game.freshTimestamp g
        active =
          ActiveReplacement.MkActiveReplacement
            { ActiveReplacement.effect =
                ReplacementEffect.DamageR
                  DamagePattern.MkDamagePattern
                    { -- Both producers say "damage that would be dealt", naming
                      -- no kind, so the shield takes combat and noncombat alike.
                      DamagePattern.whichKind = Nothing,
                      -- Nor does either name a source, which is CR 615.7's own
                      -- "the number of events or sources dealing it doesn't
                      -- matter". CR 615.9's shields against a source of a chosen
                      -- quality are a different rule, and have no producer
                      -- (#588).
                      DamagePattern.whichSource = SourceRelation.AnySource,
                      DamagePattern.whichRecipient = Just recipient
                    }
                  rewrite,
              ActiveReplacement.source = source,
              -- CR 109.5, baked as Replace's is: nothing reads it on a shield
              -- today (the pattern names its recipient outright and has no
              -- ControllerRelation to resolve), but the row cannot be built
              -- without one.
              ActiveReplacement.controller = controller,
              ActiveReplacement.timestamp = ts,
              ActiveReplacement.expiry = expiry,
              -- CR 615.7's shield is spent in DAMAGE, not in applications, so
              -- the use count is not what ends it (see Pawl.Types.Uses); a
              -- PreventAll shield has nothing to spend at all.
              ActiveReplacement.uses = Uses.Unlimited,
              -- CR 614.15: a prevention shield replaces damage from any source,
              -- including one this resolution is not itself dealing, so it is
              -- not a self-replacement.
              ActiveReplacement.origin = ReplacementOrigin.Other
            }
     in g1 {GameState.replacements = active : GameState.replacements g1}

-- One effect, applied. The case on the constructor is this module's charter.
-- `runSubgame` is the injected nested-game runner; only the PlaySubgame arm
-- consults it, and the bare applyEffect below passes noSubgame.
--
-- `controller` is the controller of the resolving spell or ability -- who searches
-- their own library (CR 701.23) -- never the effect `source`, which for an ability
-- may already have been sacrificed as a cost. That is why every arm evaluating a
-- Quantity binds its view as `Projection.viewWithLastKnown source gs` rather than
-- `fullView`: CR 608.2h covers information from a specific object including the
-- ability's own source. Uniform across the arms rather than special-cased, the
-- rule being about the source rather than which effect is asking.
--
-- `resolving` is the object ON THE STACK whose resolution this is -- the spell
-- itself, or the ABILITY object -- and is where every slot this fold DEFINES is
-- bound and where ArmDelayedTrigger reads CR 603.7c's captured environment back
-- out. NOT `source`: for an ability the two differ (CR 113.7a keeps `source` on
-- the source permanent, which is what a DealDamage must come from), and that
-- permanent can be gone before a later effect of the same list runs. The stack
-- object outlives its own effect list by construction (CR 608.2n).
--
-- It is where CR 601.2b's announced X is read from for the same reason, which is
-- why every Quantity goes through Quantity.evaluateFor with both ids rather than
-- Quantity.evaluate with `source` alone: an X-cost ability that sacrifices its
-- source to pay leaves the announced value only on the ability object (#544).
applyEffectWith :: Game Result -> ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffectWith runSubgame resolving source controller legality chosen effect = case effect of
  Effect.DealDamage ref quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        -- CR 120.1a: "Damage can't be dealt to an object that's not a battle, a
        -- creature, or a planeswalker." Both arms of the ObjectRef can name
        -- something that is not one of those, so both go through
        -- Damage.damageRecipient and neither is trusted:
        --
        --   * a SLOT bound by Pawl.Engine.Event.eventBindings names a permanent
        --     GENERICALLY -- Aether Flash's entrant under
        --     Pawl.Engine.Binding.became, tagged from a trigger condition that
        --     said nothing about card types -- and an entrant that is no longer
        --     a creature (gone by resolution, CR 608.2h) drops out here rather
        --     than becoming a damage event nothing can apply.
        --   * a SET's members are permanents matching the Filter, which is card
        --     text and need not mention a card type at all.
        --
        -- A player recipient survives it untouched: CR 115.4's "any target"
        -- includes one and CR 120.3a is what damage to it does.
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legality chosen resolving controller source gs ref)
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      -- An unevaluable quantity is a no-op, the powerOf posture.
      Nothing -> pure ()
      Just n ->
        Monad.when (n > 0) $
          -- The applied effect IS the event (the M3a spec, section 4):
          -- constructing these DamageEvents and funneling them is the whole
          -- application. CR 120.3e / 120.3a live in applyDamage.
          --
          -- ONE batch, not one call per recipient: CR 608.2f's "each such action
          -- is processed simultaneously" is what Corrosive Gale's "each creature
          -- with flying" needs, and applyDamage is the funnel that keeps it (its
          -- haddock carries the CR 615/616 reading of a batch).
          Damage.applyDamage (fmap (\recipient -> Damage.damageEvent gs DamageKind.Noncombat source recipient (Integer.toNaturalSaturating n)) recipients)
  Effect.ModifyTarget duration modification ref ->
    State.modify' $ \gs ->
      -- The affected objects are enumerated ONCE, here, by the same sweep every
      -- ObjectRef-taking opcode uses. Giant Growth's slot and Trumpet Blast's
      -- "attacking creatures" arrive as the same list, so there is one path
      -- rather than two -- and a modification that cannot land at all (a player
      -- recipient, an illegal slot per CR 608.2b, a set that matched nothing)
      -- arrives as the empty one and stores nothing.
      case objectRefObjects legality chosen resolving controller source gs ref of
        [] -> gs
        targets -> case Expiry.arm controller source duration gs of
          -- CR 611.2b: the duration never started, so the effect does nothing
          -- and is never stored.
          Nothing -> gs
          Just expiry ->
            -- CR 611.2c: "the set of objects it affects is determined when that
            -- continuous effect begins. After that point, the set won't change."
            -- THIS is that moment: the swept ids are frozen into the stored
            -- effect as Affected.TheseObjects, so a creature that becomes
            -- attacking later is not in it and one that leaves combat is still
            -- in it. Storing the Filter instead would re-derive the set at every
            -- projection and get both wrong.
            --
            -- ONE effect over the whole set rather than one per object: CR 611.2c
            -- describes a single continuous effect with a single set, and one
            -- effect is one timestamp for CR 613.7 to order.
            --
            -- CR 608.2h / 611.2d: the VALUE is locked here too -- "the answer is
            -- determined only once, when the effect is applied". The quantities
            -- are frozen to Literals against the SOURCE (which holds a chosen X)
            -- and the source's CONTROLLER (whose hand a player-scoped count
            -- counts), never against an affected object. See the P3b spec,
            -- section 2.4.
            --
            -- Nothing when a quantity cannot be evaluated at THIS moment: 608.2h
            -- gives the effect exactly one moment to determine its answer, so a
            -- value undetermined here is undetermined for good, and storing the
            -- raw quantity would only move the read to the wrong object at the
            -- wrong time. Nothing is stored instead -- the same shape the Expiry
            -- arm above takes for CR 611.2b's duration that never starts.
            case Projection.freezeQuantities gs source (Just controller) modification of
              Nothing -> gs
              Just frozen ->
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          ContinuousEffect.modification = frozen,
                          ContinuousEffect.affected = Affected.TheseObjects (Set.fromList targets)
                        }
                 in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}
  -- CR 608.2d: "if an effect of a spell or ability offers any choices other than
  -- choices already made as part of casting the spell ... the player announces
  -- these while applying the effect." Magical Hack's two basic land types, and
  -- Artificial Evolution's two creature types, are such a choice. CR 601.2b-d is
  -- the exhaustive list of what casting announces -- the modes, spliced cards,
  -- the alternative and additional costs, X, the hybrid and Phyrexian
  -- equivalents, the targets, and a division -- and a subtype word swap is none
  -- of them. So the ask is HERE, at the moment the effect is applied, and not at
  -- cast; the difference is observable, because a countered Magical Hack is then
  -- never asked at all (Pawl.ResolveSpec's MagicalHackTiming group proves both
  -- directions).
  Effect.ChangeText family forbidden slot -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
    (Just recipient, True) -> case Recipient.objectOf recipient of
      Nothing -> pure ()
      Just target -> do
        gs0 <- State.get
        let decider = Decide.deciderFor controller gs0
            -- CR 612.2: which words the player is offered is which words the
            -- card's own text names. A CLASSIFICATION of the effect (which
            -- family) and not its identity -- there is nothing here that
            -- belongs to Artificial Evolution as opposed to Magical Hack, and
            -- the "can't be Wall" restriction rides in from the data as
            -- `forbidden`.
            question = case family of
              SubtypeFamily.BasicLandType -> Prompt.ChooseLandTypeSwap decider controller resolving slot forbidden
              SubtypeFamily.CreatureType -> Prompt.ChooseCreatureTypeSwap decider controller resolving slot forbidden
        (from, to) <- Game.choose question
        State.modify' $ \gs ->
          -- CR 611.2a: the opcode states no duration, so the effect "lasts
          -- until the end of the game" -- Duration.Indefinite, armed through
          -- Pawl.Engine.Expiry like the other three storing arms rather than naming a
          -- stored Expiry here. Indefinite always arms, so the Nothing branch
          -- is unreachable; it is written out because arm is total over
          -- Duration and CR 611.2b's "never starts" is its general answer.
          case Expiry.arm controller source Duration.Indefinite gs of
            Nothing -> gs
            Just expiry ->
              -- CR 611 / 612: a continuous effect over the one target (CR 611.2c
              -- fixed set). The (from, to) is the answer just announced, baked in
              -- here; Projection rewrites both the target's type line and, at
              -- gather, any static-ability words. Resolve CONSTRUCTS the
              -- Modification but never cases on one.
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
    -- CR 608.2b: an illegal target "won't be affected by parts of a resolving
    -- spell's effect for which they're illegal", so this part does not happen --
    -- and CR 608.2d's announcement belongs to an effect that IS applied, so
    -- nothing is asked either. Unreachable for both text-changers in the pool,
    -- whose only target is this slot: with it illegal, CR 608.2b's fizzle stops
    -- the resolution before any effect is applied.
    _ -> pure ()
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
        matches1 g oid = case Game.faceOf oid g of
          Nothing -> False
          Just face -> Filter.matches searchContext (Projection.viewOfCard face) filter_
     in do
          -- CR 601.3 (Panglacial Wurm): the chance to cast a
          -- castable-while-searching card is offered AT THE SEARCH, not when the
          -- resolution began (#57). Two things follow, and both are the rule
          -- rather than conveniences:
          --
          --   * everything this resolution sequences BEFORE the search has
          --     already happened, so the offer is made in the game state the
          --     player is actually searching from -- Scapeshift's sacrificed
          --     lands are gone before the Wurm's affordability is judged;
          --   * CR 601.3's subject is "a spell or ability", and this site is
          --     reached by both. The old site was Stack's Source.OfAbility arm
          --     alone, so a searching SPELL was never offered the cast at all.
          Cast.castWhileSearching controller
          gs <- State.get
          let matches = filter (matches1 gs) (Game.zoneMembers Zone.Library controller gs)
              decider = Decide.deciderFor controller gs
          answer <- Game.choose (Prompt.SearchLibrary decider controller matches)
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
          -- Shuffle the (possibly reduced) library afterward. The CARD's
          -- instruction, not rule 701.23's -- as eleven lines above, that rule
          -- says only how to look. CR 701.23h and CR 701.24b both describe the
          -- shuffle as something an effect instructs, never as part of a search.
          lib <- State.gets (Game.zoneMembers Zone.Library controller)
          shuffleAnswer <- Game.ask (Prompt.Shuffle lib)
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
  Effect.Destroy ref regenerability mSlot -> do
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
    destroyed <- Event.destroyReturning regenerability (objectRefObjects legality chosen resolving controller source gs ref)
    -- CR 701.8b: "destroyed this way" is what the funnel DESTROYED, never what
    -- the sweep named -- an indestructible permanent (CR 702.12b) and one a
    -- regeneration shield saved (CR 701.8c) were both named and neither was
    -- destroyed, and the rule's last sentence is explicit that a permanent that
    -- reached a graveyard any other way "hasn't been 'destroyed'". Bound onto
    -- this effect's SOURCE so a later effect of the same resolution reads it as
    -- Quantity.InSlot, which is Bane of Progress' rider; the read goes through
    -- live GameState rather than the `chosen` snapshot, so it works on the
    -- ability path as well as the spell one.
    --
    -- Bound even when nothing was destroyed. Zero is an answer -- "for each
    -- permanent destroyed this way" of nothing is no counters -- where leaving
    -- the slot unbound would make the rider's quantity UNEVALUABLE, which is a
    -- different thing that Resolve's arms happen to treat the same way today.
    Monad.forM_ mSlot $ \slot ->
      State.modify' (bindAmountSlot source slot (Natural.length destroyed))
  Effect.Sacrifice slot -> do
    -- A slot a Create bound to a GROUP names every token at once ("those
    -- tokens"), so all of them are sacrificed, in mint order. Read off the
    -- resolving object's LIVE bindings, as ArmDelayedTrigger is, rather than out
    -- of `chosen`: `chosen` projects CR 601.2c's targets and a group binding is
    -- never a target, so it is not there and owes CR 608.2b nothing.
    --
    -- CR 603.7c's zone check applies per MEMBER rather than to the whole word: a
    -- member that is gone is simply not affected, and the rest still are. For a
    -- token that is Event.sacrifice finding no such object at all, since CR
    -- 111.7's state-based action has already made it cease to exist; for a card
    -- permanent it would be that funnel's CR 701.21a battlefield guard.
    bound <- State.gets (Binding.objectsOf slot . maybe Map.empty Object.bindings . Game.lookupObject resolving)
    case bound of
      -- One at a time rather than as one event (#757).
      Just victims -> Monad.mapM_ (Event.sacrifice controller) victims
      Nothing -> case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
        (Just recipient, True) -> case Recipient.objectOf recipient of
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
        (Just recipient, True) -> case Recipient.objectOf recipient of
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
  Effect.MoveToZone slot zone entry mSlot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case Recipient.objectOf recipient of
        Nothing -> pure ()
        -- CR 400.7: the funnel mints a new incarnation in `zone`, owner-relative
        -- (CR 400.3 for a library, graveyard or hand destination).
        -- CR 110.5b: the funnel is handed the riders' tap state, so a permanent
        -- an effect says enters tapped is never untapped for an instant.
        --
        -- CR 110.2a: "If an effect instructs a player to put an object onto the
        -- battlefield, that object enters the battlefield under THAT PLAYER's
        -- control unless the effect states otherwise." This effect is exactly
        -- such an instruction, so its controller (CR 109.5) is who the permanent
        -- enters under -- Meandering Towershell's "return it to the battlefield
        -- under your control" is that rule restated on the card rather than an
        -- exception to it. Handed to the FUNNEL, not applied after it returns,
        -- for the reason the tap state is: control is settled on the entering
        -- incarnation before CR 614.1c's entry loop and the Moved snapshot can
        -- read it, so a CR 616.1b entry replacement that filters on control
        -- (Gather Specimens' "under an opponent's control") sees the controller
        -- the permanent actually enters under. No card in the pool observes that
        -- ordering -- Gather Specimens' row is Uses.Unlimited, so applying it to
        -- a creature already entering under its own controller changes nothing
        -- and consumes nothing -- so it buys the ordering rather than a passing
        -- test. The funnel ignores the player entirely for a non-battlefield
        -- destination, which is CR 110.2's own scope.
        --
        -- No CR 800.4b guard, unlike Effect.GainControl's arm: `controller` here
        -- is whoever controls the resolving spell or ability, and a departed
        -- player controls neither -- CR 800.4d keeps their triggered ability off
        -- the stack and Departure.nonCardStackObjectsCease removes one already
        -- on it, while clause 1 of CR 800.4a took their spells out of the game.
        Just target -> do
          mNew <- Event.changeZoneEntering target zone (EntryRiders.tapped entry) (Just controller)
          case mNew of
            -- CR 614.6: the CR 616.1 loop cancelled the move, or the id was
            -- already gone (CR 603.7c's "no longer in the zone it's expected to
            -- be in ... the ability won't affect it"). Nothing entered, so there
            -- is nothing to join to combat and nothing to bind.
            Nothing -> pure ()
            Just newId -> do
              -- CR 508.4: "if a creature is put onto the battlefield attacking,
              -- its controller chooses which defending player ... it's
              -- attacking". The rules for that live in Pawl.Engine.Combat, which
              -- is also what keeps this from looking like a declaration -- CR
              -- 508.3a's attack triggers see nothing, INCLUDING the returning
              -- creature's own (Meandering Towershell's ruling says so in as
              -- many words). The Create arm calls the same function for the same
              -- rule.
              --
              -- It reads the new permanent's controller, and CR 506.3b refuses
              -- one who is not the active player -- which the funnel above has
              -- already settled. A Towershell its attacker does not own would
              -- otherwise return to its owner and then fail to be attacking at
              -- all (Pawl.CombatSpec pins both halves).
              Monad.when (EntryRiders.attacking entry) (Combat.putOntoBattlefieldAttacking newId)
              -- CR 603.7c: bind the arriving incarnation into the resolving
              -- object's live bindings, so a delayed ability THIS SAME
              -- resolution arms can name it -- the exiled card in "exile it.
              -- Return IT to the battlefield". Nothing to ask: CR 400.7 minted
              -- exactly one object and it is the whole candidate set.
              Monad.mapM_ (\bindTo -> State.modify' (bindSlot resolving bindTo newId)) mSlot
      _ -> pure ()
  -- CR 701.24: shuffle the slot's target into its OWNER's library. Two steps, in
  -- this order and with the owner read before either:
  --
  --   * CR 400.7's move, through the same changeZone funnel every other
  --     destination uses, so a replacement watching a library entry gets its CR
  --     616.1 opportunity. Game.insertIntoZone files a library arrival under
  --     Object.owner per CR 400.3, so the card lands in the owner's library
  --     without this arm naming a player.
  --   * CR 701.24a's randomisation, through Mulligan.shuffleLibrary -- the same
  --     call CR 103.3's opening shuffle makes.
  --
  -- The shuffle runs whether or not the move did, which is CR 701.24c, and is the
  -- whole reason the owner is read from the PRE-MOVE object rather than the
  -- incarnation the funnel mints: a cancelled move leaves no incarnation and the
  -- library still has to be shuffled.
  --
  -- An id that no longer resolves shuffles NOTHING, the one place this falls short
  -- of rule 701.24c (#558). Unreachable from the pool's card, whose single target
  -- makes CR 608.2b fizzle the ability first.
  --
  -- CR 701.24a's "so that no player knows their order" makes WHO shuffles
  -- unobservable, which is why the card's "its owner shuffles" needs nothing
  -- here: Prompt.Shuffle deliberately carries no Decider (randomness is not a
  -- choice), so there is no player for it to be asked of.
  Effect.ShuffleIntoLibrary slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient is not a card to shuffle
        Just target -> do
          gs <- State.get
          case Game.lookupObject target gs of
            Nothing -> pure ()
            Just obj -> do
              Monad.void (Event.changeZoneReturning target Zone.Library)
              Mulligan.shuffleLibrary (Object.owner obj)
      _ -> pure () -- illegal slot at resolution (CR 608.2b): no-op
  Effect.Draw ref quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        -- Whoever the PlayerRef names draws -- the controller for Divination's
        -- `Relative You`, the targeted player for Ancestral Recall's `InSlot`,
        -- each opponent for Master of the Feast's `Relative Opponent`, the whole
        -- table for Vision Skeins' `EachPlayer`.
        named = playerRefPlayers chosen legality controller gs ref
        -- CR 121.2c: the active player draws first, then each other player in turn
        -- order. The reorder lives here rather than in playerRefPlayers because
        -- that is a rule about DRAWING, and the helper's other caller has no
        -- ordering rule to obey. Observable rather than cosmetic: each draw
        -- records a zone change in the log the trigger scan reads (CR 603.2). CR
        -- 121.2d has no reader -- pawl has no teams (#175).
        --
        -- An intersection: apnapOrder supplies the ORDER and `named` the
        -- MEMBERSHIP. The direction that bites is a seat apnapOrder names and
        -- `named` does not -- a departure does not shorten the seating roster (CR
        -- 800.4k/800.4m), while playerRefPlayers stops naming them at CR 102.1.
        -- Drawing for one was never a no-op: CR 800.4a took their library with
        -- them, so drawCard would write drewFromEmpty for a player not in the game.
        drawers = filter (\pid -> List.elem pid named) (Game.apnapOrder gs)
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
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
        case Quantity.evaluateFor viewOf context gs resolving source quantity of
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
        case Quantity.evaluateFor viewOf context gs resolving source quantity of
          Just n
            | n > 0 -> do
                let held = Game.zoneMembers Zone.Hand target gs
                    -- CR 701.9a's move, through the shared discard funnel, so
                    -- the discard is recorded for a rule 701.9a trigger to read
                    -- and not merely performed.
                    bury :: [ObjectId] -> Game ()
                    bury = Monad.mapM_ (Event.discard DiscardCause.Ordinary target)
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
                    choices <- Game.choose (Prompt.ChooseDiscard decider target held count)
                    -- FILTERED AND COMPLETED, the posture PlayerSacrifices takes
                    -- below. Dropping the invalid picks is not enough: this branch
                    -- is reached only when the hand is LARGER than the count, so CR
                    -- 609.3's "as much as possible" does no work here and every
                    -- card the answer omits is one the player could have discarded.
                    -- Reject-not-repair is the COST path's option, available there
                    -- only because a cost may go unpaid.
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
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n
        | n > 0 ->
            -- CR 119.3: the life total is simply adjusted, directly on the
            -- player record. Not through Pawl.Engine.Damage: CR 119.2 makes damage a
            -- CAUSE of life loss, not a synonym for it (the opcode's own
            -- comment has the consequences). A direct subtraction is what
            -- CostComponent.PayLife does for CR 119.4's cost side, and the CR
            -- 704.5a state-based action that may follow is the existing one in
            -- Pawl.Engine.Sba.
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
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
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
        case Quantity.evaluateFor viewOf context gs resolving source quantity of
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
                    else Game.choose (Prompt.ChooseSacrifices decider victim source candidates count)
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
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      Just n
        | n > 0 -> do
            -- CR 111: create n tokens with these characteristics under the
            -- effect's controller (CR 111.2), through the single funnel -- so CR
            -- 614's token replacements (Doubling Season) get their opportunity.
            -- CR 110.5b: the funnel is handed the entry's tap state, so a token
            -- the effect says is tapped is never untapped for an instant.
            minted <- Event.createTokens controller card (Integer.toNaturalSaturating n) (EntryRiders.tapped entry)
            -- CR 508.4: "if a creature is put onto the battlefield attacking, its
            -- controller chooses which defending player ... it's attacking". The
            -- rules for that live in Pawl.Engine.Combat, which is also what keeps this
            -- from looking like a declaration -- CR 508.3a's attack triggers see
            -- nothing (Pawl.Engine.Combat.putOntoBattlefieldAttacking's own comment).
            --
            -- After the entry loops rather than inside them: CR 614.16's token
            -- replacement settles the COUNT first, so this joins the tokens that
            -- actually entered, however many that turned out to be.
            Monad.when (EntryRiders.attacking entry) (Monad.mapM_ Combat.putOntoBattlefieldAttacking minted)
            case (mSlot, namesEveryToken quantity, minted) of
              (Nothing, _, _) -> pure ()
              -- Unreachable: createTokens places every token onto the battlefield
              -- (CR 111.2). Total rather than partial: nothing bound matches "the
              -- token was never named" instead of crashing.
              (Just _, _, []) -> pure ()
              -- The card says "those tokens", so the slot holds EVERY token
              -- this Create minted (CR 111.1: each is its own object). Nothing
              -- to ask, whatever CR 614.16 did to the count -- a replacement
              -- that doubled the create just makes the group bigger, and the
              -- plural word still names all of it. CR 603.7c is what the binding
              -- is FOR: a delayed ability armed by this same resolution refers
              -- to these particular objects. Thatcher Revolt is the producer.
              (Just slot, True, _) -> State.modify' (bindObjectsSlot resolving slot (Seq.fromList minted))
              -- CR 603.7c: bind the minted token into live Object.bindings so a
              -- delayed ability THIS SAME resolution arms can name it. One token
              -- is the whole candidate list, so there is nothing to ask -- and
              -- where the rules leave nothing to ask, don't prompt.
              (Just slot, False, [only]) -> State.modify' (bindSlot resolving slot only)
              -- CR 614.16 got there first: a token replacement (Doubling Season)
              -- multiplied the count at RESOLUTION, so several tokens now stand
              -- where CR 603.7c's "it" names one PARTICULAR object. CR 707.10e is
              -- the codified analogue -- when a replacement makes a copy target
              -- more than one object, "the copy's controller chooses one of them"
              -- -- so this asks rather than picking the first, which would be the
              -- engine choosing. The plural arm above never reaches here: a card
              -- that says "them" wants all of them and asks nothing.
              --
              -- FILTERED, NOT TRUSTED, the same posture Sba.chooseLegendVictims
              -- takes: an answer naming something that was not minted falls back
              -- to the first, since the slot must end up bound either way.
              (Just slot, False, first : second : rest) -> do
                gs1 <- State.get
                let candidates = first NonEmpty.:| (second : rest)
                    decider = Decide.deciderFor controller gs1
                answer <- Game.choose (Prompt.ChooseBoundToken decider controller source candidates)
                let named = if List.elem answer (NonEmpty.toList candidates) then answer else first
                State.modify' (bindSlot resolving slot named)
      _ -> pure ()
  Effect.ArmDelayedTrigger name onset duration -> do
    gs <- State.get
    -- CR 608.2h's last-known fallback, and NOT belt and braces: the source can
    -- have left the battlefield an opcode earlier in this same list -- Meandering
    -- Towershell's "exile it. Return it ..." -- and CR 400.7 has already deleted
    -- the id `source` names by the time this runs.
    case Game.faceOfWithLastKnown source gs >>= (Map.lookup name . Face.delayedAbilities) of
      -- The dataflow lint makes a dangling name a failing test, never a silent
      -- no-op; this arm only keeps the executor total.
      Nothing -> pure ()
      Just ability ->
        -- CR 603.7d-f: the controller is the player who controlled the spell or
        -- ability AS IT RESOLVED -- `controller`, baked in now. CR 603.7a: an
        -- entry appended here can only ever match events at or after the current
        -- watermark, so it never fires on an event that already happened.
        let captured = maybe Map.empty Object.bindings (Game.lookupObject resolving gs)
            entry =
              DelayedTrigger.MkDelayedTrigger
                { DelayedTrigger.ability = ability,
                  DelayedTrigger.source = source,
                  DelayedTrigger.controller = controller,
                  DelayedTrigger.bindings = captured,
                  -- CR 603.7a's other end, from Pawl.Types.Onset: no turn
                  -- restriction for an ability armed the moment it is created
                  -- (every card but one), and the BOUNDARY -- not a turn number
                  -- -- for one printed "on your next turn". Nothing about the
                  -- live board is read: which turn that phrase names is settled
                  -- as that turn begins (Event.settleOnsets), because an
                  -- intervening extra or skipped turn can still move it.
                  DelayedTrigger.window = Event.armOnset onset,
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
  Effect.Replace duration uses origin condition re ->
    -- CR 614.3 / 615.3: install the floating replacement; Pawl.Engine.Replacement
    -- consults it at every funnel until cleanup drops it (CR 514.2) or its use is
    -- spent. Targetless and unprompted. CR 113.7: the SOURCE is this effect's
    -- source, which together with the timestamp is the row's CR 614.5 identity --
    -- what a "prevented this way" trigger would have to be keyed on (#687).
    --
    -- CR 614.15: the ORIGIN travels with the row rather than being re-derived,
    -- because it is a fact about the ability that wrote it and nothing on the
    -- board still says so once the resolution is over.
    State.modify' $ \gs ->
      -- The clause's own "if" -- Galvanic Blast's "if you control three or more
      -- artifacts". Read with the resolution's controller as CR 109.5's "you"
      -- and this effect's source as the object, the same pair
      -- Pawl.Engine.Expiry.arm reads a CR 611.2b duration with. The full view,
      -- not viewWithLastKnown: a spell creating a self-replacement is on the
      -- stack and the board is live, unlike CR 603.4's intervening "if" on a
      -- leaves-the-battlefield trigger.
      let met = maybe True (Condition.holds (Projection.fullView gs) (Filter.MkContext (Just controller) (Just source)) gs source) condition
       in case (met, Expiry.arm controller source duration gs) of
            -- The stated condition is false, so the clause creates nothing.
            (False, _) -> gs
            -- CR 611.2b: the duration never started, so no floating replacement
            -- is installed.
            (_, Nothing) -> gs
            (True, Just expiry) ->
              let (ts, gs1) = Game.freshTimestamp gs
                  active =
                    ActiveReplacement.MkActiveReplacement
                      { ActiveReplacement.effect = re,
                        ActiveReplacement.source = source,
                        -- CR 109.5: the resolution's controller, BAKED now --
                        -- the source is a spell CR 608.2n is about to put in a
                        -- graveyard, so it will have no controller to project
                        -- by the time the row is consulted. Gather Specimens'
                        -- "an opponent's" and "your" are both read off this.
                        ActiveReplacement.controller = controller,
                        ActiveReplacement.timestamp = ts,
                        ActiveReplacement.expiry = expiry,
                        ActiveReplacement.uses = uses,
                        ActiveReplacement.origin = origin
                      }
               in gs1 {GameState.replacements = active : GameState.replacements gs1}
  Effect.PreventNextDamage duration ref quantity -> do
    -- CR 615.3 / 615.7: install one floating prevention shield per recipient the
    -- ref names -- "Prevent the next 4 damage that would be dealt to any target
    -- this turn" (Mending Hands). Pawl.Engine.Replacement consults it at the damage
    -- funnel until the shield is spent (CR 615.7's "reduced to 0") or the
    -- duration expires (CR 615.3's other terminator).
    --
    -- Its own opcode rather than an Effect.Replace carrying a DamageR, for the
    -- reason Effect.PreventNextDamage's own comment gives: the pattern has to
    -- name the shielded permanent or player, which card data cannot. Everything
    -- ELSE about the row is Replace's -- Resolve bakes the source (CR 113.7),
    -- CR 109.5's controller and a fresh timestamp the same way.
    --
    -- Through Damage.damageRecipient, so the baked recipient is in the same
    -- vocabulary a DamageEvent's target arrives in: CR 120.1a's "damage can't be
    -- dealt to an object that's not a battle, a creature, or a planeswalker"
    -- means a generically-named permanent that is neither is not shieldable
    -- either, and a shield installed over it could never match anything.
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legality chosen resolving controller source gs ref)
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
      -- An unevaluable quantity is a no-op, the powerOf posture DealDamage takes.
      Nothing -> pure ()
      Just n ->
        -- CR 615.7: a shield of 0 can prevent nothing, so none is installed --
        -- the same state `setShield` leaves a spent one in.
        Monad.when (n > 0) . State.modify' $ \g0 ->
          let amount = Integer.toNaturalSaturating n
           in List.foldl' (installShield controller source duration (DamageRewrite.PreventNext amount)) g0 recipients
  Effect.PreventAllDamage duration ref -> do
    -- CR 615.1 / 615.3: install one floating prevention shield per recipient the
    -- ref names, with no amount to count down -- "prevent all damage that would
    -- be dealt to you this turn" (Selfless Squire). The row is
    -- PreventNextDamage's above in every respect but its rewrite, which is why
    -- both go through `installShield`; what differs is that CR 615.7's "reduced
    -- to 0" terminator does not exist here, so only the duration ends it.
    --
    -- Through Damage.damageRecipient for PreventNextDamage's reason (CR 120.1a).
    gs <- State.get
    let recipients = Maybe.mapMaybe (Damage.damageRecipient gs) (objectRefRecipients legality chosen resolving controller source gs ref)
    State.modify' $ \g0 -> List.foldl' (installShield controller source duration DamageRewrite.PreventAll) g0 recipients
  Effect.SkipNextPhase ref selector -> do
    -- CR 614.1b: "effects that use the word 'skip' are replacement effects", so
    -- this installs one -- floating, because a sorcery's skip outlives the
    -- sorcery (CR 614.3: floating replacements "last until they're used up").
    --
    -- CR 614.10a: one instance PER NAMED PLAYER, each with Uses.Once and each
    -- prepended as its own row rather than merged. That is where "skip the next
    -- two" comes from -- two rows with two timestamps, and Replacement.consume
    -- spends exactly the one it applied.
    --
    -- Expiry.Never, and no Duration on the opcode to derive anything else from:
    -- Fatigue states no duration, so CR 614.3's other terminator ("or their
    -- duration has expired") never fires and the skip waits however many turns it
    -- must. That is CR 614.10a's own answer for a "next" that has not come round
    -- yet -- "the other will remain until another occurrence can be skipped".
    --
    -- CR 113.7: the SOURCE is this effect's source, as Replace's is.
    --
    -- The PhaseSelector goes in untouched: whether it names one step or a whole
    -- phase (CR 500.1) is card data, and only Pawl.Engine.Replacement's comparison and
    -- Pawl.Engine.Engine's boundary question read it.
    gs <- State.get
    let named = playerRefPlayers chosen legality controller gs ref
        install pid g =
          let (ts, g1) = Game.freshTimestamp g
              active =
                ActiveReplacement.MkActiveReplacement
                  { ActiveReplacement.effect =
                      ReplacementEffect.PhaseR
                        PhasePattern.MkPhasePattern
                          { PhasePattern.whichPhase = selector,
                            -- The player the resolution named, baked now. Card
                            -- data cannot name one (see
                            -- Pawl.Types.PhasePattern).
                            PhasePattern.whosePhase = Just pid
                          },
                    ActiveReplacement.source = source,
                    -- CR 109.5: the resolution's controller. Not the same
                    -- player as `pid` above, which the effect NAMED (Fatigue's
                    -- "target player"); nothing reads this, since a PhaseR
                    -- resolves no ControllerRelation.
                    ActiveReplacement.controller = controller,
                    ActiveReplacement.timestamp = ts,
                    ActiveReplacement.expiry = Expiry.Type.Never,
                    ActiveReplacement.uses = Uses.Once,
                    ActiveReplacement.origin = ReplacementOrigin.Other
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
    -- CR 114.2: the resolving controller gets the emblem. The whole minting is
    -- Event.createEmblem's, shared with CR 701.54c's Ring emblem.
    _ <- Event.createEmblem controller card
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
              >>= Recipient.objectOf
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
      (Just recipient, True) -> do
        gs <- State.get
        -- The slot's recipient is a PROPOSED destination; what gets stored is the
        -- recipient the moving permanent's own rules name it by (attachmentFor),
        -- and Nothing there is CR 701.3b's illegal attach.
        case attachmentFor source recipient gs of
          Nothing -> pure ()
          Just attachment -> do
            let alreadyThere = case Game.lookupObject source gs of
                  Nothing -> False
                  Just obj -> Object.attachedTo obj == Just attachment
            -- CR 701.3b, both sentences: an attach that cannot legally be
            -- performed does not move the permanent at all (it stays where it was
            -- rather than becoming unattached), and attaching it to what it is
            -- ALREADY attached to "does nothing" -- which matters because of the
            -- restamp below.
            Monad.unless alreadyThere $ do
              gs1 <- State.get
              -- CR 701.3c: attaching to a DIFFERENT object or player gives it a
              -- new timestamp. Not cosmetic -- CR 613.7 orders layer effects by
              -- it, so two things modifying one creature apply in attach order.
              let (ts, gs2) = Game.freshTimestamp gs1
                  move o = o {Object.attachedTo = Just attachment, Object.timestamp = ts}
              State.put gs2 {GameState.objects = Map.adjust move source (GameState.objects gs2)}
      _ -> pure ()
  -- CR 701.3a, in the other direction from Attach above: the SLOT's target is what
  -- moves, and the destination is chosen now rather than targeted.
  Effect.AttachTarget slot filter_ ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      -- An unfilled slot, or one CR 608.2b has since made illegal: no-op.
      (Just recipient, True) -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient: nothing on the battlefield moves
        Just subject -> do
          gs <- State.get
          let host = Game.lookupObject subject gs >>= Object.attachedTo >>= Recipient.objectOf
              -- One candidate's view, with the one field a projection cannot fill:
              -- whether the SUBJECT could legally be attached here (CR 701.3a).
              -- Answered by attachmentFor, the same function the move goes through
              -- below, so an offer and a move cannot disagree. Lazy, so a filter
              -- that never names the atom pays nothing.
              viewOf oid =
                (Projection.viewOfObject oid gs)
                  { Filter.canHostSubject = Maybe.isJust (attachmentFor subject (Recipient.ToObject oid) gs)
                  }
              -- The destinations the card's own TEXT admits: battlefield
              -- permanents matching the Filter, less the one the subject already
              -- holds. That exclusion is CR 701.3b's second sentence -- attaching
              -- a permanent to what it is already attached to "does nothing" --
              -- and it is also how Crown of the Ages' "ANOTHER creature" is
              -- spelled, so a card omitting the word would behave identically.
              --
              -- Narrowed to destinations the move would be LEGAL for only when the
              -- card SAYS so, which is Filter.CanHostSubject's whole job: Aura
              -- Graft's "another permanent IT CAN ENCHANT" narrows, and Crown of
              -- the Ages' bare "another creature" does not. Narrowing a filter
              -- that does not carry the atom would answer CR 303.4j's question on
              -- the player's behalf, and CR 303.4j exists precisely because the
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
                      (\oid -> Just oid /= host && Filter.matches (Filter.MkContext (Just controller) (Just source)) (viewOf oid) filter_)
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
                  answer <- Game.choose (Prompt.ChooseAttachment (Decide.deciderFor controller gs) controller subject offered)
                  pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
              gs1 <- State.get
              -- CR 303.4j, for an Aura -- "the Aura doesn't move" -- and CR
              -- 701.3b's first sentence for the rest. A FAILURE MODE, not a
              -- fizzle: the ability resolved, and the only thing that did not
              -- happen is the move. In particular the subject stays attached to
              -- its old host rather than becoming unattached, so CR 704.5m has
              -- nothing to bury.
              -- The destination was chosen as an object off the battlefield, so
              -- it is proposed as a bare ToObject and attachmentFor re-tags it
              -- the way the subject's own enchant spec references it.
              case attachmentFor subject (Recipient.ToObject destination) gs1 of
                Nothing -> pure ()
                Just attachment -> do
                  -- CR 701.3c: attaching to a DIFFERENT object gives it a new
                  -- timestamp, which CR 613.7 orders layer effects by. Always a
                  -- different object here -- the current host was never offered.
                  let (ts, gs2) = Game.freshTimestamp gs1
                      move o = o {Object.attachedTo = Just attachment, Object.timestamp = ts}
                  State.put gs2 {GameState.objects = Map.adjust move subject (GameState.objects gs2)}
      _ -> pure ()
  Effect.ExileUntilMonarch slot ->
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case Recipient.objectOf recipient of
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
      -- CR 701.6a: the slot's target is a spell or an ability on the stack;
      -- counter it through the single funnel, which picks that rule's ending
      -- from the object's own kind (the graveyard for a spell, CR 608.2n's cease
      -- for an ability). A player recipient / illegal slot (CR 608.2b): no-op.
      --
      -- The funnel is handed THIS effect's source and controller, which is what
      -- Baral, Chief of Compliance's "whenever a spell or ability you control
      -- counters a spell" reads off the event it records: the countering object,
      -- and CR 405.4's controller of it, compared against CR 109.5's "you".
      -- Passed rather than left to be re-derived, because by the time the CR
      -- 117.5 trigger scan runs the controller can no longer be asked for
      -- exactly -- see Pawl.Types.Countering, which sets out the two cases.
      (Just recipient, True) -> mapM_ (Event.counter source controller) $ Recipient.objectOf recipient
      _ -> pure ()
  Effect.PutCounters kind quantity slot -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
    case (Map.lookup slot chosen, Map.findWithDefault False slot legality) of
      (Just recipient, True) -> case Recipient.objectOf recipient of
        Nothing -> pure () -- a player recipient takes no counters
        Just target -> case Quantity.evaluateFor viewOf context gs resolving source quantity of
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
        Game.choose (Prompt.ChooseProliferate (Decide.deciderFor controller gs) controller permanents players)
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
  -- CR 701.54a: the Ring tempts the resolving controller. The whole keyword
  -- action is Pawl.Engine.Ring.tempt's, which is where rule 701.54's text lives --
  -- this arm knows only that some effect asked for it, exactly as the arms around
  -- it know only that some effect asked for a counter or a card.
  Effect.TemptWithTheRing -> Ring.tempt controller
  Effect.GainPlayerCounters ref kind quantity -> do
    gs <- State.get
    let viewOf = Projection.viewWithLastKnown source gs
        context = Filter.MkContext (Just controller) (Just source)
        recipients = playerRefPlayers chosen legality controller gs ref
    case Quantity.evaluateFor viewOf context gs resolving source quantity of
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
  Effect.Tap ref ->
    State.modify' $ \gs ->
      -- CR 701.26a: turn each named permanent sideways. The exact mirror of the
      -- Untap arm below, enumerating its victims ONCE through the same
      -- objectRefObjects for the same CR 608.2f simultaneity, so an illegal slot
      -- (CR 608.2b), a player recipient and a set that matched nothing all
      -- arrive as the empty list and tap nothing.
      --
      -- Rule 701.26a's "only untapped permanents can be tapped" needs no guard:
      -- assigning Tapped to something already tapped leaves it exactly as it
      -- was, which is what "nothing happens" means for a state this coarse.
      let tap o = o {Object.tapped = TapState.Tapped}
       in gs
            { GameState.objects =
                foldr (Map.adjust tap) (GameState.objects gs) (objectRefObjects legality chosen resolving controller source gs ref)
            }
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
                foldr (Map.adjust untap) (GameState.objects gs) (objectRefObjects legality chosen resolving controller source gs ref)
            }
  Effect.Transform ref ->
    State.modify' $ \gs ->
      -- CR 701.27a: "To transform a permanent, turn it over so that its other
      -- face is up." One assignment to Object.face per victim, which is all a
      -- turn IS here -- every characteristic read already resolves through it
      -- (Game.faceOf), so the permanent's power, type line, keywords and
      -- abilities all change together and none of them has to be told.
      --
      -- The victims are enumerated ONCE, the sweep Tap and Untap above share, so
      -- an illegal slot (CR 608.2b), a player recipient and a set that matched
      -- nothing all arrive as the empty list and turn nothing over.
      --
      -- WHICH face is Pawl.Engine.Card.turnedOver's answer, off the card's
      -- layout: it is what withholds a turn from a permanent that is not
      -- double-faced (CR 701.27c) and from one whose other face is an instant or
      -- sorcery (CR 701.27d), so this arm never learns which card it is holding.
      --
      -- CR 701.27b is why nothing else fires: turning over is its own game
      -- action, distinct from turning a permanent face up or face down, and a
      -- card that triggers ON it needs a trigger condition pawl does not have
      -- (#695).
      --
      -- ONE fresh timestamp for the whole instruction, threaded through so CR
      -- 701.27f can later ask when this happened. Minted even when the sweep
      -- turns nothing over, which costs a number nobody reads rather than a
      -- branch: GameState.nextTimestamp is a counter with no meaning beyond its
      -- order (CR 613.7).
      let (now, g1) = Game.freshTimestamp gs
       in g1
            { GameState.objects =
                foldr (turnOver resolving now g1) (GameState.objects g1) (objectRefObjects legality chosen resolving controller source g1 ref)
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
  Effect.GainControl duration ref ->
    State.modify' $ \gs ->
      -- Enumerated ONCE, by the sweep every ObjectRef-taking opcode shares: Act
      -- of Treason's slot and Aura Thief's "all enchantments" arrive as the same
      -- list, and a player recipient, an illegal slot (CR 608.2b) and a set that
      -- matched nothing all arrive as the empty one and change nothing.
      case objectRefObjects legality chosen resolving controller source gs ref of
        [] -> gs
        targets
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
                -- creature -- untapping it is the reason to; Aura Thief's "all
                -- enchantments" sweeps its own controller's as well. So the
                -- question is asked PER OBJECT, not once for the whole set.
                --
                -- Compared against the PROJECTED controller, read before the new
                -- effect is stored, not against Object.owner: you may already
                -- control a permanent you do not own (Control Magic), and
                -- re-gaining that one interrupts nothing either.
                --
                -- CR 611.2c: one stored effect over the frozen id set, exactly as
                -- ModifyTarget stores one -- "the set of objects it affects is
                -- determined when that continuous effect begins", so an
                -- enchantment that enters after this is not in it.
                let (ts, gs1) = Game.freshTimestamp gs
                    eff =
                      ContinuousEffect.MkContinuousEffect
                        { ContinuousEffect.source = source,
                          ContinuousEffect.timestamp = ts,
                          ContinuousEffect.expiry = expiry,
                          ContinuousEffect.modification = Modification.SetController controller,
                          ContinuousEffect.affected = Affected.TheseObjects (Set.fromList targets)
                        }
                    sicken o = o {Object.sickness = Sickness.Sick}
                    moved = filter (\oid -> Projection.controllerOf oid gs /= Just controller) targets
                 in gs1
                      { GameState.continuousEffects = eff : GameState.continuousEffects gs1,
                        GameState.objects = foldr (Map.adjust sicken) (GameState.objects gs1) moved
                      }
  Effect.TakeExtraTurn ref skips -> do
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
    --
    -- CR 500.11 / 113.7: the skips ride ALONG on each entry, naming that turn
    -- and no other, with this effect's source as theirs. Nothing is installed
    -- now -- Engine.takeNextTurn does that as the turn it belongs to actually
    -- begins (see Pawl.Types.ExtraTurn for why the skip travels with the turn
    -- rather than being a "next occurrence" replacement installed here).
    let entry pid = ExtraTurn.MkExtraTurn {ExtraTurn.taker = pid, ExtraTurn.source = source, ExtraTurn.skipped = skips}
    State.modify' (\g -> g {GameState.extraTurns = List.foldl' (\ts pid -> entry pid : ts) (GameState.extraTurns g) takers})

-- The no-subgame executor (the ability path and every direct caller): a
-- PlaySubgame resolves as a draw here (see noSubgame).
applyEffect :: ObjectId -> ObjectId -> PlayerId -> Map.Map SlotName Bool -> Map.Map SlotName Recipient -> Effect Card.Type.Card -> Game ()
applyEffect = applyEffectWith noSubgame

-- CR 103.5b / CR 103.6: perform the effects of an action a card grants from a
-- player's hand. Pawl.Engine.Mulligan's two window loops reach this through the
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
        source
        player
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
-- `holder` is `resolving` -- the object ON THE STACK, which is the spell itself
-- for a spell and the ABILITY object for an activated or triggered one -- the
-- same object ArmDelayedTrigger captures from, so the two always agree. See
-- applyEffectWith for why the stack object and not the effect's `source`.
--
-- Whether this makes the slot visible to a later effect of the same fold depends
-- on the path: on the SPELL path, resolveSpellWith re-reads Object.bindings
-- before EACH effect, so a later Sacrifice/Destroy/etc. reading the same slot
-- DOES see the mid-fold value (this is exactly what lets PlaySubgame's derived
-- loser reach a follow-on DealDamage). On the ABILITY path, resolveModes still
-- folds applyEffect over a `chosen` snapshot taken once before the fold starts,
-- so a later Sacrifice/Destroy/etc. there still sees the pre-bind value
-- (Nothing). Only ArmDelayedTrigger sees it on either path, because it re-reads
-- Object.bindings from LIVE GameState rather than from `chosen`. A spell-mode
-- effect that tried to read a dangling Create slot would be caught loudly by the
-- D4 lint (declared slots == read slots) rather than silently no-op, so this gap
-- is a documentation defect, not a latent one.
bindSlot :: ObjectId -> SlotName -> ObjectId -> GameState -> GameState
bindSlot holder slot target gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObject target) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- bindSlot's plural: bind EVERY token a Create minted into `slot`, for a card
-- that refers back to all of them at once. Same holder and same reason (CR
-- 603.7c) -- ArmDelayedTrigger captures this object's whole environment, which is
-- how "those tokens" outlives the resolution that minted them.
--
-- A group slot is readable mid-fold on BOTH paths, unlike bindSlot's single
-- object: its readers (the Sacrifice arm, and slotGroup for every
-- ObjectRef-taking opcode) go to live GameState rather than to `chosen`, because
-- `chosen` carries CR 601.2c's targets and this is never one.
bindObjectsSlot :: ObjectId -> SlotName -> Seq.Seq ObjectId -> GameState -> GameState
bindObjectsSlot holder slot targets gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toObjects targets) (Object.bindings obj)}
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
--
-- `holder` is the effect's `source` and not `resolving`, which is where its
-- READER looks: the follow-on effect reads it through resolveSpellWith's
-- re-read of the resolving SPELL's bindings, and only a spell can play a subgame
-- (a subgame from an ability is deferred, see noSubgame), so for every producer
-- that can exist the two ids are the same object.
bindLoserSlot :: ObjectId -> SlotName -> PlayerId -> GameState -> GameState
bindLoserSlot holder slot loser gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer loser) (Object.bindings obj)}
   in gs {GameState.objects = Map.adjust put holder (GameState.objects gs)}

-- CR 701.8b: bind how many permanents a destruction actually destroyed into
-- `slot` on `holder`, so a later effect of the same resolution can read it as
-- Quantity.InSlot. The third of the same shape, after bindSlot (an object) and
-- bindLoserSlot (a player); this one binds a NUMBER, which rides the binding
-- field CR 601.2b's chosen X rides.
--
-- Left behind on the holder after the resolution ends, exactly as the other two
-- are. Harmless and unreadable: only an effect naming this slot can see it, the
-- D4 lint makes every such read live under an effect list that also BINDS it,
-- and a second sweep on the same holder overwrites the value before reading it.
--
-- `holder` is the effect's `source`, NOT `resolving`, and that asymmetry with
-- bindSlot above is about where each is READ rather than about what each means:
-- an amount is read back by Quantity.evaluate, which every arm calls aimed at
-- `source` (CR 608.2h's "information from a specific object ... including the
-- source of the ability itself"), while an object binding is read back by
-- ArmDelayedTrigger off the stack object. Bane of Progress binds and reads one
-- inside a TRIGGERED ability, where the two ids differ, so this is load-bearing
-- rather than a convention.
bindAmountSlot :: ObjectId -> SlotName -> Natural -> GameState -> GameState
bindAmountSlot holder slot n gs =
  let put obj = obj {Object.bindings = Map.insert slot (Binding.toAmount n) (Object.bindings obj)}
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

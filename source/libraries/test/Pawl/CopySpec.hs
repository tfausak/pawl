{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Replacement's EntryR AsCopy arm (the CR 614.12a copy choice, run
-- from inside Pawl.Engine.Event's changeZone) and its CR 707.9 exceptions
-- (Replacement.applyCopyExceptions -- CR 707.9b's Quicksilver Gargantuan and
-- Phyrexian Metamorph, the second of which is where CR 707.9d's carve-out keeps
-- the copied characteristic-defining ability, and CR
-- 707.9a's Dack's Duplicate and Omni-Changeling, the second of which is where CR
-- 604.3a makes the gained ability characteristic-defining, and CR 707.9b's
-- Sakashima the Impostor, whose name and additive supertype clauses are read
-- together by CR 704.5j, and CR 707.9a's quoted ability, Mercurial Pretender,
-- which a Clone of the copy keeps), its CR 707.5 eligible set
-- (Replacement.legalCopyTargets, Copy Enchantment's "any enchantment" against Clone's
-- "any creature", and Clever Impersonator's negated "any nonland permanent"), the
-- P2 copy gate (Clone), and
-- Pawl.Engine.Resolve's CreateCopy arm (CR 707.2's token copy, Cackling
-- Counterpart and Watchful Radstag; its count, and the simultaneous entry that
-- count buys, kicked Rite of Replication; CR 122.6's entry rider on it,
-- Littjara Mirrorlake; its tap and attack riders and its bound slot, Flamerush
-- Rider; and CR 707.9b's exception riding it, Multiversal
-- Recruitment's "except it isn't legendary" read by CR 704.5j; CR 702.128a's
-- embalm and CR 702.129a's eternalize, whose colour, mana-cost, subtype and P/T
-- exceptions a Clone of the token keeps -- graveyardTokenCopySpec) and its BecomeCopy arm (CR 707.4's
-- change of a permanent already on the battlefield, and CR 707.9a's "except it
-- has this ability" riding it -- Unstable Shapeshifter, which copies twice
-- because of it, and whose token copy copies again because CR 707.9a put the
-- ability in the copiable values; and CR 707.9c's decline-to-copy exception
-- riding both roads at once -- Vesuvan Doppelganger, whose copy keeps its own
-- colour on entry and keeps it again when the ability it quoted copies a second
-- creature), and CR 611.2a's stated DURATION riding it -- Mirrorweave, whose
-- copy ends in the cleanup step while a token copy taken under it keeps the
-- values (CR 707.2b), and under which a Clone's own stamp is masked and then
-- revealed again.
-- Gameplay-level: Clone enters via the zone-change funnel, the Counterpart is
-- cast and resolved, the Radstag evolves and the Shapeshifter's trigger resolves,
-- and their projected characteristics are asserted.
--
-- Also CR 305.7's copiable-effects clause, where a copy meets the layer system:
-- Vesuva is played as a land, copies Mutavault, and a Blood Moon arriving after
-- takes the copied abilities with the printed ones
-- (Pawl.Engine.Projection.setLandSubtypeTo) -- plus the other order, where CR
-- 614.12 leaves Vesuva no copy ability to apply at all.
--
-- And CR 707.2's copiable values at the two BATTLEFIELD-WIDE SHORT-CIRCUITS that
-- decide whether a walk is needed at all -- Pawl.Engine.Projection's
-- copiableReplacementsOf, anyCopiableKeyword and copiableMintsType, in
-- replacementsAffecting's baseHas, and Pawl.Engine.CombatRestriction's
-- baseCouldMint -- on a board that has been left holding the only copy of the
-- departed original's text, whether an Unstable Shapeshifter or a Copy
-- Enchantment / Clever Impersonator put it there (copiedAbilitySpec).
--
-- And Pawl.Engine.Resolve's CopyStackObject arm (CR 707.10's copy of a spell on the
-- stack, Twincast) with the CR 707.10c re-target prompt it raises -- including
-- the announcement that prompt's offer is judged inside, CR 707.10's copied X
-- read by a copied Stir the Grave's own target slot (stirCopySpec) -- the CR
-- 704.5e state-based action in Pawl.Engine.Sba that removes the resolved copy,
-- and Pawl.Engine.Stack's OfSpellCopy resolution arm.
--
-- And that arm's two answers to the rest of CR 707.10's sentence: who puts the
-- copy onto the stack, where the effect names somebody other than its own
-- controller (Meletis Charlatan, whose copy is the copied spell's controller's
-- and who is therefore CR 707.10c's chooser), and CR 707.9's "except ..." clause
-- riding it (Double Major's "except it isn't legendary", read by CR 704.5j over
-- the token CR 707.10f mints).
--
-- And that same arm over CR 707.10's other two nouns -- an activated and a
-- triggered ability on the stack, copied by Lithoform Engine
-- (copyAbilityOnStackSpec), where CR 707.10b keeps the original's source.
--
-- And CR 613.2b's ordering of the two halves of layer 1, where a permanent is
-- both a copy and face down: CR 708.2's listed characteristics win over what the
-- copy effect stamped, read off an attack declaration a copied Silent Arbiter
-- would otherwise bound and off the projected 2/2 (faceDownCopySpec).
--
-- And CR 707.10d's and CR 707.10e's answers whole, end to end: Zada, Hedron
-- Grinder's one copy per candidate and Ivy, Gleeful Spellthief's one copy on a
-- stated new target, the second of which is where "the copy isn't created" is
-- read off an illegal one. Radiate's candidates include players, and Precursor
-- Golem's "other" is not its source (all data/scenarios/copy).
--
-- And CR 707.12's copy of a CARD, made in the zone that card is in and then cast
-- (Pawl.Engine.Resolve.Effect's castableCopy, under Pawl.Types.OfferCast's
-- `copied`): Mizzix's Mastery, whose exiled instant stays in exile while the copy
-- goes on the stack, and whose declined copy is swept by CR 704.5e
-- (data/scenarios/copy).
--
-- And CR 702.99's cipher, whose granted trigger casts a CR 707.12 copy of the
-- encoded card: Last Thoughts (data/scenarios/copy).
--
-- And CR 707.13's copy of a card defined by NAME, created outside the game and
-- then cast: Garth One-Eye (garthSpec).
--
-- And CR 707.14's copy of a card noted as it went face down: Magar of the
-- Magic Strings (magarSpec).
module Pawl.CopySpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.PlayerEffect as PlayerEffect
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The battlefield objects whose PRINTED card has this name (a printed card is
-- unchanged by copying -- only the object's projected characteristics change).
printedOnBattlefield :: String -> GameState.GameState -> [ObjectId]
printedOnBattlefield name gs =
  let isIt oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack name)) (Game.faceOf oid gs)
   in filter isIt (Set.toList (GameState.battlefield gs))

-- Whether an object is still on the battlefield -- what a destroy is read
-- through, where printedOnBattlefield above answers by PRINTED name and so
-- cannot tell two copies of one card apart.
onBattlefield :: ObjectId -> GameState.GameState -> Bool
onBattlefield oid gs = Set.member oid (GameState.battlefield gs)

clonesOnBattlefield :: GameState.GameState -> [ObjectId]
clonesOnBattlefield = printedOnBattlefield "Clone"

cloneOnBattlefield :: GameState.GameState -> Maybe ObjectId
cloneOnBattlefield = Maybe.listToMaybe . clonesOnBattlefield

-- The highest-id (most recently entered) object in a list. Total (no partial
-- `maximum`): sort descending by Down, take the head via listToMaybe.
newest :: [ObjectId] -> Maybe ObjectId
newest = Maybe.listToMaybe . List.sortOn Ord.Down

-- Answers the as-enters copy choice with ONE NAMED object, whatever else is
-- legal, and delegates every other prompt to S.identityAnswer. Pinned rather
-- than searched (copyNewest's posture below) so that a mutation cannot be
-- repaired by the answerer finding some other legal source.
--
-- The same function serves the #222 case with an id that is not legal at all --
-- the lying interpreter. legalCopyTargets is the ONLY thing enforcing CR
-- 614.12a's same-batch exclusion, so an unchecked answer would let a Clone copy
-- something it may not.
-- CR 601.2c's target, pinned to one named permanent -- copyNamed's posture for
-- copyNamed's reason.
aimingAt :: ObjectId -> Prompt.Prompt r -> r
aimingAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

copyNamed :: ObjectId -> Prompt.Prompt r -> r
copyNamed wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

copyNewest :: Prompt.Prompt r -> r
copyNewest p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> newest legal
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- copyNewest's opposite: decline every as-enters copy choice. The token-copy
-- tests answer with this so that a token which wrongly kept its base card's own
-- `EntryR AsCopy` (Clone's) copies NOTHING and dies as a 0/0, rather than being
-- repaired into the right answer by the answerer.
declineCopy :: Prompt.Prompt r -> r
declineCopy p = case p of
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- Aims a spell's one target slot at ONE PINNED id, whatever else is legal, and
-- orders any trigger batch as it arrives. Pinned rather than searched, `rites`'
-- posture: an answerer that looked for a legal creature would find the other one
-- after a mutation and repair the assertion.
targeting :: ObjectId -> Prompt.Prompt r -> r
targeting victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- `targeting` answered by FILTERING the offered set down to the named permanent
-- instead of building a Recipient: the pool decides which constructor the offer
-- wears, and a hand-built one of another shape is a different recipient that CR
-- 608.2b's re-read at resolution drops silently.
aimByFiltering :: ObjectId -> Prompt.Prompt r -> r
aimByFiltering oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) sets
  Prompt.OrderTriggers _ _ entries -> zipWith const [0 ..] entries
  Prompt.OrderDamage _ _ events -> zipWith const [0 ..] events
  _ -> S.identityAnswer p

-- S.combatBoardOf's placement, applied to a board a resolution built rather than
-- a fixture: alice active in the declare attackers step, with bob already the
-- defending player (CR 506.2) and the rest of the turn's steps ahead.
intoCombat :: GameState.GameState -> GameState.GameState
intoCombat gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
      GameState.remaining = S.phasesAfter (Phase.Combat CombatStep.DeclareAttackers)
    }

-- The tokens on the battlefield (CR 111.6), newest first.
tokensOnBattlefield :: GameState.GameState -> [ObjectId]
tokensOnBattlefield gs = List.sortOn Ord.Down (filter (`Game.isToken` gs) (Set.toList (GameState.battlefield gs)))

-- alice casts `spell` (paying from lands already in play) and the stack top
-- resolves, then the board settles. Cast and resolution run under the same
-- answerer, so a prompt either side of the boundary is answered alike.
castAndResolve :: (forall r. Prompt.Prompt r -> r) -> Printing.Printing -> GameState.GameState -> GameState.GameState
castAndResolve answer spell board =
  let (staged, oid) = S.handOne spell board
      afterCast = S.runPure answer staged (S.cast S.alice oid)
   in resolveAndSettle answer afterCast

-- Run the priority loop to exhaustion: every trigger the board has raised
-- resolves, in order, with the state-based actions between them.
resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAll answer gs = snd (Engine.runGamePure answer gs Engine.priorityLoop)

settle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
settle answer gs = snd (Engine.runGamePure answer gs Engine.settleForPriority)

-- Resolve the stack top (a permanent enters -- the copy choice is now made INSIDE
-- that resolution, CR 614.12a) AND run the settle boundary (so a 0/0 Clone with
-- nothing to copy dies to the CR 704.5f state-based action), under the given
-- answerer.
resolveAndSettle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAndSettle answer gs =
  snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- alice, with priority in her own precombat main phase and the rest of the turn
-- ahead, so a priority loop run over the board can offer her an activation.
readiedForAlice :: GameState.GameState -> GameState.GameState
readiedForAlice gs =
  gs
    { GameState.priority = Just S.alice,
      GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.remaining = S.phasesAfter Phase.PrecombatMain
    }

-- `aimByFiltering`, plus CR 603.5's "you may" taken. Vesuvan Doppelganger's
-- quoted trigger is both optional and targeted, and declining it would leave the
-- copy it already is.
becomesCopyOf :: ObjectId -> Prompt.Prompt r -> r
becomesCopyOf victim p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> aimByFiltering victim p

-- One upkeep for alice: the step's beginning recorded as the event a trigger
-- condition reads, then the priority loop run to exhaustion so what CR 603.3
-- gathered resolves.
upkeepForAlice :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
upkeepForAlice answer gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice))
          (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice, GameState.remaining = S.phasesAfter upkeep})
   in resolveAll answer (settle answer began)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  Spec.it s "Clone copies a creature and projects its P/T (CR 707.2)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addPermanent piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId -> do
        Spec.assertEqWith s "Clone's power is the Piker's" (Projection.powerOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "Clone's toughness is the Piker's" (Projection.toughnessOf cloneId resolved) $ Just 1
        Spec.assertBool s (Projection.isCreatureOf cloneId resolved) "Clone is a creature"
        Spec.assertBool s (Projection.powerOf pikerId resolved == Just 2) "the copied Piker is untouched"

  Spec.it s "Clone with no creature to copy enters as a 0/0 and dies (CR 704.5f)" $ do
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, staged) = S.spellOnStack clone S.alice gs0
        resolved = resolveAndSettle copyNewest staged
    Spec.assertEqWith s "the 0/0 Clone is gone (state-based action)" (cloneOnBattlefield resolved) Nothing

  -- #222: an interpreter naming an id that was never offered must be refused --
  -- the Clone enters as a 0/0 and dies exactly as it does when it declines.
  --
  -- The Piker is on the board so the prompt is REALLY RAISED and really answered
  -- with the phantom; on an empty board there would be nothing eligible, the
  -- prompt would be skipped (#1512's elision), and this test would pass without
  -- the refusal ever running. The board is the "Clone copies a creature" board
  -- above, so the only variable is the answer.
  Spec.it s "#222 a copy target that was never offered is refused" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addPermanent piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        phantom = ObjectId.MkObjectId 9999
        resolved = resolveAndSettle (copyNamed phantom) staged
    Spec.assertEqWith s "the Clone copied nothing and died as a 0/0" (cloneOnBattlefield resolved) Nothing

  -- THE PROVING TEST for #1512: the eligible set is the CARD's noun phrase, not
  -- "any creature". Copy Enchantment reads "you may have this enchantment enter
  -- as a copy of any enchantment on the battlefield", so on a board carrying
  -- BOTH a creature and an enchantment the two halves must come apart -- and
  -- under the hardcoded creature set they could not, since the enchantment was
  -- not offered at all and the creature was.
  --
  -- One board, two pinned answers. The answers are pinned rather than searched
  -- so that widening the filter back to creatures cannot be repaired by an
  -- answerer finding the enchantment anyway.
  Spec.it s "Copy Enchantment copies an ENCHANTMENT the creature filter would not offer (CR 707.5)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withPiker) = S.addPermanent piker S.alice gs0
        (scalesId, board) = S.addPermanent scales S.alice withPiker
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        resolved = resolveAndSettle (copyNamed scalesId) staged
    case newest (printedOnBattlefield "Copy Enchantment" resolved) of
      Nothing -> Spec.assertFailure s "Copy Enchantment left the battlefield unexpectedly"
      Just copyId -> do
        Spec.assertEqWith s "it is the Scales" (Projection.namesOf copyId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Hardened Scales"
        Spec.assertBool s (not (Projection.isCreatureOf copyId resolved)) "and did not become a creature"

  -- The negative half, on the SAME board with the SAME mana and the SAME stock:
  -- only the pinned answer differs. The Piker is a legal copy target for a Clone
  -- and is not one for a Copy Enchantment, so the filtered-not-trusted check
  -- refuses it and the enchantment enters as its printed self.
  Spec.it s "Copy Enchantment refuses the creature on that same board (CR 707.5)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, withPiker) = S.addPermanent piker S.alice gs0
        (_, board) = S.addPermanent scales S.alice withPiker
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        resolved = resolveAndSettle (copyNamed pikerId) staged
    case newest (printedOnBattlefield "Copy Enchantment" resolved) of
      Nothing -> Spec.assertFailure s "Copy Enchantment left the battlefield unexpectedly"
      Just copyId -> do
        Spec.assertEqWith s "it stayed itself" (Projection.namesOf copyId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Copy Enchantment"
        Spec.assertBool s (not (Projection.isCreatureOf copyId resolved)) "it is not the Piker"

  -- The elision side of the invariant, which narrowing the eligible set is what
  -- makes reachable: with nothing eligible, declining is the only legal answer,
  -- so the prompt is not raised. A pair of boards differing in exactly one thing
  -- -- whether a second enchantment is on the battlefield -- since a board with
  -- no enchantment at all would also have no creature to tell "not asked" from
  -- "asked about nothing".
  Spec.it s "CR 707.5: a copy choice with nothing eligible is not asked" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    scales <- S.printingOf s registry "Hardened Scales"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let countingAnswer :: Prompt.Prompt r -> State.State Int r
        countingAnswer p = case p of
          Prompt.ChooseCopyTarget {} -> do
            State.modify' (+ 1)
            pure (S.identityAnswer p)
          _ -> pure (copyNewest p)
        asks board =
          let (_, staged) = S.spellOnStack copyEnchantment S.alice board
           in State.execState (Engine.runGame countingAnswer staged (Stack.resolveTop >> Engine.settleForPriority)) 0
        gs0 = Setup.emptyGame S.bothPlayers
        (_, withPiker) = S.addPermanent piker S.alice gs0
        (_, withScales) = S.addPermanent scales S.alice withPiker
    Spec.assertEqWith s "a creature but no enchantment: nothing to ask" (asks withPiker) 0
    Spec.assertEqWith s "an enchantment beside it: one real decision" (asks withScales) 1

  -- CR 303.4f / 614.12a: Copy Enchantment that copies an Aura has its host
  -- chosen as it enters, after the copy choice -- the 2023-09-01 ruling's
  -- "you choose what the Aura will enchant just before it enters". TWO legal
  -- hosts, and the answer pinned to the SECOND, so an engine that took the first
  -- candidate (or entered unattached for CR 704.5m to bury) fails the Mammoth's
  -- P/T.
  Spec.it s "CR 303.4f Copy Enchantment copying Unholy Strength enchants the creature its controller chooses" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    unholy <- S.printingOf s registry "Unholy Strength"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, withPiker) = S.addPermanent piker S.alice gs0
        (mammothId, withMammoth) = S.addPermanent mammoth S.alice withPiker
        (unholyId, withUnholy) = S.addPermanent unholy S.alice withMammoth
        board = S.attach unholyId pikerId withUnholy
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseAttachment {} -> mammothId
          _ -> copyNamed unholyId p
        resolved = resolveAndSettle answer staged
    Spec.assertEqWith s "the War Mammoth gets +2/+1" (S.powerToughnessOf mammothId resolved) (Just (5, 4))
    Spec.assertEqWith s "and the Piker keeps only the original's +2/+1" (S.powerToughnessOf pikerId resolved) (Just (4, 2))
    Spec.assertEqWith
      s
      "the copy is on the battlefield, attached to the Mammoth"
      (fmap (\oid -> Game.lookupObject oid resolved >>= Object.attachedTo >>= Recipient.objectOf) (printedOnBattlefield "Copy Enchantment" resolved))
      [Just mammothId]

  -- CR 303.4f's "or player" (CR 702.5d) at the same door: Copy Enchantment
  -- copying bob's Curse of Death's Hold, which enchants carol, and alice
  -- choosing bob -- the SECOND of three candidates, so neither the first seat
  -- nor the original's host can stand in for him.
  Spec.it s "CR 303.4f Copy Enchantment copying a Curse enchants the player its controller chooses" $ do
    mammoth <- S.printingOf s registry "War Mammoth"
    maiden <- S.printingOf s registry "Bird Maiden"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let (mammothId, withMammoth) = S.addPermanent mammoth S.bob S.threePlayerGame
        (maidenId, withMaiden) = S.addPermanent maiden S.carol withMammoth
        (curseId, withCurse) = S.addPermanent curse S.bob withMaiden
        board = S.attachTo curseId (Recipient.ToPlayer S.carol) withCurse
        (_, staged) = S.spellOnStack copyEnchantment S.alice board
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChoosePlayer _ _ _ offered -> case NonEmpty.toList offered of
            _ : second : _ -> second
            _ -> NonEmpty.head offered
          _ -> copyNamed curseId p
        resolved = resolveAndSettle answer staged
    Spec.assertEqWith s "bob's War Mammoth gets -1/-1 from the copy" (S.powerToughnessOf mammothId resolved) (Just (2, 2))
    Spec.assertEqWith s "and carol's Bird Maiden only the original's" (S.powerToughnessOf maidenId resolved) (Just (0, 1))

  -- CR 303.4g's stack branch, as a pair differing in ONE thing: whether bob
  -- controls a creature. Bob's Betrayal ("enchant creature an opponent
  -- controls") is on alice's Balemurk Leech, so a copy alice controls can
  -- enchant only a creature of bob's. With none, the copy "is put into its
  -- owner's graveyard instead of entering the battlefield": the Leech's
  -- enchantment-enters trigger never fires and bob keeps his life. With one, the
  -- copy enters attached to it and the Leech drains bob.
  let betrayalBoard withBobCreature = do
        leech <- S.printingOf s registry "Balemurk Leech"
        betrayal <- S.printingOf s registry "Betrayal"
        piker <- S.printingOf s registry "Goblin Piker"
        copyEnchantment <- S.printingOf s registry "Copy Enchantment"
        let gs0 = Setup.emptyGame S.bothPlayers
            (leechId, withLeech) = S.addPermanent leech S.alice gs0
            (betrayalId, withBetrayal) = S.addPermanent betrayal S.bob withLeech
            attached = S.attach betrayalId leechId withBetrayal
            (bobPikerId, board) =
              if withBobCreature
                then let (pikerId, withPiker) = S.addPermanent piker S.bob attached in (Just pikerId, withPiker)
                else (Nothing, attached)
            (spellId, staged) = S.spellOnStack copyEnchantment S.alice board
            once = resolveAndSettle (copyNamed betrayalId) staged
            drained = if null (GameState.stack once) then once else resolveAndSettle (copyNamed betrayalId) once
        pure (bobPikerId, spellId, staged, drained)
  Spec.it s "CR 303.4g Copy Enchantment copying Betrayal with nothing to enchant goes to the graveyard without entering" $ do
    (_, spellId, staged, after) <- betrayalBoard False
    Spec.assertEqWith s "the Leech never saw an enchantment enter: bob keeps his life" (S.lifeOf S.bob after) (S.lifeOf S.bob staged)
    Spec.assertEqWith s "Copy Enchantment is in alice's graveyard" (fmap (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.alice after)) [Just (CardName.MkCardName (Text.pack "Copy Enchantment"))]
    Spec.assertEqWith s "and the spell is gone from the stack" (List.elem spellId (GameState.stack after)) False
  Spec.it s "CR 303.4f Copy Enchantment copying Betrayal over a creature of bob's enters on it and the Leech drains" $ do
    (bobPikerId, _, staged, after) <- betrayalBoard True
    Spec.assertEqWith s "the Leech saw an enchantment enter: bob loses 1" (S.lifeOf S.bob after) (fmap (subtract 1) (S.lifeOf S.bob staged))
    Spec.assertEqWith
      s
      "the copy enchants bob's Piker"
      (fmap (\oid -> Game.lookupObject oid after >>= Object.attachedTo >>= Recipient.objectOf) (printedOnBattlefield "Copy Enchantment" after))
      [bobPikerId]

  Spec.it s "Clone copies base P/T, not a counter-boosted P/T (CR 707.2 falsifier)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board0) = S.addPermanent piker S.alice gs0
        -- Put a +1/+1 counter on the Piker: projected 3/2, base 2/1.
        board = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId board0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId -> do
        Spec.assertEqWith s "source is boosted to 3/2" (Projection.powerOf pikerId resolved) $ Just 3
        Spec.assertEqWith s "Clone copies the base 2, not 3" (Projection.powerOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "Clone copies the base 1, not 2" (Projection.toughnessOf cloneId resolved) $ Just 1

  Spec.it s "Clone copies a creature's activated abilities (CR 707.2)" $ do
    prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addPermanent prodigalSorcerer S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone left the battlefield unexpectedly"
      Just cloneId ->
        Spec.assertBool
          s
          (not (null (Projection.abilitiesOf cloneId resolved)))
          "Clone has the copied activated ability"

  Spec.it s "a copy of a copy resolves to the underlying creature (self-reference)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, board) = S.addPermanent piker S.alice gs0
        (_, stagedA) = S.spellOnStack clone S.alice board
        afterA = resolveAndSettle copyNewest stagedA
        (_, stagedB) = S.spellOnStack clone S.alice afterA
        afterB = resolveAndSettle copyNewest stagedB
        -- Both Clones now name "Clone"; the newest (highest id) is B.
        afterBId = newest (clonesOnBattlefield afterB)
    case afterBId of
      Nothing -> Spec.assertFailure s "no Clones on the battlefield"
      Just bId -> do
        Spec.assertEqWith s "the copy-of-a-copy is a 2/1" (Projection.powerOf bId afterB) $ Just 2
        Spec.assertBool s (Projection.isCreatureOf bId afterB) "the copy-of-a-copy is a creature"

  Spec.it s "a copy survives its source leaving the battlefield (CR 707.5 lock)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addPermanent piker S.alice gs0
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
        afterKill = S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [pikerId])
    case cloneOnBattlefield afterKill of
      Nothing -> Spec.assertFailure s "Clone should survive the source's death"
      Just cloneId -> do
        Spec.assertEqWith s "the source is gone" (Set.member pikerId (GameState.battlefield afterKill)) False
        Spec.assertEqWith s "the Clone is still a 2/1" (Projection.powerOf cloneId afterKill) $ Just 2
        Spec.assertEqWith s "the Clone is still 1 toughness" (Projection.toughnessOf cloneId afterKill) $ Just 1

  Spec.it s "Clone of Tarmogoyf copies the ABILITY, so both recompute (CR 707.2a)" $ do
    -- THE FALSIFIER for snapshotting the NUMBER: CR 707.2a says a copy
    -- acquires the abilities of the object it copies, because those values are
    -- derived from its rules text. Seeding the CDA as an evaluated integer
    -- would freeze the Clone at the graveyards' contents at the moment it
    -- entered -- P2's deferred bill, paid here.
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addPermanent tarmogoyf S.alice withBolt
        (_, staged) = S.spellOnStack clone S.alice board
        resolved = resolveAndSettle copyNewest staged
        -- A second card type reaches a graveyard AFTER the Clone entered.
        (_, later) = S.addGraveyardCard piker S.bob resolved
    case cloneOnBattlefield resolved of
      Nothing -> Spec.assertFailure s "Clone did not reach the battlefield"
      Just cloneId -> do
        Spec.assertEqWith s "at entry the Clone is the Goyf's 1/2" (Projection.powerOf cloneId resolved) $ Just 1
        Spec.assertEqWith s "at entry, toughness 1+1" (Projection.toughnessOf cloneId resolved) $ Just 2
        Spec.assertEqWith s "the source moves to 2" (Projection.powerOf goyfId later) $ Just 2
        Spec.assertEqWith s "and so does the COPY" (Projection.powerOf cloneId later) $ Just 2
        Spec.assertEqWith s "the copy's toughness moves too" (Projection.toughnessOf cloneId later) $ Just 3

  -- THE PROVING TEST for CR 707.9's exceptions. Quicksilver Gargantuan is CR
  -- 707.9d's own worked example: "except it's 7/7".
  --
  -- Three readings of the same Tarmogoyf on one board, and all three differ. The
  -- ORIGINAL carries a +1/+1 counter, so it projects one above its CDA; a Clone
  -- is the copy WITHOUT the exception, so it recomputes the CDA (CR 707.2a) at
  -- the counter-free value; the Gargantuan is the copy WITH it. Both copies are
  -- pinned to the Goyf rather than to each other, so neither reading can borrow
  -- the other's.
  Spec.it s "Quicksilver Gargantuan copies a Tarmogoyf but is 7/7 (CR 707.9b, CR 707.9d)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    clone <- S.printingOf s registry "Clone"
    gargantuan <- S.printingOf s registry "Quicksilver Gargantuan"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        -- One card type in a graveyard: the Goyf's CDA is 1/2.
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board0) = S.addPermanent tarmogoyf S.alice withBolt
        board = S.addCounter CounterKind.PlusOnePlusOne 1 goyfId board0
        (_, stagedClone) = S.spellOnStack clone S.alice board
        withClone = resolveAndSettle (copyNamed goyfId) stagedClone
        (_, stagedGargantuan) = S.spellOnStack gargantuan S.alice withClone
        resolved = resolveAndSettle (copyNamed goyfId) stagedGargantuan
        -- A second card type reaches a graveyard AFTER both copies entered.
        (_, later) = S.addGraveyardCard piker S.bob resolved
    case (cloneOnBattlefield resolved, newest (printedOnBattlefield "Quicksilver Gargantuan" resolved)) of
      (Just cloneId, Just gargantuanId) -> do
        Spec.assertEqWith s "the original is its CDA plus the counter" (S.powerToughnessOf goyfId resolved) $ Just (2, 3)
        Spec.assertEqWith s "the copy without the exception is the bare CDA" (S.powerToughnessOf cloneId resolved) $ Just (1, 2)
        Spec.assertEqWith s "the copy with it is 7/7" (S.powerToughnessOf gargantuanId resolved) $ Just (7, 7)
        -- CR 707.2 still ran: only P/T is excepted.
        Spec.assertEqWith s "and is otherwise the Goyf" (Projection.namesOf gargantuanId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Tarmogoyf"
        Spec.assertBool s (Set.member Subtype.Lhurgoyf (PC.subtypes (Projection.project gargantuanId resolved))) "the Gargantuan copied the Goyf's subtype"
        -- CR 707.9d: the CDA defining the excepted characteristic was not copied,
        -- so the Gargantuan alone does not move when the graveyards do.
        Spec.assertEqWith s "the original moves with the graveyards" (S.powerToughnessOf goyfId later) $ Just (3, 4)
        Spec.assertEqWith s "so does the copy that took the CDA" (S.powerToughnessOf cloneId later) $ Just (2, 3)
        Spec.assertEqWith s "the excepted copy does not" (S.powerToughnessOf gargantuanId later) $ Just (7, 7)
      _ -> Spec.assertFailure s "the Clone and the Gargantuan should both be on the battlefield"

  -- THE PROVING TEST for CR 604.3a's third criterion: an ability acquired
  -- through a copy effect is CHARACTERISTIC-DEFINING. Omni-Changeling {3}{U}{U}
  -- Creature -- Shapeshifter 0/0: "Changeling / Convoke / You may have this
  -- creature enter as a copy of any creature on the battlefield, except it has
  -- changeling."
  --
  -- Its convoke is transcribed and nothing below turns on the cost: the spell is
  -- put on the stack rather than cast, so no cost is paid at all.
  --
  -- The copy's own printed changeling is GONE (CR 707.2 replaced it with the
  -- Piker's text), so the exception is the only source of it. Lord of Atlantis
  -- is the reader -- "other Merfolk get +1/+1 and have islandwalk", an affected
  -- set read off the projection -- so a copy that is every creature type (CR
  -- 702.73a) is a Merfolk and gets pumped.
  --
  -- Two controls on the one board, each 2/1 for its own reason: bob's Goblin
  -- Piker is no Merfolk, and a Clone copying it is the copy WITHOUT the
  -- exception. The token copy is where the CDA claim actually bites -- CR 707.2
  -- copies the copiable values and leaves every CR 613 layer behind, so a
  -- changeling GRANTED over the copy would produce a 2/1 token here.
  Spec.it s "Omni-Changeling's copy is every creature type, and so is a token copy of it (CR 604.3a)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    lord <- S.printingOf s registry "Lord of Atlantis"
    clone <- S.printingOf s registry "Clone"
    omni <- S.printingOf s registry "Omni-Changeling"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, withLord) = S.addPermanent lord S.alice (S.landsInPlay island 3)
        (pikerId, board) = S.addPermanent piker S.bob withLord
        (_, stagedClone) = S.spellOnStack clone S.alice board
        withClone = resolveAndSettle (copyNamed pikerId) stagedClone
        (_, stagedOmni) = S.spellOnStack omni S.alice withClone
        entered = resolveAndSettle (copyNamed pikerId) stagedOmni
    case (cloneOnBattlefield entered, newest (printedOnBattlefield "Omni-Changeling" entered)) of
      (Just cloneId, Just omniId) -> do
        -- The Counterpart is aimed at the excepted copy and at nothing else: an
        -- unpinned answerer copies the lord instead, and a second lord makes
        -- every creature on the board a size that proves nothing.
        let resolved = castAndResolve (targeting omniId) counterpart entered
        case tokensOnBattlefield resolved of
          [tokenId] -> do
            -- THE GAMEPLAY ASSERTION: the lord sees a Merfolk.
            Spec.assertEqWith s "the excepted copy is a Merfolk, so 2/1 plus the lord" (S.powerToughnessOf omniId resolved) $ Just (3, 2)
            -- CR 707.2 through CR 613.3: the token reads the copiable values, and
            -- the changeling among them defines its types at layer 4 all over again.
            Spec.assertEqWith s "and so is a token copy of it (CR 707.2)" (S.powerToughnessOf tokenId resolved) $ Just (3, 2)
            -- The two controls, on the same board: neither is a changeling.
            Spec.assertEqWith s "the copy without the exception is not a Merfolk" (S.powerToughnessOf cloneId resolved) $ Just (2, 1)
            Spec.assertEqWith s "and neither is the Piker it copied" (S.powerToughnessOf pikerId resolved) $ Just (2, 1)
            -- Diagnostics, after the behaviour: the copy is the Piker by name, and
            -- the type it gained is one CR 205.3m lists rather than every subtype.
            Spec.assertEqWith s "the excepted copy is the Piker by name (CR 707.2)" (Projection.namesOf omniId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
            Spec.assertBool s (Set.member Subtype.Merfolk (Projection.subtypesOf omniId resolved)) "and a Merfolk among its creature types"
            Spec.assertBool s (not (Set.member Subtype.Island (Projection.subtypesOf omniId resolved))) "and no land type (CR 205.3m)"
          tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))
      _ -> Spec.assertFailure s "the Clone and the Omni-Changeling should both be on the battlefield"

  -- THE PROVING TEST for CR 707.9b's OTHER arm, the exception that adds a CARD
  -- TYPE. Phyrexian Metamorph {3}{U/P} Artifact Creature -- Phyrexian
  -- Shapeshifter 0/0: "You may have this creature enter as a copy of any
  -- artifact or creature on the battlefield, except it's an artifact in addition
  -- to its other types."
  --
  -- Read at GAMEPLAY level by a sweeper that asks the type rather than by a
  -- projection field: Bane of Progress destroys every artifact and enchantment as
  -- it enters, so the excepted copy of a Goblin Piker is destroyed and a CLONE of
  -- the SAME Piker -- the copy without the exception, entering the same way -- is
  -- not. The Piker itself is the third reading and survives too.
  --
  -- The Metamorph's PRINTED type line is artifact creature, which is exactly what
  -- CR 707.2 replaced when it copied the Piker: without the exception the copy is
  -- a plain creature. So "it is still an artifact" is the exception's doing and
  -- not the printed card's, and the name assertion ahead of the sweep is what
  -- pins that the copy happened at all.
  --
  -- The Bane's own +1/+1 counter is a diagnostic after the behaviour: it takes
  -- one per permanent destroyed, so 3/3 says EXACTLY ONE artifact was there and
  -- the sweep did not also take the Clone.
  Spec.it s "Phyrexian Metamorph's copy is an artifact and a sweeper takes it (CR 707.9b)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    metamorph <- S.printingOf s registry "Phyrexian Metamorph"
    bane <- S.printingOf s registry "Bane of Progress"
    let (pikerId, board) = S.addPermanent piker S.bob (Setup.emptyGame S.bothPlayers)
        (_, stagedClone) = S.spellOnStack clone S.alice board
        withClone = resolveAndSettle (copyNamed pikerId) stagedClone
        (_, stagedMetamorph) = S.spellOnStack metamorph S.alice withClone
        entered = resolveAndSettle (copyNamed pikerId) stagedMetamorph
    case (cloneOnBattlefield entered, newest (printedOnBattlefield "Phyrexian Metamorph" entered)) of
      (Just cloneId, Just metamorphId) -> do
        let (baneId, staged) = S.entersWithTrigger bane S.alice entered
            -- The settle puts the Bane's CR 603.6a trigger on the stack; the
            -- resolve runs it.
            onStack = settle (copyNamed pikerId) staged
            swept = resolveAndSettle (copyNamed pikerId) onStack
        Spec.assertBool s (not (null (GameState.stack onStack))) "the Bane's trigger really was on the stack"
        -- CR 707.2 ran: the exception modified the copy, it did not replace it.
        Spec.assertEqWith s "the Metamorph is the Piker by name (CR 707.2)" (Projection.namesOf metamorphId entered) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
        Spec.assertEqWith s "and has the Piker's 2/1" (S.powerToughnessOf metamorphId entered) $ Just (2, 1)
        -- THE GAMEPLAY ASSERTION: the sweeper's type question found an artifact.
        Spec.assertBool s (not (onBattlefield metamorphId swept)) "the excepted copy was destroyed as an artifact (CR 707.9b)"
        -- The two controls, on the same board and copied from the same Piker.
        Spec.assertBool s (onBattlefield cloneId swept) "the copy without the exception is no artifact and survives"
        Spec.assertBool s (onBattlefield pikerId swept) "and the Piker they both copied is no artifact either"
        -- Diagnostics, after the behaviour: the copied type line kept its creature
        -- half (CR 205.1b's "in addition"), and exactly one permanent was swept.
        Spec.assertEqWith s "the copy is an artifact creature, not an artifact instead" (PC.cardTypes (Projection.project metamorphId entered)) (Set.fromList [CardType.Artifact, CardType.Creature])
        Spec.assertEqWith s "the Bane counted exactly one destroyed permanent" (S.powerToughnessOf baneId swept) $ Just (3, 3)
      _ -> Spec.assertFailure s "the Clone and the Metamorph should both be on the battlefield"

  -- THE PROVING TEST for CR 707.9d's CARVE-OUT: its strip of the copied object's
  -- characteristic-defining ability "does not apply to copy effects with
  -- exceptions that state the object is a certain card type, supertype, and/or
  -- subtype 'in addition to its other types'".
  --
  -- Two copies of ONE Tarmogoyf on one board, differing only in which CR 707.9b
  -- arm their card writes. Quicksilver Gargantuan's "except it's 7/7" PROVIDES
  -- values, so CR 707.9d takes the Goyf's CDA and the Gargantuan is deaf to the
  -- graveyards; the Metamorph's "in addition to its other types" is the
  -- carve-out's own wording, so the CDA came across and its copy moves when a
  -- second card type reaches a graveyard.
  --
  -- The graveyards move AFTER both copies entered, which is what makes the CDA
  -- claim bite: at entry both readings of the Metamorph agree at 1/2, and only
  -- the later board tells "kept the ability" from "kept the number".
  Spec.it s "a type exception keeps the copied CDA where a value exception does not (CR 707.9d)" $ do
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    gargantuan <- S.printingOf s registry "Quicksilver Gargantuan"
    metamorph <- S.printingOf s registry "Phyrexian Metamorph"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        -- One card type in a graveyard: the Goyf's CDA is 1/2.
        (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
        (goyfId, board) = S.addPermanent tarmogoyf S.alice withBolt
        (_, stagedGargantuan) = S.spellOnStack gargantuan S.alice board
        withGargantuan = resolveAndSettle (copyNamed goyfId) stagedGargantuan
        (_, stagedMetamorph) = S.spellOnStack metamorph S.alice withGargantuan
        entered = resolveAndSettle (copyNamed goyfId) stagedMetamorph
        -- A second card type reaches a graveyard AFTER both copies entered.
        (_, later) = S.addGraveyardCard piker S.bob entered
        gargantuanId = newest (printedOnBattlefield "Quicksilver Gargantuan" entered)
        metamorphId = newest (printedOnBattlefield "Phyrexian Metamorph" entered)
        -- Read through the Maybe rather than behind a `case` guard, and that is
        -- the mutation talking: the wrong reading of CR 707.9d strips the copied
        -- CDA, which leaves the copy with NO power or toughness at all and a
        -- state-based action takes it off the battlefield (CR 704.5f). A guard
        -- would have absorbed that
        -- into "should be on the battlefield" and the gameplay assertion below
        -- would never have run.
        ptOf oid gs = oid >>= \o -> S.powerToughnessOf o gs
        projected f oid gs = fmap (\o -> f (Projection.project o gs)) oid
    -- THE GAMEPLAY ASSERTION, ahead of every reading taken at entry: the
    -- type-excepted copy is still there and still recomputes the Goyf's CDA.
    Spec.assertEqWith s "the type-excepted copy moves with the graveyards (CR 707.9d)" (ptOf metamorphId later) $ Just (2, 3)
    Spec.assertEqWith s "the original moves with them too" (S.powerToughnessOf goyfId later) $ Just (2, 3)
    -- The control, on the same board and off the same Goyf: the value exception
    -- DID strip the ability, which is CR 707.9d's main sentence.
    Spec.assertEqWith s "the value-excepted copy does not" (ptOf gargantuanId later) $ Just (7, 7)
    -- Diagnostics, after the behaviour: both copies are the Goyf, and only the
    -- Metamorph's gained the card type its card names.
    Spec.assertEqWith s "at entry the type-excepted copy is the bare CDA" (ptOf metamorphId entered) $ Just (1, 2)
    Spec.assertEqWith s "the type-excepted copy is the Goyf by name (CR 707.2)" (fmap (\o -> Projection.namesOf o entered) metamorphId) . Just . Set.singleton . CardName.MkCardName $ Text.pack "Tarmogoyf"
    Spec.assertEqWith s "and an artifact creature" (projected PC.cardTypes metamorphId entered) . Just $ Set.fromList [CardType.Artifact, CardType.Creature]
    Spec.assertEqWith s "where the value-excepted copy is a creature alone" (projected PC.cardTypes gargantuanId entered) . Just $ Set.singleton CardType.Creature

  -- THE PROVING TEST for CR 707.9c, CopyException.DontCopyColors. Vesuvan
  -- Doppelganger {3}{U}{U} Creature -- Shapeshifter 0/0: "You may have this
  -- creature enter as a copy of any creature on the battlefield, except it
  -- doesn't copy that creature's color and it has \"At the beginning of your
  -- upkeep, you may have this creature become a copy of target creature, except
  -- it doesn't copy that creature's color and it has this ability.\"" (Oracle
  -- text checked against api.scryfall.com, 2026-09-12. Scryfall o:"doesn't copy",
  -- 2026-09-12, returns this card and nothing else.)
  --
  -- Doom Blade is the gameplay reader for colour, graveyardTokenCopySpec's:
  -- "Destroy target nonblack creature" is castable only while some creature on
  -- the board is not black. The Doppelganger copies a black Cabal Evangel (2/2),
  -- so the Blade has a target exactly when the copy kept its printed blue.
  --
  -- The control differs in ONE thing: a Clone -- also a blue 0/0 Shapeshifter,
  -- also entering as a copy of that same Evangel, off the same board with the
  -- same two Swamps -- copies the colour, so nothing nonblack stands.
  --
  -- A Clone of the DOPPELGANGER'S COPY is the copiable-values tripwire (CR 707.2
  -- / 707.9c): the retained blue is part of the copy's own copiable values, so a
  -- Clone reads it, where a CR 613 layer-5 write would be left behind.
  Spec.it s "CR 707.9c Vesuvan Doppelganger's copy keeps its own colour, and a Clone of that copy keeps it too" $ do
    swamp <- S.printingOf s registry "Swamp"
    evangel <- S.printingOf s registry "Cabal Evangel"
    vesuvan <- S.printingOf s registry "Vesuvan Doppelganger"
    clone <- S.printingOf s registry "Clone"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (evangelId, board0) = S.addPermanent evangel S.alice (readiedForAlice (S.landsFor swamp S.alice 2 (Setup.emptyGame S.bothPlayers)))
        entering victim printing gs = resolveAndSettle (copyNamed victim) (snd (S.spellOnStack printing S.alice gs))
        entered = entering evangelId vesuvan board0
        control = entering evangelId clone board0
    case (printedOnBattlefield "Vesuvan Doppelganger" entered, clonesOnBattlefield control) of
      ([vesuvanId], [controlId]) -> do
        let cloned = entering vesuvanId clone entered
            (withBlade, bladeId) = S.handOne doomBlade entered
            (controlBlade, controlBladeId) = S.handOne doomBlade control
        -- THE GAMEPLAY ASSERTIONS, ahead of every characteristic read.
        Spec.assertBool s (doomBladeOffered bladeId withBlade) "CR 707.9c the copy did not copy the Evangel's black, so Doom Blade has a target"
        Spec.assertBool s (not (doomBladeOffered controlBladeId controlBlade)) "where a Clone of the same Evangel is black and the Blade has none"
        case clonesOnBattlefield cloned of
          [cloneId] -> Spec.assertEqWith s "CR 707.2 a Clone of the copy reads the retained blue out of its copiable values" (Projection.colorsOf cloneId cloned) (Set.singleton Color.Blue)
          _ -> Spec.assertFailure s "expected one Clone of the Doppelganger's copy"
        -- Diagnostics, after the behaviour: the copy really was the Evangel, and
        -- the control really copied its colour.
        Spec.assertEqWith s "the Doppelganger entered as the Evangel's 2/2" (S.powerToughnessOf vesuvanId entered) (Just (2, 2))
        Spec.assertEqWith s "and it is blue, not the Evangel's black" (Projection.colorsOf vesuvanId entered) (Set.singleton Color.Blue)
        Spec.assertEqWith s "where the Clone without the exception is black" (Projection.colorsOf controlId control) (Set.singleton Color.Black)
      _ -> Spec.assertFailure s "expected one Doppelganger and one Clone, one per board"

  -- CR 707.9c on the OTHER road: the same clause is written inside the ability
  -- the entry exception quotes, so the copy the CR 707.4 BecomeCopy opcode makes
  -- must retain the colour too (Pawl.Engine.Resolve.Effect's BecomeCopy arm,
  -- which reads the subject's own copiable values per subject).
  --
  -- The Doppelganger enters as the black Cabal Evangel (2/2), then its upkeep
  -- trigger copies a black Bog Wraith (3/3): every creature on the board is
  -- black, so Doom Blade has a target only if the SECOND copy retained the blue
  -- as well. Three distinct printed pairs -- 0/0, 2/2, 3/3 -- so the P/T read
  -- below names which copy happened rather than a coincidence.
  Spec.it s "CR 707.9c the ability the copy kept copies again, and that copy keeps the colour too" $ do
    swamp <- S.printingOf s registry "Swamp"
    evangel <- S.printingOf s registry "Cabal Evangel"
    wraith <- S.printingOf s registry "Bog Wraith"
    vesuvan <- S.printingOf s registry "Vesuvan Doppelganger"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (evangelId, board0) = S.addPermanent evangel S.alice (readiedForAlice (S.landsFor swamp S.alice 2 (Setup.emptyGame S.bothPlayers)))
        (wraithId, board1) = S.addPermanent wraith S.alice board0
        entered = resolveAndSettle (copyNamed evangelId) (snd (S.spellOnStack vesuvan S.alice board1))
    case printedOnBattlefield "Vesuvan Doppelganger" entered of
      [vesuvanId] -> do
        let after = readiedForAlice (upkeepForAlice (becomesCopyOf wraithId) entered)
            (withBlade, bladeId) = S.handOne doomBlade after
        -- THE GAMEPLAY ASSERTION, ahead of every characteristic read.
        Spec.assertBool s (doomBladeOffered bladeId withBlade) "CR 707.9c the second copy did not copy the Wraith's black either, so Doom Blade has a target"
        -- Diagnostics, after the behaviour: the second copy really happened, and
        -- the two creatures it was made from really are black.
        Spec.assertEqWith s "CR 707.4 the Doppelganger is the Wraith's 3/3 now" (S.powerToughnessOf vesuvanId after) (Just (3, 3))
        Spec.assertEqWith s "and still blue" (Projection.colorsOf vesuvanId after) (Set.singleton Color.Blue)
        Spec.assertEqWith s "where the Wraith it copied is black" (Projection.colorsOf wraithId after) (Set.singleton Color.Black)
        Spec.assertEqWith s "and so is the Evangel it copied first" (Projection.colorsOf evangelId after) (Set.singleton Color.Black)
      others -> Spec.assertFailure s ("expected exactly one Doppelganger, got " <> show (length others))

  -- Watchful Radstag {2}{G} 2/2 Elk Mutant: evolve, plus "whenever this creature
  -- evolves, create a token that's a copy of it". The copied permanent is the
  -- reserved self slot rather than a target, which is the whole reason this card
  -- reaches CR 608.2h where Cackling Counterpart cannot -- a gone target fizzles
  -- the spell first (CR 608.2b).
  --
  -- Hill Giant 3/3 is the entrant, beating the 2/2 on both axes so the Radstag
  -- evolves. It then carries a +1/+1 counter for the rest of both tests, which is
  -- what makes a token minted off the PROJECTION a 3/3 and CR 707.2's exclusion
  -- of counters observable.
  Spec.it s "Watchful Radstag mints a token copy of itself when it evolves (CR 702.100b, CR 707.2)" $ do
    radstag <- S.printingOf s registry "Watchful Radstag"
    giant <- S.printingOf s registry "Hill Giant"
    let (radstagId, board) = S.addPermanent radstag S.alice (Setup.emptyGame S.bothPlayers)
        (_, entered) = S.entersWithTrigger giant S.alice board
        after = resolveAll declineCopy (settle declineCopy entered)
    Spec.assertEqWith s "the Radstag evolved, so it is a 3/3" (S.powerToughnessOf radstagId after) $ Just (3, 3)
    case tokensOnBattlefield after of
      [tokenId] -> do
        Spec.assertEqWith s "the token is a Radstag" (Projection.namesOf tokenId after) . Set.singleton . CardName.MkCardName $ Text.pack "Watchful Radstag"
        Spec.assertEqWith s "and a 2/2, not the counter-boosted 3/3" (S.powerToughnessOf tokenId after) $ Just (2, 2)
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for #1183, CR 608.2h. Same board, with the Radstag killed
  -- while its own trigger is on the stack: a -5/-5 takes the evolved 3/3 to
  -- -2/-2 and CR 704.5f buries it. The token is still created, and is the
  -- Radstag's COPIABLE values -- so a fallback onto the last known PROJECTION
  -- would mint a -2/-2 that dies at once and leave no token at all.
  Spec.it s "a Radstag killed in response still mints its token copy (CR 608.2h)" $ do
    radstag <- S.printingOf s registry "Watchful Radstag"
    giant <- S.printingOf s registry "Hill Giant"
    let (radstagId, board) = S.addPermanent radstag S.alice (Setup.emptyGame S.bothPlayers)
        (_, entered) = S.entersWithTrigger giant S.alice board
        -- The evolve ability resolves; the settle that follows puts the
        -- Radstag's own "whenever this creature evolves" on the stack.
        onStack = resolveAndSettle declineCopy (settle declineCopy entered)
        shrunk = S.withEffect radstagId (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Type.Literal (-5)) (Quantity.Type.Literal (-5)))) onStack
        dead = settle declineCopy shrunk
        after = resolveAll declineCopy dead
    Spec.assertBool s (not (null (GameState.stack onStack))) "the Radstag's trigger really was on the stack"
    Spec.assertEqWith s "and the Radstag is gone before it resolves" (Set.member radstagId (GameState.battlefield dead)) False
    case tokensOnBattlefield after of
      [tokenId] -> do
        Spec.assertEqWith s "the token is a Radstag all the same" (Projection.namesOf tokenId after) . Set.singleton . CardName.MkCardName $ Text.pack "Watchful Radstag"
        Spec.assertEqWith s "at its copiable 2/2, not the -2/-2 it died at" (S.powerToughnessOf tokenId after) $ Just (2, 2)
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for CR 707.9a's second shape, the exception that names an
  -- ability rather than quoting one: Unstable Shapeshifter {3}{U} Creature --
  -- Shapeshifter 0/1, "Whenever another creature enters, this creature becomes a
  -- copy of that creature, except it has this ability."
  --
  -- Read at GAMEPLAY level over TWO entries. The first copy is what every board
  -- above already shows; the SECOND is the one only the exception can produce,
  -- since a Shapeshifter that took the Hill Giant's abilities and only those has
  -- nothing left to trigger. So 4/3 at the end is "the kept ability fired on the
  -- copy" and 3/3 is "it did not".
  --
  -- A Clone copying the Piker is the control, and it is the copy made WITHOUT the
  -- exception: it entered before the Shapeshifter was placed, so nothing it did
  -- is what stopped it copying again -- it simply never had the ability. It stays
  -- a 2/1 through both entries.
  --
  -- Three distinct printed pairs, so no reading is reached by a coincidence: the
  -- Piker's 2/1, the Hill Giant's 3/3, the Blind-Spot Giant's 4/3.
  Spec.it s "Unstable Shapeshifter keeps the ability that copied and copies again (CR 707.9a)" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    piker <- S.printingOf s registry "Goblin Piker"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    clone <- S.printingOf s registry "Clone"
    let (pikerId, board0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
        (_, stagedClone) = S.spellOnStack clone S.alice board0
        withClone = resolveAndSettle (copyNamed pikerId) stagedClone
        -- The Shapeshifter arrives AFTER the Clone, so the Clone's own entry is
        -- not what it copies and the two copies below are made by one rule each.
        (shifterId, withShifter) = S.addPermanent shapeshifter S.alice withClone
        (_, firstEntry) = S.entersWithTrigger hillGiant S.alice withShifter
        afterFirst = resolveAndSettle (targeting shifterId) (settle (targeting shifterId) firstEntry)
        (_, secondEntry) = S.entersWithTrigger blindSpotGiant S.alice afterFirst
        afterSecond = resolveAndSettle (targeting shifterId) (settle (targeting shifterId) secondEntry)
    case cloneOnBattlefield withClone of
      Nothing -> Spec.assertFailure s "the Clone should be on the battlefield"
      Just cloneId -> do
        -- THE GAMEPLAY ASSERTION, ahead of every diagnostic: the second creature
        -- to enter is what the Shapeshifter is now a copy of, which only the
        -- ability the exception kept can have done.
        Spec.assertEqWith s "the Shapeshifter copied a second creature (CR 707.9a)" (S.powerToughnessOf shifterId afterSecond) $ Just (4, 3)
        Spec.assertEqWith s "and it is the second creature by name (CR 707.2)" (Projection.namesOf shifterId afterSecond) . Set.singleton . CardName.MkCardName $ Text.pack "Blind-Spot Giant"
        -- The control, on the same board: the copy made without the exception
        -- never gains the ability, so neither entry moves it.
        Spec.assertEqWith s "the copy without the exception is still the Piker's 2/1" (S.powerToughnessOf cloneId afterSecond) $ Just (2, 1)
        Spec.assertEqWith s "and it has no triggered ability to have copied with" (length (Projection.triggeredAbilitiesOf cloneId afterSecond)) 0
        Spec.assertEqWith s "and the Piker it copied is untouched" (S.powerToughnessOf pikerId afterSecond) $ Just (2, 1)
        -- Diagnostics, after the behaviour: the first copy really happened, and
        -- the kept ability is on the copy rather than on the printed card.
        Spec.assertEqWith s "the first copy was the Hill Giant's 3/3" (S.powerToughnessOf shifterId afterFirst) $ Just (3, 3)
        Spec.assertEqWith s "with the Hill Giant's abilities plus the kept one" (length (Projection.triggeredAbilitiesOf shifterId afterFirst)) 1

  -- CR 707.2a from the OTHER side of the same rule: the Blood Moon is the COPY.
  -- Copy Enchantment enters as a copy of a Blood Moon, the original is exiled,
  -- and CR 305.7 has to go on applying from the copy alone -- which it can only
  -- do if Pawl.Engine.Projection's set-subtype scan reads the copy's static
  -- abilities rather than Copy Enchantment's printed face.
  --
  -- The original must go, and to EXILE: with two Blood Moons out the original
  -- answers for both, and every other zone is one the projection still reads a
  -- card's static abilities from. Angelic Edict ({4}{W} Sorcery, "Exile target
  -- creature or enchantment") is the only pooled way an enchantment leaves.
  --
  -- TWO victims, because CR 305.7's strip has two readers that must agree.
  -- Mutavault's printed ACTIVATED abilities go inside the layer fold, and Urborg,
  -- Tomb of Yawgmoth's printed STATIC one goes through the hoisted set-subtype
  -- scan that gates a land's own abilities from outside it -- so the Plains that
  -- Urborg would otherwise make a Swamp is what says the scan saw the copy.
  --
  -- Plains pay for the Edict, and being basic they are untouched by either Blood
  -- Moon.
  Spec.it s "CR 707.2a a copy of Blood Moon goes on setting land subtypes once the original is exiled" $ do
    plains <- S.printingOf s registry "Plains"
    mutavault <- S.printingOf s registry "Mutavault"
    urborg <- S.printingOf s registry "Urborg, Tomb of Yawgmoth"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    angelicEdict <- S.printingOf s registry "Angelic Edict"
    let (plainsId, g0) = S.addPermanent plains S.alice (S.landsInPlay plains 6)
        (mutavaultId, g1) = S.addPermanent mutavault S.alice g0
        (_, moonless) = S.addPermanent urborg S.alice g1
        (moonId, g2) = S.addPermanent bloodMoon S.alice moonless
        (_, g3) = S.spellOnStack copyEnchantment S.alice g2
        copied = S.settleSba (S.runPure (copyNamed moonId) g3 Stack.resolveTop)
        (g4, edictId) = S.handOne angelicEdict copied
        cast = S.runPure (aimingAt moonId) g4 (S.cast S.alice edictId)
        exiled = S.settleSba (S.runPure (aimingAt moonId) cast Stack.resolveTop)
        swampy gs = elem Subtype.Swamp (Set.toList (Projection.subtypesOf plainsId gs))
    -- The gameplay-level assertions the case exists for, first: both halves of
    -- CR 305.7 still apply with only the copy left.
    Spec.assertBool s (not (swampy exiled)) "CR 707.2a the copy alone still strips Urborg, so the Plains is no Swamp"
    Spec.assertBool s (elem Subtype.Mountain (Set.toList (Projection.subtypesOf mutavaultId exiled))) "and still makes Mutavault a Mountain"
    Spec.assertEqWith s "CR 305.7 with Mutavault's printed abilities stripped, so it taps for red alone (CR 305.6)" (Mana.manaTypesOf mutavaultId exiled) [ManaType.Colored Color.Red]
    Spec.assertEqWith s "and no activated ability of its own left" (Projection.abilitiesOf mutavaultId exiled) []
    -- The preconditions: the original really is gone, and both victims really had
    -- something to lose before any Blood Moon arrived.
    Spec.assertEqWith s "the original Blood Moon was exiled" (Game.lookupObject moonId exiled) Nothing
    Spec.assertBool s (swampy moonless) "with no Blood Moon out, Urborg makes the Plains a Swamp"
    Spec.assertEqWith s "and Mutavault prints two activated abilities" (length (Projection.abilitiesOf mutavaultId moonless)) 2

-- Append one card of `printing` to `pid`'s hand -- S.handOne overwrites alice's
-- hand, so a second card in it must be appended. Group-local rather than in
-- Pawl.Support: Pawl.CounterspellSpec keeps its own copy of the same shape, and
-- Pawl.Support rebuilds every spec in the tree.
handAppend :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId, GameState.GameState)
handAppend printing pid gs =
  let (printingId, gsP) = Game.intern printing gs
      (oid, gs1) = Game.freshObjectId gsP
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printingId,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.flipped = False,
            Object.exiledFaceDown = False,
            Object.exileLookers = Set.empty,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.controlClock = Map.empty,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColors = Set.empty,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.foretellCostReduction = Nothing,
            Object.warped = Nothing,
            Object.preparedCopyOf = Nothing,
            Object.ringBearerFor = Nothing,
            Object.duplicate = Nothing,
            Object.paired = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.designationValues = Map.empty,
            Object.paidCosts = Map.empty,
            Object.tributePaid = False,
            Object.bestowed = False,
            Object.mutating = False,
            Object.prototyped = False,
            Object.boughtBack = False,
            Object.unannounced = False,
            Object.spliced = Seq.empty,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.Type.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.castUsing = Nothing,
            Object.castGrant = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapFor = 0,
            Object.exertedBy = Set.empty,
            Object.activatedOnce = Map.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )

-- Answer a ChooseTargets by FILTERING the offered set down to one recipient,
-- never by building one: CR 608.2b re-reads what was chosen, and a hand-built
-- Recipient.ToObject of the same permanent is a different recipient that the
-- re-read drops with no error.
pinTarget :: Recipient.Recipient -> Prompt.Prompt r -> r
pinTarget recipient p = case p of
  Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (== recipient) offered) asked
  _ -> S.identityAnswer p

-- The stack's top object, which after a cast is the spell just cast.
topOfStack :: GameState.GameState -> Maybe ObjectId
topOfStack = Maybe.listToMaybe . GameState.stack

-- Resolve one object and settle: CR 704 runs between resolutions, which is
-- where CR 704.5e removes a resolved copy.
resolveOne :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveOne answer gs = snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

-- Synthetic Mimicry's announcement, pinned per slot name and FILTERED out of the
-- offered set for pinTarget's reason: `subject` becomes a copy of `original`.
aimMimicry :: ObjectId -> ObjectId -> Prompt.Prompt r -> r
aimMimicry subjectId originalId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "subject") then subjectId else originalId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

copySpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copySpellSpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  -- CR 707.2 / 715.3d: Synthetic Mimicry makes Battle Display, cast as an
  -- Adventure at the Bonesplitter, a copy of the Bolt. Once it is one it is no
  -- longer an Adventure, so it resolves into the graveyard rather than into
  -- exile. CR 720.3d's Omen rider reads the same face.
  Spec.it s "CR 707.2 an Adventure that becomes a copy of a Bolt goes to the graveyard" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    bolt <- S.printingOf s registry "Lightning Bolt"
    shieldbreaker <- S.printingOf s registry "Embereth Shieldbreaker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    mimicry <- S.printingOf s registry "Synthetic Mimicry"
    let lands = S.landsFor island S.alice 2 (S.landsFor mountain S.alice 2 S.threePlayerGame)
        withArtifact = snd (S.addPermanent bonesplitter S.alice lands)
        (withBolt, boltId) = S.handOne bolt withArtifact
        (displayId, withDisplay) = S.addHandCard shieldbreaker S.alice withBolt
        (mimicryId, board) = S.addHandCard mimicry S.alice withDisplay
        cast1 = snd (Engine.runGamePure S.identityAnswer board (Cast.castSpell S.manaPerformer S.alice displayId (CardName.MkCardName (Text.pack "Battle Display")) Facing.FaceUp))
        cast2 = snd (Engine.runGamePure (pinTarget (Recipient.ToPlayer S.bob)) cast1 (S.cast S.alice boltId))
        graveyardNames gs = Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard S.alice gs)
    case (topOfStack cast1, topOfStack cast2) of
      (Just displaySpell, Just boltSpell) | displaySpell /= boltSpell -> do
        let cast3 = snd (Engine.runGamePure (aimMimicry displaySpell boltSpell) cast2 (S.cast S.alice mimicryId))
            -- Mimicry, then the Bolt, then the copied Battle Display.
            after = resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne S.identityAnswer cast3))
        Spec.assertBool s (CardName.MkCardName (Text.pack "Embereth Shieldbreaker") `elem` graveyardNames after) "the card went to alice's graveyard, not into exile"
        Spec.assertEqWith s "bob took the Bolt's 3 and the copy's 3" (S.lifeOf S.bob after) (Just 14)
      _ -> Spec.assertFailure s "the spells never reached the stack"

  -- CR 707.10: "a copy of a spell is owned by the player under whose control it
  -- was put on the stack ... a copy of a spell or ability is controlled by the
  -- player under whose control it was put on the stack". Twincast states nobody
  -- else, so CopyStackObject.copier is its elided default and the seat is the
  -- copying effect's controller rather than the copied spell's; Meletis
  -- Charlatan is the case that names somebody else.
  --
  -- Renewed Faith ("You gain 6 life") rather than the Bolt above, because the
  -- Bolt cannot show this: its damage lands on a target either way, so a copy
  -- controlled by the wrong player deals the same 3 to the same player. Here the
  -- effect reads "you", so the two readings give alice 26 / bob 26 against alice
  -- 20 / bob 32, and no number is shared.
  Spec.it s "CR 707.10 the copy is controlled by the copying effect's controller" $ do
    island <- S.printingOf s registry "Island"
    twincast <- S.printingOf s registry "Twincast"
    renewedFaith <- S.printingOf s registry "Renewed Faith"
    plains <- S.printingOf s registry "Plains"
    let lands = S.landsFor plains S.bob 3 (S.landsFor island S.alice 2 S.threePlayerGame)
        (withTwincast, twincastId) = S.handOne twincast lands
        (faithId, board) = handAppend renewedFaith S.bob withTwincast
        -- bob CASTS it rather than being handed a stack object: a spell placed
        -- on the stack by hand carries no chosen modes, so nothing about it
        -- resolves and the copy would inherit that emptiness (CR 707.10 copies
        -- the decisions, and there would be none to copy).
        castFaith = snd (Engine.runGamePure S.identityAnswer board (S.cast S.bob faithId))
    case topOfStack castFaith of
      Nothing -> Spec.assertFailure s "Renewed Faith never reached the stack"
      Just faithSpell -> do
        let cast = snd (Engine.runGamePure (pinTarget (Recipient.ToObject faithSpell)) castFaith (S.cast S.alice twincastId))
            -- Twincast, then the copy, then bob's own Renewed Faith.
            after = resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne S.identityAnswer cast))
        Spec.assertEqWith s "alice controls the copy, so alice gains the 6" (S.lifeOf S.alice after) (Just 26)
        Spec.assertEqWith s "bob gains only his own 6" (S.lifeOf S.bob after) (Just 26)
        Spec.assertEqWith s "carol gains nothing" (S.lifeOf S.carol after) (Just 20)
        Spec.assertEqWith s "and the stack is empty" (length (GameState.stack after)) 0
  -- CR 707.9's exception riding CR 707.10's opcode, on Double Major {G}{U}
  -- Instant, "Copy target creature spell you control, except it isn't legendary
  -- if the spell is legendary" (Oracle text verified 2026-09-14) --
  -- CopyException.RemoveSupertypes, the same list BecomeCopy and CreateCopy
  -- carry.
  --
  -- Rograkh, Son of Rohgahh is the copied spell: a {0} Legendary Creature whose
  -- whole text box is keywords, so nothing but CR 704.5j reads the board. The
  -- copy resolves into a token (CR 707.10f) with the same NAME, so the legend
  -- rule is what the exception is visible through: without it alice controls two
  -- legendary permanents named Rograkh and puts one into a graveyard, and the
  -- token ceases to exist (CR 111.7).
  Spec.it s "CR 707.9b Double Major's copy isn't legendary, so CR 704.5j leaves both" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    rograkh <- S.printingOf s registry "Rograkh, Son of Rohgahh"
    doubleMajor <- S.printingOf s registry "Double Major"
    let lands = S.landsFor island S.alice 1 (S.landsFor forest S.alice 1 S.threePlayerGame)
        (rograkhId, withRograkh) = handAppend rograkh S.alice lands
        (majorId, board) = handAppend doubleMajor S.alice withRograkh
        cast1 = S.runPure S.identityAnswer board (S.cast S.alice rograkhId)
    case topOfStack cast1 of
      Nothing -> Spec.assertFailure s "Rograkh never reached the stack"
      Just rograkhSpell -> do
        let castMajor = S.runPure (pinTarget (Recipient.ToObject rograkhSpell)) cast1 (S.cast S.alice majorId)
            -- Double Major, then the copy it put on the stack, then Rograkh.
            after = resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne S.identityAnswer castMajor))
            -- By PROJECTED name (CR 707.2), which is the only read that sees both
            -- the printed card and the token the copy became.
            rograkhs gs = filter (\oid -> Set.member (cardNamed "Rograkh, Son of Rohgahh") (Projection.namesOf oid gs)) (Set.toList (GameState.battlefield gs))
        Spec.assertEqWith s "CR 707.9b / 704.5j both Rograkhs are on the battlefield, the copy not being legendary" (length (rograkhs after)) 2
        case S.tokensOf after of
          [tokenId] -> do
            Spec.assertBool s (not (Set.member Supertype.Legendary (Projection.supertypesOf tokenId after))) "CR 707.9b the token copy lost the supertype the exception named"
            Spec.assertEqWith s "CR 707.2 and kept the copied name, which is what CR 704.5j reads" (Projection.namesOf tokenId after) (Set.singleton (cardNamed "Rograkh, Son of Rohgahh"))
          other -> Spec.assertFailure s ("expected exactly one token copy, got " <> show (length other))
        Spec.assertEqWith s "and the stack is empty" (GameState.stack after) []

-- Resolve until the stack is empty. The two readings of a copy's targets put
-- DIFFERENT numbers of objects on the stack -- one ward trigger more under the
-- rule -- so a fixed count of resolutions would leave the two boards at
-- different depths and the assertion would be reading two different moments.
drainStack :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
drainStack answer =
  let go fuel gs
        | fuel <= 0 || null (GameState.stack gs) = gs
        | otherwise = go (fuel - 1) (resolveOne answer gs)
   in go (10 :: Int)

-- alice holds Stir the Grave and Twincast with three Swamps and two Islands
-- untapped -- exactly {2}{B} and {U}{U}, so neither cast can fail for mana --
-- and her graveyard holds `cards` in the order given. Returns the two hand
-- cards, the graveyard ids and the board, in a main phase so the sorcery is
-- castable.
stirCopyBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (ObjectId, ObjectId, [ObjectId], GameState.GameState)
stirCopyBoard swamp island stir twincast cards =
  let lands = S.landsFor island S.alice 2 (S.landsFor swamp S.alice 3 (Setup.emptyGame S.bothPlayers))
      (withStir, stirId) = S.handOne stir lands
      (twincastId, withBoth) = handAppend twincast S.alice withStir
      add (acc, g) c = let (oid, g2) = S.addGraveyardCard c S.alice g in (acc <> [oid], g2)
      (ids, board) = List.foldl' add ([], withBoth) cards
   in (stirId, twincastId, ids, board {GameState.phase = Phase.PrecombatMain})

-- Announce this X, and answer every target prompt by FILTERING the offered set
-- down to one recipient -- pinTarget's posture, with CR 601.2b's announcement in
-- front of it.
announcing :: Natural.Natural -> Recipient.Recipient -> Prompt.Prompt r -> r
announcing x recipient p = case p of
  Prompt.ChooseX {} -> x
  _ -> pinTarget recipient p

-- CR 707.10's copied announcement, read by the copy's own target slot.
--
-- Stir the Grave ({X}{B} Sorcery) is "return target creature card with mana value
-- X or less from your graveyard to the battlefield", so its slot carries a CR
-- 202.3 computed bound reading the X the caster announced (#2670 built the cast's
-- half). CR 707.10 copies "the value of X", and CR 707.10c then offers new
-- targets -- so the offer the copy's controller is given must be judged inside
-- that same announcement, which is what Resolve.chooseNewTargetsFor seeds from
-- Object.bindings.
--
-- One graveyard, four cards. alice announces X = 2 and names the Piker; the copy
-- is then offered the Evangel (the other mana value 2 creature card) and nothing
-- else. The case below and
-- data/scenarios/copy/cr-707-10c-the-copy-s-new-target-is-judged-against-the.json
-- differ in exactly one thing -- which recipient the copy's prompt is pinned
-- to -- and between them they fix the number at 2: the Evangel
-- is reachable, the mana value 3 card is not, and a bound left unanswered would
-- have admitted neither and elided CR 707.10c's prompt altogether.
--
-- THE NUMBER HAS TWO ROADS HERE, and the OBJECT one answers first, so the seed
-- has no mutation of its own: Quantity.InSlot asks the object the evaluation
-- names before it asks the context, and for a copy that object is the
-- announcement's own holder -- CR 707.10 stamped the value of X onto it, and
-- slotContext evaluates the bound with the copy's id, so `mOid >>= boundOn` is
-- what fires. Resolve.chooseNewTargetsFor also seeds those bindings into
-- Filter.boundAmounts, which is the CR-correct channel and the one every other
-- slot atom reads, but for the X it is dead code: neutralizing the seed leaves
-- this group green. Widening Filter's ManaValueAtMostAmount arm reddens the
-- case below, which is what makes this a proof of the BOUND rather than of
-- either road.
stirCopySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
stirCopySpec s registry =
  let boardOf = do
        swamp <- S.printingOf s registry "Swamp"
        island <- S.printingOf s registry "Island"
        stir <- S.printingOf s registry "Stir the Grave"
        twincast <- S.printingOf s registry "Twincast"
        cards <- traverse (S.printingOf s registry) ["Goblin Piker", "Cabal Evangel", "Kalakscion, Hunger Tyrant", "Russet Wolves"]
        pure (stirCopyBoard swamp island stir twincast cards)
      -- alice casts Stir the Grave for X = 2 at `pikerId`, then -- CR 117.3c,
      -- still holding priority -- Twincast at it, and resolves Twincast so that
      -- CR 707.10c's prompt reaches `retarget`. Then drains the stack: the copy
      -- first, the original under it.
      play retarget pikerId stirId twincastId board =
        let castStir = S.runPure (announcing 2 (Recipient.ToObject pikerId)) board (S.cast S.alice stirId)
         in do
              stirSpell <- topOfStack castStir
              let castTwincast = S.runPure (pinTarget (Recipient.ToObject stirSpell)) castStir (S.cast S.alice twincastId)
              pure (drainStack S.identityAnswer (resolveOne (pinTarget retarget) castTwincast))
   in Spec.describe s "Pawl.Engine.Copy" $ do
        -- The scenario's board and announcement, one recipient different: the
        -- mana value 3 card is the FIRST one an announced 2 excludes, so pinning
        -- the copy there is what fixes the number at 2 rather than at any bound
        -- the Evangel also satisfies. It is never offered, the answer names a
        -- recipient it was not shown, and reject-not-repair leaves the copy on the
        -- Piker. A bound that had stopped narrowing reads this as the Tyrant
        -- arriving.
        Spec.it s "CR 707.10c a card the copied X does not reach is not offered" $ do
          (stirId, twincastId, ids, board) <- boardOf
          case ids of
            [pikerId, _, tyrantId, _] ->
              case play (Recipient.ToObject tyrantId) pikerId stirId twincastId board of
                Nothing -> Spec.assertFailure s "Stir the Grave never reached the stack"
                Just after -> do
                  Spec.assertEqWith s "the mana value 3 creature card is still in the graveyard" (S.countOnBattlefieldByName (cardNamed "Kalakscion, Hunger Tyrant") S.alice after) 0
                  Spec.assertEqWith s "nor did the mana value 4 one move" (S.countOnBattlefieldByName (cardNamed "Russet Wolves") S.alice after) 0
                  Spec.assertEqWith s "the copy kept the Piker, which the original had also named, so it came back once" (S.countOnBattlefieldByName (cardNamed "Goblin Piker") S.alice after) 1
                  Spec.assertEqWith s "and no other graveyard card came back" (S.countOnBattlefieldByName (cardNamed "Cabal Evangel") S.alice after) 0
                  Spec.assertEqWith s "and everything resolved" (length (GameState.stack after)) 0
            _ -> Spec.assertFailure s "fixture should stock alice's graveyard with four cards"

-- alice's Unstable Shapeshifter becomes a copy of `original`, and the original
-- then LEAVES the battlefield.
--
-- The departure is the point. Pawl.Engine.Projection.replacementsAffecting and
-- Pawl.Engine.CombatRestriction.cantBlock are whole-board short-circuits, so
-- while the original is still there its own printed face answers for every
-- permanent and the printed read and the copiable read cannot be told apart. CR
-- 707.2b is what makes the board after its departure legal: "once an object has
-- been copied, changing the copiable values of the original object won't cause
-- the copy to change."
--
-- Every other permanent is chosen to trip neither short-circuit: bob's Cabal
-- Evangel is a black 2/2 with no abilities at all, alice's Giant Spider a green
-- 2/4 whose one keyword (reach) mints nothing, and Setup.emptyGame puts no land
-- down. Returns the Shapeshifter, the Evangel, the Spider and the board.
becameCopyBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, ObjectId, GameState.GameState)
becameCopyBoard shapeshifter evangel spider original =
  let (shifterId, board0) = S.addPermanent shapeshifter S.alice (Setup.emptyGame S.bothPlayers)
      (evangelId, board1) = S.addPermanent evangel S.bob board0
      (spiderId, board2) = S.addPermanent spider S.alice board1
      -- addPermanent alone arranges a board and fires nothing; the original is the
      -- one permanent that ENTERS, which is what raises CR 707.4's trigger.
      (originalId, entered) = S.entersWithTrigger original S.alice board2
      copied = resolveAndSettle S.identityAnswer (settle S.identityAnswer entered)
      gone = S.runPure S.identityAnswer copied (Event.changeZone originalId Zone.Graveyard)
   in (shifterId, evangelId, spiderId, gone)

-- Mark `amount` damage on one permanent through the funnel that consults the
-- replacement effects, then run CR 704's state-based actions.
dealTo :: ObjectId -> ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
dealTo src victim amount gs =
  S.settleSba
    ( S.runPure
        S.identityAnswer
        gs
        (Damage.applyDamage [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) amount False False False 0 Nothing Nothing mempty False DamageKind.Noncombat])
    )

cardNamed :: String -> CardName.CardName
cardNamed = CardName.MkCardName . Text.pack

-- CR 707.2 / 707.2a: "the copiable values are the values derived from the text
-- printed on the object (that text being name, mana cost, color indicator, card
-- type, subtype, supertype, rules text, power, toughness, and/or loyalty)."
-- Readers that asked that question off the COPIER's printed face rather than the
-- copied one, each behind a whole-board short-circuit that a copy could take out
-- entirely.
copiedAbilitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copiedAbilitySpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  -- Site one: the KEYWORD disjunct of Projection.replacementsAffecting's
  -- baseHas. Protection is the only keyword in Keyword.mintsReplacement's set
  -- that mints something a permanent which became a copy AFTER entering can
  -- still use -- every other one mints an entry or turn-up rewrite, which that
  -- permanent's entry is long past.
  --
  -- A PAIR OF SOURCES on ONE board, differing only in colour, so the survival
  -- below is rule 702.16e and not the Apostle's toughness: the black Cabal
  -- Evangel and the red Goblin Piker each deal the same 2 to the same 2/1 copy.
  Spec.it s "CR 707.2a a copy's protection is minted off the COPIED face" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    apostle <- S.printingOf s registry "Apostle of Purifying Light"
    evangel <- S.printingOf s registry "Cabal Evangel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (shifterId, evangelId, pikerId, board) = becameCopyBoard shapeshifter evangel piker apostle
        black = dealTo evangelId shifterId 2 board
        red = dealTo pikerId shifterId 2 board
    Spec.assertBool s (S.onBattlefield shifterId black) "CR 702.16e the copy survives the black source's lethal 2"
    Spec.assertEqWith s "CR 615.6 with nothing marked on it" (S.damageOf shifterId black) (Just 0)
    Spec.assertBool s (not (S.onBattlefield shifterId red)) "and the same 2 from the red source kills it, so 2 really is lethal here"
    -- The fixture's own preconditions, after the behaviour so neither can absorb
    -- a mutation aimed at it.
    Spec.assertEqWith s "the Shapeshifter is the Apostle by name (CR 707.2)" (Projection.namesOf shifterId board) (Set.singleton (cardNamed "Apostle of Purifying Light"))
    Spec.assertEqWith s "and the printed Apostle has left the battlefield, so nothing else answers for the board" (length (printedOnBattlefield "Apostle of Purifying Light" board)) 0

  -- Site two: CombatRestriction.baseCouldMint. Unleash is the ONLY keyword
  -- Keyword.mintsCombatRestriction answers True for.
  --
  -- CR 707.2's last sentence -- "counters ... are not copied" -- is why the
  -- counter is placed by hand: rule 702.98a restricts the permanent "as long as
  -- it has a +1/+1 counter on it", and unleash's own entry replacement fired as
  -- the CHAINWALKER entered, long after the Shapeshifter did. Without the
  -- counter both readings say "can block" and the case is vacuous, which is what
  -- the untouched leg below asserts.
  --
  -- The Spider carries the SAME counter and no unleash, so a reading that
  -- restricted every counter-bearing creature is distinguished too.
  Spec.it s "CR 707.2a a copy's unleash restricts blocking off the COPIED face" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    chainwalker <- S.printingOf s registry "Gore-House Chainwalker"
    evangel <- S.printingOf s registry "Cabal Evangel"
    spider <- S.printingOf s registry "Giant Spider"
    let (shifterId, _, spiderId, board) = becameCopyBoard shapeshifter evangel spider chainwalker
        counted = S.addCounter CounterKind.PlusOnePlusOne 1 spiderId (S.addCounter CounterKind.PlusOnePlusOne 1 shifterId board)
    Spec.assertBool s (not (Combat.canBlock S.alice shifterId counted)) "CR 509.1b / 702.98a the copy with a +1/+1 counter cannot block"
    Spec.assertBool s (Combat.canBlock S.alice spiderId counted) "while the Spider with the same counter can"
    Spec.assertEqWith s "so only the Spider is offered as a blocker" (Combat.legalBlockers S.alice counted) [spiderId]
    -- Rule 702.98a's own condition, which is what keeps the leg above from
    -- passing for a copy that lost blocking outright.
    Spec.assertBool s (Combat.canBlock S.alice shifterId board) "and without the counter the same copy blocks"
    Spec.assertEqWith s "the Shapeshifter is the Chainwalker by name (CR 707.2)" (Projection.namesOf shifterId board) (Set.singleton (cardNamed "Gore-House Chainwalker"))
    Spec.assertEqWith s "and the printed Chainwalker has left the battlefield" (length (printedOnBattlefield "Gore-House Chainwalker" board)) 0

  -- Site three: the PRINTED-replacement disjunct of the same baseHas. Glittering
  -- Lion prints CR 615.1's shield rather than minting it from a keyword, so it
  -- separates this disjunct from the one the first case proves.
  --
  -- The Evangel takes the same kind of damage on the same board and marks it,
  -- which is what says the shield is the Shapeshifter's own rather than a board
  -- on which no damage lands at all. Three against one, so no arithmetic
  -- coincidence pairs the two readings.
  Spec.it s "CR 707.2a a copy's PRINTED replacement effect is gathered too" $ do
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    lion <- S.printingOf s registry "Glittering Lion"
    evangel <- S.printingOf s registry "Cabal Evangel"
    spider <- S.printingOf s registry "Giant Spider"
    let (shifterId, evangelId, _, board) = becameCopyBoard shapeshifter evangel spider lion
        shielded = dealTo evangelId shifterId 3 board
        bystander = dealTo shifterId evangelId 1 board
    Spec.assertEqWith s "CR 615.1 the Lion's printed shield prevents all 3" (S.damageOf shifterId shielded) (Just 0)
    Spec.assertBool s (S.onBattlefield shifterId shielded) "so the 2/2 copy survives what would otherwise be lethal"
    Spec.assertEqWith s "while the Evangel beside it marks its 1" (S.damageOf evangelId bystander) (Just 1)
    Spec.assertEqWith s "the Shapeshifter is the Lion by name (CR 707.2)" (Projection.namesOf shifterId board) (Set.singleton (cardNamed "Glittering Lion"))
    Spec.assertEqWith s "and the printed Lion has left the battlefield" (length (printedOnBattlefield "Glittering Lion" board)) 0

  -- Site four: the TYPE disjunct of that same baseHas (Projection.copiableMintsType),
  -- which reads a card type and a subtype -- both copiable values, CR 707.2 says
  -- so outright -- for CR 306.5b's planeswalker, CR 310.4b's battle and CR
  -- 714.3a's Saga -- one case each. Copy Enchantment reaches the SUBTYPE half,
  -- since a Saga is an enchantment (CR 205.3h); Clever Impersonator's "any
  -- nonland permanent" reaches both equalities of the CARD TYPE half.
  --
  -- TWO ENTRIES, so the board that observes the disjunct differs from the one
  -- that does not in exactly the original Saga's presence. The first Copy
  -- Enchantment enters while History of Benalia is still out, so the PRINTED
  -- Saga answers the whole-board short-circuit and the lore counter goes on
  -- either way. Then the original leaves, and the second copies the first: now
  -- no printed face on the battlefield carries a Saga subtype, and reading the
  -- disjunct off the copier's face gathers nothing at all.
  --
  -- CR 707.2's last sentence keeps the second entry honest -- counters are not
  -- copied -- so the counter it ends with is CR 714.3a's own, minted for it.
  --
  -- `newest` names the SECOND copy on the settled board rather than a pre-settle
  -- one (the planeswalker case below needs that): a Saga with no lore counters is
  -- below its final chapter number, so CR 704.5s does not sacrifice it and the
  -- wrong reading leaves it on the battlefield reporting zero.
  Spec.it s "CR 707.2 a copy of a Saga enters with CR 714.3a's lore counter" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let (benaliaId, board0) = S.addPermanent benalia S.alice (Setup.emptyGame S.bothPlayers)
        (_, staged1) = S.spellOnStack copyEnchantment S.alice board0
        withOriginal = resolveAndSettle (copyNamed benaliaId) staged1
        firstId = newest (printedOnBattlefield "Copy Enchantment" withOriginal)
        gone oid = S.runPure S.identityAnswer withOriginal (Event.changeZone oid Zone.Graveyard)
        second oid = resolveAndSettle (copyNamed oid) (snd (S.spellOnStack copyEnchantment S.alice (gone benaliaId)))
    case firstId of
      Nothing -> Spec.assertFailure s "the first Copy Enchantment left the battlefield unexpectedly"
      Just copy1 -> do
        let final = second copy1
        case newest (printedOnBattlefield "Copy Enchantment" final) of
          Nothing -> Spec.assertFailure s "the second Copy Enchantment left the battlefield unexpectedly"
          Just copy2 -> do
            -- The gameplay-level assertion this case exists for, ahead of every
            -- proxy: the copy of a copy of a Saga is a Saga, so CR 714.3a mints
            -- its entry row even with no printed Saga left on the battlefield.
            Spec.assertEqWith s "CR 714.3a one lore counter on the copy of a copy" (S.counterOf CounterKind.Lore copy2 final) 1
            Spec.assertBool s (elem Subtype.Saga (Set.toList (Projection.subtypesOf copy2 final))) "and it really is a Saga (CR 707.2)"
            Spec.assertEqWith s "CR 707.3 by the copied card's name, not the copier's" (Projection.namesOf copy2 final) (Set.singleton (cardNamed "History of Benalia"))
        -- The preconditions, after the behaviour so neither can absorb a mutation
        -- aimed at it: the first copy got its counter with the printed Saga still
        -- out, and that printed Saga really has left the battlefield.
        Spec.assertEqWith s "the first copy entered with one too, while the original was out" (S.counterOf CounterKind.Lore copy1 withOriginal) 1
        Spec.assertEqWith s "and no printed Saga is left on the battlefield for the second entry" (length (printedOnBattlefield "History of Benalia" (gone benaliaId))) 0

  -- The CARD TYPE half of that same read, on Clever Impersonator ({2}{U}{U}
  -- Creature -- Shapeshifter 0/0, "You may have this creature enter as a copy of
  -- any nonland permanent on the battlefield") and CR 306.5b's loyalty.
  --
  -- The two entries the case above needs, plus a change of controller the legend
  -- rule forces: every printed planeswalker is legendary, so bob copies alice's
  -- Jace and alice copies bob's copy, leaving CR 704.5j one of each per player.
  --
  -- CR 704.5i is what makes the wrong reading loud rather than quiet: a
  -- planeswalker that entered with no loyalty counters is put into its owner's
  -- graveyard the moment state-based actions run. So the id is taken from the
  -- board BEFORE the settle -- otherwise `newest` would answer with the surviving
  -- first copy and read ITS three counters.
  Spec.it s "CR 707.2 a copy of a planeswalker enters with CR 306.5b's loyalty counters" $ do
    jace <- S.printingOf s registry "Jace Beleren"
    impersonator <- S.printingOf s registry "Clever Impersonator"
    let (jaceId, bare) = S.addPermanent jace S.alice (Setup.emptyGame S.bothPlayers)
        board0 = S.addCounter CounterKind.Loyalty 3 jaceId bare
        (_, staged1) = S.spellOnStack impersonator S.bob board0
        withOriginal = resolveAndSettle (copyNamed jaceId) staged1
        gone = S.runPure S.identityAnswer withOriginal (Event.changeZone jaceId Zone.Graveyard)
    case newest (printedOnBattlefield "Clever Impersonator" withOriginal) of
      Nothing -> Spec.assertFailure s "the first Clever Impersonator left the battlefield unexpectedly"
      Just copy1 -> do
        let entered = S.runPure (copyNamed copy1) (snd (S.spellOnStack impersonator S.alice gone)) Stack.resolveTop
            final = settle S.identityAnswer entered
        case newest (printedOnBattlefield "Clever Impersonator" entered) of
          Nothing -> Spec.assertFailure s "the second Clever Impersonator never reached the battlefield"
          Just copy2 -> do
            Spec.assertEqWith s "CR 306.5b three loyalty counters on the copy of a copy" (S.counterOf CounterKind.Loyalty copy2 final) 3
            Spec.assertBool s (S.onBattlefield copy2 final) "so CR 704.5i does not put it into the graveyard"
            Spec.assertBool s (Projection.isPlaneswalkerOf copy2 final) "and it really is a planeswalker (CR 707.2)"
        Spec.assertEqWith s "the first copy entered with three too, while the original was out" (S.counterOf CounterKind.Loyalty copy1 withOriginal) 3
        Spec.assertEqWith s "and no printed planeswalker is left on the battlefield for the second entry" (length (printedOnBattlefield "Jace Beleren" gone)) 0

  -- CR 310.4b's arm of the same read, and the third of Clever Impersonator's
  -- three eligible card types. Invasion of Dominaria is not legendary, so the
  -- controller swap the planeswalker case needs is not forced here -- it is kept
  -- anyway, because a Siege's protector must be an opponent of its controller
  -- (CR 310.12a) and two seats then leave the two copies distinguishable.
  --
  -- CR 704.5v is this case's CR 704.5i: a Siege with defense 0 is put into its
  -- owner's graveyard, so the id is again taken before the settle.
  Spec.it s "CR 707.2 a copy of a battle enters with CR 310.4b's defense counters" $ do
    invasion <- S.printingOf s registry "Invasion of Dominaria"
    impersonator <- S.printingOf s registry "Clever Impersonator"
    let (invasionId, bare) = S.addPermanent invasion S.alice (Setup.emptyGame S.bothPlayers)
        board0 = S.addCounter CounterKind.Defense 5 invasionId bare
        (_, staged1) = S.spellOnStack impersonator S.bob board0
        withOriginal = resolveAndSettle (copyNamed invasionId) staged1
        gone = S.runPure S.identityAnswer withOriginal (Event.changeZone invasionId Zone.Graveyard)
    case newest (printedOnBattlefield "Clever Impersonator" withOriginal) of
      Nothing -> Spec.assertFailure s "the first Clever Impersonator left the battlefield unexpectedly"
      Just copy1 -> do
        let entered = S.runPure (copyNamed copy1) (snd (S.spellOnStack impersonator S.alice gone)) Stack.resolveTop
            final = settle S.identityAnswer entered
        case newest (printedOnBattlefield "Clever Impersonator" entered) of
          Nothing -> Spec.assertFailure s "the second Clever Impersonator never reached the battlefield"
          Just copy2 -> do
            Spec.assertEqWith s "CR 310.4b five defense counters on the copy of a copy" (S.counterOf CounterKind.Defense copy2 final) 5
            Spec.assertBool s (S.onBattlefield copy2 final) "so CR 704.5v does not put it into the graveyard"
        Spec.assertEqWith s "the first copy entered with five too, while the original was out" (S.counterOf CounterKind.Defense copy1 withOriginal) 5
        Spec.assertEqWith s "and no printed battle is left on the battlefield for the second entry" (length (printedOnBattlefield "Invasion of Dominaria" gone)) 0

-- CR 707.10f / 608.3f: a copy of a PERMANENT spell resolves into a token
-- permanent. Lithoform Engine's third ability ("{4}, {T}: Copy target permanent
-- spell you control") is the pool's producer; copyAbilityOnStackSpec below drives
-- its first, and the {3} one is the spell copy Twincast already proves, so all
-- three legs of the printed card are exercised.
--
-- alice holds Nyxborn Rollicker -- data/cards/'s bestow card -- with the Engine
-- on the battlefield, so ONE board reaches both the creature-spell copy (CR
-- 707.10f) and the bestowed-Aura-spell copy (CR 702.103c, whose copy "is also a
-- bestowed Aura spell"). Seven Mountains: {1}{R} for the bestow cost, {4} for
-- the Engine, {R} for the Lightning Bolt the last case casts in response, so no
-- leg fails for mana.
--
-- TWO creatures to enchant, War Mammoth (3/3) and Goblin Piker (2/1), so CR
-- 601.2c's host is a choice and the copy carrying that choice (CR 707.10's "all
-- decisions made for it") is visible on the host it pumps and not on the other.
lithoformBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, ObjectId, ObjectId, ObjectId, GameState.GameState)
lithoformBoard mountain engine piker mammoth rollicker bolt =
  let base = S.landsInPlay mountain 7
      (engineId, gs1) = S.addPermanent engine S.alice base
      (bystander, gs2) = S.addPermanent piker S.alice gs1
      (host, gs3) = S.addPermanent mammoth S.alice gs2
      (gs4, spellId) = S.handOne rollicker gs3
      (boltId, board) = S.addHandCard bolt S.alice gs4
   in (engineId, bystander, host, spellId, boltId, board)

-- CR 601.2b's announcement answered by NAMING a cost, and CR 601.2c's target by
-- FILTERING the offered set -- pinTarget's reason. Pawl.AuraSpec's castingFor,
-- duplicated rather than hoisted.
castingRollicker :: [ManaSymbol.ManaSymbol] -> ObjectId -> Prompt.Prompt r -> r
castingRollicker wanted host p = case p of
  Prompt.ChooseCost _ _ _ candidates ->
    Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just (ManaCost.MkManaCost wanted)) . Cost.Type.mana) candidates)
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just host) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

bestowingRollicker :: ObjectId -> Prompt.Prompt r -> r
bestowingRollicker = castingRollicker [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

-- Every permanent named Nyxborn Rollicker alice has on the battlefield -- by
-- NAME rather than by Source, so a copy that never became a token and one that
-- became the wrong thing are both found and then told apart by Game.isToken.
rollickersOn :: Printing.Printing -> GameState.GameState -> [ObjectId]
rollickersOn printing gs =
  filter
    (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- alice casts `spellId` (`casting` answers its announcements -- bestowed or
-- printed for the Rollicker, CR 601.2b's Phyrexian route for Tamiyo), then -- CR
-- 117.3c, still holding priority -- activates the Engine's {4} ability at it and
-- resolves the ability. Returns the board with [copy, spell] on the stack.
--
-- The {4} ability is picked by its cost rather than by index, so a reordering of
-- the card file cannot silently aim the {3} one at a creature spell and have the
-- activation refuse for want of a target.
copyPermanentSpell :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> ObjectId -> GameState.GameState -> Maybe GameState.GameState
copyPermanentSpell casting engineId spellId board =
  let cast = S.runPure casting board (S.cast S.alice spellId)
      ready = cast {GameState.priority = Just S.alice}
   in do
        spell <- topOfStack cast
        ability <- List.find ((== Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) . Cost.Type.mana . ActivatedAbility.cost) (Projection.abilitiesOf engineId ready)
        let activated = S.runPure (pinTarget (Recipient.ToObject spell)) ready (Activate.activateAbility S.alice engineId ability)
        pure (resolveOne S.identityAnswer activated)

permanentCopySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentCopySpec s registry =
  let boardOf = do
        mountain <- S.printingOf s registry "Mountain"
        engine <- S.printingOf s registry "Lithoform Engine"
        piker <- S.printingOf s registry "Goblin Piker"
        mammoth <- S.printingOf s registry "War Mammoth"
        rollicker <- S.printingOf s registry "Nyxborn Rollicker"
        bolt <- S.printingOf s registry "Lightning Bolt"
        pure (rollicker, lithoformBoard mountain engine piker mammoth rollicker bolt)
   in Spec.describe s "Pawl.Engine.Copy" $ do
        -- CR 702.103e on the COPY: its host is killed in response, so as the copy
        -- begins resolving it ceases to be bestowed and resolves as a creature
        -- spell -- a token creature, attached to nothing. The board is the case
        -- above's plus one Bolt at the host after the ability has resolved.
        Spec.it s "CR 702.103e a bestowed copy whose host died resolves as a token creature" $ do
          (rollicker, (engineId, bystander, host, spellId, boltId, board)) <- boardOf
          case copyPermanentSpell (bestowingRollicker host) engineId spellId board of
            Nothing -> Spec.assertFailure s "the Rollicker never reached the stack, or the Engine offered no {4} ability"
            Just copied -> do
              let bolted = S.runPure (pinTarget (Recipient.ToCreature host)) copied {GameState.priority = Just S.alice} (S.cast S.alice boltId)
                  -- The Bolt, then the copy, then the Rollicker itself.
                  hostDead = resolveOne S.identityAnswer bolted
                  afterCopy = resolveOne S.identityAnswer hostDead
                  afterBoth = resolveOne S.identityAnswer afterCopy
              Spec.assertEqWith s "the Bolt killed the host" (Game.lookupObject host hostDead) Nothing
              Spec.assertEqWith
                s
                "CR 702.103e: the copy resolved as a creature token, attached to nothing"
                (fmap (\oid -> (Game.isToken oid afterCopy, Projection.cardTypesOf oid afterCopy, Object.attachedTo =<< Game.lookupObject oid afterCopy)) (rollickersOn rollicker afterCopy))
                [(True, Set.fromList [CardType.Creature, CardType.Enchantment], Nothing)]
              Spec.assertEqWith
                s
                "keeping Satyr, since it is a creature again"
                (fmap (\oid -> Projection.subtypesOf oid afterCopy) (rollickersOn rollicker afterCopy))
                [Set.singleton Subtype.Satyr]
              Spec.assertEqWith s "and the bystander was never enchanted" (S.powerToughnessOf bystander afterBoth) (Just (2, 1))
              Spec.assertEqWith
                s
                "CR 608.3b: the card resolved as a creature beside it"
                (List.sort (fmap (\oid -> Game.isToken oid afterBoth) (rollickersOn rollicker afterBoth)))
                [False, True]

-- Lithoform Engine's {2} ability, picked by its COST rather than by index, so a
-- reordering of the card file cannot silently aim these cases at the
-- spell-copying legs and have the activation refuse for want of a target --
-- copyPermanentSpell's reason, one ability along.
engineAbilityCopyingAbilities :: ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
engineAbilityCopyingAbilities engineId gs =
  List.find
    ((== Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) . Cost.Type.mana . ActivatedAbility.cost)
    (Projection.abilitiesOf engineId gs)

-- How many cards a player holds (CR 402.1), for the discard reads below.
handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

-- CR 707.10's OTHER two nouns, which Twincast above does not reach: an activated
-- and a triggered ability on the stack, copied by Lithoform Engine's first
-- ability -- "{2}, {T}: Copy target activated or triggered ability you control.
-- You may choose new targets for the copy" (data/cards/lithoform-engine.json,
-- Oracle text verified 2026-09-03).
--
-- CR 707.10b is what makes an ability copy different from a spell copy, and its
-- three sentences split as follows. The FIRST -- "a copy of an ability has the
-- same source as the original ability" -- and the SECOND -- "if the ability
-- refers to its source by name, the copy refers to that same object and not to
-- any other object with the same name" -- share one board: two Longtusk Cubs,
-- whose "Pay {E}{E}: Put a +1/+1 counter on Longtusk Cub" is the pool's activated
-- ability that names its own source, so the copy landing on the OTHER Cub is a
-- readable wrong answer rather than an unobservable one.
--
-- The THIRD -- "the copy is considered to be the same ability by effects that
-- count how many times that ability has resolved during the turn" -- has a board
-- of its own below, Ashling the Pilgrim being the pool's counter of resolutions.
--
-- CR 707.10's "a copy of an activated ability isn't activated" rides on the first
-- case as alice's energy: the cost was paid once, by the activation, and the copy
-- pays nothing. Its other half -- the copy carries no record of what the
-- activation SPENT, the rule's Dawnglow Infusion example putting mana outside the
-- objects-used-to-pay sentence -- is Forsworn Paladin's case in
-- data/scenarios/copy, which is also where CR 602.2a's thisAbility slot is shown
-- naming the copy; the Stifle case below is that slot answering once the
-- original has left the stack.
copyAbilityOnStackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copyAbilityOnStackSpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  -- CR 707.10b's THIRD sentence, which needs a counter of resolutions to be
  -- observable at all: Ashling the Pilgrim's "if this is the third time this
  -- ability has resolved this turn, remove all +1/+1 counters from Ashling, and
  -- it deals that much damage to each creature and each player".
  --
  -- ONE activation resolves first, so the copy is the SECOND resolution and the
  -- original the third. An engine filing the copy under a key of its own leaves
  -- both of them short of three, and the board says so in two places: Ashling
  -- alive with three counters, and bob at 20.
  --
  -- Four Mountains: two for the first activation, two for the second, and the
  -- Engine's {2} off the two its own board leaves -- six in all.
  Spec.it s "CR 707.10b a copy of an activated ability counts toward the same turn's total" $ do
    mountain <- S.printingOf s registry "Mountain"
    engine <- S.printingOf s registry "Lithoform Engine"
    ashling <- S.printingOf s registry "Ashling the Pilgrim"
    let (engineId, withEngine) = S.addPermanent engine S.alice (S.landsInPlay mountain 6)
        (ashlingId, board) = S.addPermanent ashling S.alice withEngine
    case (Maybe.listToMaybe (Projection.abilitiesOf ashlingId board), engineAbilityCopyingAbilities engineId board) of
      (Just pump, Just copier) -> do
        let once = resolveOne S.identityAnswer (S.runPure S.identityAnswer board {GameState.priority = Just S.alice} (Activate.activateAbility S.alice ashlingId pump))
            twice = S.runPure S.identityAnswer once {GameState.priority = Just S.alice} (Activate.activateAbility S.alice ashlingId pump)
        case topOfStack twice of
          Nothing -> Spec.assertFailure s "Ashling's second activation should be on the stack"
          Just abilId -> do
            let staged = S.runPure (pinTarget (Recipient.ToObject abilId)) twice {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
                -- The Engine's ability, then the copy it minted, then Ashling's
                -- own second activation.
                afterEngine = resolveOne S.identityAnswer staged
                afterCopy = resolveOne S.identityAnswer afterEngine
                afterBoth = resolveOne S.identityAnswer afterCopy
            Spec.assertEqWith s "CR 707.10b the original was the third resolution, counting the copy, so each player was dealt the 3 counters removed" (S.lifeOf S.bob afterBoth) (Just 17)
            Spec.assertEqWith s "and Ashling met its own 3 damage as a 1/1 once they came off" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Ashling the Pilgrim")) S.alice afterBoth) 0
            -- Supporting, and after the reads above so it can absorb no mutation
            -- they should catch: the copy was the second resolution and dealt
            -- nothing, leaving two counters standing.
            Spec.assertEqWith s "the copy resolved before the original and was only the second time" (S.counterOf CounterKind.PlusOnePlusOne ashlingId afterCopy) 2
            Spec.assertEqWith s "and bob was untouched at that point" (S.lifeOf S.bob afterCopy) (Just 20)
      _ -> Spec.assertFailure s "Ashling the Pilgrim should declare one activated ability, and Lithoform Engine a {2} one"
  -- The same slot's OTHER reader, on the board that tells the copy's id from the
  -- original's: Stifle counters the original while the copy is still on the
  -- stack, so an aim that named the original names nothing by the time the copy
  -- resolves. CR 707.10b's third sentence still counts the copy, the two sharing
  -- a Source, but the clause has to be able to ASK.
  --
  -- TWO resolutions first, so the copy is the third; bob at 17 and a dead Ashling
  -- are the two places the board says the clause held. A copy whose read answers
  -- nothing leaves bob at 20 and Ashling alive under three counters.
  Spec.it s "CR 707.10b a copy of an activated ability still counts once the original has been countered" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    engine <- S.printingOf s registry "Lithoform Engine"
    ashling <- S.printingOf s registry "Ashling the Pilgrim"
    stifle <- S.printingOf s registry "Stifle"
    -- Eight Mountains: six for three activations of Ashling and two for the
    -- Engine's {2}. bob's one Island casts the Stifle.
    let (engineId, withEngine) = S.addPermanent engine S.alice (S.landsFor island S.bob 1 (S.landsInPlay mountain 8))
        (ashlingId, withAshling) = S.addPermanent ashling S.alice withEngine
        (stifleId, board) = S.addHandCard stifle S.bob withAshling
    case (Maybe.listToMaybe (Projection.abilitiesOf ashlingId board), engineAbilityCopyingAbilities engineId board) of
      (Just pump, Just copier) -> do
        let activate gs = S.runPure S.identityAnswer gs {GameState.priority = Just S.alice} (Activate.activateAbility S.alice ashlingId pump)
            twice = List.foldl' (\gs _ -> resolveOne S.identityAnswer (activate gs)) board [1 .. (2 :: Int)]
            thrice = activate twice
        case topOfStack thrice of
          Nothing -> Spec.assertFailure s "Ashling's third activation should be on the stack"
          Just abilId -> do
            let staged = S.runPure (pinTarget (Recipient.ToObject abilId)) thrice {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
                -- The Engine's ability resolves, leaving the copy above the
                -- original; bob's Stifle then goes on top of both and counters
                -- the original out from under the copy.
                afterEngine = resolveOne S.identityAnswer staged
                cast = S.runPure (pinTarget (Recipient.ToObject abilId)) afterEngine {GameState.priority = Just S.bob} (S.cast S.bob stifleId)
                stifled = resolveOne S.identityAnswer cast
                afterCopy = resolveOne S.identityAnswer stifled
            Spec.assertEqWith s "CR 707.10b the copy was still the third resolution, so the 3 counters came off and each player took 3" (S.lifeOf S.bob afterCopy) (Just 17)
            Spec.assertEqWith s "and Ashling met its own 3 damage as a 1/1 once they came off" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Ashling the Pilgrim")) S.alice afterCopy) 0
            -- Supporting, and after the reads above: the original really was
            -- countered before the copy resolved, so the read could not have
            -- found it.
            Spec.assertBool s (notElem abilId (GameState.stack stifled)) "setup: the Stifle countered the original while the copy was still on the stack"
            Spec.assertBool s (elem abilId (GameState.stack afterEngine)) "setup: and the original was under the copy before the Stifle resolved"
      _ -> Spec.assertFailure s "Ashling the Pilgrim should declare one activated ability, and Lithoform Engine a {2} one"
  -- The Stifle case again on a TRIGGERED ability: Rumor Gatherer's "if this is
  -- the second time this ability has resolved this turn, draw a card instead"
  -- (Oracle text verified Scryfall 2026-09-27). The first Piker's trigger
  -- resolves; the second's is copied and then countered under the copy, so the
  -- copy is the second resolution and has to ask through its OWN thisAbility
  -- slot. A copy whose read answers nothing neither scries nor draws, leaving
  -- alice's hand empty.
  Spec.it s "CR 707.10b a copy of a triggered ability still counts once the original has been countered" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    engine <- S.printingOf s registry "Lithoform Engine"
    gatherer <- S.printingOf s registry "Rumor Gatherer"
    piker <- S.printingOf s registry "Goblin Piker"
    stifle <- S.printingOf s registry "Stifle"
    let (engineId, withEngine) = S.addPermanent engine S.alice (S.landsFor island S.bob 1 (S.landsInPlay mountain 2))
        (_, withGatherer) = S.addPermanent gatherer S.alice withEngine
        stocked = List.foldl' (\g _ -> snd (S.addLibraryCard plains S.alice g)) withGatherer [1 .. (3 :: Int)]
        (firstPiker, g1) = S.addHandCard piker S.alice stocked
        (secondPiker, g2) = S.addHandCard piker S.alice g1
        (stifleId, board) = S.addHandCard stifle S.bob g2
        enter oid gs = S.runPure S.identityAnswer gs {GameState.priority = Just S.alice} (Event.changeZone oid Zone.Battlefield >> Engine.settleForPriority)
        once = resolveOne S.identityAnswer (enter firstPiker board)
        triggered = enter secondPiker once
    case (topOfStack triggered, engineAbilityCopyingAbilities engineId triggered) of
      (Just abilId, Just copier) -> do
        let staged = S.runPure (pinTarget (Recipient.ToObject abilId)) triggered {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
            afterEngine = resolveOne S.identityAnswer staged
            cast = S.runPure (pinTarget (Recipient.ToObject abilId)) afterEngine {GameState.priority = Just S.bob} (S.cast S.bob stifleId)
            stifled = resolveOne S.identityAnswer cast
            afterCopy = resolveOne S.identityAnswer stifled
        Spec.assertEqWith s "CR 707.10b the copy was the second resolution, so alice drew a card" (handSize S.alice afterCopy) 1
        -- Supporting, and after the read above: the original really was
        -- countered before the copy resolved.
        Spec.assertBool s (notElem abilId (GameState.stack stifled)) "setup: the Stifle countered the original while the copy was still on the stack"
        Spec.assertBool s (elem abilId (GameState.stack afterEngine)) "setup: and the original was under the copy before the Stifle resolved"
        Spec.assertEqWith s "setup: both Pikers left alice's hand before the copy resolved" (handSize S.alice stifled) 0
      _ -> Spec.assertFailure s "Rumor Gatherer's trigger should be on the stack, and Lithoform Engine should declare a {2} ability"

-- CR 707.10d, end to end: Zada, Hedron Grinder {3}{R} Legendary Creature --
-- Goblin Ally 3/3, "Whenever you cast an instant or sorcery spell that targets
-- only Zada, copy that spell for each other creature you control that the spell
-- could target. Each copy targets a different one of those creatures."
-- (data/cards/zada-hedron-grinder.json, Oracle text verified 2026-09-03.)
zadaSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
zadaSpec s registry =
  let boardOf = do
        forest <- S.printingOf s registry "Forest"
        zada <- S.printingOf s registry "Zada, Hedron Grinder"
        piker <- S.printingOf s registry "Goblin Piker"
        spider <- S.printingOf s registry "Giant Spider"
        wall <- S.printingOf s registry "Wall of Stone"
        mongoose <- S.printingOf s registry "Blurred Mongoose"
        growth <- S.printingOf s registry "Giant Growth"
        let lands = S.landsFor forest S.alice 1 S.threePlayerGame
            (zadaId, g1) = S.addPermanent zada S.alice lands
            (pikerId, g2) = S.addPermanent piker S.alice g1
            (spiderId, g3) = S.addPermanent spider S.alice g2
            (wallId, g4) = S.addPermanent wall S.alice g3
            (mongooseId, g5) = S.addPermanent mongoose S.alice g4
            (withGrowth, growthId) = S.handOne growth g5
        pure (zadaId, pikerId, spiderId, wallId, mongooseId, growthId, withGrowth)
      -- The Growth is aimed at Zada and at nothing else, which is the trigger's
      -- whole condition; the copies' targets are the effect's and reach no
      -- prompt at all.
      atZada :: ObjectId -> Prompt.Prompt r -> r
      atZada zadaId p = case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((== Just zadaId) . Recipient.objectOf) offered) asked
        _ -> S.identityAnswer p
   in Spec.describe s "Pawl.Engine.Copy" $ do
        -- CR 608.2h through rule 707.10d's "could target": carol Cancels the
        -- Growth while Zada's trigger waits, and the trigger still copies it for
        -- each other creature the Growth, as it last existed, could target.
        Spec.it s "CR 608.2h Zada still copies a Growth countered while the trigger waited" $ do
          (zadaId, pikerId, _, _, _, growthId, base) <- boardOf
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          let (cancelId, board) = S.addHandCard cancel S.carol (S.landsFor island S.carol 3 base)
              placed = snd (Engine.runGamePure (atZada zadaId) (snd (Engine.runGamePure (atZada zadaId) board {GameState.priority = Just S.alice} (S.cast S.alice growthId))) Engine.settleForPriority)
              growthSpell = last (GameState.stack placed)
              cancelled = snd (Engine.runGamePure (atZada growthSpell) placed {GameState.priority = Just S.carol} (S.cast S.carol cancelId))
              after = drainStack (atZada zadaId) cancelled
          Spec.assertEqWith s "CR 608.2h a copy of the countered Growth pumped the Piker" (S.powerToughnessOf pikerId after) (Just (5, 4))
          Spec.assertEqWith s "CR 701.6a the countered Growth itself pumped nothing, and Zada stays 3/3" (S.powerToughnessOf zadaId after) (Just (3, 3))

-- CR 707.10e, end to end: Ivy, Gleeful Spellthief {G}{U} Legendary Creature --
-- Faerie Rogue 2/1, "Flying. Whenever a player casts a spell that targets only a
-- single creature other than Ivy, you may copy that spell. The copy targets
-- Ivy." (data/cards/ivy-gleeful-spellthief.json, Oracle text verified
-- 2026-09-05.)
ivySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ivySpec s registry =
  let growthBoard = do
        forest <- S.printingOf s registry "Forest"
        ivy <- S.printingOf s registry "Ivy, Gleeful Spellthief"
        spider <- S.printingOf s registry "Giant Spider"
        growth <- S.printingOf s registry "Giant Growth"
        let lands = S.landsFor forest S.bob 1 S.threePlayerGame
            (ivyId, g1) = S.addPermanent ivy S.alice lands
            (spiderId, g2) = S.addPermanent spider S.bob g1
            (growthId, g3) = S.addHandCard growth S.bob g2
        pure (ivyId, spiderId, growthId, g3)
      -- What each object on the stack targets, top first, read live off its
      -- bindings the way Pawl.Engine.Resolve.Effect.targetsOnStack does, less CR
      -- 201.5's reserved self slot, the sibling helper's reason.
      stackTargets gs = fmap (\oid -> Set.toList (Foldable.fold (Map.elems (Map.delete Binding.triggerSource (Binding.targetsOf (maybe Map.empty Object.bindings (Game.lookupObject oid gs))))))) (GameState.stack gs)
   in Spec.describe s "Pawl.Engine.Copy" $ do
        -- CR 608.2h over a spell that has left the stack: carol Cancels the
        -- Growth while Ivy's trigger waits, and the trigger still copies it, as
        -- the Growth last existed -- Ivy's ruling (2022-09-09). CR 707.10e's
        -- "could target" is that last-known spell's, so the copy is aimed at Ivy.
        Spec.it s "CR 608.2h the copy is made even when the Growth was countered first" $ do
          (ivyId, spiderId, growthId, base) <- growthBoard
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          let (cancelId, board) = S.addHandCard cancel S.carol (S.landsFor island S.carol 3 base)
              -- bob aims the Growth at his Spider and carol the Cancel at the
              -- Growth's stack object (CR 400.7 gave it a new id), each FILTERED
              -- out of the offered set.
              aimed :: [ObjectId] -> Prompt.Prompt r -> r
              aimed victims p = case p of
                Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter (maybe False (`elem` victims) . Recipient.objectOf) offered) asked
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                _ -> S.identityAnswer p
              cast = snd (Engine.runGamePure (aimed [spiderId]) board {GameState.priority = Just S.bob} (S.cast S.bob growthId))
              triggered = snd (Engine.runGamePure (aimed [spiderId]) cast Engine.settleForPriority)
              growthSpell = last (GameState.stack triggered)
              answer :: Prompt.Prompt r -> r
              answer = aimed [spiderId, growthSpell]
              cancelled = snd (Engine.runGamePure answer triggered {GameState.priority = Just S.carol} (S.cast S.carol cancelId))
              -- The Cancel, then Ivy's trigger: the moment the copy either
              -- exists or does not, with the Growth already in bob's graveyard.
              afterTrigger = resolveOne answer (resolveOne answer cancelled)
              after = drainStack answer afterTrigger
          Spec.assertEqWith s "CR 608.2h the copy of the countered Growth pumped Ivy" (S.powerToughnessOf ivyId after) (Just (5, 4))
          Spec.assertEqWith s "CR 707.10e the copy alone was on the stack, naming Ivy" (fmap (Maybe.mapMaybe Recipient.objectOf) (stackTargets afterTrigger)) [[ivyId]]
          Spec.assertEqWith s "CR 701.6a the countered Growth pumped nothing, and the Spider stays 2/4" (S.powerToughnessOf spiderId after) (Just (2, 4))

-- CR 115.1's "targets only a single ..." NARROWED by a description of the one
-- target, end to end: Leyline of Resonance {2}{R}{R} Enchantment, "If this card
-- is in your opening hand, you may begin the game with it on the battlefield.
-- Whenever you cast an instant or sorcery spell that targets only a single
-- creature you control, copy that spell. You may choose new targets for the
-- copy." (data/cards/leyline-of-resonance.json, Oracle text verified
-- 2026-09-05.)
--
-- The spell alice casts is Angelic Edict, whose slot names CR 110.1's PERMANENT
-- pool, so its one target is a Recipient.ToObject. That is deliberate: the
-- condition is answered off the TARGET's own view, so a "target permanent" spell
-- aimed at a creature satisfies "a single creature" exactly as a "target
-- creature" spell does, which is what the printed template asks and what the
-- recipient tag alone could not say.
--
-- Two boards differing in ONE thing -- which creature the Edict names -- carry
-- the "you control" half: alice's own Spider on one, bob's Piker on the other,
-- with carol sitting out so the three roles stay apart.
leylineOfResonanceSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
leylineOfResonanceSpec s registry =
  let boardOf = do
        plains <- S.printingOf s registry "Plains"
        leyline <- S.printingOf s registry "Leyline of Resonance"
        spider <- S.printingOf s registry "Giant Spider"
        berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
        piker <- S.printingOf s registry "Goblin Piker"
        edict <- S.printingOf s registry "Angelic Edict"
        let lands = S.landsFor plains S.alice 5 S.threePlayerGame
            (_, g1) = S.addPermanent leyline S.alice lands
            (spiderId, g2) = S.addPermanent spider S.alice g1
            (berserkersId, g3) = S.addPermanent berserkers S.alice g2
            (pikerId, g4) = S.addPermanent piker S.bob g3
            (edictId, g5) = S.addHandCard edict S.alice g4
        pure (spiderId, berserkersId, pikerId, edictId, g5)
      -- Pin one announcement by FILTERING the offered set, never by building a
      -- recipient: CR 608.2b re-reads what was chosen. Reaches the cast and CR
      -- 707.10c's re-target prompt alike, so each phase below is run with its
      -- own.
      aimAt :: ObjectId -> Prompt.Prompt r -> r
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) asked
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- alice casts the Edict at `victim`, CR 603.3b puts any trigger above it,
      -- and that trigger alone resolves -- the moment the copy either exists or
      -- does not. CR 707.10c's prompt then aims the copy at `newTarget`.
      afterTrigger victim newTarget edictId board =
        let cast = snd (Engine.runGamePure (aimAt victim) board {GameState.priority = Just S.alice} (S.cast S.alice edictId))
         in resolveOne (aimAt newTarget) (snd (Engine.runGamePure S.identityAnswer cast Engine.settleForPriority))
   in Spec.describe s "Pawl.Engine.Copy" . Spec.it s "CR 115.1 the trigger reads the one target's own description, not the pool its slot named" $ do
        (spiderId, berserkersId, pikerId, edictId, board) <- boardOf
        let ownRun = drainStack S.identityAnswer (afterTrigger spiderId berserkersId edictId board)
            othersRun = drainStack S.identityAnswer (afterTrigger pikerId berserkersId edictId board)
            standing gs = (onBattlefield spiderId gs, onBattlefield berserkersId gs, onBattlefield pikerId gs)
        -- The fixture's own precondition: all three creatures are on the
        -- battlefield to start with, so every read below is of a change.
        Spec.assertEqWith s "all three creatures start on the battlefield" (standing board) (True, True, True)
        Spec.assertEqWith s "CR 115.1 alice's Spider is one creature she controls, so the copy exiled the Berserkers too" (standing ownRun) (False, False, True)
        Spec.assertEqWith s "CR 115.1 bob's Piker is not, so no copy was made and both of alice's creatures stand" (standing othersRun) (True, True, False)
        Spec.assertBool s (spiderId /= berserkersId && berserkersId /= pikerId) "the three creatures are distinct objects"

-- CR 613.2b: layer 1b (face-down) applies after layer 1a (copy effects), so CR
-- 708.2's listed characteristics replace what a copy effect stamped rather than
-- being replaced by it. A permanent that is both a copy and face down therefore
-- has no name and no abilities, and its copy stamp rides underneath for CR 708.8
-- to revert to.
--
-- Silent Arbiter ({4} Artifact Creature -- Construct 1/5, "No more than one
-- creature can attack each combat. No more than one creature can block each
-- combat") is the card, and its attack bound is what makes the rule readable off
-- a DECLARATION: that bound is one of CR 613.11's fourteen rule-affecting families,
-- which Projection.ruleAbilitiesOf gathers off the copiable snapshot and no other
-- read reaches. Cyber Conversion ({U}{U} Instant, "Turn target creature face
-- down. It's a 2/2 Cyberman artifact creature") is the turner, chosen over
-- Ixidron because it names ONE creature: the two Goblin Pikers that carry the
-- declaration must stay face up, or their own facing would be a second thing the
-- pair of boards differs in.
--
-- THE PRINTED ARBITER IS DESTROYED before either leg runs. The bound is global
-- (Pawl.CombatEffectSpec's BoundedDeclaration group proves that), so an Arbiter
-- left standing would hold alice to one attacker whatever the Clone answered.
faceDownCopyBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe (GameState.GameState, ObjectId, ObjectId, ObjectId, ObjectId)
faceDownCopyBoard island arbiter clone piker cyber =
  let gs0 = S.landsFor island S.bob 2 (Setup.emptyGame S.bothPlayers)
      (arbiterId, gs1) = S.addPermanent arbiter S.alice gs0
      (one, gs2) = S.addPermanent piker S.alice gs1
      (two, gs3) = S.addPermanent piker S.alice gs2
      (_, staged) = S.spellOnStack clone S.alice gs3
      resolved = resolveAndSettle (copyNamed arbiterId) staged
      killed = S.runPure S.identityAnswer resolved (Event.destroy Regenerability.Regenerable [arbiterId])
      (withSpell, spell) = S.handOne cyber killed
   in fmap (\cloneId -> (withSpell, spell, cloneId, one, two)) (cloneOnBattlefield killed)

faceDownCopySpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
faceDownCopySpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  -- THE PROVING TEST for the fourteen rule-affecting families, read through
  -- Pawl.Engine.CombatRestriction off Projection.ruleAbilitiesOf.
  --
  -- THE PAIR: one board, one spell, and the only difference is whether bob's
  -- Conversion resolved. The single-Piker declaration is asserted legal on BOTH
  -- legs, so the refusal on the face-up leg is the bound talking rather than
  -- summoning sickness, a tap or a missing defender.
  Spec.it s "CR 708.2 a face-down copy of Silent Arbiter no longer bounds the attack" $ do
    island <- S.printingOf s registry "Island"
    arbiter <- S.printingOf s registry "Silent Arbiter"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    cyber <- S.printingOf s registry "Cyber Conversion"
    case faceDownCopyBoard island arbiter clone piker cyber of
      Nothing -> Spec.assertFailure s "the Clone should be on the battlefield as a copy of the Arbiter"
      Just (board, spell, cloneId, one, two) -> do
        let down = intoCombat (S.runPure (aimByFiltering cloneId) board (S.cast S.bob spell >> Stack.resolveTop))
            up = intoCombat board
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [one, two] down) "CR 708.2 the face-down copy has no abilities, so both Pikers attack"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [one, two] up)) "CR 707.2a face up, the same copy holds alice to one attacker"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [one] down) "CR 508.1a one Piker attacks on the face-down board"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [one] up) "and on the face-up board too, so the refusal above is the bound"
        -- The proxies, after the behaviours.
        Spec.assertBool s (maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject cloneId down)) "setup: the Conversion turned the copy face down"
        Spec.assertEqWith s "setup: and it is still face up on the other leg" (fmap Object.facing (Game.lookupObject cloneId up)) (Just Facing.FaceUp)
        Spec.assertEqWith s "setup: the printed Arbiter is gone, so the bound on the face-up board is the copy's" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Silent Arbiter")) S.alice up) 0
        Spec.assertBool s (Maybe.isJust (Binding.copyOf . Object.bindings =<< Game.lookupObject cloneId down)) "CR 708.8 the copy stamp rides through underneath the listing, ready to be reverted to"

-- alice with one copy of `card` in her graveyard and `lands` untapped, holding
-- priority in her main phase with an empty stack, so CR 602.5d's sorcery timing
-- is met. `twins` more copies of `card` sit on her battlefield.
graveyardCopyBoard :: Printing.Printing -> Printing.Printing -> Int -> Int -> (ObjectId, [ObjectId], GameState.GameState)
graveyardCopyBoard card land lands twins =
  let add (ids, gs) _ = let (oid, gs') = S.addPermanent card S.alice gs in (ids <> [oid], gs')
      (twinIds, withTwins) = List.foldl' add ([], S.landsInPlay land lands) [1 .. twins]
      (gyId, withCard) = S.addGraveyardCard card S.alice withTwins
   in ( gyId,
        twinIds,
        withCard
          { GameState.priority = Just S.alice,
            GameState.activePlayer = S.alice,
            GameState.phase = Phase.PostcombatMain
          }
      )

-- Activate the one ability the graveyard card offers and resolve it. The roster
-- is matched on exactly one ability, so a board that offered none or two comes
-- back unchanged and fails the token match rather than passing elsewhere.
activateFromGraveyard :: ObjectId -> GameState.GameState -> GameState.GameState
activateFromGraveyard gyId gs = case Activatable.abilitiesFor gyId gs of
  [ability] -> S.runPure S.identityAnswer gs (Activate.activateAbility S.alice gyId ability >> Stack.resolveTop)
  _ -> gs

isActivationOf :: ObjectId -> A.Action -> Bool
isActivationOf oid a = case a of
  A.Activate o _ -> o == oid
  _ -> False

-- Is Doom Blade, sitting in alice's hand, castable? CR 601.2c: only with a legal
-- target, and its one slot is "target nonblack creature".
doomBladeOffered :: ObjectId -> GameState.GameState -> Bool
doomBladeOffered bladeId gs =
  any
    ( \a -> case a of
        A.Cast o _ _ -> o == bladeId
        _ -> False
    )
    (Action.legalActions S.alice gs)

-- CR 702.128a and CR 702.129a, Pawl.Engine.Keyword.graveyardTokenCopy: the card
-- is exiled as a cost and a token copy of it is created with CR 707.9b's
-- exceptions -- colour, no mana cost, Zombie, and eternalize's 4/4.
--
-- Tah-Crop Skirmisher {1}{U} Creature -- Snake Warrior 2/1, "Embalm {3}{U}", and
-- Proven Combatant {U} Creature -- Human Warrior 1/1, "Eternalize {4}{U}{U}"
-- (Oracle text checked against Scryfall): blue cards with a mana cost, so the
-- white or black token with none differs on every excepted characteristic.
--
-- A Clone of each token is the copiable-values tripwire (CR 707.2 / 707.9b): it
-- takes the exceptions with it, where a Clone of the printed card beside it on
-- the same board is the control.
graveyardTokenCopySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardTokenCopySpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  Spec.it s "CR 702.128a embalm exiles the card for a white Zombie token copy with no mana cost, and a Clone of it keeps all three" $ do
    island <- S.printingOf s registry "Island"
    skirmisher <- S.printingOf s registry "Tah-Crop Skirmisher"
    clone <- S.printingOf s registry "Clone"
    let (gyId, twinIds, board) = graveyardCopyBoard skirmisher island 8 1
        embalmed = activateFromGraveyard gyId board
        zombieSnakeWarrior = Set.fromList [Subtype.Snake, Subtype.Warrior, Subtype.Zombie]
    Spec.assertBool s (any (isActivationOf gyId) (Action.legalActions S.alice board)) "embalm is offered from the graveyard"
    -- CR 113.6m: the Skirmisher on the battlefield HAS the ability too, and the
    -- same Islands would pay its mana, but its cost exiles a graveyard card.
    Spec.assertBool s (not (any (\a -> any (`isActivationOf` a) twinIds) (Action.legalActions S.alice board))) "CR 113.6m but not from the battlefield"
    -- CR 602.5d: the same board a step later, so only the timing differs.
    Spec.assertBool
      s
      (not (any (isActivationOf gyId) (Action.legalActions S.alice (board {GameState.phase = Phase.Ending EndingStep.EndStep}))))
      "CR 602.5d but not in the end step"
    case (tokensOnBattlefield embalmed, twinIds) of
      ([tokenId], [twinId]) -> do
        Spec.assertEqWith s "CR 707.9b the token is white, not the card's blue" (Projection.colorsOf tokenId embalmed) (Set.singleton Color.White)
        Spec.assertEqWith s "CR 202.3a with no mana cost its mana value is 0" (PC.manaValue (Projection.project tokenId embalmed)) (Just 0)
        Spec.assertEqWith s "and it has no mana cost" (PC.manaCost (Projection.project tokenId embalmed)) Nothing
        Spec.assertEqWith s "CR 205.1b a Zombie in addition to its other types" (Projection.subtypesOf tokenId embalmed) zombieSnakeWarrior
        Spec.assertEqWith s "CR 707.2 otherwise the card: its 2/1" (S.powerToughnessOf tokenId embalmed) (Just (2, 1))
        Spec.assertEqWith s "the card was exiled to pay the cost" (length (Game.zoneMembers Zone.Exile S.alice embalmed)) 1
        Spec.assertEqWith s "and left the graveyard" (Game.zoneMembers Zone.Graveyard S.alice embalmed) []
        let cloned = castAndResolve (copyNamed tokenId) clone embalmed
            control = castAndResolve (copyNamed twinId) clone embalmed
        case (cloneOnBattlefield cloned, cloneOnBattlefield control) of
          (Just cloneId, Just controlId) -> do
            Spec.assertEqWith s "CR 707.9b a Clone of the token is white" (Projection.colorsOf cloneId cloned) (Set.singleton Color.White)
            Spec.assertEqWith s "CR 707.9b with mana value 0" (PC.manaValue (Projection.project cloneId cloned)) (Just 0)
            Spec.assertEqWith s "CR 707.9b and a Zombie" (Projection.subtypesOf cloneId cloned) zombieSnakeWarrior
            Spec.assertEqWith s "where a Clone of the card is blue" (Projection.colorsOf controlId control) (Set.singleton Color.Blue)
            Spec.assertEqWith s "with mana value 2" (PC.manaValue (Projection.project controlId control)) (Just 2)
            Spec.assertEqWith s "and no Zombie" (Projection.subtypesOf controlId control) (Set.fromList [Subtype.Snake, Subtype.Warrior])
          _ -> Spec.assertFailure s "both Clones should be on the battlefield"
      (tokens, _) -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- Doom Blade is the gameplay reader for colour: "Destroy target nonblack
  -- creature" has a target only while something on the board is not black. The
  -- two boards differ in one Proven Combatant on the battlefield, which is what
  -- makes the negative mean the tokens are black rather than that the Blade is
  -- uncastable.
  Spec.it s "CR 702.129a eternalize makes a black 4/4 Zombie token copy, and neither it nor a Clone of it is a Doom Blade target" $ do
    island <- S.printingOf s registry "Island"
    swamp <- S.printingOf s registry "Swamp"
    combatant <- S.printingOf s registry "Proven Combatant"
    clone <- S.printingOf s registry "Clone"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let (gyId, _, board) = graveyardCopyBoard combatant island 10 0
        eternalized = activateFromGraveyard gyId board
    case tokensOnBattlefield eternalized of
      [tokenId] -> do
        let cloned = castAndResolve (copyNamed tokenId) clone eternalized
            (withBlade, bladeId) = S.handOne doomBlade (S.landsFor swamp S.alice 2 cloned)
            (_, withTwin) = S.addPermanent combatant S.alice withBlade
        -- THE GAMEPLAY ASSERTIONS, ahead of every characteristic read.
        Spec.assertBool s (not (doomBladeOffered bladeId withBlade)) "CR 707.9b the token and its Clone are black, so Doom Blade has no target"
        Spec.assertBool s (doomBladeOffered bladeId withTwin) "where a Proven Combatant beside them is a target"
        Spec.assertEqWith s "CR 707.9b the token is 4/4, not 1/1" (S.powerToughnessOf tokenId eternalized) (Just (4, 4))
        Spec.assertEqWith s "CR 202.3a with mana value 0" (PC.manaValue (Projection.project tokenId eternalized)) (Just 0)
        Spec.assertEqWith s "CR 205.1b and a Zombie Human Warrior" (Projection.subtypesOf tokenId eternalized) (Set.fromList [Subtype.Human, Subtype.Warrior, Subtype.Zombie])
        case cloneOnBattlefield cloned of
          Just cloneId -> do
            Spec.assertEqWith s "CR 707.9b a Clone of the token is 4/4 too" (S.powerToughnessOf cloneId cloned) (Just (4, 4))
            Spec.assertEqWith s "and has mana value 0" (PC.manaValue (Projection.project cloneId cloned)) (Just 0)
          Nothing -> Spec.assertFailure s "the Clone should be on the battlefield"
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

-- CR 707.2's token copy entering tapped and attacking (CR 110.5b, CR 508.4), and
-- named later in the same resolution (CR 603.7c): Flamerush Rider, "Whenever this
-- creature attacks, create a token that's a copy of another target attacking
-- creature and that's tapped and attacking. Exile the token at end of combat."
--
-- alice attacks bob with the 3/3 Rider and a 2/1 Goblin Piker; the trigger's one
-- legal target is the Piker, so the token is a second 2/1 Piker.
flamerushRiderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
flamerushRiderSpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  Spec.it s "CR 508.4 Flamerush Rider's copy of the Piker enters tapped and attacking, deals damage, and is exiled at end of combat" $ do
    rider <- S.printingOf s registry "Flamerush Rider"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [rider, piker] []
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        tokens = S.tokensOf atBlockers
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith
      s
      "CR 110.5b / CR 508.4 one Piker token, tapped and attacking bob"
      (fmap (\oid -> (PC.names (Projection.project oid atBlockers), fmap Object.tapped (Game.lookupObject oid atBlockers), Map.lookup oid (Combat.Type.attackers (GameState.combat atBlockers)))) tokens)
      [(Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")), Just TapState.Tapped, Just (AttackTarget.OfPlayer S.bob))]
    Spec.assertEqWith s "CR 510.1b bob takes 3 from the Rider, 2 from the Piker and 2 from its copy" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith s "CR 603.7c the token named by the trigger is exiled at end of combat" (S.tokensOf after) []

-- CR 707.13 on Garth One-Eye {W}{U}{B}{R}{G}, "{T}: Choose a card name that
-- hasn't been chosen from among Disenchant, Braingeyser, Terror, Shivan Dragon,
-- Regrowth, and Black Lotus. Create a copy of the card with the chosen name. You
-- may cast the copy. (You still pay its costs.)" (Oracle text and rulings checked
-- on Scryfall, 2026-09-23).
--
-- Black Lotus costs {0}, so every board has NO MANA: a copy the engine mistook
-- for a graveyard card would be taxed {2} by bob's Aven Interrupter and not be
-- castable at all, and Grafdigger's Cage would forbid it outright.
garthSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
garthSpec s registry = Spec.describe s "GarthOneEye" $ do
  Spec.it s "CR 707.13 Garth One-Eye's cast copy of Black Lotus has Black Lotus's characteristics and resolves as a token" $ do
    garth <- S.printingOf s registry "Garth One-Eye"
    lotus <- S.printingOf s registry "Black Lotus"
    let reference = [lotus]
        (garthId, board) = garthBoard garth (Setup.emptyGame S.bothPlayers)
        cast = activateGarth (naming reference lotusName OptionalDecision.Exercises) garthId board
        spell = List.find (\oid -> Maybe.isJust (Game.lookupObject oid cast)) (GameState.stack cast)
        resolved = S.settleSba (S.runPure (naming reference lotusName OptionalDecision.Exercises) cast Stack.resolveTop)
        lotuses = filter (\oid -> PC.names (Projection.project oid resolved) == Set.singleton lotusName) (Set.toList (GameState.battlefield resolved))
    Spec.assertEqWith
      s
      "the spell on the stack is Black Lotus: its name, its types and its {0}"
      (fmap (\sid -> let pc = Projection.project sid cast in (PC.names pc, PC.cardTypes pc, PC.manaCost pc)) spell)
      (Just (Set.singleton lotusName, Set.singleton CardType.Artifact, Just (ManaCost.MkManaCost [])))
    Spec.assertEqWith s "CR 601.2a it was cast from no zone" (fmap (\sid -> Game.lookupObject sid cast >>= Object.castFrom) spell) (Just Nothing)
    Spec.assertEqWith s "CR 608.3f the resolved Lotus is a token" (fmap (`Game.isToken` resolved) lotuses) [True]
    Spec.assertEqWith s "and nothing is left outside the game" (GameState.outsideCopies resolved) Set.empty

  Spec.it s "CR 707.13 a second activation of the same Garth does not offer Black Lotus again" $ do
    garth <- S.printingOf s registry "Garth One-Eye"
    lotus <- S.printingOf s registry "Black Lotus"
    let reference = [lotus]
        answer :: Prompt.Prompt r -> r
        answer = naming reference lotusName OptionalDecision.Exercises
        (garthId, board) = garthBoard garth (Setup.emptyGame S.bothPlayers)
        first = S.runPure answer (activateGarth answer garthId board) Stack.resolveTop
        untapped = first {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Untapped}) garthId (GameState.objects first)}
        second = activateGarth answer garthId untapped
    Spec.assertEqWith s "the first activation cast the Lotus copy" (lotusesOnStack first + length (lotusesOnBattlefield first)) 1
    Spec.assertEqWith s "the second, named Black Lotus again, cast nothing" (lotusesOnStack second) 0
    Spec.assertEqWith s "Black Lotus is remembered for this Garth" (Map.lookup garthId (GameState.namedCopyChoices second)) (Just (Set.singleton lotusName))

  Spec.it s "CR 707.13 a declined copy leaves nothing behind, and a name the reference does not know makes no copy" $ do
    garth <- S.printingOf s registry "Garth One-Eye"
    lotus <- S.printingOf s registry "Black Lotus"
    let (garthId, board) = garthBoard garth (Setup.emptyGame S.bothPlayers)
        declined = activateGarth (naming [lotus] lotusName OptionalDecision.Declines) garthId board
        unknown = activateGarth (naming [] lotusName OptionalDecision.Exercises) garthId board
        copies gs = filter (\o -> case Object.source o of Source.OfCardCopy _ -> True; _ -> False) (Map.elems (GameState.objects gs))
    Spec.assertEqWith s "declined: no copy of a card exists anywhere" (length (copies declined), GameState.outsideCopies declined) (0, Set.empty)
    Spec.assertEqWith s "declined: the name is still spent" (Map.lookup garthId (GameState.namedCopyChoices declined)) (Just (Set.singleton lotusName))
    Spec.assertEqWith s "unknown: nothing was cast" (length (GameState.stack unknown), length (copies unknown)) (0, 0)

lotusName :: CardName.CardName
lotusName = CardName.MkCardName (Text.pack "Black Lotus")

-- alice's settled Garth in her main phase, with priority.
garthBoard :: Printing.Printing -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
garthBoard garth gs =
  let (garthId, withGarth) = S.addPermanent garth S.alice gs
   in ( garthId,
        withGarth
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Activates Garth's one ability and resolves it.
activateGarth :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateGarth answer garthId board = case Activatable.abilitiesFor garthId board of
  [ability] -> S.runPure answer board (Activate.activateAbility S.alice garthId ability >> Stack.resolveTop)
  _ -> board

-- Names `name` when a card name is asked for, answers CR 108.1's lookup from
-- `reference` alone, and takes or declines the offered cast.
naming :: [Printing.Printing] -> CardName.CardName -> OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
naming reference name decision p = case p of
  Prompt.ChooseCardName {} -> name
  Prompt.LookUpCard wanted -> fmap Printing.card (List.find ((== wanted) . S.printingName) reference)
  Prompt.OfferedCast {} -> decision
  _ -> S.identityAnswer p

lotusesOnStack :: GameState.GameState -> Int
lotusesOnStack gs = length (filter (\oid -> PC.names (Projection.project oid gs) == Set.singleton lotusName) (GameState.stack gs))

lotusesOnBattlefield :: GameState.GameState -> [ObjectId.ObjectId]
lotusesOnBattlefield gs = filter (\oid -> PC.names (Projection.project oid gs) == Set.singleton lotusName) (Set.toList (GameState.battlefield gs))

-- CR 707.14 on Magar of the Magic Strings {1}{B}{R}, "{1}{B}{R}: Note the name
-- of target instant or sorcery card in your graveyard and put it onto the
-- battlefield face down. It's a 3/3 creature with 'Whenever this creature deals
-- combat damage to a player, you may create a copy of the card with the noted
-- name. You may cast the copy without paying its mana cost' and 'If this
-- creature would leave the battlefield, exile it instead of putting it anywhere
-- else.'" (Oracle text and rulings checked on Scryfall, 2026-09-24).
--
-- alice's graveyard holds Divination and a Lightning Bolt, so the target is a
-- real choice; her library holds three Islands, so Divination's two draws are
-- what the hand counts.
magarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
magarSpec s registry = Spec.describe s "MagarOfTheMagicStrings" $ do
  Spec.it s "CR 707.14 the face-down Divination connects and alice casts a copy of Divination for free" $ do
    (downId, board) <- magarBoard s registry
    let after = S.runCombat (magarAnswer Nothing) board
    Spec.assertEqWith s "CR 707.14 the copy resolved: alice drew two" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
    Spec.assertEqWith s "CR 601.2i alice cast one spell, the copy" (PlayerEffect.castsThisTurn S.alice after) (PlayerEffect.castsThisTurn S.alice board + 1)
    Spec.assertEqWith s "CR 510.1b the 3/3 and Magar dealt 6 to bob" (S.lifeOf S.bob after) (Just 14)
    Spec.assertEqWith s "the card itself stayed face down on the battlefield" (fmap Object.facing (Game.lookupObject downId after)) (fmap Object.facing (Game.lookupObject downId board))
    Spec.assertEqWith s "and nothing is left outside the game" (GameState.outsideCopies after) Set.empty

  Spec.it s "CR 708.2 the face-down permanent is a nameless 3/3 creature with the two listed abilities" $ do
    (downId, board) <- magarBoard s registry
    let pc = Projection.project downId board
    Spec.assertEqWith s "no name, a creature" (PC.names pc, PC.cardTypes pc) (Set.empty, Set.singleton CardType.Creature)
    Spec.assertEqWith s "3/3" (Projection.powerOf downId board, Projection.toughnessOf downId board) (Just 3, Just 3)
    Spec.assertEqWith s "one triggered and one replacement ability" (length (PC.triggeredAbilities pc), length (PC.replacementEffects pc)) (1, 1)

  -- The rulings' Clone case: the copy has both abilities (CR 708.2 makes them
  -- copiable values) and no noted name. Only the Clone attacks, so a trigger
  -- that copied the card under its source would cast a Clone.
  Spec.it s "CR 707.14 a Clone of the face-down 3/3 connects and creates no copy" $ do
    (downId, board) <- magarBoard s registry
    clone <- S.printingOf s registry "Clone"
    let resolved = cloneOnto clone downId board
        entered = Set.toList (Set.difference (GameState.battlefield resolved) (GameState.battlefield board))
    case entered of
      [cloneId] -> do
        let ready = settleObject cloneId resolved
            pc = Projection.project cloneId ready
            after = S.runCombat (magarAnswer (Just [cloneId])) ready
        Spec.assertEqWith s "CR 708.2 the Clone is a nameless 3/3 with the trigger and the replacement" (PC.names pc, Projection.powerOf cloneId ready, length (PC.triggeredAbilities pc), length (PC.replacementEffects pc)) (Set.empty, Just 3, 1, 1)
        Spec.assertEqWith s "CR 707.14 the Clone's trigger created nothing: alice cast no spell in combat" (PlayerEffect.castsThisTurn S.alice after) (PlayerEffect.castsThisTurn S.alice ready)
        Spec.assertEqWith s "and her hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
        Spec.assertEqWith s "CR 510.1b the Clone alone dealt bob 3" (S.lifeOf S.bob after) (Just 17)
      other -> Spec.assertFailure s ("expected one Clone to enter, got " <> show (length other))

-- alice's settled Magar in combat's declare attackers step, bob defending, and
-- Magar's ability activated at Divination and resolved. Answers the face-down
-- permanent's id, settled -- CR 302.6 would otherwise keep it home, and settling
-- it is the one fixture step that is not the cards' own doing.
magarBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId, GameState.GameState)
magarBoard s registry = magarBoardAt s registry "Divination"

-- magarBoard with this card in Divination's place.
magarBoardAt :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m (ObjectId, GameState.GameState)
magarBoardAt s registry targetName = do
  magar <- S.printingOf s registry "Magar of the Magic Strings"
  divination <- S.printingOf s registry targetName
  bolt <- S.printingOf s registry "Lightning Bolt"
  island <- S.printingOf s registry "Island"
  let (gs0, mine, _) = S.combatBoardOf [magar] []
      (divinationId, gs1) = S.addGraveyardCard divination S.alice gs0
      (_, gs2) = S.addGraveyardCard bolt S.alice gs1
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) gs2 [1 :: Int .. 3]
      funded = stocked {GameState.manaPool = Map.singleton S.alice (Mana.Type.MkMana [floating Color.Black, floating Color.Red, floating Color.Red]), GameState.priority = Just S.alice}
  case mine of
    [magarId] -> case Activatable.abilitiesFor magarId funded of
      [ability] -> do
        let resolved = S.runPure (targetingCard divinationId) funded (Activate.activateAbility S.alice magarId ability >> Stack.resolveTop)
        case Set.toList (Set.difference (GameState.battlefield resolved) (GameState.battlefield funded)) of
          [downId] -> pure (downId, settleObject downId resolved)
          _ -> pure (magarId, resolved)
      _ -> pure (magarId, funded)
    _ -> pure (ObjectId.MkObjectId 0, funded)

-- alice casts Clone and copies `original` as it enters (CR 707.5).
cloneOnto :: Printing.Printing -> ObjectId -> GameState.GameState -> GameState.GameState
cloneOnto clone original board =
  let (_, staged) = S.spellOnStack clone S.alice board
   in snd (Engine.runGamePure (copyingOnto original) staged (Stack.resolveTop >> Engine.settleForPriority))

copyingOnto :: ObjectId -> Prompt.Prompt r -> r
copyingOnto original p = case p of
  Prompt.ChooseCopyTarget {} -> Just original
  _ -> S.identityAnswer p

-- Aims every target slot at this card when it is offered.
targetingCard :: ObjectId -> Prompt.Prompt r -> r
targetingCard card p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just card) . Recipient.objectOf) sets
  _ -> S.identityAnswer p

-- One ordinary unit of floating mana of this colour.
floating :: Color.Color -> ManaUnit.ManaUnit
floating color =
  ManaUnit.MkManaUnit
    { ManaUnit.manaType = ManaType.Colored color,
      ManaUnit.tags = Set.empty,
      ManaUnit.retention = ManaRetention.Ordinary,
      ManaUnit.restriction = Nothing,
      ManaUnit.rider = Nothing,
      ManaUnit.sourceChosenSubtype = Nothing
    }

-- CR 302.6: as if the object had been under alice's control since her turn began.
settleObject :: ObjectId -> GameState.GameState -> GameState.GameState
settleObject oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) oid (GameState.objects gs)}

-- Attacks bob -- with everything, or with only the listed attackers -- and takes
-- every "may" and every offered cast.
magarAnswer :: Maybe [ObjectId] -> Prompt.Prompt r -> r
magarAnswer only p = case p of
  Prompt.DeclareAttackers _ _ ids -> maybe ids (filter (`elem` ids)) only
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  Prompt.OfferedCast {} -> OptionalDecision.Exercises
  _ -> S.attackTo S.bob p

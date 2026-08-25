{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Replacement's EntryR AsCopy arm (the CR 614.12a copy choice, run
-- from inside Pawl.Engine.Event's changeZone) and its CR 707.9 exceptions
-- (Replacement.applyCopyExceptions, Quicksilver Gargantuan), its CR 707.5 eligible set
-- (Replacement.legalCopyTargets, Copy Enchantment's "any enchantment" against Clone's
-- "any creature"), the P2 copy gate (Clone), and
-- Pawl.Engine.Resolve's CreateCopy arm (CR 707.2's token copy, Cackling
-- Counterpart and Watchful Radstag; its count, and the simultaneous entry that
-- count buys, kicked Rite of Replication; and CR 122.6's entry rider on it,
-- Littjara Mirrorlake) and its BecomeCopy arm (CR 707.4's
-- change of a permanent already on the battlefield, Unstable Shapeshifter).
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
-- And Pawl.Engine.Resolve's CopySpell arm (CR 707.10's copy of a spell on the
-- stack, Twincast) with the CR 707.10c re-target prompt it raises, the CR 704.5e
-- state-based action in Pawl.Engine.Sba that removes the resolved copy, and
-- Pawl.Engine.Stack's OfSpellCopy resolution arm.
module Pawl.CopySpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The battlefield objects whose PRINTED card has this name (a printed card is
-- unchanged by copying -- only the object's projected characteristics change).
printedOnBattlefield :: String -> GameState.GameState -> [ObjectId]
printedOnBattlefield name gs = filter isIt (Set.toList (GameState.battlefield gs))
  where
    isIt oid = maybe False (\f -> Face.name f == CardName.MkCardName (Text.pack name)) (Game.faceOf oid gs)

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

-- Rite of Replication {2}{U}{U} Sorcery: "Kicker {5} ... Create a token that's a
-- copy of target creature. If this spell was kicked, create five of those tokens
-- instead" -- two clauses on Quantity.WasKicked, the kicked one carrying a count
-- of five (data/cards/rite-of-replication.json).
--
-- Answers CR 702.33a's kicker question with `decision` and aims the one target
-- slot at `victim` -- PINNED to that id rather than searched for, so a mutation
-- cannot be repaired by an answerer that finds another legal target. Every
-- as-enters copy choice a minted token then makes goes to copyNewest.
rites :: KickerDecision.KickerDecision -> ObjectId -> Prompt.Prompt r -> r
rites decision victim p = case p of
  Prompt.ChooseKicker {} -> decision
  Prompt.ChooseTargets _ _ _ sets -> Map.map (const (Set.singleton (Recipient.ToCreature victim))) sets
  _ -> copyNewest p

-- What each token on the battlefield IS -- its name and its projected P/T --
-- rather than how many there are. A batch that minted the right NUMBER of the
-- wrong things fails on this where a length check would not.
mintedTokens :: GameState.GameState -> [(Set.Set CardName.CardName, Maybe (Integer, Integer))]
mintedTokens gs = fmap (\oid -> (Projection.namesOf oid gs, S.powerToughnessOf oid gs)) (tokensOnBattlefield gs)

-- Vesuva Land: "You may have this land enter tapped as a copy of any land on the
-- battlefield" (data/cards/vesuva.json; Oracle text checked against
-- api.scryfall.com, 2026-08-21). The whole of its printed text, and what makes a
-- copy of a LAND reachable at all in data/cards.
--
-- alice's board: two Mountains, Mutavault, `mMoon` when one is passed, and Vesuva
-- in her hand with the turn's land drop unspent. `mMoon` puts Blood Moon there
-- BEFORE Vesuva is played, which the CR 614.12 case wants; the CR 305.7 case
-- passes Nothing and adds it after, since a Blood Moon already out leaves no copy
-- ability to apply.
--
-- The Mountains are BASIC on purpose: Blood Moon's printed criterion is NONBASIC
-- lands, so the mana that pays Mutavault's animation is the same whether or not a
-- Blood Moon is on the board, and a refused activation is never a refusal to
-- pay.
vesuvaBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> (ObjectId, GameState.GameState)
vesuvaBoard mountain mutavault vesuva mMoon =
  let base = S.landsInPlay mountain 2
      (mutavaultId, g1) = S.addCreature mutavault S.alice base
      g2 = maybe g1 (\moon -> snd (S.addCreature moon S.alice g1)) mMoon
      g3 = snd (S.addHandCard vesuva S.alice g2)
   in ( mutavaultId,
        g3
          { GameState.priority = Just S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice
          }
      )

-- Plays whatever land the board offers and pins the as-enters copy choice to ONE
-- named permanent, copyNamed's posture for copyNamed's reason: an answerer that
-- searched for a legal source would find another land after a mutation.
playsAndCopies :: ObjectId -> Prompt.Prompt r -> r
playsAndCopies wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  _ -> S.playLandAnswer p

-- playsAndCopies' opposite: play the land and DECLINE the copy, which is the
-- other half of the printed "may".
playsAndDeclines :: Prompt.Prompt r -> r
playsAndDeclines p = case p of
  Prompt.ChooseCopyTarget {} -> Nothing
  _ -> S.playLandAnswer p

-- Takes ONE named activated ability of ONE named permanent whenever the priority
-- loop offers it, and passes otherwise -- so an ability that reaches the stack
-- did so because the engine offered it, not because this answerer reached past a
-- gate.
activates :: ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Prompt.Prompt r -> r
activates srcId ability p = case p of
  Prompt.ChooseAction _ _ actions ->
    let wanted a = case a of
          A.Activate oid ab -> oid == srcId && ab == ability
          _ -> False
     in case filter wanted actions of
          h : _ -> h
          [] -> A.Pass
  _ -> S.identityAnswer p

-- `activates` with a target slot to fill: the ability Littjara Mirrorlake
-- activates says "target creature you control". FILTERED out of the offered
-- candidates rather than built, so the recipient is the one the engine itself
-- offered for that pool -- a hand-built Recipient of the same permanent is a
-- different recipient, and CR 608.2b's re-read at resolution drops it silently.
-- Pinned to one id for `activates`' reason: an answerer that took whatever was
-- legal would find the other creature after a mutation.
activatesTargeting :: ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> ObjectId -> Prompt.Prompt r -> r
activatesTargeting srcId ability victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter (\r -> Recipient.objectOf r == Just victim) candidates) sets
  _ -> activates srcId ability p

-- The +1/+1 counters on one object. Duplicated from Pawl.ReplacementSpec rather
-- than hoisted into Pawl.Support, which rebuilds every spec in the tree.
plusOnesOn :: ObjectId -> GameState.GameState -> Natural.Natural
plusOnesOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)

-- Littjara Mirrorlake Land: "This land enters tapped. {T}: Add {U}.
-- {2}{G}{G}{U}, {T}, Sacrifice this land: Create a token that's a copy of target
-- creature you control, except it enters with an additional +1/+1 counter on it.
-- Activate only as a sorcery." (data/cards/littjara-mirrorlake.json; Oracle text
-- checked against api.scryfall.com, 2026-08-25.) Its SECOND printed ability is
-- the copy one -- the first is the mana ability CR 605.3b keeps off the stack.
sacrificeAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
sacrificeAbility = Maybe.listToMaybe . drop 1 . Face.activatedAbilities . S.combinedFace

-- alice controls the Mirrorlake untapped, a Goblin Piker carrying TWO +1/+1
-- counters, and five other lands -- two Forests and three Islands, which is
-- exactly {2}{G}{G}{U} once the Mirrorlake itself is tapped for the cost and so
-- cannot pay for anything. Returns the Mirrorlake and the Piker.
--
-- The Mirrorlake is placed already on the battlefield and UNTAPPED: its own entry
-- rewrite would tap it, and a tapped land cannot pay the {T} in its cost.
mirrorlakeBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, GameState.GameState)
mirrorlakeBoard forest island piker mirrorlake =
  let base = S.landsFor island S.alice 3 (S.landsInPlay forest 2)
      (pikerId, g1) = S.addCreature piker S.alice base
      g2 = S.addCounter CounterKind.PlusOnePlusOne 2 pikerId g1
      (lakeId, g3) = S.addCreature mirrorlake S.alice g2
   in ( lakeId,
        pikerId,
        g3
          { GameState.priority = Just S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice
          }
      )

-- Mutavault's SECOND printed ability -- "{1}: This land becomes a 2/2 creature
-- with all creature types until end of turn. It's still a land". The first is the
-- mana ability CR 605.3b keeps off the stack, which no priority window offers.
animationAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
animationAbility = Maybe.listToMaybe . drop 1 . Face.activatedAbilities . S.combinedFace

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  Spec.it s "Clone copies a creature and projects its P/T (CR 707.2)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board) = S.addCreature piker S.alice gs0
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
        (_, board) = S.addCreature piker S.alice gs0
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
        (_, withPiker) = S.addCreature piker S.alice gs0
        (scalesId, board) = S.addCreature scales S.alice withPiker
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
        (pikerId, withPiker) = S.addCreature piker S.alice gs0
        (_, board) = S.addCreature scales S.alice withPiker
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
        (_, withPiker) = S.addCreature piker S.alice gs0
        (_, withScales) = S.addCreature scales S.alice withPiker
    Spec.assertEqWith s "a creature but no enchantment: nothing to ask" (asks withPiker) 0
    Spec.assertEqWith s "an enchantment beside it: one real decision" (asks withScales) 1

  Spec.it s "Clone copies base P/T, not a counter-boosted P/T (CR 707.2 falsifier)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, board0) = S.addCreature piker S.alice gs0
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
        (_, board) = S.addCreature prodigalSorcerer S.alice gs0
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
        (_, board) = S.addCreature piker S.alice gs0
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
        (pikerId, board) = S.addCreature piker S.alice gs0
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
        (goyfId, board) = S.addCreature tarmogoyf S.alice withBolt
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
        (goyfId, board0) = S.addCreature tarmogoyf S.alice withBolt
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

  -- THE PROVING TEST for WHERE the exception lands: in the copy's own COPIABLE
  -- values (CR 707.9b), not in a CR 613 layer over them. A token copy of the
  -- Gargantuan reads the copiable values (CR 707.2), so it is a Tarmogoyf at 7/7
  -- that ignores the graveyards. Had the exception been layered on the object
  -- instead, the token would have copied the Goyf's CDA and read 1/2, then 2/3;
  -- had the token fallen back on its own printed card it would be 7/7 but named
  -- Quicksilver Gargantuan. The name and the pair together separate all three.
  --
  -- The Goyf is BOB's, so the Gargantuan is the Counterpart's only legal target
  -- ("target creature you control").
  Spec.it s "a token copy of an excepted copy keeps the exception (CR 707.9b)" $ do
    island <- S.printingOf s registry "Island"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    tarmogoyf <- S.printingOf s registry "Tarmogoyf"
    gargantuan <- S.printingOf s registry "Quicksilver Gargantuan"
    piker <- S.printingOf s registry "Goblin Piker"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, withBolt) = S.addGraveyardCard lightningBolt S.alice (S.landsInPlay island 3)
        (goyfId, board) = S.addCreature tarmogoyf S.bob withBolt
        (_, staged) = S.spellOnStack gargantuan S.alice board
        withGargantuan = resolveAndSettle (copyNamed goyfId) staged
        resolved = castAndResolve declineCopy counterpart withGargantuan
        (_, later) = S.addGraveyardCard piker S.bob resolved
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token is named for the Goyf, not the Gargantuan" (Projection.namesOf tokenId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Tarmogoyf"
        Spec.assertEqWith s "and is 7/7, the excepted value" (S.powerToughnessOf tokenId resolved) $ Just (7, 7)
        Spec.assertEqWith s "the Goyf itself moves with the graveyards" (S.powerToughnessOf goyfId later) $ Just (2, 3)
        Spec.assertEqWith s "the token does not" (S.powerToughnessOf tokenId later) $ Just (7, 7)
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  Spec.it s "Cackling Counterpart mints a token copy of the targeted creature (CR 707.2, CR 111.3)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, board) = S.addCreature piker S.alice (S.landsInPlay island 3)
        resolved = castAndResolve declineCopy counterpart board
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token's name is the copied creature's" (Projection.namesOf tokenId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
        Spec.assertEqWith s "the token's power is the copied creature's" (Projection.powerOf tokenId resolved) $ Just 2
        Spec.assertEqWith s "the token's toughness is the copied creature's" (Projection.toughnessOf tokenId resolved) $ Just 1
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for the copiable stamp. The target is itself a copy, so
  -- its printed card (Clone, a 0/0 with an as-enters copy ability) and its
  -- copiable values (the Piker's) disagree -- and CR 707.2's "as modified by
  -- other copy effects" says the token takes the latter. Under declineCopy a
  -- token that fell back on the printed card is a 0/0 that CR 704.5f buries.
  Spec.it s "a token copy of a Clone copies what the Clone copies (CR 707.2)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, board) = S.addCreature piker S.bob (S.landsInPlay island 3)
        (_, staged) = S.spellOnStack clone S.alice board
        -- alice's Clone is now her only creature, so it is the Counterpart's
        -- only legal target ("target creature you control").
        withClone = resolveAndSettle copyNewest staged
        resolved = castAndResolve declineCopy counterpart withClone
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token is named for the Piker, not the Clone" (Projection.namesOf tokenId resolved) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
        Spec.assertEqWith s "the token is a 2, not a 0" (Projection.powerOf tokenId resolved) $ Just 2
        Spec.assertEqWith s "the token is a 1, not a 0" (Projection.toughnessOf tokenId resolved) $ Just 1
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- CR 707.2's exclusion: counters are not copied. The falsifier for stamping
  -- the PROJECTION rather than the copiable values.
  Spec.it s "a token copy of a counter-boosted creature is the base P/T (CR 707.2)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (pikerId, board0) = S.addCreature piker S.alice (S.landsInPlay island 3)
        board = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId board0
        resolved = castAndResolve declineCopy counterpart board
    Spec.assertEqWith s "the source is boosted to 3/2" (Projection.powerOf pikerId resolved) $ Just 3
    case tokensOnBattlefield resolved of
      [tokenId] -> do
        Spec.assertEqWith s "the token copies the base 2, not 3" (Projection.powerOf tokenId resolved) $ Just 2
        Spec.assertEqWith s "the token copies the base 1, not 2" (Projection.toughnessOf tokenId resolved) $ Just 1
      tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

  -- THE PROVING TEST for CR 122.6 on the COPY opcode: "except it enters with an
  -- additional +1/+1 counter on it" is a rider the effect states, not something
  -- copied off the original -- CR 707.2 excludes counters from the copiable
  -- values either way.
  --
  -- The Piker carries TWO counters, which is what separates the three readings.
  -- A rider that never reaches Event.createTokens leaves the token at ZERO. The
  -- rule's answer is ONE. An implementation that copied the original's counters
  -- and added the rider's would say THREE. With a bare Piker the second and third
  -- readings both say one and the board proves nothing.
  --
  -- Driven through the priority loop rather than Activate.activateAbility, the
  -- Vesuva case's reason: the ability reaches the stack because the engine offered
  -- it under CR 602.5d's sorcery-speed restriction.
  Spec.it s "Littjara Mirrorlake's copy token enters with the counter the effect states (CR 122.6, CR 707.2)" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    mirrorlake <- S.printingOf s registry "Littjara Mirrorlake"
    case sacrificeAbility mirrorlake of
      Nothing -> Spec.assertFailure s "Littjara Mirrorlake prints no second activated ability"
      Just ability -> do
        let (lakeId, pikerId, board) = mirrorlakeBoard forest island piker mirrorlake
            after = S.runPure (activatesTargeting lakeId ability pikerId) board Engine.priorityLoop
        case tokensOnBattlefield after of
          [tokenId] -> do
            Spec.assertEqWith s "the token enters with the one counter the effect stated" (plusOnesOn tokenId after) 1
            Spec.assertEqWith s "the original keeps its own two, which were never copied" (plusOnesOn pikerId after) 2
            Spec.assertEqWith s "the token is a copy of the Piker" (Projection.namesOf tokenId after) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
            Spec.assertEqWith s "so it is the printed 2/1 plus its one counter" (S.powerToughnessOf tokenId after) $ Just (3, 2)
            Spec.assertEqWith s "against the original's 2/1 plus two" (S.powerToughnessOf pikerId after) $ Just (4, 3)
          tokens -> Spec.assertFailure s ("expected exactly one token, got " <> show (length tokens))

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
    let (radstagId, board) = S.addCreature radstag S.alice (Setup.emptyGame S.bothPlayers)
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
    let (radstagId, board) = S.addCreature radstag S.alice (Setup.emptyGame S.bothPlayers)
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

  -- CR 707.1's count. Nine Islands is the KICKED cost ({2}{U}{U} plus {5}), and
  -- the kicked test that follows is the same board, the same answerer and the
  -- same pinned target but for the one kicker answer -- so the count is the only
  -- thing the two boards disagree about.
  Spec.it s "unkicked Rite of Replication mints one token copy (CR 707.1)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rite <- S.printingOf s registry "Rite of Replication"
    let (pikerId, board) = S.addCreature piker S.alice (S.landsInPlay island 9)
        resolved = castAndResolve (rites KickerDecision.Declines pikerId) rite board
    Spec.assertEqWith s "one token, and it is the Piker" (mintedTokens resolved) [(Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")), Just (2, 1))]

  Spec.it s "kicked Rite of Replication mints five instead (CR 702.33d, CR 707.1)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rite <- S.printingOf s registry "Rite of Replication"
    let (pikerId, board) = S.addCreature piker S.alice (S.landsInPlay island 9)
        resolved = castAndResolve (rites KickerDecision.Kicks pikerId) rite board
    Spec.assertEqWith s "five tokens, and every one of them is the Piker" (mintedTokens resolved) (replicate 5 (Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")), Just (2, 1)))

  -- THE PROVING TEST for CR 614.12's batch exclusion and for CR 616.1g's
  -- containment. Five token Clones enter at ONE moment, each with its own
  -- `EntryR AsCopy`, so each runs an entry loop that finds a real candidate --
  -- and copyNewest names the highest-id legal creature, which is the Giant only
  -- while the four siblings entering beside it are kept out of the offer.
  --
  -- Both mutations are visible here: dropping Event.createTokens' per-token
  -- runEntry leaves five 0/0 Clones that CR 704.5f buries, and dropping
  -- Replacement.legalCopyTargets' batch exclusion has a token name a sibling
  -- that has not copied anything yet and become a 0/0 in its turn.
  Spec.it s "five token Clones enter at once, each choosing, and none may copy a sibling (CR 614.12, CR 616.1g)" $ do
    island <- S.printingOf s registry "Island"
    clone <- S.printingOf s registry "Clone"
    giant <- S.printingOf s registry "Hill Giant"
    rite <- S.printingOf s registry "Rite of Replication"
    let (cloneId, board0) = S.addCreature clone S.alice (S.landsInPlay island 9)
        -- A +1/+1 counter is what keeps a Clone that copied NOTHING alive past
        -- CR 704.5f, and so leaves an `EntryR AsCopy` on the battlefield for the
        -- Rite to copy. Counters are not copiable (CR 707.2), so each token is a
        -- printed 0/0 Clone carrying the copy ability rather than a 1/1.
        board1 = S.addCounter CounterKind.PlusOnePlusOne 1 cloneId board0
        -- Added AFTER the Clone, so it is the highest-id creature already on the
        -- battlefield and copyNewest names it -- unless a sibling token, minted
        -- later still, is wrongly offered.
        (_, board) = S.addCreature giant S.bob board1
        resolved = castAndResolve (rites KickerDecision.Kicks cloneId) rite board
    Spec.assertEqWith s "five tokens entered, and every one copied the Giant rather than an entering sibling" (mintedTokens resolved) (replicate 5 (Set.singleton (CardName.MkCardName (Text.pack "Hill Giant")), Just (3, 3)))
    Spec.assertEqWith s "the copied Clone itself still copied nothing" (S.powerToughnessOf cloneId resolved) (Just (1, 1))

  -- THE PROVING TEST for #1738, on the CR 614.12a channel the case above proves
  -- for a TOKEN batch. Rise of the Dark Realms returns every creature card from
  -- every graveyard as ONE CR 608.2f event, so a Clone in that batch makes its
  -- copy choice before the permanent enters -- and a creature card returned
  -- beside it is not on the battlefield to be copied.
  --
  -- NO creature on either battlefield, which is what makes the board
  -- discriminate: one already there is a legal copy target under both readings.
  -- The Piker is buried FIRST so it takes the lower ObjectId and moves first
  -- (Resolve.graveyardCardsOf sorts ascending, S.addGraveyardCard mints in call
  -- order), which is the only order in which the per-object reading has anything
  -- to offer; the mirrored leg pins that the answer does not depend on it.
  --
  -- copyNewest rather than declineCopy: under the per-object reading a real
  -- ChooseCopyTarget is raised and taking it is what produces the second Piker,
  -- while the correct reading raises no prompt at all (Replacement.apply skips a
  -- forced selection with no legal candidate). So the Clone enters its printed
  -- 0/0 and CR 704.5f puts it back into alice's graveyard.
  Spec.it s "CR 608.2f a reanimated Clone may not copy a creature reanimated beside it (#1738)" $ do
    swamp <- S.printingOf s registry "Swamp"
    rise <- S.printingOf s registry "Rise of the Dark Realms"
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    let outcome buried =
          let graves = List.foldl' (\g printing -> snd (S.addGraveyardCard printing S.alice g)) (S.landsInPlay swamp 9) buried
              after = castAndResolve copyNewest rise graves
           in ( List.sort [Projection.namesOf oid after | oid <- Set.toList (GameState.battlefield after), Projection.isCreatureOf oid after],
                List.sort (fmap (fmap Face.name . (`Game.faceOf` after)) (Game.zoneMembers Zone.Graveyard S.alice after))
              )
        pikerFirst = outcome [piker, clone]
        cloneFirst = outcome [clone, piker]
        named = CardName.MkCardName . Text.pack
    Spec.assertEqWith
      s
      "one Piker on the battlefield, and the 0/0 Clone is back in the graveyard beside the spent sorcery (CR 704.5f)"
      pikerFirst
      ( [Set.singleton (named "Goblin Piker")],
        List.sort [Just (named "Clone"), Just (named "Rise of the Dark Realms")]
      )
    Spec.assertEqWith
      s
      "and the batch's processing order changes nothing (CR 608.2f)"
      cloneFirst
      pikerFirst

  -- THE PROVING TEST for #313, and it is CR 707.4's own worked example: an
  -- Unstable Shapeshifter under a Giant Growth becomes a copy of a creature that
  -- enters later and "will still get +3/+3 from the Giant Growth". The rule's
  -- three claims all read off one board:
  --
  --   * the copy happened, so the Shapeshifter is not its printed 0/1 any more;
  --   * the copied values are the ORIGINAL's copiable ones (CR 707.2), 4/3;
  --   * the noncopy effect presently affecting the permanent survives, so the
  --     answer is 7/6 rather than 4/3 -- which is what putting the change at
  --     layer 1 (CR 613.1a) buys, since layers 2-7 re-apply over the new base.
  --
  -- Blind-Spot Giant is 4/3 deliberately: 4 /= 3, and neither is 0 or 1, so power
  -- and toughness cannot be swapped without the assertion seeing it, and 7/6
  -- cannot be reached by any other pairing on this board -- including the 8/7 a
  -- copy taken off the Giant's counter-boosted projection would give. Goblin Piker (2/1) is
  -- the SECOND creature, put down before the Giant so that the condition's
  -- "another creature" (Not IsSource) is a real restriction rather than trivially
  -- true -- and it is left alone, which is the check that the effect swept the
  -- subject ref rather than the battlefield.
  --
  -- The entering Giant is asserted UNCHANGED, which is the other reading of the
  -- rule: "the entrant becomes a copy of the Shapeshifter" produces a board this
  -- one distinguishes, since the Giant would then be 0/1.
  Spec.it s "Unstable Shapeshifter becomes a copy and keeps a noncopy effect (CR 707.4)" $ do
    forest <- S.printingOf s registry "Forest"
    shapeshifter <- S.printingOf s registry "Unstable Shapeshifter"
    piker <- S.printingOf s registry "Goblin Piker"
    blindSpotGiant <- S.printingOf s registry "Blind-Spot Giant"
    growth <- S.printingOf s registry "Giant Growth"
    let (shifterId, board0) = S.addCreature shapeshifter S.alice (S.landsInPlay forest 1)
        (pikerId, board) = S.addCreature piker S.alice board0
        grown = castAndResolve (targeting shifterId) growth board
        (giantId, entered0) = S.entersWithTrigger blindSpotGiant S.alice grown
        -- CR 707.2's exclusion, made observable: a +1/+1 counter takes the Giant's
        -- PROJECTION to 5/4 while its copiable values stay 4/3, so the assertions
        -- below tell the two reads apart. Without it both readings say 4/3 and the
        -- test could not see a copy taken off the projection.
        entered = S.addCounter CounterKind.PlusOnePlusOne 1 giantId entered0
        -- The settle puts the Shapeshifter's CR 603.6a trigger on the stack; the
        -- resolve runs it. The narrowest path that shows the behaviour.
        onStack = settle (targeting shifterId) entered
        after = resolveAndSettle (targeting shifterId) onStack
    Spec.assertBool s (not (null (GameState.stack onStack))) "the Shapeshifter's trigger really was on the stack"
    Spec.assertEqWith s "before: the printed 0/1 plus the Giant Growth" (S.powerToughnessOf shifterId onStack) $ Just (3, 4)
    -- Asserted BEFORE the Shapeshifter's own pair so that the two mutations stay
    -- disjoint: swapping the refs reddens these two and leaves the Shapeshifter
    -- at 3/4, while neutralising the stamp reddens only the pair below.
    Spec.assertEqWith s "the creature that entered is untouched, counter and all" (S.powerToughnessOf giantId after) $ Just (5, 4)
    Spec.assertEqWith s "and so is the other creature already there" (S.powerToughnessOf pikerId after) $ Just (2, 1)
    Spec.assertEqWith s "after: the copiable 4/3 -- not the counter-boosted 5/4 -- plus the SAME Giant Growth" (S.powerToughnessOf shifterId after) $ Just (7, 6)
    Spec.assertEqWith s "and it is the Giant by name (CR 707.2)" (Projection.namesOf shifterId after) . Set.singleton . CardName.MkCardName $ Text.pack "Blind-Spot Giant"
    -- Not implemented: CR 707.9a's "except it has this ability" (#1292). pawl's
    -- Shapeshifter takes the Giant's abilities and only those, so it loses the
    -- trigger that copied and can never copy again -- STRICTER than printed, and
    -- this is where that is observable.
    Spec.assertEqWith s "it has the Giant's abilities and only those" (length (Projection.triggeredAbilitiesOf shifterId after)) 0

  -- THE PROVING TEST for CR 305.7's THIRD clause: a land whose subtype is set to a
  -- basic type "loses all abilities generated from its rules text, its old land
  -- types, and any copiable effects affecting that land". Vesuva enters as a copy
  -- of Mutavault -- a copiable effect, applied in layer 1 (CR 613.2a), so the
  -- animation and the {C} are Vesuva's own -- and a Blood Moon arriving AFTER
  -- takes them.
  --
  -- The clause falls out of WHERE the copy lives rather than needing a layer-1
  -- unwind: Projection.copiableCharacteristics SEEDS the fold with the copy
  -- snapshot, so by layer 4 the copied text is as much "the land's rules text" as
  -- a printed line, and setLandSubtypeTo's one strip reaches both. CR 305.7 asks
  -- for no less -- the copy keeps nothing either clause would spare.
  --
  -- ONE entered board, forked by adding Blood Moon to it, so the two legs differ
  -- in that permanent and in nothing else -- and the copy on the stripped leg is
  -- the very same copy the other leg animates. The ORDER is what makes the clause
  -- reachable at all: a Blood Moon already out strips Vesuva's own copy ability
  -- before it can apply, which is the case below.
  --
  -- Vesuva NAMES Mutavault on both legs (CR 305.7 changes no name), which is what
  -- keeps the stripped leg from passing because no copy ever happened.
  --
  -- The animation is driven through the priority loop rather than
  -- Activate.activateAbility, which does not gate: the negative has to be the
  -- engine refusing to offer the action, not this test declining to take it.
  Spec.it s "CR 305.7 Blood Moon strips the abilities Vesuva copied from another land" $ do
    mountain <- S.printingOf s registry "Mountain"
    mutavault <- S.printingOf s registry "Mutavault"
    vesuva <- S.printingOf s registry "Vesuva"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    case animationAbility mutavault of
      Nothing -> Spec.assertFailure s "Mutavault prints no second activated ability"
      Just animation -> do
        let (mutavaultId, board) = vesuvaBoard mountain mutavault vesuva Nothing
            without = S.runPure (playsAndCopies mutavaultId) board Engine.priorityLoop
            with = snd (S.addCreature bloodMoon S.alice without)
            named = CardName.MkCardName . Text.pack
        case newest (printedOnBattlefield "Vesuva" without) of
          Nothing -> Spec.assertFailure s "Vesuva never reached the battlefield"
          Just vesuvaId -> do
            let animate gs = S.runPure (activates vesuvaId animation) gs Engine.priorityLoop
                plain = animate without
                mooned = animate with
            -- The copy is real: Vesuva took Mutavault's animation and became a
            -- 2/2. This is also the assertion a strip that reached too far -- one
            -- with no Blood Moon to switch it on -- would redden.
            Spec.assertEqWith s "the copied animation resolves and Vesuva is a 2/2" (S.powerToughnessOf vesuvaId plain) $ Just (2, 2)
            -- CR 305.7's third clause: the same copy, under Blood Moon, has no
            -- animation to offer, so nothing animates.
            Spec.assertEqWith s "under Blood Moon the copied animation is gone, and Vesuva is no creature" (S.powerToughnessOf vesuvaId mooned) Nothing
            -- Not vacuous: the copy is still there to be stripped. A name is not
            -- among the things CR 305.7 takes.
            Spec.assertEqWith s "the mooned Vesuva is still a copy of Mutavault by name (CR 707.2)" (Projection.namesOf vesuvaId mooned) $ Set.singleton (named "Mutavault")
            -- The mana half of the same clause, with CR 305.6's replacement for
            -- it: Mutavault's copied "{T}: Add {C}" goes, and the new Mountain
            -- type hands back red.
            Spec.assertEqWith s "the copy taps for the colorless it copied" (Mana.manaTypesOf vesuvaId plain) [ManaType.Colorless]
            Spec.assertEqWith s "and under Blood Moon for red alone (CR 305.6)" (Mana.manaTypesOf vesuvaId mooned) [ManaType.Colored Color.Red]
            -- CR 614.1d rides the same printed sentence: Vesuva enters TAPPED as a
            -- copy.
            Spec.assertEqWith s "and it entered tapped, as the printed sentence says" (fmap Object.tapped (Game.lookupObject vesuvaId without)) $ Just TapState.Tapped

  -- The declining half of the printed "may", which is what keeps `tapped` on the
  -- AsCopy rewrite rather than in a second EntryRewrite.Tapped beside it: Vesuva's
  -- own ruling (2021-03-19) says that a Vesuva which chooses no land "enters the
  -- battlefield untapped as itself, and will not be able to tap for mana". Both
  -- halves are asserted, and a second replacement would falsify the first.
  Spec.it s "a Vesuva that declines the copy enters untapped and taps for nothing (CR 614.1c)" $ do
    mountain <- S.printingOf s registry "Mountain"
    mutavault <- S.printingOf s registry "Mutavault"
    vesuva <- S.printingOf s registry "Vesuva"
    let (_, board) = vesuvaBoard mountain mutavault vesuva Nothing
        played = S.runPure playsAndDeclines board Engine.priorityLoop
        named = CardName.MkCardName . Text.pack
    case newest (printedOnBattlefield "Vesuva" played) of
      Nothing -> Spec.assertFailure s "Vesuva never reached the battlefield"
      Just vesuvaId -> do
        Spec.assertEqWith s "it taps for no mana at all -- Vesuva prints no mana ability" (Mana.manaTypesOf vesuvaId played) []
        Spec.assertEqWith s "and it entered untapped, the tapping having gone with the declined copy" (fmap Object.tapped (Game.lookupObject vesuvaId played)) $ Just TapState.Untapped
        Spec.assertEqWith s "and it is still itself by name" (Projection.namesOf vesuvaId played) $ Set.singleton (named "Vesuva")

  -- The OTHER order, and the reason the case above adds Blood Moon afterwards: CR
  -- 614.12 checks the entering permanent's characteristics "as it would exist on
  -- the battlefield, taking into account ... continuous effects that already exist
  -- and would apply to the permanent". A Blood Moon already out has stripped
  -- Vesuva's own copy ability by then (CR 305.7's FIRST clause), so there is no
  -- copy to make -- Blood Moon's own ruling (2020-08-07) says as much of every
  -- ability that applies as a land enters.
  --
  -- The same answerer, which is what makes this a real refusal: it names
  -- Mutavault whenever a copy choice is raised, so a Vesuva that still had its
  -- ability would copy.
  Spec.it s "CR 614.12 a Vesuva entering under Blood Moon has no copy ability left to apply" $ do
    mountain <- S.printingOf s registry "Mountain"
    mutavault <- S.printingOf s registry "Mutavault"
    vesuva <- S.printingOf s registry "Vesuva"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    let (mutavaultId, board) = vesuvaBoard mountain mutavault vesuva (Just bloodMoon)
        played = S.runPure (playsAndCopies mutavaultId) board Engine.priorityLoop
        named = CardName.MkCardName . Text.pack
    case newest (printedOnBattlefield "Vesuva" played) of
      Nothing -> Spec.assertFailure s "Vesuva never reached the battlefield"
      Just vesuvaId -> do
        Spec.assertEqWith s "Vesuva copied nothing and is still itself by name" (Projection.namesOf vesuvaId played) $ Set.singleton (named "Vesuva")
        -- The tapped half of the printed sentence went with the copy half: both
        -- are the one ability, and it was stripped before either could apply.
        Spec.assertEqWith s "and it entered untapped, the ability having gone with the rest" (fmap Object.tapped (Game.lookupObject vesuvaId played)) $ Just TapState.Untapped
        -- CR 305.6: what a Vesuva with nothing copied taps for is the new Mountain
        -- type's red, where a Vesuva that copied nothing and kept its printed text
        -- would tap for nothing at all.
        Spec.assertEqWith s "and it taps for red as a Mountain" (Mana.manaTypesOf vesuvaId played) [ManaType.Colored Color.Red]
        Spec.assertBool s (elem Subtype.Mountain (Set.toList (Projection.subtypesOf vesuvaId played))) "Blood Moon made it a Mountain"

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
    let (plainsId, g0) = S.addCreature plains S.alice (S.landsInPlay plains 6)
        (mutavaultId, g1) = S.addCreature mutavault S.alice g0
        (_, moonless) = S.addCreature urborg S.alice g1
        (moonId, g2) = S.addCreature bloodMoon S.alice moonless
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
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
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
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.Type.MkMana [],
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )

-- THREE seats: the copy's controller (alice), the original's target (bob) and
-- somewhere else for CR 707.10c to send the copy (carol). Two would collapse the
-- last two onto one player, and the re-target case would prove nothing.
--
-- alice holds Lightning Bolt and Twincast with a Mountain and two Islands
-- untapped -- exactly both costs, so neither cast can fail for mana.
twincastBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, GameState.GameState)
twincastBoard mountain island bolt twincast =
  let lands = S.landsFor island S.alice 2 (S.landsFor mountain S.alice 1 S.threePlayerGame)
      (withBolt, boltId) = S.handOne bolt lands
      (twincastId, board) = handAppend twincast S.alice withBolt
   in (boltId, twincastId, board)

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

-- alice casts Lightning Bolt at bob, then -- CR 117.3c, still holding priority --
-- Twincast at the Bolt. Returns the board with [Twincast, Bolt] on the stack.
--
-- `answer` resolves Twincast, and so is the answerer CR 707.10c's prompt reaches.
boltThenTwincast :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> ObjectId -> GameState.GameState -> Maybe GameState.GameState
boltThenTwincast answer boltId twincastId board =
  let cast1 = snd (Engine.runGamePure (pinTarget (Recipient.ToPlayer S.bob)) board (S.cast S.alice boltId))
   in do
        boltSpell <- topOfStack cast1
        let cast2 = snd (Engine.runGamePure (pinTarget (Recipient.ToObject boltSpell)) cast1 (S.cast S.alice twincastId))
        -- Twincast, then the copy it put on the stack, then the Bolt itself.
        pure (resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne answer cast2)))

copySpellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copySpellSpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  -- CR 707.10 end to end: the copy exists, carries the original's decisions (CR
  -- 707.10's "all decisions made for it" -- here the Bolt's target), resolves as
  -- a spell of its own, and then does NOT reach a graveyard.
  --
  -- The two assertions cannot reach each other's values, which is what makes the
  -- pair discriminating. bob at 14 rather than 17 is the copy existing AND
  -- resolving -- an engine that minted an object but never resolved it reads 17.
  -- alice's graveyard holding two cards rather than three is CR 704.5e: a copy
  -- minted as an ordinary card-backed spell deals the same 3 damage and is then
  -- filed into a graveyard by CR 608.2n, so the damage cannot tell that bug
  -- apart and the count is the only place the state-based action is visible.
  Spec.it s "CR 707.10 Twincast copies a Bolt, the copy resolves, and CR 704.5e removes it" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    bolt <- S.printingOf s registry "Lightning Bolt"
    twincast <- S.printingOf s registry "Twincast"
    let (boltId, twincastId, board) = twincastBoard mountain island bolt twincast
    case boltThenTwincast (pinTarget (Recipient.ToPlayer S.bob)) boltId twincastId board of
      Nothing -> Spec.assertFailure s "the Bolt never reached the stack"
      Just after -> do
        Spec.assertEqWith s "bob took the copy's 3 and the Bolt's 3" (S.lifeOf S.bob after) (Just 14)
        Spec.assertEqWith s "carol, whom neither targeted, is untouched" (S.lifeOf S.carol after) (Just 20)
        Spec.assertEqWith s "and alice, who left the copy where it was, took none" (S.lifeOf S.alice after) (Just 20)
        -- BY NAME as well as by count: a count alone passes on a graveyard
        -- holding the copy and missing the Bolt.
        Spec.assertEqWith
          s
          "alice's graveyard holds the two CARDS and not the copy"
          (List.sort (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid after)) (Game.zoneMembers Zone.Graveyard S.alice after)))
          (List.sort (fmap (CardName.MkCardName . Text.pack) ["Lightning Bolt", "Twincast"]))
        Spec.assertEqWith s "and the stack is empty" (length (GameState.stack after)) 0
  -- CR 707.10c: "the player may leave any number of the targets unchanged ... if
  -- the player chooses to change some or all of the targets, the new targets must
  -- be legal". The board is the case above's, differing in ONE thing -- the
  -- answerer that CR 707.10c's prompt reaches -- so the life totals below are the
  -- prompt's doing and nothing else's.
  Spec.it s "CR 707.10c the copy's controller sends it at a different player" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    bolt <- S.printingOf s registry "Lightning Bolt"
    twincast <- S.printingOf s registry "Twincast"
    let (boltId, twincastId, board) = twincastBoard mountain island bolt twincast
    case boltThenTwincast (pinTarget (Recipient.ToPlayer S.carol)) boltId twincastId board of
      Nothing -> Spec.assertFailure s "the Bolt never reached the stack"
      Just after -> do
        Spec.assertEqWith s "carol took the re-targeted copy's 3" (S.lifeOf S.carol after) (Just 17)
        Spec.assertEqWith s "bob took only the original Bolt's 3" (S.lifeOf S.bob after) (Just 17)
        Spec.assertEqWith s "and alice, who cast both, took none" (S.lifeOf S.alice after) (Just 20)
  -- CR 707.10: "a copy of a spell is owned by the player under whose control it
  -- was put on the stack ... a copy of a spell or ability is controlled by the
  -- player under whose control it was put on the stack". The copying effect's
  -- controller, never the copied spell's.
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
  -- CR 109.5's "you", which the case above cannot reach: Renewed Faith says "you"
  -- with a PlayerRef the resolution answers from its controller, where Char's
  -- "and 2 damage to you" says it with the reserved `you` SLOT -- and that slot is
  -- stamped with the CASTER as the original is cast. CR 707.10 copies the
  -- decisions and not the caster, so the copy's `you` is alice.
  --
  -- bob's Char sends 4 at carol and 2 at bob; alice's copy sends 4 at carol
  -- (unchanged, CR 707.10c) and 2 at ALICE. carol 12 / bob 18 / alice 18, against
  -- carol 12 / bob 16 / alice 20 for a copy that kept the caster's `you` -- alice
  -- and bob differ under the two readings and carol does not, which is the point:
  -- the TARGET is copied and the "you" is not.
  Spec.it s "CR 707.10 the copy's own \"you\" is its controller, not the copied spell's caster" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    twincast <- S.printingOf s registry "Twincast"
    char <- S.printingOf s registry "Char"
    let lands = S.landsFor mountain S.bob 3 (S.landsFor island S.alice 2 S.threePlayerGame)
        (withTwincast, twincastId) = S.handOne twincast lands
        (charId, board) = handAppend char S.bob withTwincast
        castChar = snd (Engine.runGamePure (pinTarget (Recipient.ToPlayer S.carol)) board (S.cast S.bob charId))
    case topOfStack castChar of
      Nothing -> Spec.assertFailure s "Char never reached the stack"
      Just charSpell -> do
        let cast = snd (Engine.runGamePure (pinTarget (Recipient.ToObject charSpell)) castChar (S.cast S.alice twincastId))
            -- Twincast, then the copy (CR 707.10c leaves carol targeted), then
            -- bob's own Char.
            after = resolveOne S.identityAnswer (resolveOne S.identityAnswer (resolveOne (pinTarget (Recipient.ToPlayer S.carol)) cast))
        Spec.assertEqWith s "alice takes the COPY's 2, being the copy's you" (S.lifeOf S.alice after) (Just 18)
        Spec.assertEqWith s "bob takes only his own Char's 2" (S.lifeOf S.bob after) (Just 18)
        Spec.assertEqWith s "carol takes 4 from each, the target having been copied" (S.lifeOf S.carol after) (Just 12)

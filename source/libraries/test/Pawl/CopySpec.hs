{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Replacement's EntryR AsCopy arm (the CR 614.12a copy choice, run
-- from inside Pawl.Engine.Event's changeZone) and its CR 707.9 exceptions
-- (Replacement.applyCopyExceptions -- CR 707.9b's Quicksilver Gargantuan, and CR
-- 707.9a's Dack's Duplicate and Omni-Changeling, the second of which is where CR
-- 604.3a makes the gained ability characteristic-defining), its CR 707.5 eligible set
-- (Replacement.legalCopyTargets, Copy Enchantment's "any enchantment" against Clone's
-- "any creature", and Clever Impersonator's negated "any nonland permanent"), the
-- P2 copy gate (Clone), and
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
-- And that same arm over CR 707.10's other two nouns -- an activated and a
-- triggered ability on the stack, copied by Lithoform Engine
-- (copyAbilityOnStackSpec), where CR 707.10b keeps the original's source.
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
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
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
import qualified Pawl.Types.AttackTarget as AttackTarget
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
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.Mana as Mana.Type
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
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

-- CR 302.6: a permanent that entered this turn has not been under its
-- controller's control continuously since their turn began. Written by hand
-- because S.spellOnStack records Sickness.Settled, so the permanent a resolution
-- makes out of it would otherwise be able to attack with no haste at all -- and
-- asserted on the board before the attack, since Dack's Duplicate's haste half
-- rests on it.
sickened :: ObjectId -> GameState.GameState -> GameState.GameState
sickened oid gs = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}

-- Distinct life totals, so no reading of "the player with the most life" (CR
-- 702.105a) is reached by a coincidence: bob is ahead and is the one attacked.
withLives :: Integer -> Integer -> GameState.GameState -> GameState.GameState
withLives a b gs =
  let at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in gs {GameState.players = at S.alice a (at S.bob b (GameState.players gs))}

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
activates :: ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Prompt.Prompt r -> r
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
-- Named rather than "whatever is legal" for `activates`' reason, even where one
-- board offers a single candidate: an answerer that searched would quietly repair
-- a mutation on a board that later grows a second creature.
activatesTargeting :: ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> ObjectId -> Prompt.Prompt r -> r
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
sacrificeAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
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
animationAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
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

  -- THE PROVING TEST for CR 707.9a, the exception that makes the copy GAIN an
  -- ability. Dack's Duplicate {2}{U}{R} Creature -- Shapeshifter 0/0: "You may
  -- have this creature enter as a copy of any creature on the battlefield,
  -- except it has haste and dethrone."
  --
  -- Both keywords are read at GAMEPLAY level in one attack: without haste (CR
  -- 702.10b) the copy could not be declared at all, and dethrone (CR 702.105a)
  -- is the +1/+1 counter it takes for attacking the player with the most life.
  -- So 3/2 is "both arrived" and 2/1 is "neither did".
  --
  -- A CLONE copying the SAME Piker is the control: the copy without the
  -- exception, entering the same turn, equally sick. It cannot attack and takes
  -- no counter, which is what separates the exception from the copy road.
  -- Everything else about the Duplicate is asserted to be the Piker's (CR
  -- 707.2), since an exception modifies the copying process rather than
  -- replacing it.
  Spec.it s "Dack's Duplicate copies a creature and gains haste and dethrone (CR 707.9a)" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    clone <- S.printingOf s registry "Clone"
    duplicate <- S.printingOf s registry "Dack's Duplicate"
    let (pikerId, board) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        (_, stagedClone) = S.spellOnStack clone S.alice board
        withClone = resolveAndSettle (copyNamed pikerId) stagedClone
        (_, stagedDuplicate) = S.spellOnStack duplicate S.alice withClone
        entered = resolveAndSettle (copyNamed pikerId) stagedDuplicate
    case (cloneOnBattlefield entered, newest (printedOnBattlefield "Dack's Duplicate" entered)) of
      (Just cloneId, Just duplicateId) -> do
        let ready = sickened duplicateId (sickened cloneId (withLives 15 20 entered))
            fought = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (S.attackTo S.bob) (intoCombat ready)
        -- The precondition the haste half rests on: neither copy is settled, so
        -- CR 302.6 is a real bar for the one without haste.
        Spec.assertEqWith s "both copies are summoning sick (CR 302.6)" (fmap Object.sickness (Game.lookupObject duplicateId ready), fmap Object.sickness (Game.lookupObject cloneId ready)) (Just Sickness.Sick, Just Sickness.Sick)
        -- CR 707.2 ran: the exception modified the copy, it did not replace it.
        Spec.assertEqWith s "the Duplicate is the Piker by name (CR 707.2)" (Projection.namesOf duplicateId entered) . Set.singleton . CardName.MkCardName $ Text.pack "Goblin Piker"
        Spec.assertBool s (Set.member Subtype.Goblin (Projection.subtypesOf duplicateId entered)) "and a Goblin, the Piker's own subtype"
        Spec.assertEqWith s "and the Piker's 2/1" (S.powerToughnessOf duplicateId entered) $ Just (2, 1)
        -- THE GAMEPLAY ASSERTION, ahead of the two diagnostics below: it attacked
        -- (haste) and grew (dethrone).
        Spec.assertEqWith s "the Duplicate attacked and dethrone grew it to 3/2" (S.powerToughnessOf duplicateId fought) $ Just (3, 2)
        Spec.assertEqWith s "the copy without the exception is still a 2/1" (S.powerToughnessOf cloneId fought) $ Just (2, 1)
        Spec.assertEqWith s "and the Clone never joined the attack (CR 508.1a)" (Map.keys (Combat.Type.attackers (GameState.combat fought))) [duplicateId]
      _ -> Spec.assertFailure s "the Clone and the Duplicate should both be on the battlefield"

  -- THE PROVING TEST for CR 604.3a's third criterion: an ability acquired
  -- through a copy effect is CHARACTERISTIC-DEFINING. Omni-Changeling {3}{U}{U}
  -- Creature -- Shapeshifter 0/0: "Changeling / Convoke / You may have this
  -- creature enter as a copy of any creature on the battlefield, except it has
  -- changeling."
  --
  -- Not implemented: convoke (#877), so pawl's Omni-Changeling pays {3}{U}{U} in
  -- full -- stricter than printed, and nothing below turns on the cost.
  --
  -- The copy's own printed changeling is GONE (CR 707.2 replaced it with the
  -- Piker's text), so the exception is the only source of it. Lord of Atlantis
  -- is the reader -- "other Merfolk get +1/+1 and have islandwalk", an affected
  -- set read off the projection -- so a copy that is every creature type (CR
  -- 702.73a) is a Merfolk and gets pumped.
  --
  -- Three readings of one board, all different: bob's Goblin Piker is no
  -- Merfolk and stays 2/1; a Clone copying it is the copy WITHOUT the exception
  -- and stays 2/1; the Omni-Changeling copy is 3/2. The token copy is the fourth
  -- and is where the CDA claim actually bites -- CR 707.2 copies the copiable
  -- values and leaves every CR 613 layer behind, so a changeling GRANTED over
  -- the copy would produce a 2/1 token here.
  Spec.it s "Omni-Changeling's copy is every creature type, and so is a token copy of it (CR 604.3a)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    lord <- S.printingOf s registry "Lord of Atlantis"
    clone <- S.printingOf s registry "Clone"
    omni <- S.printingOf s registry "Omni-Changeling"
    counterpart <- S.printingOf s registry "Cackling Counterpart"
    let (_, withLord) = S.addCreature lord S.alice (S.landsInPlay island 3)
        (pikerId, board) = S.addCreature piker S.bob withLord
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
        resolved = castAndResolve (rites (KickerDecision.MkKickerDecision 0) pikerId) rite board
    Spec.assertEqWith s "one token, and it is the Piker" (mintedTokens resolved) [(Set.singleton (CardName.MkCardName (Text.pack "Goblin Piker")), Just (2, 1))]

  Spec.it s "kicked Rite of Replication mints five instead (CR 702.33d, CR 707.1)" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rite <- S.printingOf s registry "Rite of Replication"
    let (pikerId, board) = S.addCreature piker S.alice (S.landsInPlay island 9)
        resolved = castAndResolve (rites (KickerDecision.MkKickerDecision 1) pikerId) rite board
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
        resolved = castAndResolve (rites (KickerDecision.MkKickerDecision 1) cloneId) rite board
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
            Object.kicked = Map.empty,
            Object.bestowed = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.Type.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
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

-- CR 601.2c's whole announcement for Fall of the Hammer, pinned per slot name and
-- FILTERED out of the offered set for pinTarget's reason. Reaches both the cast
-- and CR 707.10c's re-target prompt, which is why each run below hands it its
-- own pair.
retargetHammer :: ObjectId -> ObjectId -> Prompt.Prompt r -> r
retargetHammer dealerId victimId p = case p of
  Prompt.ChooseTargets _ _ _ asked ->
    Map.mapWithKey
      ( \slot (_, offered) ->
          let wanted = if slot == SlotName.MkSlotName (Text.pack "dealer") then dealerId else victimId
           in Set.filter ((==) (Just wanted) . Recipient.objectOf) offered
      )
      asked
  _ -> S.identityAnswer p

-- alice casts Fall of the Hammer with the Giant dealing to bob's Wall, then --
-- CR 117.3c, still holding priority -- Twincast at it, and resolves Twincast,
-- then the copy, then the original. `answer` is what CR 707.10c's prompt reaches.
hammerThenTwincast :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> ObjectId -> ObjectId -> ObjectId -> GameState.GameState -> Maybe GameState.GameState
hammerThenTwincast answer dealerId victimId hammerId twincastId board =
  let cast1 = snd (Engine.runGamePure (retargetHammer dealerId victimId) board (S.cast S.alice hammerId))
   in do
        hammerSpell <- topOfStack cast1
        let cast2 = snd (Engine.runGamePure (pinTarget (Recipient.ToObject hammerSpell)) cast1 (S.cast S.alice twincastId))
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
  -- CR 707.10c through CR 601.2c: the new targets are judged as ONE
  -- announcement, not slot by slot. The offered set per slot is the union over
  -- what a sibling slot could still take, so a pair of slots that exclude each
  -- other passes the per-slot check and only the joint re-derivation catches it;
  -- the rule's own no-op is what a rejection falls to, "the player may leave any
  -- number of the targets unchanged".
  --
  -- Fall of the Hammer {1}{R} Instant (data/cards/fall-of-the-hammer.json):
  -- "Target creature you control deals damage equal to its power to another
  -- target creature." Its victim slot is Filter.Not (Filter.IsBound "dealer"),
  -- a filter-side sibling read (Pawl.TargetSpec has its cast-time cases).
  --
  -- THREE RUNS off one board, differing in exactly one thing -- the answer CR
  -- 707.10c's prompt is given -- and no two share a number. The Wall of Stone is
  -- 0/8 so that nothing dies in any run: a dead dealer would leave the ORIGINAL
  -- spell with an illegal target and make every run read zero for its own reason.
  Spec.it s "CR 707.10c a copy's new targets are judged as one announcement" $ do
    mountain <- S.printingOf s registry "Mountain"
    island <- S.printingOf s registry "Island"
    giant <- S.printingOf s registry "Hill Giant"
    spider <- S.printingOf s registry "Giant Spider"
    wall <- S.printingOf s registry "Wall of Stone"
    hammer <- S.printingOf s registry "Fall of the Hammer"
    twincast <- S.printingOf s registry "Twincast"
    let lands = S.landsFor island S.alice 2 (S.landsFor mountain S.alice 2 S.threePlayerGame)
        (giantId, g1) = S.addCreature giant S.alice lands
        (spiderId, g2) = S.addCreature spider S.alice g1
        (wallId, g3) = S.addCreature wall S.bob g2
        (withHammer, hammerId) = S.handOne hammer g3
        (twincastId, board) = handAppend twincast S.alice withHammer
        run dealerId victimId = hammerThenTwincast (retargetHammer dealerId victimId) giantId wallId hammerId twincastId board
        atSelf = run giantId giantId
        atSpider = run spiderId wallId
        atGiant = run spiderId giantId
        damage oid = fmap (S.damageOf oid)
    -- The behaviour first: naming the Giant in both of the copy's slots is not an
    -- announcement CR 601.2c allows, so the copy keeps the targets CR 707.10
    -- gave it and the Wall takes the Giant's 3 twice over.
    Spec.assertEqWith s "the rejected re-target leaves the copy where it was, so the Wall takes the Giant's 3 from the copy and 3 from the original" (damage wallId atSelf) (Just (Just 6))
    Spec.assertEqWith s "and the Giant, named twice, took none of its own damage" (damage giantId atSelf) (Just (Just 0))
    -- The prompt is LIVE on this board, which is what keeps the run above from
    -- passing off a prompt that was never raised: the same prompt moves the
    -- copy's dealer slot to the Spider, and 2 + 3 is a number no other run reads.
    Spec.assertEqWith s "re-targeting the copy's dealer at the Spider, the Wall takes 2 from the copy and 3 from the original" (damage wallId atSpider) (Just (Just 5))
    -- And the Giant IS offered for the victim slot, so the first run's rejection
    -- is the joint check rather than a slot the offer had emptied.
    Spec.assertEqWith s "with the Spider dealing instead, the Giant is a legal victim and takes its 2" (damage giantId atGiant) (Just (Just 2))
    Spec.assertEqWith s "while the Wall then takes only the original's 3" (damage wallId atGiant) (Just (Just 3))

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

-- CR 702.21a's observer for a copy's targets: bob's Tomakul Honor Guard, {1}{G}
-- 3/1 whose whole text box is "Ward {2}", against alice's Twincast.
--
-- THREE SEATS, and each holds one role: alice copies, bob controls the warded
-- creature, and carol is neither -- so "a spell an opponent controls" is not the
-- same sentence as "a spell alice controls".
--
-- alice's own Goblin Piker is what the ORIGINAL Giant Growth names in the
-- re-target case, so the Guard is a target of the COPY and of nothing else there.
--
-- FIVE LANDS: one Forest and two Islands are the Growth and the Twincast exactly,
-- and the two Forests left over are the ward cost alice CAN pay and declines --
-- so a countered copy is her declining rather than her being unable.
wardedCopyBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId, ObjectId, ObjectId, ObjectId, GameState.GameState)
wardedCopyBoard forest island guard piker growth twincast =
  let lands = S.landsFor island S.alice 2 (S.landsFor forest S.alice 3 S.threePlayerGame)
      (withGrowth, growthId) = S.handOne growth lands
      (twincastId, withTwincast) = S.addHandCard twincast S.alice withGrowth
      (guardId, withGuard) = S.addCreature guard S.bob withTwincast
      (pikerId, gs) = S.addCreature piker S.alice withGuard
   in (guardId, pikerId, growthId, twincastId, gs)

-- Resolve until the stack is empty. The two readings of a copy's targets put
-- DIFFERENT numbers of objects on the stack -- one ward trigger more under the
-- rule -- so a fixed count of resolutions would leave the two boards at
-- different depths and the assertion would be reading two different moments.
drainStack :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
drainStack answer = go (10 :: Int)
  where
    go fuel gs
      | fuel <= 0 || null (GameState.stack gs) = gs
      | otherwise = go (fuel - 1) (resolveOne answer gs)

copyTargetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copyTargetSpec s registry =
  let boardOf = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        guard <- S.printingOf s registry "Tomakul Honor Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        growth <- S.printingOf s registry "Giant Growth"
        twincast <- S.printingOf s registry "Twincast"
        pure (wardedCopyBoard forest island guard piker growth twincast)
   in Spec.describe s "Pawl.Engine.Copy" $ do
        -- CR 115.1: "these targets are declared as part of the process of putting
        -- the spell or ability on the stack", and CR 707.10c puts the copy on the
        -- stack WITH the targets its controller has just decided on. So the
        -- Guard becomes a target of the copy, and CR 702.21a's ward fires on it.
        --
        -- The Growth's own target is alice's Piker, so nothing but the copy ever
        -- names the Guard: 3/1 against 6/4 is the copy having been countered,
        -- and the Piker's 5/4 is the original resolving either way.
        Spec.it s "CR 115.1 the copy's NEW target becomes a target, and ward fires" $ do
          (guardId, pikerId, growthId, twincastId, board) <- boardOf
          let castGrowth = S.runPure (pinTarget (Recipient.ToCreature pikerId)) board (S.cast S.alice growthId)
          case topOfStack castGrowth of
            Nothing -> Spec.assertFailure s "Giant Growth never reached the stack"
            Just growthSpell -> do
              let cast = S.runPure (pinTarget (Recipient.ToObject growthSpell)) castGrowth (S.cast S.alice twincastId)
                  -- Twincast resolves, and CR 707.10c's prompt sends the copy at
                  -- the Guard instead of the Piker.
                  copied = resolveOne (pinTarget (Recipient.ToCreature guardId)) cast
                  after = drainStack S.identityAnswer copied
              Spec.assertEqWith s "CR 702.21a countered the copy, so the Guard is still a 3/1" (S.powerToughnessOf guardId after) (Just (3, 1))
              Spec.assertEqWith s "and alice's Piker took the ORIGINAL Growth" (S.powerToughnessOf pikerId after) (Just (5, 4))
              Spec.assertEqWith s "the ward trigger went on the stack over the copy and the Growth" (length (GameState.stack copied)) 3
              Spec.assertEqWith s "and everything resolved" (length (GameState.stack after)) 0
        -- The same rule for a target the copy KEPT. CR 707.10 copies the
        -- decisions and CR 707.10c leaves an unchanged target unchanged, but the
        -- copy is "itself a spell" and the Guard becomes a target of THAT spell
        -- too, so ward fires a second time.
        --
        -- Here the Growth names the Guard, so ward fires once as it is cast; the
        -- copy's own trigger is the second. Both are declined, so 3/1 is both
        -- spells countered and 6/4 is the copy having resolved -- which is what
        -- an engine that recorded only a CHANGED target reads.
        Spec.it s "CR 707.10 a target the copy KEPT becomes a target of the copy too" $ do
          (guardId, pikerId, growthId, twincastId, board) <- boardOf
          let castGrowth = S.runPure (pinTarget (Recipient.ToCreature guardId)) board (S.cast S.alice growthId)
          case topOfStack castGrowth of
            Nothing -> Spec.assertFailure s "Giant Growth never reached the stack"
            Just growthSpell -> do
              let cast = S.runPure (pinTarget (Recipient.ToObject growthSpell)) castGrowth (S.cast S.alice twincastId)
                  copied = resolveOne (pinTarget (Recipient.ToCreature guardId)) cast
                  after = drainStack S.identityAnswer copied
              Spec.assertEqWith s "both the copy and the Growth were countered, so the Guard is a 3/1" (S.powerToughnessOf guardId after) (Just (3, 1))
              Spec.assertEqWith s "alice's Piker, whom nothing named, is untouched" (S.powerToughnessOf pikerId after) (Just (2, 1))
              Spec.assertEqWith s "TWO ward triggers stand over the copy and the Growth" (length (GameState.stack copied)) 4
              Spec.assertEqWith s "and everything resolved" (length (GameState.stack after)) 0

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
      add (acc, g) c = let (oid, g') = S.addGraveyardCard c S.alice g in (acc <> [oid], g')
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
-- else. The pair below differs in exactly one thing -- which recipient the copy's
-- prompt is pinned to -- and between them they fix the number at 2: the Evangel
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
-- second case, which is what makes this a proof of the BOUND rather than of
-- either road.
stirCopySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
        -- THE proving case: the copy is offered, and takes, a card the ANNOUNCED
        -- X admits and the original did not name.
        Spec.it s "CR 707.10c the copy's new target is judged against the copied X" $ do
          (stirId, twincastId, ids, board) <- boardOf
          case ids of
            [pikerId, evangelId, _, _] ->
              case play (Recipient.ToObject evangelId) pikerId stirId twincastId board of
                Nothing -> Spec.assertFailure s "Stir the Grave never reached the stack"
                Just after -> do
                  Spec.assertEqWith s "CR 202.3: the copy returned the OTHER mana value 2 creature card" (S.countOnBattlefieldByName (cardNamed "Cabal Evangel") S.alice after) 1
                  Spec.assertEqWith s "and the original returned the one it named" (S.countOnBattlefieldByName (cardNamed "Goblin Piker") S.alice after) 1
                  Spec.assertEqWith s "the mana value 3 card stayed in the graveyard" (S.countOnBattlefieldByName (cardNamed "Kalakscion, Hunger Tyrant") S.alice after) 0
                  Spec.assertEqWith s "and everything resolved" (length (GameState.stack after)) 0
            _ -> Spec.assertFailure s "fixture should stock alice's graveyard with four cards"
        -- The same board, the same announcement, one recipient different: the
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
  let (shifterId, board0) = S.addCreature shapeshifter S.alice (Setup.emptyGame S.bothPlayers)
      (evangelId, board1) = S.addCreature evangel S.bob board0
      (spiderId, board2) = S.addCreature spider S.alice board1
      -- addCreature alone arranges a board and fires nothing; the original is the
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
        (Damage.applyDamage [DamageEvent.MkDamageEvent src (Recipient.ToCreature victim) amount False False False 0 Nothing DamageKind.Noncombat])
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
    let (benaliaId, board0) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
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
    let (jaceId, bare) = S.addCreature jace S.alice (Setup.emptyGame S.bothPlayers)
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
    let (invasionId, bare) = S.addCreature invasion S.alice (Setup.emptyGame S.bothPlayers)
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
      (engineId, gs1) = S.addCreature engine S.alice base
      (bystander, gs2) = S.addCreature piker S.alice gs1
      (host, gs3) = S.addCreature mammoth S.alice gs2
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

bestowingRollicker, printedRollicker :: ObjectId -> Prompt.Prompt r -> r
bestowingRollicker = castingRollicker [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]
printedRollicker = castingRollicker [ManaSymbol.OfType (ManaType.Colored Color.Red)]

-- Every permanent named Nyxborn Rollicker alice has on the battlefield -- by
-- NAME rather than by Source, so a copy that never became a token and one that
-- became the wrong thing are both found and then told apart by Game.isToken.
rollickersOn :: Printing.Printing -> GameState.GameState -> [ObjectId]
rollickersOn printing gs =
  filter
    (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- alice casts the Rollicker (`casting` decides bestowed or printed), then -- CR
-- 117.3c, still holding priority -- activates the Engine's {4} ability at it and
-- resolves the ability. Returns the board with [copy, Rollicker] on the stack.
--
-- The {4} ability is picked by its cost rather than by index, so a reordering of
-- the card file cannot silently aim the {3} one at a creature spell and have the
-- activation refuse for want of a target.
copyRollicker :: (forall r. Prompt.Prompt r -> r) -> ObjectId -> ObjectId -> GameState.GameState -> Maybe GameState.GameState
copyRollicker casting engineId spellId board =
  let cast = S.runPure casting board (S.cast S.alice spellId)
      ready = cast {GameState.priority = Just S.alice}
   in do
        spell <- topOfStack cast
        ability <- List.find ((== Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) . Cost.Type.mana . ActivatedAbility.cost) (Projection.abilitiesOf engineId ready)
        let activated = S.runPure (pinTarget (Recipient.ToObject spell)) ready (Activate.activateAbility S.alice engineId ability)
        pure (resolveOne S.identityAnswer activated)

permanentCopySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
        -- CR 707.10f end to end: the copy resolves and a TOKEN Rollicker stands on
        -- the battlefield before the card's own spell has resolved, then the card
        -- beside it. The token read is first: an engine that dropped the copy
        -- reads no Rollicker at all, one that filed it as a card reads a
        -- non-token, and one that let CR 704.5e remove it reads nothing after
        -- settling -- resolveOne settles.
        Spec.it s "CR 707.10f a copy of a creature spell resolves as a token creature beside the card" $ do
          (rollicker, (engineId, _, host, spellId, _, board)) <- boardOf
          case copyRollicker (printedRollicker host) engineId spellId board of
            Nothing -> Spec.assertFailure s "the Rollicker never reached the stack, or the Engine offered no {4} ability"
            Just copied -> do
              let afterCopy = resolveOne S.identityAnswer copied
                  afterBoth = resolveOne S.identityAnswer afterCopy
              Spec.assertEqWith
                s
                "CR 707.10f: the copy resolved into ONE Rollicker, and it is a token"
                (fmap (\oid -> Game.isToken oid afterCopy) (rollickersOn rollicker afterCopy))
                [True]
              Spec.assertEqWith
                s
                "CR 111.13: with the spell's characteristics, a 1/1"
                (fmap (\oid -> S.powerToughnessOf oid afterCopy) (rollickersOn rollicker afterCopy))
                [Just (1, 1)]
              Spec.assertEqWith
                s
                "CR 707.10: under the copying effect's controller"
                (fmap (\oid -> Projection.controllerOf oid afterCopy) (rollickersOn rollicker afterCopy))
                [Just S.alice]
              Spec.assertEqWith
                s
                "CR 608.3a: the card then resolves beside it, one token and one card"
                (List.sort (fmap (\oid -> Game.isToken oid afterBoth) (rollickersOn rollicker afterBoth)))
                [False, True]
              Spec.assertEqWith
                s
                "and neither reached a graveyard"
                (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid afterBoth)) (Game.zoneMembers Zone.Graveyard S.alice afterBoth))
                []
              Spec.assertEqWith s "and the stack is empty" (length (GameState.stack afterBoth)) 0
        -- CR 702.103c: the copy of a bestowed Aura spell is a bestowed Aura
        -- spell, so CR 608.3c puts it onto the battlefield attached -- to the
        -- host the ORIGINAL chose, since CR 707.10 copies its target and the
        -- Engine's third ability chooses no new ones. The board is the case
        -- above's, differing in ONE thing: the cost CR 601.2b's announcement
        -- settled on.
        --
        -- The host's power is the gameplay-level read and comes first: 4/4 after
        -- the copy alone is the token pumping it, 5/5 after both is the card
        -- pumping it too, and the bystander at 2/1 is the choice having been
        -- carried rather than re-made.
        Spec.it s "CR 702.103c a copy of a bestowed Rollicker resolves as a token Aura attached to the same host" $ do
          (rollicker, (engineId, bystander, host, spellId, _, board)) <- boardOf
          case copyRollicker (bestowingRollicker host) engineId spellId board of
            Nothing -> Spec.assertFailure s "the Rollicker never reached the stack, or the Engine offered no {4} ability"
            Just copied -> do
              let afterCopy = resolveOne S.identityAnswer copied
                  afterBoth = resolveOne S.identityAnswer afterCopy
              Spec.assertEqWith s "CR 702.103b: the token Aura pumps the host to 4/4" (S.powerToughnessOf host afterCopy) (Just (4, 4))
              Spec.assertEqWith s "and the card's Aura makes it 5/5" (S.powerToughnessOf host afterBoth) (Just (5, 5))
              Spec.assertEqWith s "and the creature nobody chose is untouched" (S.powerToughnessOf bystander afterBoth) (Just (2, 1))
              Spec.assertEqWith
                s
                "CR 303.4: the token entered attached to the host"
                (fmap (\oid -> Object.attachedTo =<< Game.lookupObject oid afterCopy) (rollickersOn rollicker afterCopy))
                [Just (Recipient.ToCreature host)]
              Spec.assertEqWith
                s
                "CR 702.103b: and it is an Enchantment, not a Creature"
                (fmap (\oid -> Projection.cardTypesOf oid afterCopy) (rollickersOn rollicker afterCopy))
                [Set.singleton CardType.Enchantment]
              Spec.assertEqWith
                s
                "an Aura, with CR 205.1a having taken Satyr"
                (fmap (\oid -> Projection.subtypesOf oid afterCopy) (rollickersOn rollicker afterCopy))
                [Set.singleton Subtype.Aura]
              Spec.assertEqWith
                s
                "CR 707.10f: and a token"
                (fmap (\oid -> Game.isToken oid afterCopy) (rollickersOn rollicker afterCopy))
                [True]
        -- CR 702.103e on the COPY: its host is killed in response, so as the copy
        -- begins resolving it ceases to be bestowed and resolves as a creature
        -- spell -- a token creature, attached to nothing. The board is the case
        -- above's plus one Bolt at the host after the ability has resolved.
        Spec.it s "CR 702.103e a bestowed copy whose host died resolves as a token creature" $ do
          (rollicker, (engineId, bystander, host, spellId, boltId, board)) <- boardOf
          case copyRollicker (bestowingRollicker host) engineId spellId board of
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
-- copyRollicker's reason, one ability along.
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
-- Not implemented: the THIRD sentence's count -- how many times an ability has
-- resolved during the turn -- which nothing in pawl keeps, so no board here can
-- show that a copy counts as the same ability (gap #3135).
--
-- CR 707.10's "a copy of an activated ability isn't activated" rides on the first
-- case as alice's energy: the cost was paid once, by the activation, and the copy
-- pays nothing.
copyAbilityOnStackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
copyAbilityOnStackSpec s registry = Spec.describe s "Pawl.Engine.Copy" $ do
  -- TWO Cubs, alike in everything but which one's ability was copied, which is
  -- the pair CR 707.10b's second sentence needs: one Cub cannot tell "the same
  -- object" from "an object with that name".
  --
  -- Both counters land on the SAME Cub, so the read that discriminates is its
  -- count of two -- a copy that resolved against the wrong Cub leaves one each,
  -- and a copy that was never minted leaves one and zero.
  Spec.it s "CR 707.10b a copied activated ability keeps its source, and the copy is not activated" $ do
    mountain <- S.printingOf s registry "Mountain"
    engine <- S.printingOf s registry "Lithoform Engine"
    cub <- S.printingOf s registry "Longtusk Cub"
    let base = S.landsInPlay mountain 2
        (engineId, withEngine) = S.addCreature engine S.alice base
        (cubA, withA) = S.addCreature cub S.alice withEngine
        (cubB, withB) = S.addCreature cub S.alice withA
        -- Exactly one activation's worth of energy (CR 107.14), so a copy that
        -- charged its own cost could not have paid it.
        board = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice withB
    case (Maybe.listToMaybe (Projection.abilitiesOf cubA board), engineAbilityCopyingAbilities engineId board) of
      (Just pump, Just copier) -> do
        let activated = S.runPure S.identityAnswer board {GameState.priority = Just S.alice} (Activate.activateAbility S.alice cubA pump)
        case topOfStack activated of
          Nothing -> Spec.assertFailure s "the Cub's ability should be on the stack"
          Just abilId -> do
            let staged = S.runPure (pinTarget (Recipient.ToObject abilId)) activated {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
                -- The Engine's ability, then the copy it minted, then the Cub's
                -- own ability.
                afterEngine = resolveOne S.identityAnswer staged
                afterCopy = resolveOne S.identityAnswer afterEngine
                afterBoth = resolveOne S.identityAnswer afterCopy
            Spec.assertEqWith s "CR 707.10b both counters are on the Cub whose ability was copied" (S.counterOf CounterKind.PlusOnePlusOne cubA afterBoth) 2
            Spec.assertEqWith s "and none on the other Longtusk Cub" (S.counterOf CounterKind.PlusOnePlusOne cubB afterBoth) 0
            Spec.assertEqWith s "CR 707.10: the copy was not activated, so the energy paid once" (S.playerCounterOf PlayerCounterKind.Energy S.alice afterBoth) 0
            -- Supporting, and after the reads above so it can absorb no mutation
            -- they should catch: the copy really was a second object on the stack.
            Spec.assertEqWith s "the copy resolved before the original, leaving one counter" (S.counterOf CounterKind.PlusOnePlusOne cubA afterCopy) 1
            Spec.assertEqWith s "and the stack is empty" (GameState.stack afterBoth) []
      _ -> Spec.assertFailure s "Longtusk Cub should declare one activated ability, and Lithoform Engine a {2} one"
  -- CR 707.10c on an ABILITY, where the offer is a real choice: the copy is aimed
  -- at alice and the original stays on bob, so the two seats' life totals are
  -- 19 and 19. An engine that ignored the offer leaves 20 and 18, and one that
  -- minted no copy leaves 20 and 19 -- three distinct boards.
  Spec.it s "CR 707.10c new targets are chosen for a copied activated ability" $ do
    mountain <- S.printingOf s registry "Mountain"
    engine <- S.printingOf s registry "Lithoform Engine"
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let base = S.landsInPlay mountain 2
        (engineId, withEngine) = S.addCreature engine S.alice base
        (sorcererId, withSorcerer) = S.addCreature sorcerer S.alice withEngine
        -- CR 302.6: the Sorcerer's {T} is not payable until it has settled.
        board = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
    case (Maybe.listToMaybe (Projection.abilitiesOf sorcererId board), engineAbilityCopyingAbilities engineId board) of
      (Just ping, Just copier) -> do
        let pinged = S.runPure (pinTarget (Recipient.ToPlayer S.bob)) board {GameState.priority = Just S.alice} (Activate.activateAbility S.alice sorcererId ping)
        case topOfStack pinged of
          Nothing -> Spec.assertFailure s "the Sorcerer's ability should be on the stack"
          Just abilId -> do
            let staged = S.runPure (pinTarget (Recipient.ToObject abilId)) pinged {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
                -- The only ChooseTargets this run raises is CR 707.10c's, so this
                -- answerer cannot be confused with the two announcements above.
                afterEngine = resolveOne (pinTarget (Recipient.ToPlayer S.alice)) staged
                afterCopy = resolveOne S.identityAnswer afterEngine
                afterBoth = resolveOne S.identityAnswer afterCopy
            Spec.assertEqWith s "CR 707.10c the copy dealt its damage to alice, whom the original never targeted" (S.lifeOf S.alice afterBoth) (Just 19)
            Spec.assertEqWith s "and the original still dealt its damage to bob" (S.lifeOf S.bob afterBoth) (Just 19)
            Spec.assertEqWith s "the copy resolved first: bob was untouched at that point" (S.lifeOf S.bob afterCopy) (Just 20)
            Spec.assertEqWith s "and the stack is empty" (GameState.stack afterBoth) []
      _ -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability, and Lithoform Engine a {2} one"
  -- The rule's third noun. Ravenous Rats' "When Ravenous Rats enters, target
  -- opponent discards a card" is a TRIGGERED ability with a target, so one case
  -- reaches both the copy of a trigger and CR 707.10c over the slots an ability
  -- object declares -- which come off the modal its Source carries, there being
  -- no card behind an ability (CR 113.7a).
  --
  -- THREE seats, because two would leave "target opponent" one answer and CR
  -- 707.10c's offer elided as indistinguishable: bob is the original's target and
  -- carol the copy's, so each holds one card afterwards rather than one seat
  -- holding none.
  Spec.it s "CR 707.10 a triggered ability is copied, and its copy takes a new target" $ do
    swamp <- S.printingOf s registry "Swamp"
    engine <- S.printingOf s registry "Lithoform Engine"
    rats <- S.printingOf s registry "Ravenous Rats"
    let withLands = S.landsFor swamp S.alice 4 S.threePlayerGame
        (engineId, withEngine) = S.addCreature engine S.alice withLands
        -- TWO cards each, so a seat that discarded twice and a seat that
        -- discarded once are different boards.
        stock pid gs = snd (S.addHandCard rats pid (snd (S.addHandCard rats pid gs)))
        (board, ratsId) = S.handOne rats (stock S.carol (stock S.bob withEngine))
    case engineAbilityCopyingAbilities engineId board of
      Nothing -> Spec.assertFailure s "Lithoform Engine should declare a {2} ability"
      Just copier -> do
        let cast = S.runPure S.identityAnswer board {GameState.priority = Just S.alice} (S.cast S.alice ratsId)
            -- The Rats resolve and enter; CR 603.3b puts the trigger on the
            -- stack, and CR 603.3d announces its target there.
            triggered = resolveOne (pinTarget (Recipient.ToPlayer S.bob)) cast
        case topOfStack triggered of
          Nothing -> Spec.assertFailure s "the Rats' entry trigger should be on the stack"
          Just trigId -> do
            let staged = S.runPure (pinTarget (Recipient.ToObject trigId)) triggered {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
                afterEngine = resolveOne (pinTarget (Recipient.ToPlayer S.carol)) staged
                afterCopy = resolveOne S.identityAnswer afterEngine
                afterBoth = resolveOne S.identityAnswer afterCopy
            Spec.assertEqWith s "CR 707.10c the copy made carol discard, whom the trigger never targeted" (handSize S.carol afterBoth) 1
            Spec.assertEqWith s "and the trigger itself still made bob discard" (handSize S.bob afterBoth) 1
            Spec.assertEqWith s "the copy resolved first: bob still held both cards then" (handSize S.bob afterCopy) 2
            Spec.assertEqWith s "and the stack is empty" (GameState.stack afterBoth) []

-- CR 707.10d, end to end: Zada, Hedron Grinder {3}{R} Legendary Creature --
-- Goblin Ally 3/3, "Whenever you cast an instant or sorcery spell that targets
-- only Zada, copy that spell for each other creature you control that the spell
-- could target. Each copy targets a different one of those creatures."
-- (data/cards/zada-hedron-grinder.json, Oracle text verified 2026-09-03.)
--
-- Both halves of the rule are on one board: the COUNT (one copy per candidate
-- the Growth could target) and the TARGETS (the effect picks them, and no
-- prompt offers them to anyone). Blurred Mongoose is what makes "could target"
-- do work -- 2/1 with shroud (CR 702.18a), a creature alice controls that the
-- Growth cannot target, so rule 707.10d's last sentence gives it no copy.
--
-- FIVE DISTINCT PAIRS after the Growths, so no two reads share a number: the
-- Piker 5/4, the Spider 5/7, the Wall 3/11, Zada 6/6 from the original alone,
-- and the Mongoose still 2/1.
zadaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
            (zadaId, g1) = S.addCreature zada S.alice lands
            (pikerId, g2) = S.addCreature piker S.alice g1
            (spiderId, g3) = S.addCreature spider S.alice g2
            (wallId, g4) = S.addCreature wall S.alice g3
            (mongooseId, g5) = S.addCreature mongoose S.alice g4
            (withGrowth, growthId) = S.handOne growth g5
        pure (zadaId, pikerId, spiderId, wallId, mongooseId, growthId, withGrowth)
      -- The Growth is aimed at Zada and at nothing else, which is the trigger's
      -- whole condition; the copies' targets are the effect's and reach no
      -- prompt at all.
      atZada :: ObjectId -> Prompt.Prompt r -> r
      atZada zadaId p = case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((== Just zadaId) . Recipient.objectOf) offered) asked
        _ -> S.identityAnswer p
      -- What each object on the stack targets, top first, read live off its
      -- bindings the way Pawl.Engine.Resolve.targetsOnStack does.
      stackTargets gs = fmap (\oid -> Set.toList (Foldable.fold (Map.elems (Binding.targetsOf (maybe Map.empty Object.bindings (Game.lookupObject oid gs)))))) (GameState.stack gs)
   in Spec.describe s "Pawl.Engine.Copy" $ do
        Spec.it s "CR 707.10d Zada copies the spell once per creature it could target, each copy on a different one" $ do
          (zadaId, pikerId, spiderId, wallId, mongooseId, growthId, board) <- boardOf
          let cast = snd (Engine.runGamePure (atZada zadaId) board {GameState.priority = Just S.alice} (S.cast S.alice growthId))
              -- CR 603.3b puts Zada's trigger on the stack above the Growth;
              -- draining resolves the trigger, then its copies, then the Growth.
              placed = snd (Engine.runGamePure (atZada zadaId) cast Engine.settleForPriority)
              -- The trigger alone, which is the moment the copies exist and
              -- none has resolved -- where "a copy isn't created" is visible.
              afterTrigger = resolveOne (atZada zadaId) placed
              after = drainStack (atZada zadaId) placed
              pt oid = S.powerToughnessOf oid after
          -- The fixture's own precondition, which no reading of rule 707.10d can
          -- redden: the Mongoose is on the battlefield to be passed over.
          Spec.assertBool s (S.powerToughnessOf mongooseId board == Just (2, 1)) "the Mongoose starts 2/1"
          -- Rule 707.10d's last sentence at the only place it is observable: a
          -- copy made for the Mongoose anyway would be COUNTERED for an illegal
          -- target (CR 608.2b) and leave every P/T below reading the same, so
          -- the copies are counted and named where they sit on the stack.
          Spec.assertEqWith
            s
            "CR 707.10d one copy per creature the Growth could target, each on a different one, and none for the Mongoose"
            (List.sort (concatMap (Maybe.mapMaybe Recipient.objectOf) (List.init (stackTargets afterTrigger))))
            (List.sort [pikerId, spiderId, wallId])
          Spec.assertEqWith s "CR 707.10d a copy targeted the Piker" (pt pikerId) (Just (5, 4))
          Spec.assertEqWith s "CR 707.10d a different copy targeted the Spider" (pt spiderId) (Just (5, 7))
          Spec.assertEqWith s "CR 707.10d and a third the Wall" (pt wallId) (Just (3, 11))
          Spec.assertEqWith s "CR 707.10d the Mongoose has shroud, so the spell could not target it" (pt mongooseId) (Just (2, 1))
          Spec.assertEqWith s "and Zada took only the original Growth, no copy having been made for it" (pt zadaId) (Just (6, 6))
          Spec.assertEqWith s "and the stack is empty" (length (GameState.stack after)) 0
        -- CR 707.10d's one player choice: "the copies are put onto the stack
        -- with those targets in the order of their controller's choice". Two
        -- runs off one board differing in exactly the answer
        -- Prompt.OrderForEach is given, read off the STACK before anything
        -- resolves -- which is the moment the order is observable.
        Spec.it s "CR 707.10d the copies go onto the stack in their controller's chosen order" $ do
          (zadaId, _, _, _, _, growthId, board) <- boardOf
          let answering :: ([Natural.Natural] -> [Natural.Natural]) -> (forall r. Prompt.Prompt r -> r)
              answering reorder p = case p of
                Prompt.OrderForEach _ _ _ group -> reorder (zipWith const [0 ..] group)
                _ -> atZada zadaId p
              stacked reorder =
                let cast = snd (Engine.runGamePure (answering reorder) board {GameState.priority = Just S.alice} (S.cast S.alice growthId))
                    placed = snd (Engine.runGamePure (answering reorder) cast Engine.settleForPriority)
                    -- The trigger alone, so the copies are on the stack and none
                    -- of them has resolved.
                    afterTrigger = resolveOne (answering reorder) placed
                 in stackTargets afterTrigger
              offered = stacked id
              reversed = stacked List.reverse
          Spec.assertEqWith s "CR 707.10d reversing the answer reverses the copies on the stack" (take 3 reversed) (List.reverse (take 3 offered))
          Spec.assertBool s (take 3 offered /= take 3 reversed) "and the two orders differ, so the prompt was live"
          Spec.assertEqWith s "three copies over the Growth, in both runs" (length offered, length reversed) (4, 4)

-- The slots a COPIED TRIGGER declares, which CR 603.2's bindings narrow: Questing
-- Beast's "target planeswalker THAT PLAYER controls" (Filter.ControlledByBound
-- "thatPlayer"). Pawl.Engine.Engine.placeBorne bakes that map into the modal as
-- the trigger goes on the stack, and CR 707.10 copies the bindings onto the copy,
-- so the copy's slots have to be baked from the copy's own map before CR 707.10c
-- can offer anything: an unbaked ControlledByBound admits nobody, which would
-- leave the offer elided as "settled" and the copy silently on the original's
-- target.
--
-- THREE SEATS with carol holding BOTH planeswalkers, on 9 and 6 loyalty: the two
-- readings are 5 / 2 against 1 / 6, and no number is shared.
copiedTriggerTargetSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
copiedTriggerTargetSpec s registry =
  let pinWalker :: ObjectId -> Prompt.Prompt r -> r
      pinWalker oid p = case p of
        Prompt.ChooseTargets _ _ _ asked -> fmap (\(_, offered) -> Set.filter ((== Just oid) . Recipient.objectOf) offered) asked
        _ -> S.identityAnswer p
   in Spec.describe s "Pawl.Engine.Copy" $ do
        Spec.it s "CR 707.10c a copied trigger's slot is baked, so the offer is a real choice" $ do
          beast <- S.printingOf s registry "Questing Beast"
          engine <- S.printingOf s registry "Lithoform Engine"
          karn <- S.printingOf s registry "Karn Liberated"
          -- TWO PRINTINGS, not one twice: CR 704.5j would put one of a pair of
          -- Karns into a graveyard before the trigger ever fired.
          jace <- S.printingOf s registry "Jace Beleren"
          forest <- S.printingOf s registry "Forest"
          let (gs0, mine, _, others) = S.threePlayerCombat [beast, engine] [] [karn, jace]
          case (mine, others) of
            ([beastId, engineId], [firstWalker, secondWalker]) -> do
              let loyal = S.addCounter CounterKind.Loyalty 6 secondWalker (S.addCounter CounterKind.Loyalty 9 firstWalker gs0)
                  board = S.landsFor forest S.alice 2 loyal
                  plan :: Prompt.Prompt r -> r
                  plan p = case p of
                    Prompt.ChooseDefender {} -> S.carol
                    Prompt.ChooseAttackTarget _ _ _ options -> Maybe.fromMaybe (NonEmpty.head options) (List.find (== AttackTarget.OfPlayer S.carol) (NonEmpty.toList options))
                    Prompt.DeclareBlockers {} -> Map.empty
                    Prompt.ChooseTargets {} -> pinWalker firstWalker p
                    _ -> S.aggressiveAnswer p
                  atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan board
                  fought = S.runPure plan atDamage Damage.dealCombatDamage
                  placed = S.runPure plan fought Engine.settleForPriority
              case (engineAbilityCopyingAbilities engineId placed, topOfStack placed) of
                (Just copier, Just trigId) -> do
                  let staged = S.runPure (pinTarget (Recipient.ToObject trigId)) placed {GameState.priority = Just S.alice} (Activate.activateAbility S.alice engineId copier)
                      -- The Engine's ability, then the copy, then the trigger.
                      afterEngine = resolveOne (pinWalker secondWalker) staged
                      after = resolveOne S.identityAnswer (resolveOne S.identityAnswer afterEngine)
                  Spec.assertEqWith s "CR 707.10c the copy dealt its damage to the walker alice re-targeted it at" (S.counterOf CounterKind.Loyalty secondWalker after) 2
                  Spec.assertEqWith s "and the trigger itself still dealt its own to the walker it announced" (S.counterOf CounterKind.Loyalty firstWalker after) 5
                  Spec.assertBool s (beastId /= engineId) "the Beast and the Engine are distinct objects"
                (_, _) -> Spec.assertFailure s "the Beast's trigger should be on the stack and the Engine should declare a {2} ability"
            _ -> Spec.assertFailure s "fixture should give alice a Beast and an Engine and carol two planeswalkers"

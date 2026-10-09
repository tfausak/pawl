{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Combat over attack targets other than a player (CR 508.1) and
-- the defending player they imply: planeswalkers, battles, shared blockers,
-- last-known and split defenders, Soul Snare and Meandering Towershell. Split out of Pawl.CombatEffectSpec, which keeps the machinery.
module Pawl.PlaneswalkerCombatSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.CombatEffectSpec (attackJaceAndBob, attackThePlaneswalker, blockAndWane, blockWithJace, creaturePlaneswalkerBoard, runToEndOfCombat, runToEndOfCombatWith, tapStateOf)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
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
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.TapState as TapState

-- CR 506.4d: "A permanent that's both a blocking creature and a planeswalker
-- that's being attacked is removed from combat if it stops being both a creature
-- and a planeswalker. If it stops being one of those card types but continues to
-- be the other, it continues to be either a blocking creature or a planeswalker
-- that's being attacked, whichever is appropriate."
--
-- The combat POSITION #981 said no board could reach. It is reachable, and nothing
-- in the engine stood in the way: both roles belong to the DEFENDING player -- CR
-- 508.1b's attacked planeswalker is one they control (CR 306.6) and CR 509.1a's
-- blockers are theirs too -- and canBlockGiven gates on controller, battlefield
-- membership, tap state, creature-ness and CR 509.1b's restrictions, none of which
-- excludes a permanent that is itself being attacked.
--
-- Four pool cards carry the rule, every oracle text checked against Scryfall (two
-- Llanowar Elves and a Plains are scaffolding -- see creaturePlaneswalkerBoard):
--
--   * Jace Beleren ({1}{U}{U} Legendary Planeswalker -- Jace) is bob's, and the
--     permanent that holds both roles.
--   * Liquimetal Coating ({2} Artifact, "{T}: Target permanent becomes an artifact
--     in addition to its other types until end of turn") is alice's; its target
--     slot carries no filter, so it reaches an opponent's planeswalker.
--   * March of the Machines ({3}{U} Enchantment, "Each noncreature artifact is an
--     artifact creature with power and toughness each equal to its mana value")
--     animates the coated Jace. CR 613.8's dependency is what makes the pair work:
--     March is the older effect, so timestamp order alone would ask it about a
--     Jace that is not yet an artifact. Pawl.ProjectionSpec's "CR 613.8b whole
--     cards" case pins that on this very pair, and Jace's mana value 3 is why a
--     planeswalker survives where that case's land -- mana value 0 -- is buried by
--     CR 704.5f.
--   * Wane ({W} Instant, "Destroy target enchantment", the right half of
--     Wax // Wane) kills March after blockers are declared. Liquimetal's effect is
--     UntilEndOfTurn and outlives its source, so Jace stops being a CREATURE while
--     staying an artifact PLANESWALKER (CR 611.3b for the animation ending, CR
--     613.1d for card types being a layer-4 read) -- exactly CR 506.4d's "stops
--     being one of those card types but continues to be the other".
--
-- Every leg hands over at the declare blockers step, typeChangeRemovalSpec's
-- pattern, so the block is declared before the type change lands, and stops at the
-- end of combat step where CR 511.3 leaves the record live.
creaturePlaneswalkerCombatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
creaturePlaneswalkerCombatSpec s registry = Spec.describe s "CreaturePlaneswalkerInCombat" $ do
  Spec.it s "CR 506.4d whole cards: a blocking Jace that stops being a creature is still a planeswalker that's being attacked" $ do
    jace <- S.printingOf s registry "Jace Beleren"
    elves <- S.printingOf s registry "Llanowar Elves"
    coating <- S.printingOf s registry "Liquimetal Coating"
    march <- S.printingOf s registry "March of the Machines"
    plains <- S.printingOf s registry "Plains"
    waxWane <- S.printingOf s registry "Wane"
    case creaturePlaneswalkerBoard jace elves coating march plains waxWane of
      Nothing -> Spec.assertFailure s "fixture should give alice two Llanowar Elves and a Coating with one activated ability, and bob a Jace"
      Just (gs, atJace, atBob, jaceId, marchId) -> do
        -- The fixture pins. Without these the discriminating assertions below can
        -- pass for the wrong reason: a Jace that was never animated is never a
        -- blocking creature, and every later reading is about a different rule.
        Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf jaceId gs)) "CR 205.1b: the Coating made Jace an artifact"
        Spec.assertBool s (Projection.isCreatureOf jaceId gs) "CR 613.8: so March animates him"
        Spec.assertEqWith s "a 3/3, his mana value" (S.powerToughnessOf jaceId gs) (Just (3, 3))
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackJaceAndBob atJace atBob) gs
            atEnd = runToEndOfCombat (blockAndWane jaceId atBob marchId) atBlockers
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the block is declared before the kill" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        Spec.assertEqWith s "one attacker really was announced at the planeswalker (CR 508.1b)" (Map.lookup atJace (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfPlaneswalker jaceId))
        Spec.assertEqWith s "and the other at bob" (Map.lookup atBob (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfPlayer S.bob))
        Spec.assertEqWith s "the leg reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertBool s (not (S.onBattlefield marchId atEnd)) "the Wane really did destroy March of the Machines"
        Spec.assertBool s (not (Projection.isCreatureOf jaceId atEnd)) "CR 611.3b: so Jace stopped being a creature"
        Spec.assertBool s (Projection.isPlaneswalkerOf jaceId atEnd) "and is still a planeswalker"
        Spec.assertBool s (S.onBattlefield jaceId atEnd) "and still on the battlefield, so this is the card-types clause and not the leaves-the-battlefield one"
        -- CR 506.4d's first half: he "continues to be a planeswalker that's being
        -- attacked". The record is keyed by the ATTACKER -- Jace is an attack
        -- TARGET, never an attacker -- so this is the entry an engine that treated
        -- removal from combat as removing attacked-ness too would have deleted.
        Spec.assertEqWith s "CR 506.4d: he continues to be a planeswalker that's being attacked" (Map.lookup atJace attackers) (Just (AttackTarget.OfPlaneswalker jaceId))
        -- CR 506.4d's second half: he stopped being a creature, so he stops being
        -- a blocking one. Asserted BEFORE the loyalty reading below, which is the
        -- shared gameplay consequence both halves land in: a sampler that never
        -- swept him out of the blocker set would show up there too, and the
        -- failure a reader wants to see first is the one about blocking.
        Spec.assertEqWith s "CR 506.4: Jace is blocking nothing" (Combat.blockersOf atBob atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked atBob atEnd) "CR 509.1h: but that attacker remains blocked"
        Spec.assertEqWith s "CR 510.1c: so it assigns no combat damage, and nothing was marked on Jace" (S.damageOf jaceId atEnd) (Just 0)
        Spec.assertEqWith s "CR 306.8 / 120.3c: only the attacker aimed at him took loyalty, 5 - 1" (S.counterOf CounterKind.Loyalty jaceId atEnd) 4
        Spec.assertEqWith s "and bob takes nothing from it" (S.lifeOf S.bob atEnd) (Just 20)
  Spec.it s "CR 506.4d the control leg: with March left alone Jace blocks, survives, and is attacked too" $ do
    -- The same board, the same block, differing in exactly one thing: alice never
    -- casts the Wane. Without it an engine that swept Jace out of combat on any
    -- resolution -- or that never let him block at all -- would pass the case
    -- above.
    jace <- S.printingOf s registry "Jace Beleren"
    elves <- S.printingOf s registry "Llanowar Elves"
    coating <- S.printingOf s registry "Liquimetal Coating"
    march <- S.printingOf s registry "March of the Machines"
    plains <- S.printingOf s registry "Plains"
    waxWane <- S.printingOf s registry "Wane"
    case creaturePlaneswalkerBoard jace elves coating march plains waxWane of
      Nothing -> Spec.assertFailure s "fixture should give alice two Llanowar Elves and a Coating with one activated ability, and bob a Jace"
      Just (gs, atJace, atBob, jaceId, marchId) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackJaceAndBob atJace atBob) gs
            atEnd = runToEndOfCombat (blockWithJace jaceId atBob) atBlockers
        Spec.assertBool s (S.onBattlefield marchId atEnd) "March of the Machines survives"
        Spec.assertBool s (Projection.isCreatureOf jaceId atEnd) "so Jace is still a creature"
        Spec.assertEqWith s "and still blocking the attacker aimed at bob" (Combat.blockersOf atBob atEnd) (Set.singleton jaceId)
        Spec.assertBool s (not (S.onBattlefield atBob atEnd)) "which his 3 power kills"
        Spec.assertEqWith s "CR 120.3e: both attackers' damage is marked on him as a creature" (S.damageOf jaceId atEnd) (Just 2)
        -- CR 120.3c AND CR 120.3e off each damage event, which is the reading
        -- Pawl.DamageSpec's CreatureAndPlaneswalker group proves: 5 - 1 (the
        -- attacker aimed at him) - 1 (the attacker he blocks) = 3, where the leg
        -- above reads 4 because only the first of those two ever lands.
        Spec.assertEqWith s "and both attackers' 1 came off his loyalty" (S.counterOf CounterKind.Loyalty jaceId atEnd) 3
        Spec.assertBool s (S.onBattlefield jaceId atEnd) "CR 704.5g and CR 704.5i: 2 marked on a 3-toughness creature and 3 loyalty left, so neither is lethal"
        Spec.assertEqWith s "and he is being attacked all along (CR 508.1b)" (Map.lookup atJace (Combat.Type.attackers (GameState.combat atEnd))) (Just (AttackTarget.OfPlaneswalker jaceId))

-- CR 508.4: "If a creature is put onto the battlefield attacking, its controller
-- chooses which defending player ... it's attacking ... Such creatures are
-- 'attacking' but, for the purposes of trigger events and effects, they never
-- 'attacked'."
--
-- Hanweir Garrison is the producer here: "Whenever this creature attacks,
-- create two 1/1 red Human creature tokens that are tapped and attacking."
putOntoBattlefieldAttackingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
putOntoBattlefieldAttackingSpec s registry = Spec.describe s "PutOntoBattlefieldAttacking" $ do
  Spec.it s "CR 508.4 whole card: Hanweir Garrison's two Humans enter tapped and attacking" $ do
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (gs, mine, _) = S.combatBoardOf [garrison] []
        -- The vantage point is the declare blockers step: the trigger fired
        -- at the declaration (CR 508.2b) and resolved in the declare
        -- attackers step's priority round, and CR 511.3 has not yet cleared
        -- the record.
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        tokens = S.tokensOf atBlockers
        attackers = Combat.Type.attackers (GameState.combat atBlockers)
        sicknessOf oid = fmap Object.sickness (Game.lookupObject oid atBlockers)
    Spec.assertEqWith s "the fixture reached the declare blockers step" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith s "the trigger fired once: two tokens" (length tokens) 2
    mapM_ (\oid -> Spec.assertEqWith s "tapped" (tapStateOf oid atBlockers) (Just TapState.Tapped)) tokens
    mapM_ (\oid -> Spec.assertEqWith s "attacking bob" (Map.lookup oid attackers) (Just (AttackTarget.OfPlayer S.bob))) tokens
    -- CR 302.6 restricts a creature from ATTACKING, and CR 508.4c exempts a
    -- creature put onto the battlefield attacking from the restrictions that
    -- apply to the declaration of attackers -- so a token that has been
    -- controlled for no time at all is attacking anyway.
    mapM_ (\oid -> Spec.assertEqWith s "still summoning sick" (sicknessOf oid) (Just Sickness.Sick)) tokens
    case mine of
      [garrisonId] -> Spec.assertEqWith s "and the Garrison itself is attacking" (Map.lookup garrisonId attackers) (Just (AttackTarget.OfPlayer S.bob))
      _ -> Spec.assertFailure s "fixture should have one Hanweir Garrison"
  Spec.it s "CR 508.3a the tokens are attacking, and the attack trigger fired only for the Garrison" $ do
    -- THE discriminating case, and the one a naive implementation gets
    -- wrong: CR 508.3a's "such abilities won't trigger if a creature is put
    -- onto the battlefield attacking", and CR 508.4's "such creatures are
    -- 'attacking' but ... they never 'attacked'". An engine that put the
    -- tokens into combat by routing them through the declaration would
    -- record them here, and every "whenever a creature attacks" ability
    -- would then fire for the tokens as well.
    --
    -- Two Garrisons, so the assertion is a LIST and not a singleton: a
    -- declaration really does record one entry per creature, which is what
    -- makes the tokens' absence a fact about the tokens rather than about
    -- the shape of the log.
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (gs, mine, _) = S.combatBoardOf [garrison, garrison] []
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        tokens = S.tokensOf atBlockers
        attackers = Combat.Type.attackers (GameState.combat atBlockers)
    Spec.assertEqWith s "each Garrison's trigger fired once: four tokens" (length tokens) 4
    Spec.assertEqWith s "all six creatures are attacking" (Map.size attackers) 6
    Spec.assertEqWith s "but only the two Garrisons were DECLARED" (S.attackerDeclarationsOf atBlockers) mine
    mapM_ (\oid -> Spec.assertBool s (notElem oid (S.attackerDeclarationsOf atBlockers)) "no token was declared") tokens

-- CR 306.6 / CR 508.1b: attacking a planeswalker, through Jace Beleren.
--
-- Jace Beleren is the whole board on bob's side: {1}{U}{U} Legendary
-- Planeswalker -- Jace, with printed loyalty 3, which is what makes every
-- assertion here arithmetic rather than a threshold nobody can miss -- a 2/1
-- Goblin Piker takes two of the three (CR 306.8), and two of them take all three
-- and reach CR 704.5i.
--
-- PlaneswalkerSpec covers the card itself, including CR 306.5b's entry
-- replacement; the counters here are placed as a state fixture, because a
-- combat board cannot reach the sorcery-speed cast that would place them.
jaceBoard :: Printing.Printing -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], ObjectId.ObjectId)
jaceBoard jace mine =
  let (gs, ours, theirs) = S.combatBoardOf mine [jace]
   in case theirs of
        [jaceId] -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, jaceId)
        -- Unreachable (combatBoardOf returns one id per printing), and total
        -- rather than an `error`: S.noSource names no object, so a fixture that
        -- somehow got here fails the first assertion instead of the suite.
        _ -> (gs, ours, S.noSource)

-- Record every CR 508.1b announcement the engine asks for -- the creature and the
-- options it was offered -- and answer it with the planeswalker. The prompt is
-- elided at one candidate, so an empty log is the assertion that nothing was
-- asked.
announcementLog :: Prompt.Prompt r -> State.State [(ObjectId.ObjectId, [AttackTarget.AttackTarget])] r
announcementLog p = case p of
  Prompt.ChooseAttackTarget _ _ oid options -> do
    State.modify' (\seen -> seen <> [(oid, NonEmpty.toList options)])
    pure (attackThePlaneswalker p)
  _ -> pure (attackThePlaneswalker p)

-- Declare attackers under the recording interpreter, keeping the log.
announcementsFor :: GameState.GameState -> [(ObjectId.ObjectId, [AttackTarget.AttackTarget])]
announcementsFor gs = State.execState (Engine.runGame announcementLog gs (Combat.declareAttackers S.manaPerformer S.alice)) []

planeswalkerAttackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
planeswalkerAttackSpec s registry = Spec.describe s "AttackingAPlaneswalker" $ do
  -- The pair that makes the announcement a choice: ONE board, two interpreters,
  -- two different games. An engine that answered CR 508.1b for the player could
  -- not produce both lines.
  Spec.it s "CR 508.1b both answers are reachable: the same board, attacked the other way" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker]
        atJace = S.runCombat attackThePlaneswalker gs
        atBob = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "attacking Jace: bob is untouched" (S.lifeOf S.bob atJace) (Just 20)
    Spec.assertEqWith s "attacking Jace: two counters gone" (S.counterOf CounterKind.Loyalty jaceId atJace) 1
    Spec.assertEqWith s "attacking bob: he takes two" (S.lifeOf S.bob atBob) (Just 18)
    Spec.assertEqWith s "attacking bob: Jace keeps all three" (S.counterOf CounterKind.Loyalty jaceId atBob) 3
  -- The regression guard, and the elision: CR 508.1b calls for no announcement
  -- when the defending player controls no planeswalker, so the engine must not
  -- ask -- and the board must play exactly as it did before the prompt existed.
  Spec.it s "CR 508.1b with no planeswalker the announcement is not asked at all" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker, piker] []
    Spec.assertEqWith s "nothing was asked" (announcementsFor gs) []
    Spec.assertEqWith s "and the two Pikers still connect for 4" (S.lifeOf S.bob (S.runCombat attackThePlaneswalker gs)) (Just 16)
  -- CR 506.4 / CR 506.4c / CR 510.1b, at gameplay level and without an
  -- instant: two first strikers kill Jace in the FIRST combat damage step
  -- (CR 510.4), and the Piker attacking the same planeswalker then has nothing
  -- to assign in the second -- "If it isn't currently attacking anything (if,
  -- for example, it was attacking a planeswalker that has left the
  -- battlefield), it assigns no combat damage."
  --
  -- The control is the same board attacked the other way: 2 + 2 + 2 is bob at
  -- 14, so the missing 2 here is the rule and not a board that never dealt it.
  Spec.it s "CR 510.1b whole cards: a planeswalker killed by first strike leaves its attacker assigning nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    tiger <- S.printingOf s registry "Sabretooth Tiger"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [tiger, tiger, piker]
        atFirstStrike = S.runToStep (Phase.Combat CombatStep.CombatDamage) attackThePlaneswalker gs
        atSecond = snd (Engine.runGamePure attackThePlaneswalker atFirstStrike Engine.runStep)
        after = S.runCombat attackThePlaneswalker gs
        control = S.runCombat S.aggressiveAnswer gs
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield atSecond))) "the two 2/1 first strikers buried Jace (CR 704.5i)"
    case reverse mine of
      thePiker : _ -> do
        -- CR 506.4c: the Piker is still an attacking creature, though it is
        -- attacking nothing. Removing it from combat instead is the bug this
        -- pins.
        Spec.assertBool
          s
          (Map.member thePiker (Combat.Type.attackers (GameState.combat atSecond)))
          "CR 506.4c: the Piker remains an attacking creature"
        -- "It assigns no combat damage" is a claim about ASSIGNMENT, so it is
        -- asserted on the CR 608.2i damage log and not only on bob's life total:
        -- the planeswalker's id still names an object in the graveyard, so an
        -- engine that skipped CR 506.4 would deal the Piker's 2 to a permanent
        -- that is not there and leave every life total looking right.
        Spec.assertEqWith
          s
          "the Piker assigned no combat damage (CR 510.1b)"
          (filter (\ev -> DamageEvent.source ev == thePiker) (S.damageEventsOf after))
          []
      _ -> Spec.assertFailure s "fixture should have three attackers"
    Spec.assertEqWith s "so bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "the same board attacked at bob is 2 + 2 + 2" (S.lifeOf S.bob control) (Just 14)

-- Run whole combat steps under a MONADIC interpreter, so an assignment prompt can
-- be recorded as well as answered. S.runCombat's interpreter is pure and cannot
-- report what it was offered, and what CR 702.19c is about is the shape of the
-- offer.
runCombatLogging ::
  (forall r. Prompt.Prompt r -> State.State [Map.Map Recipient.Recipient Natural] r) ->
  GameState.GameState ->
  (GameState.GameState, [Map.Map Recipient.Recipient Natural])
runCombatLogging answer gs0 =
  let go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || not (S.inCombatPhase (GameState.phase g))
          then pure g
          else do
            (_, next) <- Engine.runGame answer g Engine.runStep
            go (n - 1) next
   in State.runState (go 24 gs0) []

-- Record every CR 702.19b/702.19c threshold map the engine offers, and answer it
-- with `answer` -- a fixed division the test picked, which is what makes the
-- assignment a CHOICE the interpreter made rather than one the engine computed.
assignmentLog ::
  Map.Map Recipient.Recipient Natural ->
  Prompt.Prompt r ->
  State.State [Map.Map Recipient.Recipient Natural] r
assignmentLog answer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds _ -> do
    State.modify' (\seen -> seen <> [thresholds])
    pure answer
  _ -> pure (attackThePlaneswalker p)

-- assignmentLog with one pinned division PER ASSIGNING CREATURE, which is what a
-- board with two of them needs: a division picked by searching the offer for a
-- legal one would find another after the engine's check moved, and the case would
-- stay green while proving nothing. An unlisted creature is answered with the
-- empty division, which never totals its power and so assigns nothing.
pinnedAssignments ::
  (forall a. Prompt.Prompt a -> a) ->
  [(ObjectId.ObjectId, Map.Map Recipient.Recipient Natural)] ->
  Prompt.Prompt r ->
  State.State [Map.Map Recipient.Recipient Natural] r
pinnedAssignments base answers p = case p of
  Prompt.AssignCombatDamage _ _ source thresholds _ -> do
    State.modify' (\seen -> seen <> [thresholds])
    pure (Maybe.fromMaybe Map.empty (List.lookup source answers))
  _ -> pure (base p)

-- CR 702.19c / CR 702.19e / CR 702.19f: trample over planeswalkers, through
-- Thrasta, Tempest's Roar -- the only card that prints it.
--
-- A 7/7 into a 3-loyalty Jace Beleren, so the three numbers the rule turns on --
-- power, loyalty, and the 4 that spills past it -- are all distinct and no two
-- readings of CR 702.19c land on the same board.
--
-- "That planeswalker's controller" and "the defending player" are one seat on the
-- two-seat boards, which is what keeps the arithmetic above about CR 702.19c and
-- nothing else. The last case is the three-seat board where they come apart, and
-- CR 802.2a is the rule it reads.
--
-- Thrasta's cost reduction is implemented and dormant here: nothing is cast on
-- these boards, so CR 601.2f is never reached. Pawl.CostSpec is where it is
-- proved.
--
-- Its hexproof clause is dormant for a different reason:
-- S.combatBoardOf puts Thrasta onto the battlefield without a zone change, so
-- Quantity.EnteredThisTurn reads 0 and the CR 604.2 gate is shut. Pawl.ConditionSpec
-- is where the clause is proved.
trampleOverPlaneswalkersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trampleOverPlaneswalkersSpec s registry = Spec.describe s "TrampleOverPlaneswalkers" $ do
  Spec.it s "CR 702.19c an unblocked 7/7 pays Jace's 3 loyalty and sends the other 4 at bob" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [thrasta]
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 4)]
        (after, offered) = runCombatLogging (assignmentLog answer) gs
    Spec.assertEqWith
      s
      "CR 702.19c: the planeswalker at its LOYALTY, its controller behind it at 0"
      offered
      [Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 0)]]
    Spec.assertEqWith s "bob took the 4 past Jace" (S.lifeOf S.bob after) (Just 16)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "CR 704.5i: Jace took all 3 and is buried"
  -- The pair that makes CR 702.19c's "may be assigned as the attacking creature's
  -- controller chooses" a choice: ONE board, two interpreters, two games. An
  -- engine that computed "the excess goes to the player" passes the case above
  -- and fails this one.
  Spec.it s "CR 702.19c the whole 7 may stay on Jace instead" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [thrasta]
        answer = Map.singleton (Recipient.ToPlaneswalker jaceId) 7
        (after, offered) = runCombatLogging (assignmentLog answer) gs
    Spec.assertEqWith s "the same offer was made" (fmap Map.keys offered) [[Recipient.ToPlaneswalker jaceId, Recipient.ToPlayer S.bob]]
    Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "Jace dies either way"
  -- CR 702.19c's LAST sentence: "when checking for assigned damage equal to a
  -- planeswalker's loyalty, take into account damage from other creatures that's
  -- being assigned during the same combat damage step". A 2/1 Goblin Piker
  -- attacking Jace beside Thrasta covers 2 of the 3 loyalty, so Thrasta owes it 1
  -- and 6 reaches bob -- where a threshold read per attacker makes Thrasta owe the
  -- whole 3 and rejects this division outright.
  --
  -- The pair below is ONE difference: whether the Piker is announced attacking
  -- Jace or attacking bob. Same cards, same seats, same pinned division for
  -- Thrasta -- and the same offer, asserted in both, so what moved is the CHECK
  -- and not what Thrasta was asked.
  --
  -- Every number distinct: 7 power over 3 loyalty, split 1 + 6, with the Piker's 2
  -- the only way the loyalty is covered. No two readings of the rule agree here --
  -- per attacker, Thrasta assigns nothing at all.
  Spec.it s "CR 702.19c another attacker's damage pays down the loyalty Thrasta must cover" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker, thrasta]
        thrastaId = case mine of [_, t] -> t; _ -> S.noSource
        pikerId = case mine of [p, _] -> p; _ -> S.noSource
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 1), (Recipient.ToPlayer S.bob, 6)]
        offer = [Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 0)]]
        -- One offer either way: the Piker's own assignment is forced (CR 510.1b,
        -- one recipient), so the only division asked for is Thrasta's.
        --
        -- CR 508.1b: the defending player heads the options (Combat.attackTargets
        -- orders them), so this announces the Piker at bob and Thrasta at Jace.
        pikerAtBob :: Prompt.Prompt a -> a
        pikerAtBob p = case p of
          Prompt.ChooseAttackTarget _ _ oid options | oid == pikerId -> NonEmpty.head options
          _ -> attackThePlaneswalker p
        (shared, sharedOffer) = runCombatLogging (pinnedAssignments attackThePlaneswalker [(thrastaId, answer)]) gs
        (alone, aloneOffer) = runCombatLogging (pinnedAssignments pikerAtBob [(thrastaId, answer)]) gs
    Spec.assertEqWith s "CR 702.19c: Jace is offered at his LOYALTY either way" sharedOffer offer
    Spec.assertEqWith s "and the same offer when the Piker is elsewhere" aloneOffer offer
    Spec.assertEqWith s "the Piker's 2 plus Thrasta's 1 is Jace's whole loyalty, so 6 reaches bob" (S.lifeOf S.bob shared) (Just 14)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield shared))) "CR 704.5i: Jace took 3 between them"
    -- The Piker at bob instead: nothing else is assigning to Jace, so Thrasta's 1
    -- leaves him short and the division is rejected -- Thrasta assigns nothing and
    -- only the Piker's 2 lands.
    Spec.assertEqWith s "with the Piker at bob, only its own 2 reaches him" (S.lifeOf S.bob alone) (Just 18)
    Spec.assertBool s (Set.member jaceId (GameState.battlefield alone)) "and Jace is untouched"
  -- CR 702.2c is about a CREATURE: "any nonzero amount of combat damage assigned
  -- to a creature by a source with deathtouch". A planeswalker's bar is CR
  -- 702.19c's count of loyalty counters, which deathtouch says nothing about, so
  -- Typhoid Rats' 1 in the Piker's seat pays 1 of Jace's 3 and no more -- leaving
  -- Thrasta's 1 + 6 short, and rejected.
  --
  -- The Rats stand where the 2/1 Piker stood in the case above, so the board is
  -- that one with a smaller, deathtouch attacker: an engine that read CR 702.2c on
  -- every recipient rather than on creatures alone lets the whole 6 through here.
  -- The Piker's 2 covered the loyalty between them and this 1 leaves it one short,
  -- so the two cases land on different boards for the reason the rule gives.
  Spec.it s "CR 702.2c does not clear a planeswalker's loyalty bar" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    rats <- S.printingOf s registry "Typhoid Rats"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [rats, thrasta]
        thrastaId = case mine of [_, t] -> t; _ -> S.noSource
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 1), (Recipient.ToPlayer S.bob, 6)]
        (after, offered) = runCombatLogging (pinnedAssignments attackThePlaneswalker [(thrastaId, answer)]) gs
    Spec.assertEqWith s "Jace is offered at his loyalty, as ever" offered [Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 0)]]
    Spec.assertEqWith s "the Rats' deathtouch 1 counts as 1, so Thrasta's division is rejected" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "CR 306.8: only the Rats' 1 came off Jace" (S.counterOf CounterKind.Loyalty jaceId after) 2
  -- CR 702.19e, the exception to CR 506.4c: two 2/1 first strikers bury Jace in the
  -- FIRST combat damage step (CR 510.4), and Thrasta -- still recorded as attacking
  -- it -- assigns to the defending player in the second. The control is the same
  -- board with War Mammoth in Thrasta's seat, where CR 506.4c stands and the
  -- attacker assigns nothing (the existing CR 510.1b case above is that rule).
  Spec.it s "CR 702.19e whole cards: a planeswalker killed by first strike does not stop the trampler" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    warMammoth <- S.printingOf s registry "War Mammoth"
    tiger <- S.printingOf s registry "Sabretooth Tiger"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [tiger, tiger, thrasta]
        (control, _, _) = jaceBoard jace [tiger, tiger, warMammoth]
        after = S.runCombat attackThePlaneswalker gs
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "the two first strikers buried Jace"
    Spec.assertEqWith s "CR 702.19e: Thrasta's 7 reached bob anyway" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith
      s
      "CR 506.4c / CR 510.1b: a plain trampler in the same seat assigns nothing"
      (S.lifeOf S.bob (S.runCombat attackThePlaneswalker control))
      (Just 20)
  -- The case above at THREE seats, where CR 802.2a is what picks the seat: alice
  -- attacks CAROL's Jace with Thrasta and the same two first strikers, both
  -- opponents defend (CR 802.2, the default option), and bob heads CR 802.4's
  -- APNAP order. So the two readings of rule 702.19e's "the defending player"
  -- name different players and the board tells them apart -- carol, who
  -- controlled the planeswalker, against bob, who merely comes first.
  --
  -- Nothing is on either opponent's battlefield, so Thrasta is unblocked and its
  -- whole 7 is forced (CR 510.1b): what the case reads is WHO took it, not how it
  -- was divided, which the two-seat cases above already prove.
  Spec.it s "CR 802.2a the removed planeswalker's trampler drains ITS controller, not the first defender" $ do
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    tiger <- S.printingOf s registry "Sabretooth Tiger"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs0, ours, _, hers) = S.threePlayerCombat [thrasta, tiger, tiger] [] [jace]
        thrastaId = case ours of t : _ -> t; _ -> S.noSource
        jaceId = case hers of [j] -> j; _ -> S.noSource
        staged = S.addCounter CounterKind.Loyalty 3 jaceId gs0
        after = S.runCombat attackThePlaneswalker staged
        -- The same run stopped before blockers, which is where CR 511.3 has not
        -- yet cleared the combat record the premises below read.
        declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker staged
    Spec.assertEqWith s "CR 702.19e / CR 802.2a: Thrasta's 7 reached carol, who controlled the Jace" (S.lifeOf S.carol after) (Just 13)
    Spec.assertEqWith s "and bob, who merely heads the defending players, is untouched" (S.lifeOf S.bob after) (Just 20)
    -- The premises, after the gameplay assertions so neither can absorb a
    -- mutation of them.
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "the two first strikers buried carol's Jace"
    Spec.assertEqWith s "CR 802.2 both opponents defend, bob first" (Combat.Type.defenders (GameState.combat declared)) [S.bob, S.carol]
    Spec.assertEqWith
      s
      "and Thrasta really was announced at carol's Jace"
      (Map.lookup thrastaId (Combat.Type.attackers (GameState.combat declared)))
      (Just (AttackTarget.OfPlaneswalker jaceId))
    Spec.assertEqWith s "which carol controls" (Projection.controllerOf jaceId declared) (Just S.carol)

-- CR 702.19b's last sentence, the twin of CR 702.19c's above: "when checking for
-- assigned lethal damage, take into account damage already marked on the creature
-- and damage from other creatures that's being assigned during the same combat
-- damage step". The second half needs ONE creature blocking TWO attackers, which
-- is Palace Guard's "can block any number of creatures" (CR 509.1a, through
-- Pawl.Engine.BlockPermission).
--
-- Two cases, for the rule's two consumers: the CHECK on a division (below) and
-- the elision that decides whether a division is asked for at all (after it).
blockingAll :: [ObjectId.ObjectId] -> Prompt.Prompt a -> a
blockingAll attackers p = case p of
  Prompt.DeclareBlockers _ _ blockers _ -> Map.fromList (fmap (\b -> (b, Set.fromList attackers)) blockers)
  _ -> S.aggressiveAnswer p

sharedBlockerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sharedBlockerSpec s registry = Spec.describe s "SharedBlocker" $ do
  -- Panglacial Wurm (9/5 trample) and Thrasta (7/7 trample) into the 1/4 Guard, so
  -- both are past its bar and both are asked to divide -- a creature whose power
  -- the bar absorbs is forced instead, which is the case after this one. Between
  -- them they owe the Guard 4 once, and the division here pays it 1 + 3: read per
  -- attacker, both are short and BOTH assign nothing, so no two readings of the
  -- rule land on the same board.
  Spec.it s "CR 702.19b two tramplers owe one shared blocker a single lethal bar" $ do
    wurm <- S.printingOf s registry "Panglacial Wurm"
    thrasta <- S.printingOf s registry "Thrasta, Tempest's Roar"
    guard <- S.printingOf s registry "Palace Guard"
    let (gs, mine, theirs) = S.combatBoardOf [wurm, thrasta] [guard]
        (wurmId, thrastaId) = case mine of [w, t] -> (w, t); _ -> (S.noSource, S.noSource)
        guardId = case theirs of [g] -> g; _ -> S.noSource
        -- 1 + 3 onto the Guard is its whole toughness between them, and each
        -- trampler spills the rest.
        answers =
          [ (wurmId, Map.fromList [(Recipient.ToCreature guardId, 1), (Recipient.ToPlayer S.bob, 8)]),
            (thrastaId, Map.fromList [(Recipient.ToCreature guardId, 3), (Recipient.ToPlayer S.bob, 4)]),
            -- CR 510.1d: the Guard divides its own 1 power among the creatures it
            -- blocks. Pinned onto the Wurm so the board says which.
            (guardId, Map.singleton (Recipient.ToCreature wurmId) 1)
          ]
        (both, bothOffered) = runCombatLogging (pinnedAssignments (blockingAll [wurmId, thrastaId]) answers) gs
        (one, oneOffered) = runCombatLogging (pinnedAssignments (blockingAll [wurmId]) answers) gs
    Spec.assertEqWith
      s
      "CR 702.19b: each trampler is offered the Guard's WHOLE bar, and the defending player behind it"
      bothOffered
      [ Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)],
        Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)],
        Map.fromList [(Recipient.ToCreature wurmId, 0), (Recipient.ToCreature thrastaId, 0)]
      ]
    Spec.assertEqWith s "8 + 4 spilled past the Guard" (S.lifeOf S.bob both) (Just 8)
    Spec.assertBool s (not (Set.member guardId (GameState.battlefield both))) "CR 704.5g: the Guard took its 4"
    -- The same board with the Guard declared against the Wurm alone: nothing else
    -- is assigning to it, so the Wurm's 1 leaves it short and that division is
    -- rejected. Thrasta is unblocked and its 7 is forced (CR 510.1b).
    Spec.assertEqWith s "only the Wurm is asked once it is blocked alone" oneOffered [Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)]]
    Spec.assertEqWith s "so bob takes Thrasta's 7 and nothing of the Wurm's" (S.lifeOf S.bob one) (Just 13)
    Spec.assertBool s (Set.member guardId (GameState.battlefield one)) "and the Guard is untouched"
  -- The same rule reaching the PROMPT rather than the check. Rhox Maulers is a 4/4
  -- trampler into a 1/4 Guard: its whole power is the Guard's bar, so on its own
  -- there is nothing to ask and Damage.attackerAssignment forces all 4 onto the
  -- Guard. Beside the Wurm there IS something to ask -- the Wurm can pay part of
  -- that bar -- and the division below spends 1 on the Guard and 3 on bob.
  --
  -- The pair is one difference again: whether the Guard is declared against the
  -- Wurm as well. With the Maulers blocked ALONE nothing else can pay the bar, the
  -- rules leave nothing to ask, and no division is offered at all -- so an engine
  -- that kept the elision unconditionally passes the negative and fails this
  -- positive.
  Spec.it s "CR 702.19b a trampler its blocker's bar absorbs is still asked once another attacker shares that blocker" $ do
    maulers <- S.printingOf s registry "Rhox Maulers"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    guard <- S.printingOf s registry "Palace Guard"
    let (gs, mine, theirs) = S.combatBoardOf [maulers, wurm] [guard]
        (maulersId, wurmId) = case mine of [m, w] -> (m, w); _ -> (S.noSource, S.noSource)
        guardId = case theirs of [g] -> g; _ -> S.noSource
        -- 1 + 4 is past the Guard's bar of 4 on purpose: "at least" (CR 702.19b),
        -- so the two boards below cannot land on the same life total by paying it
        -- exactly.
        answers =
          [ (maulersId, Map.fromList [(Recipient.ToCreature guardId, 1), (Recipient.ToPlayer S.bob, 3)]),
            (wurmId, Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 5)]),
            (guardId, Map.singleton (Recipient.ToCreature wurmId) 1)
          ]
        (both, bothOffered) = runCombatLogging (pinnedAssignments (blockingAll [maulersId, wurmId]) answers) gs
        (alone, aloneOffered) = runCombatLogging (pinnedAssignments (blockingAll [maulersId]) answers) gs
    Spec.assertEqWith
      s
      "the Maulers are asked to divide, and offered the same bar the Wurm is"
      (fmap Map.keys bothOffered)
      [ [Recipient.ToCreature guardId, Recipient.ToPlayer S.bob],
        [Recipient.ToCreature guardId, Recipient.ToPlayer S.bob],
        [Recipient.ToCreature maulersId, Recipient.ToCreature wurmId]
      ]
    Spec.assertEqWith s "3 of the Maulers' 4 and 5 of the Wurm's 9 spill past the Guard" (S.lifeOf S.bob both) (Just 12)
    -- Blocked alone, the Maulers have nowhere their 4 could go but the Guard, and
    -- the unblocked Wurm has nothing to divide either (CR 510.1b).
    Spec.assertEqWith s "blocked alone, no division is asked for at all" aloneOffered []
    Spec.assertEqWith s "so bob takes the Wurm's whole 9 and none of the Maulers' 4" (S.lifeOf S.bob alone) (Just 11)
  -- CR 702.2c inside CR 702.19b's last sentence: the OTHER creature's damage is
  -- deathtouch damage, so it counts toward the shared Guard's bar as LETHAL and
  -- not as its face value of 1. Typhoid Rats (1/1 deathtouch) and Panglacial Wurm
  -- (9/5 trample) into the 1/4 Guard: the Rats' 1 is all the bar the Wurm has to
  -- wait on, so the Wurm's whole 9 may spill past.
  --
  -- The pair is ONE difference -- whether the first attacker has deathtouch --
  -- with Llanowar Elves as the 1/1 that does not (its mana ability is out of
  -- reach: an attacking creature is tapped). Same seats, same blocks, the same
  -- pinned division for the Wurm, and the same offer asserted on both, so what
  -- moves is the CHECK.
  --
  -- The two readings differ by exactly the Guard's remaining toughness: 9 through
  -- against 6, since without deathtouch the Wurm owes the Guard 4 - 1 = 3 first.
  -- The third board below spends that 3 to show it, so the negative's 20 is not
  -- the only thing separating them and no board is a coincidence of the others.
  Spec.it s "CR 702.2c another creature's deathtouch damage is lethal on the shared blocker" $ do
    rats <- S.printingOf s registry "Typhoid Rats"
    elves <- S.printingOf s registry "Llanowar Elves"
    wurm <- S.printingOf s registry "Panglacial Wurm"
    guard <- S.printingOf s registry "Palace Guard"
    let board first =
          let (gs, mine, theirs) = S.combatBoardOf [first, wurm] [guard]
              (firstId, wurmId) = case mine of [f, w] -> (f, w); _ -> (S.noSource, S.noSource)
              guardId = case theirs of [g] -> g; _ -> S.noSource
           in (gs, firstId, wurmId, guardId)
        (deadly, ratsId, deadlyWurm, deadlyGuard) = board rats
        (plain, elvesId, plainWurm, plainGuard) = board elves
        -- Nothing at all on the Guard from the Wurm: with the bar met by the
        -- Rats' deathtouch there is no floor left to pay, which is the whole
        -- difference between the readings.
        spillItAll wurmId guardId =
          [ (wurmId, Map.fromList [(Recipient.ToCreature guardId, 0), (Recipient.ToPlayer S.bob, 9)]),
            -- CR 510.1d: the Guard's own 1 power, pinned onto the Wurm, which
            -- survives it either way -- so the Guard is the only creature whose
            -- fate the boards can disagree about.
            (guardId, Map.singleton (Recipient.ToCreature wurmId) 1)
          ]
        -- The same board, paying the bar down by the numbers instead: 1 + 3 is the
        -- Guard's whole 4 and 6 is what is left to spill.
        payTheBar =
          [ (plainWurm, Map.fromList [(Recipient.ToCreature plainGuard, 3), (Recipient.ToPlayer S.bob, 6)]),
            (plainGuard, Map.singleton (Recipient.ToCreature plainWurm) 1)
          ]
        -- Both divisions the step asks for, in the order it asks them: the Wurm
        -- over the Guard and bob (CR 702.19b), then the Guard's own 1 over the two
        -- creatures it blocks (CR 510.1d).
        offers firstId wurmId guardId =
          [ Map.fromList [(Recipient.ToCreature guardId, 4), (Recipient.ToPlayer S.bob, 0)],
            Map.fromList [(Recipient.ToCreature firstId, 0), (Recipient.ToCreature wurmId, 0)]
          ]
        (withDeathtouch, deadlyOffered) =
          runCombatLogging (pinnedAssignments (blockingAll [ratsId, deadlyWurm]) (spillItAll deadlyWurm deadlyGuard)) deadly
        (without, plainOffered) =
          runCombatLogging (pinnedAssignments (blockingAll [elvesId, plainWurm]) (spillItAll plainWurm plainGuard)) plain
        (paid, _) = runCombatLogging (pinnedAssignments (blockingAll [elvesId, plainWurm]) payTheBar) plain
    -- The OFFER is unchanged: a threshold is the blocker's own toughness-minus-
    -- marked bar (Damage.blockerThreshold), and CR 702.2c reaches the CHECK, which
    -- is not settled until the whole step is announced.
    Spec.assertEqWith
      s
      "the Wurm is offered the Guard's whole bar of 4"
      deadlyOffered
      (offers ratsId deadlyWurm deadlyGuard)
    Spec.assertEqWith
      s
      "and the same offer without deathtouch"
      plainOffered
      (offers elvesId plainWurm plainGuard)
    Spec.assertEqWith s "CR 702.2c: the Rats' 1 is lethal, so all 9 reach bob" (S.lifeOf S.bob withDeathtouch) (Just 11)
    Spec.assertBool s (not (Set.member deadlyGuard (GameState.battlefield withDeathtouch))) "CR 704.5h: the Guard took deathtouch damage"
    Spec.assertBool s (Set.member ratsId (GameState.battlefield withDeathtouch)) "the Rats took none of the Guard's damage and live"
    -- Without deathtouch that 1 is a plain 1, the Guard is 3 short, and
    -- the Wurm's division is rejected outright -- it assigns nothing at all.
    Spec.assertEqWith s "1 of plain damage leaves the bar unmet, so the Wurm assigns nothing" (S.lifeOf S.bob without) (Just 20)
    Spec.assertBool s (Set.member plainGuard (GameState.battlefield without)) "and the Guard survives on 1 damage"
    -- The same board paying that 3: the most that can reach bob without deathtouch.
    Spec.assertEqWith s "paying the bar by the numbers costs the Wurm exactly 3" (S.lifeOf S.bob paid) (Just 14)
    Spec.assertBool s (not (Set.member plainGuard (GameState.battlefield paid))) "CR 704.5g: 1 + 3 is the Guard's whole toughness"

-- Aim a spell's every target slot at one object, whatever Recipient arm names it.
-- The filter rather than a built Recipient, so the answer is drawn from what the
-- engine offered.
aimedAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets ->
    fmap (\(_, candidates) -> Set.filter (\r -> Recipient.objectOf r == Just oid) candidates) sets
  _ -> S.identityAnswer p

-- CR 802.2a: alice attacks CAROL's Jace Beleren with a Bog Wraith at three seats,
-- with both opponents defending (CR 802.2, the default option). bob is FIRST in CR
-- 802.4's APNAP order, so an engine folding "a defending player" onto the group's
-- head answers bob where the rule answers carol.
--
-- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so CR 702.14c
-- is exactly an ability of an attacking creature referring to a defending player
-- and no other text is in play. `bobsLand` and `carolsLand` are the ONE thing the
-- two cases below differ in.
splitDefenderJaceBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  String ->
  String ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
splitDefenderJaceBoard s registry bobsLand carolsLand = do
  bogWraith <- S.printingOf s registry "Bog Wraith"
  piker <- S.printingOf s registry "Goblin Piker"
  jace <- S.printingOf s registry "Jace Beleren"
  bobs <- S.printingOf s registry bobsLand
  carols <- S.printingOf s registry carolsLand
  let (gs0, ours, _, hers) = S.threePlayerCombat [bogWraith] [piker, bobs] [jace, carols, piker]
  case (ours, hers) of
    ([wraith], [jaceId, _, blocker]) -> do
      let staged = S.addCounter CounterKind.Loyalty 3 jaceId gs0
          settled = S.runPure S.identityAnswer staged (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
       in pure (S.runPure attackThePlaneswalker settled (Combat.declareAttackers S.manaPerformer S.alice), wraith, blocker, jaceId)
    _ -> Spec.assertFailure s "fixture should give alice a Wraith and carol a Jace and a blocker"

-- CR 802.2a: with several defending players, "a defending player" is resolved per
-- attacking creature from what that creature is attacking -- never off the head of
-- the group. The planeswalker arm is what this proves; Pawl.BattleSpec's own
-- "CR 802.2a" pair is the battle arm's.
splitDefenderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
splitDefenderSpec s registry = Spec.describe s "SplitDefendingPlayer" $ do
  let blocks blocker wraith = Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith))
      blockOf pid blocker attacker = Combat.legalBlockDeclaration pid (Map.singleton blocker (Set.singleton attacker))
  Spec.it s "CR 802.2a the swampwalking attacker reads the planeswalker's controller, not the first defending player" $ do
    -- bob holds the Swamp and carol the Island. Carol controls the attacked
    -- Jace, so CR 702.14c asks about HER lands and the block is legal; an engine
    -- answering bob finds his Swamp and calls it illegal.
    (gs, wraith, blocker, jaceId) <- splitDefenderJaceBoard s registry "Swamp" "Island"
    Spec.assertBool s (blocks blocker wraith gs) "CR 702.14c carol has no Swamp, so her block is legal"
    -- The premises, after the gameplay assertion so neither can absorb a
    -- mutation of it: two defending players with bob at the head, and the Wraith
    -- really attacking carol's Jace.
    Spec.assertEqWith s "CR 802.2 both opponents defend, bob first" (Combat.Type.defenders (GameState.combat gs)) [S.bob, S.carol]
    Spec.assertEqWith
      s
      "and the Wraith is attacking carol's Jace"
      (Map.lookup wraith (Combat.Type.attackers (GameState.combat gs)))
      (Just (AttackTarget.OfPlaneswalker jaceId))
    Spec.assertEqWith s "which carol controls" (Projection.controllerOf jaceId gs) (Just S.carol)
  Spec.it s "CR 702.14c and the same board with the lands swapped stops that block" $ do
    -- THE FALSIFIER, differing in one thing: carol now holds the Swamp. Without
    -- it the case above would pass on an engine that had lost swampwalk
    -- altogether rather than one that reads the right seat.
    (gs, wraith, blocker, _) <- splitDefenderJaceBoard s registry "Island" "Swamp"
    Spec.assertBool s (not (blocks blocker wraith gs)) "CR 702.14c carol's own Swamp stops her block"
  -- CR 506.4c's creature at three seats: Jace is buried after the declaration,
  -- so the Piker "continues to be an attacking creature, although it is not
  -- attacking any player, planeswalker, or battle. It may be blocked." By whom
  -- is CR 802.2a -- the controller of the planeswalker it was attacking before
  -- the removal -- and CR 802.4a then bars everyone else. Before that reading
  -- the engine offered the creature to EVERY defending player, so bob's block
  -- here judged legal; an engine offering it to nobody fails carol's.
  --
  -- bob's block is judged FIRST: it is the assertion the fold-onto-everyone
  -- reading fails, and carol's would pass under it.
  Spec.it s "CR 802.4a once the planeswalker is gone, only ITS controller may block the creature attacking nothing" $ do
    (gs, attacker, bobs, carols, jaceId) <- removedJaceBlockBoard s registry True
    Spec.assertBool s (not (blockOf S.bob bobs attacker gs)) "CR 802.4a: the Piker is attacking neither bob, a planeswalker he controls nor a battle he protects, so his block is illegal"
    Spec.assertBool s (blockOf S.carol carols attacker gs) "CR 506.4c / CR 802.2a: carol controlled the Jace it was attacking, so her block is legal"
    -- The premises, after the gameplay assertions so neither can absorb a
    -- mutation of them.
    Spec.assertBool s (not (S.onBattlefield jaceId gs)) "CR 704.5i: the Bolt's 3 took all of Jace's loyalty"
    Spec.assertBool s (Map.member attacker (Combat.Type.attackers (GameState.combat gs))) "CR 506.4c: still an attacking creature"
    Spec.assertEqWith s "CR 802.2 both opponents defend, bob first" (Combat.Type.defenders (GameState.combat gs)) [S.bob, S.carol]
    Spec.assertEqWith s "CR 802.4b: the Piker is on carol's list alone" (fmap (\d -> Combat.attackersOn d gs) [S.bob, S.carol]) [[], [attacker]]
  Spec.it s "CR 802.4a the same two blocks judge the same way while the planeswalker is still attacked" $ do
    -- The pair, differing in whether the Bolt was cast: the removal changes
    -- nothing about who may block, which is what CR 506.4c's "It may be blocked"
    -- asks of it.
    (gs, attacker, bobs, carols, jaceId) <- removedJaceBlockBoard s registry False
    Spec.assertBool s (not (blockOf S.bob bobs attacker gs)) "CR 802.4a: bob's block is illegal while the Piker attacks carol's Jace"
    Spec.assertBool s (blockOf S.carol carols attacker gs) "CR 802.4a: carol's is legal, the Piker attacking a planeswalker she controls"
    Spec.assertBool s (S.onBattlefield jaceId gs) "Jace is still on the battlefield"
    Spec.assertEqWith s "and carol controls him" (Projection.controllerOf jaceId gs) (Just S.carol)

-- CR 506.4c at three seats: alice attacks CAROL's Jace Beleren with a Goblin
-- Piker, both opponents defend (CR 802.2, the default option), and each holds a
-- Goblin Piker to block with. With `bolted`, alice's Lightning Bolt buries Jace
-- after the declaration -- CR 506.4's "leaves the battlefield", by CR 704.5i --
-- so the attacker is attacking nothing. `bolted` is the ONE thing the two cases
-- above differ in.
--
-- Plain Pikers on every side, so no ability of the attacker refers to a
-- defending player and the block is judged on CR 802.4a alone.
removedJaceBlockBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
removedJaceBlockBoard s registry bolted = do
  piker <- S.printingOf s registry "Goblin Piker"
  mountain <- S.printingOf s registry "Mountain"
  jace <- S.printingOf s registry "Jace Beleren"
  bolt <- S.printingOf s registry "Lightning Bolt"
  let (gs0, ours, yours, hers) = S.threePlayerCombat [piker, mountain] [piker] [jace, piker]
  case (ours, yours, hers) of
    ([attacker, _], [bobs], [jaceId, carols]) -> do
      let (boltId, gs1) = S.addHandCard bolt S.alice gs0
          staged = S.addCounter CounterKind.Loyalty 3 jaceId gs1
          settled =
            (S.runPure S.identityAnswer staged (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat)))
              { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                GameState.priority = Just S.alice
              }
          declared = S.runPure attackThePlaneswalker settled (Combat.declareAttackers S.manaPerformer S.alice)
          burned = S.settleSba (S.runPure (aimedAtObject jaceId) declared (do S.cast S.alice boltId; Stack.resolveTop))
      pure (if bolted then burned else declared, attacker, bobs, carols, jaceId)
    _ -> Spec.assertFailure s "fixture should give alice a Piker and a Mountain, bob a Piker, and carol a Jace and a Piker"

-- Soul Snare's only activated ability -- "{W}, Sacrifice this enchantment: Exile
-- target creature that's attacking you or a planeswalker you control" -- read off
-- the JSON-loaded printing for removalAbility's reason, so every leg below
-- exercises the codec's parse of the committed card data.
soulSnareAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
soulSnareAbility printing = case Face.activatedAbilities (S.combinedFace printing) of
  [ability] -> Just ability
  _ -> Nothing

-- Fire ONE Soul Snare at `victim`, pinning bob as the defending player (CR
-- 506.2a) and announcing every attack at a planeswalker.
--
-- STATEFUL for mazeAnswer's reason. The target set is FILTERED rather than
-- replaced, so a leg whose slot does not admit the victim takes no target at all
-- instead of quietly succeeding on a hand-built recipient -- and the activation
-- is simply never offered on such a leg, CR 602.2b's target choice being part of
-- what makes the ability activatable.
soulSnareAnswer ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  State.State Bool r
soulSnareAnswer snareId ability victim p = case p of
  Prompt.ChooseAction _ _ actions -> do
    tried <- State.get
    if tried || notElem (A.Activate snareId ability) actions
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate snareId ability)
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (\(_, rs) -> Set.filter (== Recipient.ToCreature victim) rs) sets)
  Prompt.ChooseDefender {} -> pure S.bob
  _ -> pure (attackThePlaneswalker p)

-- THREE seats and a stolen planeswalker, stolenJaceLandwalkBoard's shape: alice
-- attacks with one Goblin Piker, bob is the defending player and controls carol's
-- Jace Beleren through a Confiscate, and bob and carol hold ONE Soul Snare and
-- ONE Plains each.
--
-- Every element is load-bearing. The two Snares are the same card with the same
-- {W} available at the same seat count, so a leg that fails cannot fail for want
-- of mana -- the only difference between them is who holds one. Jace's OWNER is
-- carol and his CONTROLLER is bob, so reading CR 508.1b's controller and reading
-- CR 108.3's owner name different seats and answer the pair the opposite way
-- round.
-- Loyalty 5 against a 2/1 leaves 3, so the damaged and undamaged readings differ.
--
-- Positioned at the BEGINNING of combat with the defender unchosen, so the
-- engine's own declare attackers step builds the combat record: the fixture
-- declares nothing by hand.
--
-- Returns the state, the Piker, Jace, bob's Snare and carol's Snare.
soulSnareBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
soulSnareBoard s registry = do
  piker <- S.printingOf s registry "Goblin Piker"
  plains <- S.printingOf s registry "Plains"
  jace <- S.printingOf s registry "Jace Beleren"
  confiscate <- S.printingOf s registry "Confiscate"
  snare <- S.printingOf s registry "Soul Snare"
  let (gs0, ours, _, hers) = S.threePlayerCombat [piker] [plains] [jace, plains]
  case (ours, hers) of
    ([pikerId], jaceId : _) -> do
      let (confiscateId, gs1) = S.addPermanent confiscate S.bob gs0
          gs2 = S.addCounter CounterKind.Loyalty 5 jaceId (S.attachTo confiscateId (Recipient.ToObject jaceId) gs1)
          (bobSnare, gs3) = S.addPermanent snare S.bob gs2
          (carolSnare, gs4) = S.addPermanent snare S.carol gs3
      pure (gs4, pikerId, jaceId, bobSnare, carolSnare)
    _ -> Spec.assertFailure s "fixture should have one Piker and one Jace"

-- CR 508.1b's SECOND subject -- "a planeswalker they control", the middle of CR
-- 509.1a's and CR 802.4a's three-way list -- through the pool's cleanest
-- producer: Soul Snare {W} -- Enchantment, "{W}, Sacrifice this enchantment:
-- Exile target creature that's attacking you or a planeswalker you control."
-- (Murders at Karlov Manor Commander; oracle text checked against Scryfall.) Its
-- target slot is Or [IsAttackingPlayer You, IsAttackingPlaneswalker You], and the
-- second atom is the whole of what this board pays for -- the Piker attacks a
-- planeswalker and never a player, so the first atom answers False on every leg.
--
-- A PAIR of legs off ONE board differing in exactly one thing: which seat's Soul
-- Snare is fired. CR 508.1b reads the attacked planeswalker's CONTROLLER, so bob's
-- admits the Piker and carol's does not. An engine reading the OWNER answers both
-- the other way round; one dropping the PlayerRelation answers both yes.
soulSnareSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
soulSnareSpec s registry = Spec.describe s "SoulSnare" $ do
  Spec.it s "CR 508.1b whole card: Soul Snare reaches a creature attacking a planeswalker you CONTROL" $ do
    snare <- S.printingOf s registry "Soul Snare"
    (gs, pikerId, jaceId, bobSnare, carolSnare) <- soulSnareBoard s registry
    case soulSnareAbility snare of
      Just ability -> do
        let defended = runToEndOfCombatWith (soulSnareAnswer bobSnare ability pikerId) gs
            -- The control leg: the same board, the same answerer, the same {W}
            -- and the same ability -- fired from carol's seat instead of bob's.
            bystander = runToEndOfCombatWith (soulSnareAnswer carolSnare ability pikerId) gs
        -- GAMEPLAY FIRST, on the quantity the two readings differ on: the Piker's
        -- 2 never reached Jace, because bob exiled it before combat damage.
        Spec.assertEqWith s "CR 306.8: bob's Snare exiled the attacker, so Jace's loyalty is untouched" (S.counterOf CounterKind.Loyalty jaceId defended) 5
        Spec.assertEqWith s "control: carol's Snare cannot name it, so the Piker's 2 comes off Jace's loyalty" (S.counterOf CounterKind.Loyalty jaceId bystander) 3
        Spec.assertBool s (not (S.onBattlefield pikerId defended)) "the Piker was exiled"
        Spec.assertBool s (S.onBattlefield pikerId bystander) "control: on carol's leg it is untouched"
        Spec.assertBool s (not (S.onBattlefield bobSnare defended)) "bob's Snare paid its own sacrifice, so the ability really was activated"
        Spec.assertBool s (S.onBattlefield carolSnare bystander) "control: carol's Snare is unsacrificed, so hers was never activated"
        -- Anti-vacuity, read on the leg where nothing was exiled: the Piker IS an
        -- attacking creature, and what it is attacking is Jace rather than bob.
        Spec.assertEqWith
          s
          "CR 508.1b: the Piker really was announced at Jace and not at bob"
          (Map.lookup pikerId (Combat.Type.attackers (GameState.combat bystander)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
        -- The two seats the pair tells apart: CR 508.1b's controller is bob, CR
        -- 108.3's owner is carol, and the Confiscate is what separates them.
        Spec.assertEqWith s "CR 613.1b: bob controls Jace through the Confiscate" (Projection.controllerOf jaceId bystander) (Just S.bob)
        Spec.assertEqWith s "CR 108.3: carol owns him" (fmap Object.owner (Game.lookupObject jaceId bystander)) (Just S.carol)
      Nothing -> Spec.assertFailure s "Soul Snare should have exactly one activated ability"

-- CR 508.4 / CR 508.3a / CR 508.8, through the one card in the pool that puts a
-- creature onto the battlefield attacking WITHOUT anything having been declared.
--
-- Meandering Towershell {3}{G}{G} -- Creature -- Turtle 5/9: "Islandwalk.
-- Whenever this creature attacks, exile it. Return it to the battlefield under
-- your control tapped and attacking at the beginning of the declare attackers
-- step on your next turn."
--
-- Hanweir Garrison, the group above, cannot reach either of the two rules these
-- cases are about. Its tokens arrive only because the Garrison itself was
-- declared, so CR 508.8's second clause is never in question there; and a token
-- can never fire a GARRISON's own attack trigger, so CR 508.3a's "including its
-- own triggered ability" has no falsifier there either. The Towershell is both:
-- it returns on a turn its controller declares nothing, and the ability that
-- must not fire is its own.
towershellSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
towershellSpec s registry = Spec.describe s "MeanderingTowershell" $ do
  let boardWith theirs = do
        towershell <- S.printingOf s registry "Meandering Towershell"
        island <- S.printingOf s registry "Island"
        pure (towershellBoard towershell island theirs)
      boardOf = boardWith []
      towershellName = CardName.MkCardName $ Text.pack "Meandering Towershell"
  Spec.it s "CR 508.3a whole card: attacking exiles it, so CR 506.4 leaves it dealing no damage" $ do
    (gs, ours) <- boardOf
    let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
    Spec.assertEqWith s "it was declared as an attacker" (S.attackerDeclarationsOf atBlockers) [ours]
    Spec.assertEqWith s "and its own trigger exiled it" (S.countOnBattlefieldByName towershellName S.alice atBlockers) 0
    Spec.assertEqWith s "it is the one card in exile" (Set.size (GameState.exile atBlockers)) 1
    -- CR 508.8's FIRST clause is historical (CR 508.1k), so the two steps stay
    -- even though the attacker is gone -- the same fact TurnSpec's Ray of
    -- Command case pins, reached here by the card exiling itself.
    Spec.assertEqWith s "the declare blockers step was reached anyway" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith s "and one delayed ability is waiting" (length (GameState.delayedTriggers atBlockers)) 1
    -- CR 506.4: the exiled Towershell left the battlefield, so it is no longer a
    -- live combat participant and deals no combat damage. The stale entry stays
    -- in the record on purpose (see Pawl.Engine.Projection's filterReads); every
    -- combat-damage read filters it out by zone instead (Damage.onBattlefield).
    let afterDamage = runToTurnStep 1 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith s "a 5/9 that left combat deals nobody 5" (S.lifeOf S.bob afterDamage) (Just 20)

  -- CR 508.4's CHOICE, which this card is the pool's only producer of: the
  -- Towershell returns attacking on a turn nothing is declared, and its
  -- controller says what it is attacking as it enters. Its own ruling is the
  -- one being obeyed -- "you choose which opponent or opposing planeswalker
  -- it's attacking. It doesn't have to attack the same opponent ... that it was
  -- when it was exiled."
  --
  -- Both answers are asserted on ONE board, which is what makes this a choice
  -- and not a default: aimed at Jace, its 5 damage buries a 3-loyalty
  -- planeswalker (CR 306.8, CR 704.5i) and bob keeps his 20; aimed at bob, he
  -- takes 5 and Jace keeps all three counters.
  Spec.it s "CR 508.4 whole card: the returned Towershell chooses the planeswalker" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    island <- S.printingOf s registry "Island"
    jace <- S.printingOf s registry "Jace Beleren"
    let (base, _) = towershellBoard towershell island [jace]
        jaceId = case filter (\oid -> Projection.isPlaneswalkerOf oid base) (Set.toList (GameState.battlefield base)) of
          oid : _ -> oid
          [] -> S.noSource
        gs = S.addCounter CounterKind.Loyalty 3 jaceId base
        atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs
        after = runToTurnStep 3 Phase.PostcombatMain attackThePlaneswalker gs
        control = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith
      s
      "it entered attacking the planeswalker (CR 508.4)"
      (Map.elems (Combat.Type.attackers (GameState.combat atReturn)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "a 5/9 buried a 3-loyalty Jace"
    Spec.assertEqWith s "and bob took none of it" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "aimed at bob instead, he takes 5" (S.lifeOf S.bob control) (Just 15)
    Spec.assertEqWith s "and Jace keeps all three counters" (S.counterOf CounterKind.Loyalty jaceId control) 3

-- alice at her declare attackers step with one Meandering Towershell and bob
-- defending, both players holding a small library so the draw steps of the turns
-- these tests run through do not empty one (CR 104.3c).
--
-- The library cards are Islands, which is deliberate rather than filler: an
-- Island is the only land in the pool the Towershell's own islandwalk (CR
-- 702.14) reads, and a library is not the battlefield, so CR 702.14c's "the
-- defending player controls at least one land with the specified land type"
-- cannot see one there. A case that wants the evasion says so by putting an
-- Island in `theirs`, which is bob's BATTLEFIELD.
towershellBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId)
towershellBoard towershell island theirs =
  let (base, ours, _) = S.combatBoardOf [towershell] theirs
      stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 6]
      gs = stock S.bob (stock S.alice base)
   in case ours of
        [oid] -> (gs, oid)
        -- Unreachable (combatBoardOf returns one id per printing), and total
        -- rather than an `error`: S.noSource names no object, so a fixture that
        -- somehow got here fails the first assertion instead of the whole suite.
        _ -> (gs, S.noSource)

-- runToStep's multi-turn twin: run whole steps until the board is at `phase` on
-- turn `turn`, WITHOUT running that step. Bounded so a bug cannot loop forever,
-- and it stops on a finished game so an empty library ends the run rather than
-- spinning.
runToTurnStep :: Natural -> Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToTurnStep turn phase answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (GameState.result g)
          || (GameState.turnNumber g == turn && GameState.phase g == phase)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 64 gs0

-- Are all of these permanents tapped? What a test asks of the Forests to see CR
-- 508.1j's payment: it spends exactly what tapping them produced, so the pool is
-- empty again afterwards and the tapped lands are the payment's only trace.
allTapped :: [ObjectId.ObjectId] -> GameState.GameState -> Bool
allTapped oids gs = all (\oid -> tapStateOf oid gs == Just TapState.Tapped) oids

-- The complement, and NOT `not . allTapped`: a payment that tapped one Forest of
-- two would satisfy that, and what these cases assert is that nothing was spent.
allUntapped :: [ObjectId.ObjectId] -> GameState.GameState -> Bool
allUntapped oids gs = all (\oid -> tapStateOf oid gs == Just TapState.Untapped) oids

-- How many of these permanents are still on the battlefield. What a test asks of
-- the lands to see a NON-MANA payment: a sacrificed land is gone (CR 701.21a),
-- where a spent Forest is merely tapped, so the two tolls leave different traces.
stillThere :: [ObjectId.ObjectId] -> GameState.GameState -> Int
stillThere oids gs = length (filter (\oid -> Set.member oid (GameState.battlefield gs)) oids)

-- Set a seat's life directly, so a toll's life route can be priced against a
-- total the fixture chose. Pawl.CoinSpec's `atLife`, duplicated rather than
-- hoisted.
atLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
atLife pid n gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}

-- CR 506.4e: a permanent being attacked that is both a planeswalker and a battle
-- stays attacked while it is either, whichever kind of announcement named it.
--
-- Jace Beleren is bob's, coated by Liquimetal Coating, animated by Karn's Touch
-- ({U}{U} Instant, "Target noncreature artifact becomes an artifact creature with
-- power and toughness each equal to its mana value until end of turn") so that
-- Synthetic Besiege the Front can make him a battle too, all before the
-- declaration: CR 310.11 only gives a protector to a battle that isn't being
-- attacked. Then one of two cards takes a type away mid-combat:
--
--   * Magnetic Theft ({R} Instant, "Attach target Equipment to target
--     creature") puts alice's Luxior, Giada's Gift ("Equipped permanent isn't a
--     planeswalker and is a creature in addition to its other types") on him.
--     Luxior is the one printing that removes the planeswalker type alone
--     (Scryfall o:"isn't a planeswalker", 2026-09-29).
--   * Synthetic Lift the Siege ({1}{W} Instant, "Until end of turn, target
--     battle isn't a battle") takes the battle type. Synthetic because no printing
--     removes it alone (Scryfall o:"isn't a battle" and o:"not a battle", both
--     empty 2026-09-29).
--
-- Oracle texts checked against Scryfall, 2026-09-29. One Llanowar Elves attacks,
-- so each reading is 5 against 4.
planeswalkerBattleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
planeswalkerBattleSpec s registry = Spec.describe s "PlaneswalkerBattleInCombat" $ do
  Spec.it s "CR 506.4e whole cards: attacked as a planeswalker, it stops being one and is still a battle that's being attacked" $ do
    theft <- S.printingOf s registry "Magnetic Theft"
    mountain <- S.printingOf s registry "Mountain"
    board <- planeswalkerBattleBoard s registry
    case board of
      Nothing -> Spec.assertFailure s "fixture should give alice one Llanowar Elves, a Coating and Luxior, and bob a Jace"
      Just (gs0, elf, jaceId, luxiorId) -> do
        pinPlaneswalkerBattle s jaceId gs0
        let (_, gs1) = S.addPermanent mountain S.alice gs0
            (spell, gs) = S.addHandCard theft S.alice gs1
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (announceAt (AttackTarget.OfPlaneswalker jaceId) elf) gs
            atEnd = runToEndOfCombat (castAt spell [luxiorId, jaceId]) atBlockers
        Spec.assertEqWith s "CR 508.1b: the Elf really was announced at the planeswalker" (Map.lookup elf (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfPlaneswalker jaceId))
        -- GAMEPLAY FIRST: without CR 506.4e the Elf attacks nothing and the
        -- defense stays at 5.
        Spec.assertEqWith s "CR 506.4e / 310.6: the Elf's 1 came off his defense" (S.counterOf CounterKind.Defense jaceId atEnd) 4
        Spec.assertEqWith s "CR 701.3a: Luxior is on Jace" (Game.hostOf luxiorId atEnd) (Just jaceId)
        Spec.assertBool s (not (Projection.isPlaneswalkerOf jaceId atEnd)) "CR 613.1d: so he is no planeswalker"
        Spec.assertBool s (Projection.isBattleOf jaceId atEnd) "but still a battle"
        Spec.assertEqWith s "and his loyalty is untouched" (S.counterOf CounterKind.Loyalty jaceId atEnd) 5
        Spec.assertEqWith s "the leg reached the end of combat step" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
  Spec.it s "CR 506.4e whole cards: attacked as a battle, it stops being one and is still a planeswalker that's being attacked" $ do
    lift <- S.printingOf s registry "Synthetic Lift the Siege"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    board <- planeswalkerBattleBoard s registry
    case board of
      Nothing -> Spec.assertFailure s "fixture should give alice one Llanowar Elves, a Coating and Luxior, and bob a Jace"
      Just (gs0, elf, jaceId, _) -> do
        let (_, gs1) = S.addPermanent plains S.alice gs0
            (_, gs2) = S.addPermanent island S.alice gs1
            (spell, gs) = S.addHandCard lift S.alice gs2
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (announceAt (AttackTarget.OfBattle jaceId) elf) gs
            atEnd = runToEndOfCombat (castAt spell [jaceId]) atBlockers
        Spec.assertEqWith s "CR 508.1b: the Elf really was announced at the battle" (Map.lookup elf (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfBattle jaceId))
        -- GAMEPLAY FIRST: without CR 506.4e the Elf attacks nothing and the
        -- loyalty stays at 5.
        Spec.assertEqWith s "CR 506.4e / 306.8: the Elf's 1 came off his loyalty, his protector controlling him" (S.counterOf CounterKind.Loyalty jaceId atEnd) 4
        Spec.assertBool s (not (Projection.isBattleOf jaceId atEnd)) "CR 613.1d: he is no battle"
        Spec.assertBool s (Projection.isPlaneswalkerOf jaceId atEnd) "but still a planeswalker"
        Spec.assertEqWith s "CR 310.9g: bob still protects him, and controls him" (Battle.protectorOf jaceId atEnd, Projection.controllerOf jaceId atEnd) (Just S.bob, Just S.bob)
        Spec.assertEqWith s "and his defense is untouched" (S.counterOf CounterKind.Defense jaceId atEnd) 5
  Spec.it s "CR 506.4e whole card: a creature attacking the planeswalker is attacking a battle bob protects, so his Snare exiles it" $ do
    snare <- S.printingOf s registry "Synthetic Bulwark Snare"
    plains <- S.printingOf s registry "Plains"
    board <- planeswalkerBattleBoard s registry
    case (board, Face.activatedAbilities (S.combinedFace snare)) of
      (Just (gs0, elf, jaceId, _), [ability]) -> do
        let (_, gs1) = S.addPermanent plains S.bob gs0
            (snareId, gs) = S.addPermanent snare S.bob gs1
            atEnd = runToEndOfCombat (snareAt snareId ability elf (announceAt (AttackTarget.OfPlaneswalker jaceId) elf)) gs
        -- GAMEPLAY FIRST: the Snare's filter admits only a creature attacking bob
        -- or a battle he protects, and the Elf was announced at the planeswalker.
        Spec.assertBool s (not (S.onBattlefield elf atEnd)) "CR 506.4e: the Elf is attacking a battle bob protects, so his Snare exiled it"
        Spec.assertBool s (not (S.onBattlefield snareId atEnd)) "the Snare paid its own sacrifice, so the ability really was activated"
        Spec.assertEqWith s "so nothing came off Jace" (S.counterOf CounterKind.Defense jaceId atEnd, S.counterOf CounterKind.Loyalty jaceId atEnd) (5, 5)
      _ -> Spec.assertFailure s "fixture should give alice one Llanowar Elves and bob a Jace, and the Snare one ability"

-- alice has one Llanowar Elves, a Liquimetal Coating, an unattached Luxior and
-- five tapped Islands; bob has Jace Beleren at five loyalty, made an artifact
-- (the Coating), a 3/3 creature (Karn's Touch) and a battle with five defense
-- counters (Synthetic Besiege the Front), protected by bob (CR 310.9a's last
-- sentence, CR 310.11). The board sits at the declare attackers step, before the
-- declaration. The Elf is seated after the casts, so no payment can tap it.
-- Returns the state, the Elf, Jace and Luxior.
planeswalkerBattleBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m (Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId))
planeswalkerBattleBoard s registry =
  fmap (fmap (\(gs, elf, jaceId, luxiorId, _) -> (gs, elf, jaceId, luxiorId))) (planeswalkerBattleBoardWith True s registry)

-- planeswalkerBattleBoard, with `besiegeFirst` False leaving Synthetic Besiege
-- the Front in alice's hand beside three untapped Islands, so Jace is an artifact
-- creature planeswalker and no battle. Also returns the Besiege.
planeswalkerBattleBoardWith ::
  (Monad m) =>
  Bool ->
  Spec.Spec m n ->
  Registry.Registry m ->
  m (Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId))
planeswalkerBattleBoardWith besiegeFirst s registry = do
  jace <- S.printingOf s registry "Jace Beleren"
  elves <- S.printingOf s registry "Llanowar Elves"
  coating <- S.printingOf s registry "Liquimetal Coating"
  luxior <- S.printingOf s registry "Luxior, Giada's Gift"
  island <- S.printingOf s registry "Island"
  touch <- S.printingOf s registry "Karn's Touch"
  besiege <- S.printingOf s registry "Synthetic Besiege the Front"
  let (gs0, _, theirs) = S.combatBoardOf [] [jace]
      (coatingId, gs1) = S.addPermanent coating S.alice gs0
      (luxiorId, gs2) = S.addPermanent luxior S.alice gs1
      gs3 = S.landsFor island S.alice 5 gs2
      (touchId, gs4) = S.addHandCard touch S.alice gs3
      (besiegeId, gs5) = S.addHandCard besiege S.alice gs4
  pure $ case (theirs, Face.activatedAbilities (S.combinedFace coating)) of
    ([jaceId], coat : _) ->
      let ready = (S.addCounter CounterKind.Loyalty 5 jaceId gs5) {GameState.priority = Just S.alice}
          made =
            S.runPure (targetingOnly [jaceId]) ready $ do
              Activate.activateAbility S.alice coatingId coat
              Stack.resolveTop
              S.cast S.alice touchId
              Stack.resolveTop
              Monad.when besiegeFirst $ do
                S.cast S.alice besiegeId
                Stack.resolveTop
              Engine.settleForPriority
          (elf, seated) = S.addPermanent elves S.alice made
       in Just (seated, elf, jaceId, luxiorId, besiegeId)
    _ -> Nothing

-- The fixture's own preconditions, asserted on the board it returns.
pinPlaneswalkerBattle :: (Monad m) => Spec.Spec m n -> ObjectId.ObjectId -> GameState.GameState -> m ()
pinPlaneswalkerBattle s jaceId gs = do
  Spec.assertBool s (Projection.isPlaneswalkerOf jaceId gs) "Jace is a planeswalker"
  Spec.assertBool s (Projection.isBattleOf jaceId gs) "and a battle (Synthetic Besiege the Front)"
  Spec.assertEqWith s "Karn's Touch made him a 3/3, his mana value" (S.powerToughnessOf jaceId gs) (Just (3, 3))
  Spec.assertEqWith s "CR 310.9a / 310.11: bob protects him" (Battle.protectorOf jaceId gs) (Just S.bob)
  Spec.assertEqWith s "with five defense counters" (S.counterOf CounterKind.Defense jaceId gs) 5

-- Every target slot takes the offered members naming one of `oids`: FILTERED
-- rather than built, so each slot keeps the recipient tag its pool offers.
targetingOnly :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
targetingOnly oids p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (\r -> List.elem (Recipient.objectOf r) (fmap Just oids)) . snd) sets
  _ -> S.identityAnswer p

-- Declare `elf` alone and announce it at `target`; pass priority.
announceAt :: AttackTarget.AttackTarget -> ObjectId.ObjectId -> Prompt.Prompt r -> r
announceAt target elf p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== elf) ids
  Prompt.ChooseAttackTarget _ _ _ options -> if List.elem target (NonEmpty.toList options) then target else NonEmpty.head options
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.ChooseAction {} -> A.Pass
  _ -> S.aggressiveAnswer p

-- Nobody blocks; whoever is offered the cast of `spell` takes it, aimed at
-- `oids` through targetingOnly.
castAt :: ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt.Prompt r -> r
castAt spell oids p = case p of
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    a : _ -> a
    [] -> A.Pass
  Prompt.ChooseTargets {} -> targetingOnly oids p
  _ -> S.aggressiveAnswer p

-- `fallback`, plus: fire bob's `snare` at `victim` the first time it is offered.
snareAt ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  ObjectId.ObjectId ->
  (Prompt.Prompt r -> r) ->
  Prompt.Prompt r ->
  r
snareAt snare ability victim fallback p = case p of
  Prompt.ChooseAction _ _ actions
    | List.elem (A.Activate snare ability) actions -> A.Activate snare ability
  Prompt.ChooseTargets _ seat _ _
    | seat == S.bob -> targetingOnly [victim] p
  _ -> fallback p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  creaturePlaneswalkerCombatSpec s registry
  planeswalkerBattleSpec s registry
  putOntoBattlefieldAttackingSpec s registry
  towershellSpec s registry
  planeswalkerAttackSpec s registry
  trampleOverPlaneswalkersSpec s registry
  sharedBlockerSpec s registry
  splitDefenderSpec s registry
  soulSnareSpec s registry

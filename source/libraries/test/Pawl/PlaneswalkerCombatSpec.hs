{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Combat over attack targets other than a player (CR 508.1) and
-- the defending player they imply: planeswalkers, battles, shared blockers,
-- last-known and split defenders, Soul Snare and Meandering Towershell. Split out of Pawl.CombatEffectSpec, which keeps the machinery.
module Pawl.PlaneswalkerCombatSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Pawl.CombatEffectSpec (attackJaceAndBob, attackThePlaneswalker, blockAndSong, blockAndWane, blockWithJace, creaturePlaneswalkerBoard, runToEndOfCombat, runToEndOfCombatWith, tapStateOf)
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Phasing as Phasing
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
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

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
-- Six pool cards carry the rule, every oracle text checked against Scryfall (two
-- Llanowar Elves, a Plains and three Forests are scaffolding -- see
-- creaturePlaneswalkerBoard):
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
--   * Song of the Dryads ({2}{G} Enchantment -- Aura, "Enchant permanent /
--     Enchanted permanent is a colorless Forest land") is the first sentence's
--     card: CR 205.1a makes the set REPLACE the existing card types, so the
--     animated Jace stops being a creature and a planeswalker in one resolution.
--     It is the only card in data/cards/ whose SetCardType is aimed at ANOTHER
--     permanent (grep the constructor name: Gliding Licid's sets Enchantment on
--     itself), and it sets Land, which is why the mirror leg below is still
--     waiting on card data.
--   * Vedalken Orrery ({4} Artifact, "You may cast spells as though they had
--     flash") is alice's, and is what makes that cast reachable: CR 303.1 admits
--     an enchantment only in a main phase, and the block has to be declared first
--     (CR 601.3b for the permission, CR 702.8a for the window it carries).
--     March animates it too, exactly as it animates the Coating, which changes
--     nothing here: attackJaceAndBob declares only the two Elves as attackers.
--
-- The mirror case, where the permanent stops being a PLANESWALKER and stays a
-- creature, is unproven here rather than asserted (gap #1846): it needs an effect
-- that sets the card type to Creature, and the Song -- the corpus's only
-- SetCardType aimed at another permanent -- sets Land. Kenrith's Transformation prints one and is not in the
-- pool yet.
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
  Spec.it s "CR 506.4d whole cards: a blocking Jace that stops being BOTH card types is removed from combat" $ do
    jace <- S.printingOf s registry "Jace Beleren"
    elves <- S.printingOf s registry "Llanowar Elves"
    coating <- S.printingOf s registry "Liquimetal Coating"
    march <- S.printingOf s registry "March of the Machines"
    plains <- S.printingOf s registry "Plains"
    waxWane <- S.printingOf s registry "Wane"
    forest <- S.printingOf s registry "Forest"
    song <- S.printingOf s registry "Song of the Dryads"
    orrery <- S.printingOf s registry "Vedalken Orrery"
    case creaturePlaneswalkerBoard jace elves coating march plains waxWane of
      Nothing -> Spec.assertFailure s "fixture should give alice two Llanowar Elves and a Coating with one activated ability, and bob a Jace"
      Just (gs0, atJace, atBob, jaceId, marchId) -> do
        -- Both additions go on AFTER the fixture returns rather than into it. The
        -- Song costs {2}{G} and both Elves are attacking and tapped, but widening
        -- the shared board with green would make Wax castable in the leg above and
        -- falsify blockAndWane's "a lone Plains cannot pay its {G}". The Orrery is
        -- what makes the cast reachable at all: the Song is an enchantment, so CR
        -- 303.1 would leave it in hand for the whole combat phase, and the Orrery's
        -- CR 601.3b permission carries CR 702.8a's window -- any time you could
        -- cast an instant -- so it is castable once the block has been declared.
        let (_, gs1) = S.addPermanent forest S.alice gs0
            (_, gs2) = S.addPermanent forest S.alice gs1
            (_, gs3) = S.addPermanent forest S.alice gs2
            (_, gs4) = S.addPermanent orrery S.alice gs3
            (_, gs) = S.addHandCard song S.alice gs4
        -- The same fixture pins the leg above takes, for the same reason.
        Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf jaceId gs)) "CR 205.1b: the Coating made Jace an artifact"
        Spec.assertBool s (Projection.isCreatureOf jaceId gs) "CR 613.8: so March animates him"
        Spec.assertEqWith s "a 3/3, his mana value" (S.powerToughnessOf jaceId gs) (Just (3, 3))
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackJaceAndBob atJace atBob) gs
            atEnd = runToEndOfCombat (blockAndSong jaceId atBob) atBlockers
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertEqWith s "one attacker really was announced at the planeswalker (CR 508.1b)" (Map.lookup atJace (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfPlaneswalker jaceId))
        Spec.assertEqWith s "and the other at bob" (Map.lookup atBob (Combat.Type.attackers (GameState.combat atBlockers))) (Just (AttackTarget.OfPlayer S.bob))
        Spec.assertEqWith s "the leg reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertBool s (S.onBattlefield marchId atEnd) "March of the Machines survives -- this leg destroys nothing"
        -- CR 205.1a: the Song's set REPLACES the card types rather than adding to
        -- them, so both of CR 506.4d's roles end at one resolution.
        Spec.assertBool s (not (Projection.isCreatureOf jaceId atEnd)) "CR 205.1a: Jace stopped being a creature"
        Spec.assertBool s (not (Projection.isPlaneswalkerOf jaceId atEnd)) "and stopped being a planeswalker too"
        Spec.assertBool s (S.onBattlefield jaceId atEnd) "and is still on the battlefield, so this is the card-types clause and not the leaves-the-battlefield one"
        -- The gameplay readings first, because they are what the rule is about and
        -- what the two engine-level readings below are only evidence for. Damage is
        -- the one that separates the two halves' failures: the attacker Jace
        -- blocked would mark him (CR 510.1c) if the block survived, and the
        -- attacker aimed at him would take loyalty (CR 306.8 / 120.3c) if he were
        -- still attacked, so 0 and 5 fail independently.
        Spec.assertEqWith s "CR 510.1c: the attacker Jace blocked assigns nothing, so nothing is marked on him" (S.damageOf jaceId atEnd) (Just 0)
        Spec.assertEqWith s "CR 510.1b: and the attacker aimed at him assigns nothing either, so loyalty is untouched at 5" (S.counterOf CounterKind.Loyalty jaceId atEnd) 5
        Spec.assertEqWith s "and bob takes nothing: the attacker he would have taken damage from is still blocked" (S.lifeOf S.bob atEnd) (Just 20)
        -- Half one, which the leg above also reaches: the block goes.
        Spec.assertEqWith s "CR 506.4: Jace is blocking nothing" (Combat.blockersOf atBob atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked atBob atEnd) "CR 509.1h: but that attacker remains blocked"
        -- Half two, which no other leg can assert: he stops being attacked as well.
        -- pawl reads attacked-ness at Combat.stillAttacked, and CR 506.4c keeps
        -- the ATTACKER in combat, so its Combat.attackers entry still names the
        -- planeswalker -- asserting that entry is gone would fail a correct
        -- engine.
        Spec.assertBool s (not (Combat.stillAttacked jaceId atEnd)) "CR 506.4: and he stops being attacked"
        Spec.assertEqWith s "CR 506.4c: while the attacker aimed at him stays in combat, record entry and all" (Map.lookup atJace attackers) (Just (AttackTarget.OfPlaneswalker jaceId))

-- CR 508.4: "If a creature is put onto the battlefield attacking, its controller
-- chooses which defending player ... it's attacking ... Such creatures are
-- 'attacking' but, for the purposes of trigger events and effects, they never
-- 'attacked'."
--
-- Hanweir Garrison is the pool's only source of one: "Whenever this creature
-- attacks, create two 1/1 red Human creature tokens that are tapped and
-- attacking."
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
  Spec.it s "CR 510.1b the tokens deal combat damage like any attacker" $ do
    garrison <- S.printingOf s registry "Hanweir Garrison"
    let (gs, _, _) = S.combatBoardOf [garrison] []
        after = S.runCombat S.aggressiveAnswer gs
    -- The 2/3 Garrison plus two 1/1 tokens, all unblocked, against bob's 20.
    Spec.assertEqWith s "bob takes 2 + 1 + 1" (S.lifeOf S.bob after) (Just 16)

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
  Spec.it s "CR 508.1b a creature is declared attacking the planeswalker, not its controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker]
        atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs
    case mine of
      [attacker] ->
        Spec.assertEqWith
          s
          "the record names the planeswalker (CR 508.1b)"
          (Map.lookup attacker (Combat.Type.attackers (GameState.combat atBlockers)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
      _ -> Spec.assertFailure s "fixture should have one attacker"
  Spec.it s "CR 306.8 whole cards: a 2/1 attacking Jace takes two loyalty counters and bob takes nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker]
        after = S.runCombat attackThePlaneswalker gs
    Spec.assertEqWith s "CR 306.8: 3 - 2" (S.counterOf CounterKind.Loyalty jaceId after) 1
    Spec.assertEqWith s "CR 510.1b: the damage did not reach its controller" (S.lifeOf S.bob after) (Just 20)
    Spec.assertBool s (Set.member jaceId (GameState.battlefield after)) "CR 704.5i does not apply at loyalty 1"
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
  Spec.it s "CR 704.5i two attackers take all three loyalty counters and Jace is buried" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker, piker]
        after = S.runCombat attackThePlaneswalker gs
    Spec.assertEqWith s "loyalty 0 (Natural, not wrapped past zero)" (S.counterOf CounterKind.Loyalty jaceId after) 0
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "CR 704.5i: off the battlefield"
    Spec.assertEqWith s "CR 704.5i: in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    Spec.assertEqWith s "and none of the 4 damage splashed onto bob" (S.lifeOf S.bob after) (Just 20)
  -- CR 508.1b's announcement is asked PER CREATURE, and the answers are
  -- independent: two Pikers, one at Jace and one at bob.
  Spec.it s "CR 508.1b the announcement is per creature, and the two may differ" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker, piker]
        splitting :: Prompt.Prompt r -> r
        splitting p = case p of
          -- The first Piker (the lower id) is sent at Jace and the second at bob.
          Prompt.ChooseAttackTarget _ _ oid options ->
            if Just oid == Maybe.listToMaybe mine
              then attackThePlaneswalker p
              else NonEmpty.head options
          _ -> S.aggressiveAnswer p
        after = S.runCombat splitting gs
    Spec.assertEqWith s "one Piker's 2 went to Jace" (S.counterOf CounterKind.Loyalty jaceId after) 1
    Spec.assertEqWith s "and the other's 2 went to bob" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 508.1b the prompt is asked once per attacker, over the defending player and their planeswalker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker, piker]
    Spec.assertEqWith
      s
      "two attackers, two announcements, each offering both targets"
      (announcementsFor gs)
      (fmap (\oid -> (oid, [AttackTarget.OfPlayer S.bob, AttackTarget.OfPlaneswalker jaceId])) mine)
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
  -- CR 702.19f, the negative control: a plain trampler attacking a planeswalker
  -- can assign the defending player nothing, "even if ... the damage the attacking
  -- creature could assign is greater than the planeswalker's loyalty".
  --
  -- Panglacial Wurm and not War Mammoth, and that is the whole point of the case:
  -- a 3/3 into 3 loyalty is forced whether or not the keyword is there, so it
  -- could not tell the two apart. The Wurm is 9/5 with plain trample, so 6 would
  -- spill past Jace if CR 702.19f were not enforced.
  Spec.it s "CR 702.19f plain trample offers the defending player nothing" $ do
    wurm <- S.printingOf s registry "Panglacial Wurm"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [wurm]
        answer = Map.fromList [(Recipient.ToPlaneswalker jaceId, 3), (Recipient.ToPlayer S.bob, 6)]
        (after, offered) = runCombatLogging (assignmentLog answer) gs
    Spec.assertEqWith s "no division was ever asked for, so no map held the player" offered []
    Spec.assertEqWith s "bob is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.assertBool s (not (Set.member jaceId (GameState.battlefield after))) "all 9 went to Jace (CR 704.5i)"
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

-- THREE seats and a stolen planeswalker: alice attacks with Bog Wraith, bob is
-- the defending player and controls carol's Jace Beleren through a Confiscate,
-- and each of the two holds one land. With `bolted`, alice burns Jace off the
-- battlefield after the declaration, which is CR 506.4's "leaves the
-- battlefield" -- so the Wraith is attacking nothing and CR 508.5's second
-- sentence is what names its defending player.
--
-- Three seats and Confiscate together are what make the readings of that sentence
-- distinguishable. Jace's OWNER is carol and its CONTROLLER is bob, so the
-- last-known defending player (bob) and the seat any object-reading answer lands on
-- (carol, CR 108.3's owner, since the buried planeswalker leaves nothing to read a
-- controller off) hold different lands. On a board without the Aura the two
-- coincide and only liveness is proved.
stolenJaceLandwalkBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  String ->
  String ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
stolenJaceLandwalkBoard s registry bolted defendersLand ownersLand = do
  bogWraith <- S.printingOf s registry "Bog Wraith"
  piker <- S.printingOf s registry "Goblin Piker"
  mountain <- S.printingOf s registry "Mountain"
  confiscate <- S.printingOf s registry "Confiscate"
  jace <- S.printingOf s registry "Jace Beleren"
  bolt <- S.printingOf s registry "Lightning Bolt"
  bobs <- S.printingOf s registry defendersLand
  carols <- S.printingOf s registry ownersLand
  let (gs0, ours, yours, hers) = S.threePlayerCombat [bogWraith, mountain] [piker, bobs] [jace, carols]
  case (ours, yours, hers) of
    (wraith : _, blocker : _, jaceId : _) -> do
      let (confiscateId, gs1) = S.addPermanent confiscate S.bob gs0
          (boltId, gs2) = S.addHandCard bolt S.alice gs1
          gs3 = S.addCounter CounterKind.Loyalty 3 jaceId (S.attachTo confiscateId (Recipient.ToObject jaceId) gs2)
          board =
            gs3
              { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                GameState.priority = Just S.alice,
                -- CR 506.2a / CR 507.1's choice, stated rather than run: bob
                -- defends, which is what puts carol's Jace among the attackable
                -- planeswalkers (CR 306.6 reads the CONTROLLER).
                GameState.combat = (GameState.combat gs3) {Combat.Type.defenders = [S.bob]}
              }
          declared = snd (Engine.runGamePure attackThePlaneswalker board (Combat.declareAttackers S.manaPerformer S.alice))
          burned = S.runPure (aimedAtObject jaceId) declared (do S.cast S.alice boltId; Stack.resolveTop)
      pure (if bolted then S.settleSba burned else declared, wraith, blocker, jaceId)
    _ -> Spec.assertFailure s "fixture should have a Wraith, a blocker and a Jace"

-- CR 508.5's second sentence: once a creature is no longer attacking anything,
-- the defending player its abilities refer to is the controller of the
-- planeswalker it WAS attacking before that planeswalker was removed from combat
-- -- last known information. CR 702.19e is what settles that such a creature
-- still HAS a defending player at all: it assigns its damage to one.
--
-- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so CR
-- 702.14c is exactly an ability of an attacking creature that refers to a
-- defending player and no other text is in play. Each pair of cases differs in one
-- thing -- which of the two seats holds the Swamp -- and the removed pair differs
-- from the still-attacked pair in one more, whether the Bolt was cast, so no case
-- can pass because of the board rather than the rule.
lastKnownDefendingPlayerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lastKnownDefendingPlayerSpec s registry = Spec.describe s "LastKnownDefendingPlayer" $ do
  let blocks blocker wraith = Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton wraith))
  Spec.it s "CR 702.14c the premise: the stolen planeswalker's CONTROLLER is the defending player while it is attacked" $ do
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry False "Swamp" "Island"
    Spec.assertBool s (S.onBattlefield jaceId gs) "Jace is still on the battlefield"
    Spec.assertEqWith s "and bob controls him through the Confiscate" (Projection.controllerOf jaceId gs) (Just S.bob)
    Spec.assertEqWith
      s
      "the Wraith really is attacking him"
      (Map.lookup wraith (Combat.Type.attackers (GameState.combat gs)))
      (Just (AttackTarget.OfPlaneswalker jaceId))
    Spec.assertBool s (not (blocks blocker wraith gs)) "bob's Swamp stops the block"
  Spec.it s "CR 508.5 the same block stays illegal once the planeswalker has left combat" $ do
    -- THE CASE. Jace is gone, so the Wraith attacks nothing (CR 506.4c) and its
    -- swampwalk reads the player it was attacking through -- bob, who holds the
    -- Swamp. Reading the planeswalker itself finds no object at all once the CR
    -- 704.5i burial has run, so a live read answers no defending player and calls
    -- this block legal; reading the owner answers carol, whose land is an Island,
    -- and calls it legal too.
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry True "Swamp" "Island"
    Spec.assertBool s (not (S.onBattlefield jaceId gs)) "CR 704.5i: the Bolt's 3 took all of Jace's loyalty"
    Spec.assertBool s (Map.member wraith (Combat.Type.attackers (GameState.combat gs))) "CR 506.4c: still an attacking creature"
    Spec.assertBool s (not (blocks blocker wraith gs)) "bob's Swamp still stops the block"
  Spec.it s "CR 508.5 the gone planeswalker's OWNER is not the seat that is read" $ do
    -- THE FALSIFIER, and the same board with the two lands swapped: carol owns
    -- the Jace and holds the Swamp, bob defends and holds the Island. An engine
    -- that reads the buried planeswalker's owner calls this block illegal.
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry True "Island" "Swamp"
    Spec.assertBool s (not (S.onBattlefield jaceId gs)) "Jace is gone here too"
    Spec.assertBool s (blocks blocker wraith gs) "no Swamp on bob's side, so the block is legal"
  Spec.it s "CR 508.5 nor is it the seat that is read while the planeswalker is still attacked" $ do
    -- The pair above with Jace ALIVE, which is what makes the two falsifiers a
    -- reading of CR 508.5 rather than of "the planeswalker is gone": carol owns
    -- him and holds the Swamp, bob controls him and holds the Island, and CR
    -- 508.5's first sentence names the CONTROLLER. An engine reading the owner
    -- calls this block illegal, and calls the premise case legal.
    (gs, wraith, blocker, jaceId) <- stolenJaceLandwalkBoard s registry False "Island" "Swamp"
    Spec.assertBool s (S.onBattlefield jaceId gs) "Jace is still on the battlefield"
    Spec.assertBool s (blocks blocker wraith gs) "the owner's Swamp is not bob's, so the block is legal"

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

-- CR 506.4 / CR 802.2a at three seats: alice attacks CAROL's Jace Beleren with a
-- Bog Wraith, both opponents defend (CR 802.2, the default option), and BOB then
-- takes the Jace mid-combat with an Aura Graft that moves carol's Confiscate onto
-- him. That is CR 506.4's "if its controller ... changes", so the planeswalker is
-- removed from combat and CR 506.4c leaves the Wraith attacking nothing -- and
-- rule 802.2a's third sentence names the seat it had BEFORE the removal, carol.
--
-- Both seats DEFEND, which is what no live read can see through: the stolen Jace
-- is still controlled by a defending player, so Combat.attackablePlaneswalkers
-- still finds him and Combat.stillAttacked still says he is attacked. Only the
-- seat recorded as the Wraith joined combat tells the two apart.
--
-- Bog Wraith is "Creature -- Wraith 3/3, Swampwalk" and nothing else, so CR
-- 702.14c is exactly an ability of an attacking creature that refers to a
-- defending player and no other text is in play. `bobsLand` and `carolsLand` are
-- the ONE thing the two cases below differ in, and the two readings of rule
-- 802.2a invert between them: whichever seat holds the Swamp is the seat whose
-- block is stopped.
--
-- Carol's own Bonesplitter is what the Confiscate starts on, so the Graft always
-- MOVES it (CR 701.3b) and nothing about the board changes until it lands on
-- Jace. Loyalty 5 against a 3/3 keeps CR 704.5i from burying him, which would
-- answer through the departure clause instead of the control one.
stolenByBobJaceBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  String ->
  String ->
  m (GameState.GameState, GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
stolenByBobJaceBoard s registry bobsLand carolsLand = do
  bogWraith <- S.printingOf s registry "Bog Wraith"
  piker <- S.printingOf s registry "Goblin Piker"
  jace <- S.printingOf s registry "Jace Beleren"
  bonesplitter <- S.printingOf s registry "Bonesplitter"
  confiscate <- S.printingOf s registry "Confiscate"
  graft <- S.printingOf s registry "Aura Graft"
  island <- S.printingOf s registry "Island"
  bobs <- S.printingOf s registry bobsLand
  carols <- S.printingOf s registry carolsLand
  let (gs0, ours, yours, hers) = S.threePlayerCombat [bogWraith] [piker, bobs] [jace, carols, bonesplitter, piker]
  case (ours, yours, hers) of
    ([wraith], [bobsBlocker, _], [jaceId, _, splitter, carolsBlocker]) -> do
      let (auraId, gs1) = S.addPermanent confiscate S.carol (S.landsFor island S.bob 3 gs0)
          gs2 = S.addCounter CounterKind.Loyalty 5 jaceId (S.attachTo auraId (Recipient.ToObject splitter) gs1)
          (spell, gs3) = S.addHandCard graft S.bob gs2
          settled = S.runPure S.identityAnswer gs3 (Engine.runTurnBasedActions (Phase.Combat CombatStep.BeginningOfCombat))
          declared = S.runPure attackThePlaneswalker settled (Combat.declareAttackers S.manaPerformer S.alice)
          -- CR 117.5's settle follows the resolution, graftOnto's reason: it is
          -- where Combat.noteAttackingNothing samples rule 506.4.
          stolen =
            S.runPure (graftAnswer auraId jaceId) declared $ do
              S.cast S.bob spell
              Stack.resolveTop
              Engine.settleForPriority
      pure (declared, stolen, wraith, bobsBlocker, carolsBlocker, jaceId)
    _ -> Spec.assertFailure s "fixture should give alice a Wraith, bob a blocker and a land, and carol a Jace, a land, a Bonesplitter and a blocker"

-- CR 802.2a: with several defending players, "a defending player" is resolved per
-- attacking creature from what that creature is attacking -- never off the head of
-- the group. The planeswalker arm is what this proves; Pawl.BattleSpec's own
-- "CR 802.2a" pair is the battle arm's.
splitDefenderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
splitDefenderSpec s registry = Spec.describe s "SplitDefendingPlayer" $ do
  let blocks blocker wraith = Combat.legalBlockDeclaration S.carol (Map.singleton blocker (Set.singleton wraith))
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
  -- CR 802.2a's THIRD sentence at three seats: bob steals the attacked
  -- planeswalker mid-combat, CR 506.4 removes it from combat, and the seat the
  -- Wraith's swampwalk reads is the one carol held before the theft. An engine
  -- reading the planeswalker's live controller answers bob and inverts both
  -- cases; an engine that never notices the removal answers bob too, since bob
  -- is a defending player who now controls the attacked planeswalker.
  Spec.it s "CR 802.2a a planeswalker stolen mid-combat leaves its attacker reading the seat it was taken from" $ do
    (declared, stolen, wraith, bobs, carols, jaceId) <- stolenByBobJaceBoard s registry "Island" "Swamp"
    Spec.assertBool s (not (blockOf S.bob bobs wraith stolen)) "CR 802.4a: the Wraith is attacking neither bob nor a planeswalker he controlled when it joined combat, so his block is illegal however the theft left the board"
    Spec.assertBool s (not (blockOf S.carol carols wraith stolen)) "CR 702.14c: carol's own Swamp stops her block, the Wraith still reading her seat"
    Spec.assertEqWith s "CR 802.4a: and the Wraith is on carol's list alone" (fmap (\d -> Combat.attackersOn d stolen) [S.bob, S.carol]) [[], [wraith]]
    -- The premises, after the gameplay assertions so neither can absorb a
    -- mutation of them.
    Spec.assertBool s (not (blockOf S.carol carols wraith declared)) "control: the same block is illegal before the theft, so the theft changed nothing about who may block"
    Spec.assertEqWith s "CR 613.1b: bob controls Jace once the Graft has moved the Confiscate" (fmap (Projection.controllerOf jaceId) [declared, stolen]) [Just S.carol, Just S.bob]
    Spec.assertBool s (Set.member jaceId (GameState.battlefield stolen)) "CR 506.4: he never left the battlefield, so this is the controller clause"
    Spec.assertEqWith
      s
      "CR 506.4c: and it is still an attacking creature, its entry still naming Jace"
      (Map.lookup wraith (Combat.Type.attackers (GameState.combat stolen)))
      (Just (AttackTarget.OfPlaneswalker jaceId))
    Spec.assertEqWith s "CR 802.2 both opponents defend, bob first" (Combat.Type.defenders (GameState.combat stolen)) [S.bob, S.carol]
  Spec.it s "CR 702.14c and the same theft with the lands swapped leaves that block legal" $ do
    -- THE FALSIFIER, differing in one thing: bob holds the Swamp and carol an
    -- Island. An engine reading the thief's seat finds his Swamp and calls this
    -- block illegal, which is the exact inverse of the case above.
    (declared, stolen, wraith, _, carols, _) <- stolenByBobJaceBoard s registry "Swamp" "Island"
    Spec.assertBool s (blockOf S.carol carols wraith stolen) "CR 702.14c: carol has no Swamp, so her block is legal however many bob holds"
    Spec.assertBool s (blockOf S.carol carols wraith declared) "control: and it was legal before the theft too"
  -- The same theft on the DAMAGE road: CR 506.4 took Jace out of combat, so by CR
  -- 506.4c the Wraith is attacking nothing and CR 510.1b gives it nothing to
  -- assign to. Nothing on the board NOW can say so -- bob is a defending player
  -- and he controls the attacked planeswalker -- which is what
  -- Combat.noteAttackingNothing's record of the seat answers.
  Spec.it s "CR 510.1b the stolen planeswalker takes no combat damage from the creature that was attacking it" $ do
    (declared, stolen, _, _, _, jaceId) <- stolenByBobJaceBoard s registry "Island" "Swamp"
    let fight = runToEndOfCombatWith (pure . attackThePlaneswalker)
    Spec.assertEqWith s "CR 506.4c: the theft left the Wraith attacking nothing, so Jace keeps all five loyalty" (S.counterOf CounterKind.Loyalty jaceId (fight stolen)) 5
    Spec.assertEqWith s "control: with no Graft cast the same Wraith takes him to 2" (S.counterOf CounterKind.Loyalty jaceId (fight declared)) 2
  where
    blockOf pid blocker attacker = Combat.legalBlockDeclaration pid (Map.singleton blocker (Set.singleton attacker))

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

-- Announce exactly one target and choose `victim` for it, for a spell whose slot
-- takes "any number" (CR 601.2c). The set is FILTERED rather than replaced, for
-- soulSnareAnswer's reason.
aimedAtOne :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAtOne victim p = case p of
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const (1 :: Natural)) offers
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
  _ -> S.aggressiveAnswer p

-- bob casts `spell` naming `victim` alone, and resolves it.
concealOne :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
concealOne victim spell gs =
  S.runPure (aimedAtOne victim) gs $ do
    S.cast S.bob spell
    Stack.resolveTop

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
soulSnareSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
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
  -- CR 506.4 / CR 506.4c: a planeswalker that PHASES OUT is removed from combat,
  -- and the creature that was attacking it "continues to be an attacking creature,
  -- although it is not attacking any player, planeswalker, or battle" -- so Soul
  -- Snare's Or [IsAttackingPlayer You, IsAttackingPlaneswalker You] has to answer
  -- False on both atoms and the ability stops being activatable at all.
  --
  -- The trap this pays for is CR 506.4c's own requirement: Game.removeFromCombat
  -- deletes ONLY the departed planeswalker's key, so the attacker's entry survives
  -- still naming him, and Projection.controllerOf answers off GameState.objects,
  -- which CR 702.26d keeps a phased-out permanent in. Both halves are correct; the
  -- composition was not.
  --
  -- A PAIR off ONE board differing in exactly one thing: WHICH permanent Clever
  -- Concealment ({2}{W}{W} Instant, Marvel Super Heroes Commander; oracle text
  -- checked against Scryfall) is aimed at. Same spell, same five Plains, same
  -- activation, same answerer -- bob phases out his Jace on one leg and his
  -- Bonesplitter on the other, so a leg that failed for want of mana or of a
  -- resolved Concealment would fail on both.
  --
  -- The phase-out happens AFTER the declaration, which is the whole point: the
  -- Confiscate pair above applies its control change before combat, so the
  -- planeswalker is on the battlefield the entire time and no guard is load-bearing
  -- there.
  Spec.it s "CR 506.4c a planeswalker that phases out stops being attacked, so Soul Snare cannot name the attacker" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    jace <- S.printingOf s registry "Jace Beleren"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    snare <- S.printingOf s registry "Soul Snare"
    conceal <- S.printingOf s registry "Clever Concealment"
    let (gs0, ours, theirs) = S.combatBoardOf [piker] [jace, bonesplitter, snare]
    case (soulSnareAbility snare, ours, theirs) of
      (Just ability, [pikerId], [jaceId, splitter, snareId]) -> do
        let gs1 = S.addCounter CounterKind.Loyalty 5 jaceId (S.landsFor plains S.bob 5 gs0)
            (spell, gs2) = S.addHandCard conceal S.bob gs1
            declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs2
            gone = concealOne jaceId spell declared
            -- The control: the same Concealment aimed at the Bonesplitter instead,
            -- so Jace is still there and still attacked.
            stays = concealOne splitter spell declared
            hidden = runToEndOfCombatWith (soulSnareAnswer snareId ability pikerId) gone
            attacked = runToEndOfCombatWith (soulSnareAnswer snareId ability pikerId) stays
        -- GAMEPLAY FIRST, on the one quantity the two readings differ on: whether
        -- the attacker is exiled. Reading the departed planeswalker's controller
        -- anyway makes bob's Snare reach a creature attacking nothing.
        Spec.assertBool s (S.onBattlefield pikerId hidden) "CR 506.4c: the Piker attacks nothing, so bob's Snare cannot name it and it survives"
        Spec.assertBool s (not (S.onBattlefield pikerId attacked)) "control: with the Bonesplitter phased out instead, the same Snare exiles it"
        Spec.assertBool s (S.onBattlefield snareId hidden) "so bob's Snare is unsacrificed: the ability was never activatable"
        Spec.assertBool s (not (S.onBattlefield snareId attacked)) "control: there it paid its own sacrifice, so the activation really happened"
        -- Anti-vacuity. The declaration is real, and it named Jace rather than bob,
        -- so the first atom of the Or is False on both legs.
        Spec.assertEqWith
          s
          "setup: the Piker was announced at Jace and not at bob"
          (Map.lookup pikerId (Combat.Type.attackers (GameState.combat declared)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
        -- Each leg's Concealment resolved, and moved the permanent the leg names.
        Spec.assertEqWith s "CR 702.26b: Jace phased out on one leg only" (fmap (Phasing.isPhasedOut jaceId) [gone, stays]) [True, False]
        Spec.assertEqWith s "and the Bonesplitter on the other only" (fmap (Phasing.isPhasedOut splitter) [gone, stays]) [False, True]
        -- CR 702.26d: phasing changes no zone, so S.onBattlefield still says yes for
        -- him. GameState.battlefield is the set every live read walks, and the set
        -- the guard asks about.
        Spec.assertEqWith s "CR 702.26b: he is off GameState.battlefield all the same" (Set.member jaceId (GameState.battlefield gone)) False
        -- CR 506.4c itself, which is why the guard cannot be a Map.delete: the
        -- attacker's entry SURVIVES the removal, still naming the departed Jace.
        Spec.assertEqWith
          s
          "CR 506.4c: the Piker is still an attacking creature, its entry still naming Jace"
          (Map.lookup pikerId (Combat.Type.attackers (GameState.combat gone)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
        -- And Jace is still an object with a controller to read, which is the whole
        -- reason a battlefield guard rather than a missing lookup is what closes it.
        Spec.assertEqWith s "CR 702.26d: he is still in GameState.objects under bob" (Projection.controllerOf jaceId gone) (Just S.bob)
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Jace, a Bonesplitter and a Soul Snare"
  -- CR 506.4's two remaining planeswalker clauses -- "if its controller ...
  -- changes" and "if it's a planeswalker that's being attacked and stops being a
  -- planeswalker" -- which unlike the phase-out above leave the object on the
  -- battlefield under the same id, so the membership guard cannot see either.
  --
  -- ONE spell reaches both at instant speed, after the declaration: Aura Graft
  -- {1}{U} -- Instant, "Gain control of target Aura that's attached to a
  -- permanent. Attach it to another permanent it can enchant." (oracle text
  -- checked against Scryfall, 2026-08-29). The Aura it moves is what picks the
  -- clause: a Confiscate ("You control enchanted permanent") moves the CONTROLLER
  -- and leaves the card type alone; a Song of the Dryads ("Enchanted permanent is
  -- a colorless Forest land") moves the CARD TYPE and leaves the controller alone.
  -- So each leg isolates one conjunct of the guard, and neither is a sorcery-speed
  -- effect applied before combat -- #2617's mistake, which is why its Confiscate
  -- pair could not see this.
  Spec.it s "CR 506.4 a planeswalker whose CONTROLLER changes stops being attacked, so the new controller's Soul Snare cannot name the attacker" $ do
    (board, ability, ids) <- graftBoard s registry "Confiscate"
    case (ability, ids) of
      (Just snareAbility, MkGraftIds {graftPiker = pikerId, graftAliceSnare = aliceSnare, graftJace = jaceId, graftBobSnare = bobSnare, graftSpare = spare, graftAura = aura, graftSpell = graft}) -> do
        let declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker board
            -- The PAIR, differing in exactly one thing: which permanent alice's
            -- one Aura Graft moves bob's Confiscate onto. Same spell and the same
            -- lands paying for it on both legs, and each Snare has its own seat's
            -- Plains, so no leg can fail for want of mana.
            stolen = graftOnto aura jaceId graft declared
            elsewhere = graftOnto aura spare graft declared
            aliceFires = runToEndOfCombatWith (soulSnareAnswer aliceSnare snareAbility pikerId)
            bobFires = runToEndOfCombatWith (soulSnareAnswer bobSnare snareAbility pikerId)
        -- GAMEPLAY FIRST. alice now controls Jace, so an engine reading his
        -- controller anyway hands alice's Snare a creature "attacking a
        -- planeswalker you control" and lets her exile her own attacker.
        --
        -- No board can give ALICE's Snare a positive leg, and that is the rule
        -- rather than a hole in the fixture: CR 506.2 admits only the DEFENDING
        -- player's planeswalkers into a declaration, so a seat this atom answers
        -- for is never a seat that just took the planeswalker. What proves her
        -- Snare is funded and offerable is the mutation: dropping the controller
        -- conjunct makes exactly this leg exile the Piker.
        Spec.assertBool s (S.onBattlefield pikerId (aliceFires stolen)) "CR 506.4: Jace left combat with bob's control of him, so alice's Snare cannot name the Piker"
        Spec.assertBool s (S.onBattlefield aliceSnare (aliceFires stolen)) "and alice's Snare is unsacrificed: the ability was never activatable"
        -- The control, on the leg where the Confiscate went to the spare
        -- Bonesplitter: Jace is bob's and still attacked, so the same Snare card
        -- with the same {W} does exile the Piker.
        Spec.assertBool s (not (S.onBattlefield pikerId (bobFires elsewhere))) "control: with the Confiscate moved to the Bonesplitter instead, bob's Snare exiles the attacker"
        Spec.assertBool s (not (S.onBattlefield bobSnare (bobFires elsewhere))) "control: there it paid its own sacrifice, so the activation really happened"
        -- bob's own Snare on the stolen leg. False under BOTH readings, so it
        -- proves nothing about the guard -- it is here because CR 506.4c says the
        -- Piker keeps attacking, and no seat may reach it.
        Spec.assertBool s (S.onBattlefield pikerId (bobFires stolen)) "CR 506.4c: nor can bob's, the Piker attacking nothing at all"
        -- Anti-vacuity: the declaration named Jace, the Graft resolved and moved
        -- the Aura, and the clause under test is the CONTROLLER one alone.
        Spec.assertEqWith
          s
          "setup: the Piker was announced at Jace and not at bob"
          (Map.lookup pikerId (Combat.Type.attackers (GameState.combat declared)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
        Spec.assertEqWith s "CR 613.1b: alice controls Jace on one leg and bob on the other" (fmap (Projection.controllerOf jaceId) [stolen, elsewhere]) [Just S.alice, Just S.bob]
        Spec.assertBool s (Set.member jaceId (GameState.battlefield stolen)) "CR 506.4: he never left the battlefield, so the phase-out guard above cannot be what answers"
        Spec.assertBool s (Projection.isPlaneswalkerOf jaceId stolen) "nor did he stop being a planeswalker: the Confiscate writes layer 2 only"
        Spec.assertEqWith
          s
          "CR 506.4c: the Piker is still an attacking creature, its entry still naming Jace"
          (Map.lookup pikerId (Combat.Type.attackers (GameState.combat stolen)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
      _ -> Spec.assertFailure s "fixture should give alice a Piker and a Snare and bob a Jace, a Snare and two Bonesplitters"
  Spec.it s "CR 506.4 a planeswalker that stops being a planeswalker stops being attacked, so Soul Snare cannot name the attacker" $ do
    (board, ability, ids) <- graftBoard s registry "Song of the Dryads"
    case (ability, ids) of
      (Just snareAbility, MkGraftIds {graftPiker = pikerId, graftJace = jaceId, graftBobSnare = bobSnare, graftSpare = spare, graftAura = aura, graftSpell = graft}) -> do
        let declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker board
            -- The same pair, one Aura over: the Song of the Dryads lands on Jace
            -- or on the spare Bonesplitter.
            enchanted = graftOnto aura jaceId graft declared
            elsewhere = graftOnto aura spare graft declared
            bobFires = runToEndOfCombatWith (soulSnareAnswer bobSnare snareAbility pikerId)
        -- GAMEPLAY FIRST, and here the OBSERVER is bob throughout: the Song moves
        -- no control, so an engine that asks only who controls the attacked
        -- permanent answers bob on both legs and exiles the Piker on both.
        Spec.assertBool s (S.onBattlefield pikerId (bobFires enchanted)) "CR 506.4: a colorless Forest land is not a planeswalker, so bob's Snare cannot name the Piker"
        Spec.assertBool s (S.onBattlefield bobSnare (bobFires enchanted)) "and bob's Snare is unsacrificed: the ability was never activatable"
        Spec.assertBool s (not (S.onBattlefield pikerId (bobFires elsewhere))) "control: with the Song moved to the Bonesplitter instead, the same Snare exiles the attacker"
        Spec.assertBool s (not (S.onBattlefield bobSnare (bobFires elsewhere))) "control: there it paid its own sacrifice, so the activation really happened"
        -- Anti-vacuity, and the isolation: CR 613.1d's layer 4 moved, layer 2 did
        -- not, and the object never left the battlefield.
        Spec.assertEqWith s "CR 613.1d: Jace is no longer a planeswalker on one leg only" (fmap (Projection.isPlaneswalkerOf jaceId) [enchanted, elsewhere]) [False, True]
        Spec.assertEqWith s "CR 613.1b: bob controls him on both, so the controller clause is not what answers" (fmap (Projection.controllerOf jaceId) [enchanted, elsewhere]) [Just S.bob, Just S.bob]
        Spec.assertBool s (Set.member jaceId (GameState.battlefield enchanted)) "CR 506.4: nor did he leave the battlefield"
        Spec.assertEqWith
          s
          "CR 506.4c: the Piker is still an attacking creature, its entry still naming Jace"
          (Map.lookup pikerId (Combat.Type.attackers (GameState.combat enchanted)))
          (Just (AttackTarget.OfPlaneswalker jaceId))
      _ -> Spec.assertFailure s "fixture should give alice a Piker and a Snare and bob a Jace, a Snare and two Bonesplitters"
  -- CR 506.4's controller clause read as the EVENT it is: a planeswalker whose
  -- controller changes and changes BACK inside one combat stays removed from
  -- combat, and CR 506.4c leaves the creature attacking nothing for the rest of
  -- it. No reading of the CURRENT board can answer this, however many conjuncts
  -- it gets -- which is what Pawl.Types.Combat's attackingNothing record is for.
  --
  -- TWO Aura Grafts move the one Confiscate twice, so both legs end with the
  -- same board: Jace on the battlefield, still a planeswalker, under bob, with
  -- the Confiscate on a Bonesplitter. The legs differ only in whether the Aura
  -- passed through Jace on the way, and each Graft is settled before the next
  -- (CR 117.5), which is the moment rule 506.4's sampler runs and the moment a
  -- real game cannot skip between two spells resolving.
  Spec.it s "CR 506.4 a planeswalker whose controller changes and changes BACK stays removed from combat, so Soul Snare still cannot name the attacker" $ do
    (board, ability, ids, second) <- regraftBoard s registry
    case (ability, ids) of
      (Just snareAbility, MkGraftIds {graftPiker = pikerId, graftJace = jaceId, graftBobSnare = bobSnare, graftHost = host, graftSpare = spare, graftAura = aura, graftSpell = graft}) -> do
        let declared = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker board
            -- Leg one: the Confiscate goes to Jace, then on to the spare
            -- Bonesplitter. Leg two: it goes to the spare Bonesplitter and back
            -- to the one it started on, so Jace is never touched.
            stolen = graftOnto aura jaceId graft declared
            sidestep = graftOnto aura spare graft declared
            thereAndBack = graftOnto aura spare second stolen
            neverJace = graftOnto aura host second sidestep
            bobFires = runToEndOfCombatWith (soulSnareAnswer bobSnare snareAbility pikerId)
            -- A third run of each leg with the Snare never activated, so that
            -- combat damage actually happens: the OTHER reader of the record is
            -- Damage.combatRecipient, and CR 510.1b gives a creature attacking
            -- nothing nothing to assign to.
            noSnare = runToEndOfCombatWith (pure . attackThePlaneswalker)
        -- GAMEPLAY FIRST. Both boards read identically to anything asking about
        -- the board NOW, so an engine that re-derives rule 506.4 hands bob's
        -- Snare a creature "attacking a planeswalker you control" on both.
        Spec.assertBool s (S.onBattlefield pikerId (bobFires thereAndBack)) "CR 506.4: Jace left combat when alice took him, and giving him back does not put him back, so bob's Snare cannot name the Piker"
        Spec.assertBool s (S.onBattlefield bobSnare (bobFires thereAndBack)) "and bob's Snare is unsacrificed: the ability was never activatable"
        Spec.assertBool s (not (S.onBattlefield pikerId (bobFires neverJace))) "control: with the Confiscate never on Jace, the same Snare exiles the attacker"
        Spec.assertBool s (not (S.onBattlefield bobSnare (bobFires neverJace))) "control: there it paid its own sacrifice, so the activation really happened"
        Spec.assertEqWith s "CR 510.1b: and with no Snare fired the Piker assigns nothing on that leg, so Jace keeps all five loyalty" (S.counterOf CounterKind.Loyalty jaceId (noSnare thereAndBack)) 5
        Spec.assertEqWith s "control: on the leg he never left combat the same Piker takes him to 3" (S.counterOf CounterKind.Loyalty jaceId (noSnare neverJace)) 3
        -- The pair really is one board differing in one thing: alice held Jace
        -- midway on one leg only, and by the end bob holds him on both.
        Spec.assertEqWith s "CR 613.1b: alice controls Jace midway on one leg only" (fmap (Projection.controllerOf jaceId) [stolen, sidestep]) [Just S.alice, Just S.bob]
        Spec.assertEqWith s "and bob controls him again on BOTH, so no live read can tell the legs apart" (fmap (Projection.controllerOf jaceId) [thereAndBack, neverJace]) [Just S.bob, Just S.bob]
        Spec.assertEqWith s "nor can the card type: he is a planeswalker on both" (fmap (Projection.isPlaneswalkerOf jaceId) [thereAndBack, neverJace]) [True, True]
        Spec.assertEqWith s "nor the battlefield: he never left it" (fmap (Set.member jaceId . GameState.battlefield) [thereAndBack, neverJace]) [True, True]
        -- The record, and CR 506.4c's requirement that the entry survive it.
        Spec.assertEqWith s "CR 506.4c: the Piker is attacking nothing on one leg only" (fmap (Set.member pikerId . Combat.Type.attackingNothing . GameState.combat) [thereAndBack, neverJace]) [True, False]
        Spec.assertEqWith
          s
          "CR 506.4c: and it is still an attacking creature, its entry still naming Jace"
          (fmap (Map.lookup pikerId . Combat.Type.attackers . GameState.combat) [thereAndBack, neverJace])
          [Just (AttackTarget.OfPlaneswalker jaceId), Just (AttackTarget.OfPlaneswalker jaceId)]
        -- Anti-vacuity: both Grafts resolved on both legs, so the Aura really
        -- travelled twice and no leg failed for want of mana.
        Spec.assertEqWith s "CR 701.3a: the Aura ends on a Bonesplitter on both legs" (fmap (Projection.hostOf aura) [thereAndBack, neverJace]) [Just spare, Just host]
      _ -> Spec.assertFailure s "fixture should give alice a Piker and a Snare and bob a Jace, a Snare and two Bonesplitters"

-- The ids graftBoard hands back. A record rather than a tuple, and BUILT AND READ
-- by field name rather than by position: every field is an ObjectId, so a
-- positional site would take a reordering silently.
data GraftIds = MkGraftIds
  { graftPiker :: ObjectId.ObjectId,
    graftAliceSnare :: ObjectId.ObjectId,
    graftJace :: ObjectId.ObjectId,
    graftBobSnare :: ObjectId.ObjectId,
    graftHost :: ObjectId.ObjectId,
    graftSpare :: ObjectId.ObjectId,
    graftAura :: ObjectId.ObjectId,
    graftSpell :: ObjectId.ObjectId
  }

-- One board for both CR 506.4 cases above, differing only in which Aura is
-- already on the battlefield.
--
-- alice is active with one Goblin Piker and one Soul Snare; bob defends with a
-- Jace Beleren, a Soul Snare and TWO Bonesplitters. The named Aura is bob's and
-- starts attached to the first Bonesplitter, so Aura Graft always MOVES it (CR
-- 701.3b keeps the current host out of Attach.hostsFor) and the second Bonesplitter
-- is the destination the control leg uses.
--
-- Every element is load-bearing. The two Snares are the same printing and each
-- seat holds Plains of its own, so the two differ only in who holds one. Loyalty 5 against a
-- 2/1 keeps CR 704.5i from burying Jace mid-combat, which would answer through the
-- battlefield guard instead. alice's lands cover {1}{U} and {W} together, so the
-- Graft and her own activation cannot compete for mana.
--
-- Positioned at the declare attackers step with bob already the defending player,
-- which is combatBoardOf's shape: the engine's own step builds the combat record.
graftBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  String ->
  m (GameState.GameState, Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)), GraftIds)
graftBoard s registry auraName = do
  piker <- S.printingOf s registry "Goblin Piker"
  plains <- S.printingOf s registry "Plains"
  island <- S.printingOf s registry "Island"
  jace <- S.printingOf s registry "Jace Beleren"
  bonesplitter <- S.printingOf s registry "Bonesplitter"
  snare <- S.printingOf s registry "Soul Snare"
  graft <- S.printingOf s registry "Aura Graft"
  aura <- S.printingOf s registry auraName
  let (gs0, ours, theirs) = S.combatBoardOf [piker, snare] [jace, snare, bonesplitter, bonesplitter]
  case (ours, theirs) of
    ([pikerId, aliceSnare], [jaceId, bobSnare, host, spare]) -> do
      let gs1 = S.landsFor plains S.bob 3 (S.landsFor plains S.alice 2 (S.landsFor island S.alice 3 gs0))
          (auraId, gs2) = S.addPermanent aura S.bob gs1
          gs3 = S.addCounter CounterKind.Loyalty 5 jaceId (S.attachTo auraId (Recipient.ToObject host) gs2)
          (spell, gs4) = S.addHandCard graft S.alice gs3
      pure
        ( gs4,
          soulSnareAbility snare,
          MkGraftIds
            { graftPiker = pikerId,
              graftAliceSnare = aliceSnare,
              graftJace = jaceId,
              graftBobSnare = bobSnare,
              graftHost = host,
              graftSpare = spare,
              graftAura = auraId,
              graftSpell = spell
            }
        )
    _ -> Spec.assertFailure s "fixture should give alice a Piker and a Snare and bob a Jace, a Snare and two Bonesplitters"

-- alice casts Aura Graft naming `aura` and moves it onto `host`, and resolves it.
--
-- Both choices are FILTERED rather than replaced, for soulSnareAnswer's reason: a
-- leg whose slot or whose Attach.hostsFor list does not admit the id takes the
-- fallback instead of quietly succeeding, and each case's per-leg assertions on
-- Jace's controller and card type are what catch that.
--
-- CR 117.5's settle follows the resolution: it is where Combat.removeChanged
-- samples rule 506.4's derived clauses, and no real game can skip it between one
-- spell resolving and the next being cast.
graftOnto :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
graftOnto aura host spell gs =
  S.runPure (graftAnswer aura host) gs $ do
    S.cast S.alice spell
    Stack.resolveTop
    Engine.settleForPriority

-- graftBoard's board plus a SECOND Aura Graft and three more Islands to cast it
-- with, for the one case that has to move an Aura twice inside one combat. The
-- extra lands are alice's own, so neither Snare's {W} can be spent on a Graft.
regraftBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  m
    ( GameState.GameState,
      Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)),
      GraftIds,
      ObjectId.ObjectId
    )
regraftBoard s registry = do
  island <- S.printingOf s registry "Island"
  graft <- S.printingOf s registry "Aura Graft"
  (gs, ability, ids) <- graftBoard s registry "Confiscate"
  let (second, gs1) = S.addHandCard graft S.alice (S.landsFor island S.alice 3 gs)
  pure (gs1, ability, ids, second)

graftAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
graftAnswer aura host p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just aura) . Recipient.objectOf) legal) sets
  Prompt.ChooseAttachment _ _ _ offered -> if List.elem host (NonEmpty.toList offered) then host else NonEmpty.head offered
  _ -> S.aggressiveAnswer p

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
  -- CR 110.2's "under your control", the clause the card prints and the engine
  -- used to drop. It differs from the owner's control only when the player who
  -- attacked with the Towershell does not own it, which the card's own ruling
  -- calls out: "If you attack with a Meandering Towershell that you don't own,
  -- you'll control it when it returns."
  --
  -- bob OWNS it; alice steals it and attacks. The steal is Expiry.AtCleanup, so
  -- it is long gone by the return turn -- and it never applied to the returning
  -- incarnation anyway, since CR 400.7 mints a fresh id. So alice controlling
  -- what comes back can only be CR 110.2a's entry controller.
  Spec.it s "CR 110.2 a Towershell its attacker does not own returns under the ATTACKER's control" $ do
    towershell <- S.printingOf s registry "Meandering Towershell"
    island <- S.printingOf s registry "Island"
    let (base, _, theirs) = S.combatBoardOf [] [towershell]
        stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 6]
        stocked = stock S.bob (stock S.alice base)
    case theirs of
      [] -> Spec.assertFailure s "fixture should have given bob a Towershell"
      oid : _ -> do
        let stolen = S.giveControl oid S.alice stocked
            atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer stolen
            isTowershell g o = case Game.cardOf o g of
              Nothing -> False
              Just card -> S.nameOf card == towershellName
            towershells g = filter (isTowershell g) (Set.toList (GameState.battlefield g))
        -- The premise: bob owns it and alice is the one attacking with it.
        Spec.assertEqWith s "bob owns it" (fmap Object.owner (Game.lookupObject oid stolen)) (Just S.bob)
        Spec.assertEqWith s "alice controls it as it attacks" (Projection.controllerOf oid stolen) (Just S.alice)
        Spec.assertEqWith s "the return turn is alice's" (GameState.activePlayer atReturn) S.alice
        case towershells atReturn of
          [back] -> do
            -- CR 400.7: a different id from the one that attacked, so nothing
            -- from before the exile carries over on its own.
            Spec.assertBool s (back /= oid) "a fresh incarnation returned"
            Spec.assertEqWith s "still owned by bob" (fmap Object.owner (Game.lookupObject back atReturn)) (Just S.bob)
            Spec.assertEqWith s "but controlled by alice, who attacked with it" (Projection.controllerOf back atReturn) (Just S.alice)
            -- And CR 506.3b's consequence: a permanent put onto the battlefield
            -- attacking must be the ACTIVE player's, so getting the control
            -- wrong would also have left it not attacking at all.
            Spec.assertBool s (Map.member back (Combat.Type.attackers (GameState.combat atReturn))) "and it is attacking"
            -- Entering under someone's control is BASE state (CR 110.2), not a
            -- continuous effect, so there is no duration for a cleanup step to
            -- run out. Read a turn later, which is what separates it from the
            -- AtCleanup the test fixture's own steal uses: were this carried by
            -- any turn-scoped effect the Towershell would revert to bob here,
            -- and every assertion above would still have passed.
            let laterTurn = runToTurnStep 4 Phase.PostcombatMain S.aggressiveAnswer atReturn
            Spec.assertEqWith s "and alice still controls it a turn later" (Projection.controllerOf back laterTurn) (Just S.alice)
          other -> Spec.assertFailure s ("expected one returned Towershell, got " <> show (length other))

  Spec.it s "CR 508.8 whole card: it returns attacking with NOTHING declared, and the two steps stay" $ do
    -- The reason this card was worth adding: the rule's SECOND clause standing
    -- alone, at gameplay level. alice declares no attacker on the return
    -- turn -- she has none to declare -- and the declare blockers step happens
    -- regardless, because a creature was put onto the battlefield attacking.
    --
    -- Reaching the declare blockers step at all IS the assertion: had the
    -- Towershell not joined combat, Combat.skipEmptyCombat would have dropped
    -- that step and the run would have sailed past it.
    (gs, _) <- boardOf
    let atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        attackers = Combat.Type.attackers (GameState.combat atReturn)
    Spec.assertEqWith s "the return turn is alice's" (GameState.activePlayer atReturn) S.alice
    Spec.assertEqWith s "and the declare blockers step was NOT skipped" (GameState.phase atReturn) (Phase.Combat CombatStep.DeclareBlockers)
    Spec.assertEqWith s "no creature was declared as an attacker" (S.attackerDeclarationsOf atReturn) []
    Spec.assertEqWith s "the Towershell is back on the battlefield" (S.countOnBattlefieldByName towershellName S.alice atReturn) 1
    case Map.toList attackers of
      [(returned, target)] -> do
        Spec.assertEqWith s "attacking bob (CR 508.4)" target (AttackTarget.OfPlayer S.bob)
        Spec.assertEqWith s "and it entered tapped (CR 110.5b)" (tapStateOf returned atReturn) (Just TapState.Tapped)
        Spec.assertBool s (S.onBattlefield returned atReturn) "the attacker is the returned permanent"
      other -> Spec.assertFailure s ("exactly one attacking creature expected, got " <> show (length other))
  Spec.it s "CR 508.3a on the return its OWN attack trigger does not fire" $ do
    -- The discriminating case. Its ruling: "If Meandering Towershell enters the
    -- battlefield attacking, it wasn't declared as an attacking creature that
    -- turn. Abilities that trigger when a creature attacks, INCLUDING ITS OWN
    -- TRIGGERED ABILITY, won't trigger." An engine that routed the return
    -- through the declaration would exile it again on the spot and arm a second
    -- delayed ability, so both halves are asserted.
    (gs, _) <- boardOf
    let atReturn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
    Spec.assertEqWith s "it is still on the battlefield, not exiled again" (S.countOnBattlefieldByName towershellName S.alice atReturn) 1
    Spec.assertEqWith s "the delayed store is empty: nothing armed a second return" (length (GameState.delayedTriggers atReturn)) 0
    Spec.assertEqWith s "and no declaration was recorded for it" (S.attackerDeclarationsOf atReturn) []
  Spec.it s "CR 508.8 the combat damage step is not skipped either: bob takes 5" $ do
    -- The other half of that clause, and the end-to-end statement of it. The
    -- declare blockers step being reached says the schedule kept it; this says
    -- the combat damage step ran and the creature that never attacked dealt its
    -- damage anyway (CR 508.4: such creatures ARE attacking).
    (gs, _) <- boardOf
    let afterCombat = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith s "a 5/9 connected" (S.lifeOf S.bob afterCombat) (Just 15)
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
  Spec.it s "CR 702.14c whole card: islandwalk keeps the returned Towershell unblockable" $ do
    -- The pool's first ISLANDwalk (Bog Wraith, #500's card, prints swampwalk),
    -- and the only window in which this card's own evasion can be read: on the
    -- turn it is declared it exiles itself before blockers are declared, so the
    -- return turn is where the keyword does its work.
    --
    -- bob controls an Island and a Wall of Stone, and blocks with everything he
    -- can -- so a Towershell without islandwalk would be blocked here and deal
    -- bob nothing.
    --
    -- A WALL and not a Goblin Piker, because bob's own turn falls between the
    -- two combats: CR 702.3b keeps a creature with defender out of the
    -- declaration, so the Wall is still untapped when the Towershell comes back,
    -- where a Piker would have attacked on turn 2 and be tapped (CR 509.1a) --
    -- unable to block for a reason that has nothing to do with evasion.
    wall <- S.printingOf s registry "Wall of Stone"
    island <- S.printingOf s registry "Island"
    (gs, _) <- boardWith [island, wall]
    let afterCombat = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    Spec.assertEqWith s "the Wall could not block it (CR 702.14c)" (S.lifeOf S.bob afterCombat) (Just 15)

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

-- Answers CR 107.4f's mana-or-life announcement with `way` wherever it is on
-- offer, and defers everything else to S.aggressiveAnswer -- Pawl.ManaSpec's
-- `announces`, duplicated rather than hoisted.
--
-- The fall-through matters to the case that uses it: on a board offering only
-- one route this answers the same way whatever `way` says, so two legs that
-- DIFFER are two legs that were each asked.
announcesWay :: PhyrexianPayment.PhyrexianPayment -> Prompt.Prompt r -> r
announcesWay way p = case p of
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers ->
    if elem way (NonEmpty.toList offers) then way else NonEmpty.head offers
  _ -> S.aggressiveAnswer p

-- Set a seat's life directly, so a toll's life route can be priced against a
-- total the fixture chose. Pawl.CoinSpec's `atLife`, duplicated rather than
-- hoisted.
atLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
atLife pid n gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  creaturePlaneswalkerCombatSpec s registry
  putOntoBattlefieldAttackingSpec s registry
  towershellSpec s registry
  planeswalkerAttackSpec s registry
  trampleOverPlaneswalkersSpec s registry
  sharedBlockerSpec s registry
  lastKnownDefendingPlayerSpec s registry
  splitDefenderSpec s registry
  soulSnareSpec s registry

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Combat from the declaration outward: the set-shaped
-- declaration rules (CR 506.5, CR 508.1d bounds), the combat keywords that bite
-- after blockers (vigilance, first and double strike, trample), damage
-- assignment across planeswalkers and shared blockers, removal from combat (CR
-- 506.4), and the continuous effects that change control, type or text mid-combat.
-- The declaration and evasion half is Pawl.CombatSpec, which describes under the
-- same name. Also Pawl.Engine.AttackCost and Pawl.Engine.BlockCost, whose only
-- consumer is Pawl.Engine.Combat's CR 508.1d / CR 509.1c cost clause and CR
-- 508.1h / CR 509.1d total.
module Pawl.CombatEffectSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone

-- The helpers below are duplicated from Pawl.CombatSpec rather than hoisted into
-- Pawl.Support, which every spec in the tree imports and so rebuilds.

declaredAttackers :: GameState.GameState -> [ObjectId.ObjectId]
declaredAttackers gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
   in (after, ours, yours)

-- Any printings at all onto `who`'s battlefield, on a board that already exists.
-- S.addCreature is any-printing rather than creature-only, which is how
-- landSubtypeStripSpec below reaches an enchantment.
withPermanents :: PlayerId.PlayerId -> [Printing.Printing] -> GameState.GameState -> GameState.GameState
withPermanents who ps gs = List.foldl' (\g p -> snd (S.addCreature p who g)) gs ps

-- An attacking board with a Lure attached to the first attacker.
luring :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
luring lure mine theirs =
  let (gs, ours, yours) = attacking mine theirs
   in case ours of
        -- Unreachable: every caller passes at least one attacking printing.
        [] -> (gs, ours, yours)
        attacker : _ ->
          let (aura, withAura) = S.addCreature lure S.alice gs
           in (S.attach aura attacker withAura, ours, yours)

-- A Curse of the Nightly Hunt attached to `who`, on a fresh combat board.
cursing :: Printing.Printing -> PlayerId.PlayerId -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
cursing curse who mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
   in (cursingBoard curse who gs, ours, yours)

-- The same Curse, attached to a board that already exists. What `cursing` is
-- built from, and what a board it cannot build -- `jaceBoard`'s, which needs its
-- planeswalker's loyalty counters placed first -- reaches for instead.
cursingBoard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
cursingBoard curse who gs =
  let (aura, withAura) = S.addCreature curse S.alice gs
   in S.attachTo aura (Recipient.ToPlayer who) withAura

-- CR 508.1c read through CR 506.5, proved by Bonded Construct ("{1} Artifact
-- Creature -- Construct 2/1, This creature can't attack alone") -- the attacking
-- side's SET-SHAPED restriction, and the twin of Pawl.CombatSpec's Menace group
-- across the combat phase.
--
-- What makes it a different KIND of restriction from Pacifism's, and not merely a
-- narrower one: there is no answer to "may this creature attack?" at all. The
-- Construct may attack in some declarations and not in others, so it stays on CR
-- 508.1a's candidate list and the illegality is a property of the declaration.
-- The first assertion of every case below is that it is still offered, because
-- "the attack was refused" is also what a bug that drops it from the candidate
-- list produces -- and that bug would pass every negative assertion here.
--
-- The requirement cases are the subtle half. CR 508.1d's maximum is "the maximum
-- possible number of requirements that could be obeyed WITHOUT DISOBEYING ANY
-- RESTRICTIONS", and before this card no attacking restriction could take a
-- required creature's declaration away, so the maximum was always every
-- requirement at once. Under a Curse of the Nightly Hunt a lone Construct is
-- required to attack and forbidden to attack alone, and the maximum is zero.
attacksAloneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attacksAloneSpec s registry = Spec.describe s "AttacksAlone" $ do
  Spec.it s "CR 506.5 a lone Bonded Construct is a legal CANDIDATE that may not be the whole declaration" $ do
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    let (gs, mine, _) = S.combatBoardOf [bondedConstruct] []
    case mine of
      [construct] -> do
        Spec.assertBool s (Combat.canAttack S.alice construct gs) "the Construct can attack"
        Spec.assertEqWith s "and is offered" (Combat.legalAttackers S.alice gs) [construct]
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "but attacking alone is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "and declining stays legal"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 506.5 a Goblin Piker beside it makes the same Construct's attack legal" $ do
    -- The permitted case on the SAME board as the refused one, which is what
    -- separates this restriction from summoning sickness, a tap, or a defender:
    -- none of those changes its answer when a second creature is declared.
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [bondedConstruct, piker] []
    case mine of
      [construct, other] -> do
        Spec.assertEqWith s "both are offered" (Combat.legalAttackers S.alice gs) [construct, other]
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "the Construct alone is still illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [construct, other] gs) "the two together are legal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [other] gs) "and the Piker alone is legal, so nothing blanket-refused"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1c two Bonded Constructs may attack together, which is that rule's own Example" $ do
    -- Verbatim: "A player controls two creatures, each with a restriction that
    -- states 'This creature can't attack alone.' It's legal to declare both as
    -- attackers." The reading it falsifies is "each restricted creature needs an
    -- UNRESTRICTED companion", which passes every other case in this group.
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    let (gs, mine, _) = S.combatBoardOf [bondedConstruct, bondedConstruct] []
    case mine of
      [first, second] -> do
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] gs) "declaring both is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first] gs)) "either one alone is illegal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [second] gs)) "in both directions"
      _ -> Spec.assertFailure s "fixture should have two Constructs"
  Spec.it s "CR 508.1d a required creature that can't attack alone makes the maximum ZERO" $ do
    -- The board CR 508.1d's closed form got wrong: the Curse requires the
    -- Construct to attack if able, the Construct may not attack alone, and there
    -- is nobody to attack with -- so no legal declaration obeys the requirement
    -- and declining attains the maximum. A ceiling that assumed "every required
    -- creature at once is legal" answers this by forcing an illegal attack.
    --
    -- The lone Piker under the same Curse is the control, and it is the case that
    -- makes this one non-vacuous: there declining IS illegal, so the Curse is
    -- live and it is the Construct's restriction that moved the maximum.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [bondedConstruct] []
        (control, _, _) = cursing curse S.alice [piker] []
    case mine of
      [construct] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] control)) "a required Piker may not decline"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "but a required Construct with no company may"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "and attacking alone stays illegal, requirement or no requirement"
      _ -> Spec.assertFailure s "fixture should have one creature"
  Spec.it s "CR 508.1d with company the maximum is BOTH, and the Piker alone no longer attains it" $ do
    -- The other side of the same interaction. One Curse over two able creatures
    -- is two requirements, both obeyable at once because attacking together is
    -- legal -- so the restriction bounds the maximum here without zeroing it, and
    -- the declaration that obeys only the Piker's is now illegal.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [bondedConstruct, piker] []
    case mine of
      [construct, other] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "declining is illegal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [other] gs)) "the Piker alone obeys one of two"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "the Construct alone is illegal twice over, on the restriction and on the count"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [construct, other] gs) "only both together is legal"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1d a Ghostly Prison excuses the whole maximum even where attacking together is legal" $ do
    -- The cost clause reaching the ENUMERATION, which is the one path of
    -- attackCeiling the cases above leave untested: the restriction is in force,
    -- so the closed form is not taken, and the search still has to be over the
    -- creatures that attack FREELY. With the Prison out and no Forests, neither
    -- creature does, so the maximum is zero even though attacking together would
    -- obey both requirements and disobey nothing.
    --
    -- An enumeration that ranged over every candidate would answer two here and
    -- make declining illegal, which is CR 508.1d's third sentence exactly
    -- backwards: a player is never required to pay a cost to attack.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _) = imprisoning prison forest S.bob [bondedConstruct, piker] 0
        taxed = cursingBoard curse S.alice board
        (plain, _, _) = cursing curse S.alice [bondedConstruct, piker] []
    case mine of
      [construct, other] -> do
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] plain)) "without the Prison, declining is illegal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] taxed) "with it, declining is legal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [construct, other] taxed) "and attacking together anyway is still legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] taxed)) "while the Construct alone stays illegal, cost or no cost"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 506.5 whole cards: a lone Construct sits out a real declare attackers step" $ do
    -- The gameplay-level case, through the priority loop and CR 703.4i's
    -- turn-based action rather than a direct call, with the interpreter that
    -- attacks with everything it is offered.
    --
    -- THREE boards, because two would not be enough. The Construct with a Piker
    -- connects for 4; the Construct alone is refused and bob takes nothing; the
    -- PIKER alone connects for 2, which is what rules out "a lone attacker never
    -- gets through" as the explanation of the middle board.
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pair, mine, _) = S.combatBoardOf [bondedConstruct, piker] []
        (lone, _, _) = S.combatBoardOf [bondedConstruct] []
        (lonePiker, _, _) = S.combatBoardOf [piker] []
        after = S.runCombat S.aggressiveAnswer pair
        refused = S.runCombat S.aggressiveAnswer lone
        control = S.runCombat S.aggressiveAnswer lonePiker
    Spec.assertEqWith s "with company, bob takes four" (S.lifeOf S.bob after) (Just 16)
    Spec.assertEqWith s "and both were declared" (S.attackerDeclarationsOf after) mine
    Spec.assertEqWith s "alone, bob takes nothing" (S.lifeOf S.bob refused) (Just 20)
    Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf refused) []
    Spec.assertEqWith s "while a lone PIKER connects for two" (S.lifeOf S.bob control) (Just 18)

-- CR 508.1c and CR 509.1b, proved by Silent Arbiter ("{4} Artifact Creature --
-- Construct 1/5, No more than one creature can attack each combat. No more than
-- one creature can block each combat.") -- the restriction that forbids a
-- declaration for its SIZE, and the third shape of combat restriction after
-- Pacifism's per-creature one and Bonded Construct's set-shaped one.
--
-- What makes it a different kind again from Bonded Construct's: that one NAMES
-- creatures and asks what the declaration holds, so a board of two of them
-- allows a two-creature attack (CR 508.1c's Example). This one names no creature
-- at all, so no Affected could carry it and no candidate list can hide it -- and
-- it is not scoped to its controller either, which the cases below prove by
-- putting the Arbiter on the DEFENDING player's battlefield and holding the
-- attacking player to one attacker.
--
-- The first assertion of every case is that the creatures are still OFFERED, on
-- attacksAloneSpec's terms: "the attack was refused" is also what a bug that
-- subtracted them from CR 508.1a's or CR 509.1a's candidate list produces, and
-- that bug would pass every negative assertion here.
--
-- The empty declaration is asserted LEGAL wherever no requirement is in force,
-- because "no more than one" is a ceiling and not a quota: a reading of the bound
-- as "exactly one" passes every other assertion in the first case.
boundedDeclarationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
boundedDeclarationSpec s registry = Spec.describe s "BoundedDeclaration" $ do
  Spec.it s "CR 508.1c a Silent Arbiter allows EITHER attacker but not both" $ do
    -- Both single-creature declarations are legal on the SAME board the
    -- two-creature one is refused on, which is what separates a size bound from
    -- summoning sickness, a tap, a defender, or the Arbiter simply not being a
    -- candidate: none of those changes its answer when a creature is dropped
    -- from the declaration.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [silentArbiter, piker] []
        (control, theirs, _) = S.combatBoardOf [piker, piker] []
    case (mine, theirs) of
      ([arbiter, other], [first, second]) -> do
        Spec.assertEqWith s "both are offered" (Combat.legalAttackers S.alice gs) [arbiter, other]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [arbiter] gs) "the Arbiter alone is legal"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [other] gs) "the Piker alone is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [arbiter, other] gs)) "the two together are not"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] gs) "and declining stays legal, so the bound is a ceiling and not a quota"
        -- The control that makes the refusal the BOUND talking: two Goblin
        -- Pikers and no Arbiter attack together happily.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] control) "two creatures attack together without the Arbiter"
      _ -> Spec.assertFailure s "fixture should have two creatures a side"
  Spec.it s "CR 508.1c the bound is GLOBAL: bob's Arbiter holds ALICE to one attacker" $ do
    -- Silent Arbiter's sentence says "no more than one creature", not "no more
    -- than one creature you control". A reader that scoped the bound to its
    -- source's controller passes every other attacking case in this group, since
    -- the Arbiter sits on alice's side in all of them.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker, piker] [silentArbiter]
        (control, _, _) = S.combatBoardOf [piker, piker] [piker]
    case mine of
      [first, second] -> do
        Spec.assertEqWith s "both of alice's are offered" (Combat.legalAttackers S.alice gs) [first, second]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first] gs) "one of them may attack"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first, second] gs)) "but not both"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] control) "and with a plain Piker there instead, both may"
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 509.1b the same Arbiter holds bob to one BLOCKER" $ do
    -- The blocking half of the same card, and the same anti-vacuity shape: both
    -- of bob's Pikers are offered, either may block alone, and the pair is
    -- refused. Alice attacks with one creature because the Arbiter's first
    -- sentence already holds her to one -- which is the attacking half's global
    -- reach, observed again.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [silentArbiter, piker, piker]
        (control, plain, others) = attacking [piker] [piker, piker]
    case (mine, theirs, plain, others) of
      ([a], [arbiter, first, second], [b], [x, y]) -> do
        Spec.assertEqWith s "all three of bob's are offered" (Combat.legalBlockers S.bob gs) [arbiter, first, second]
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton a)) gs) "one blocker is legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton second (Set.singleton a)) gs) "either one of them"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton a), (second, Set.singleton a)]) gs)) "two are not"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty gs) "and declining stays legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(x, Set.singleton b), (y, Set.singleton b)]) control) "two Pikers double block without the Arbiter"
      _ -> Spec.assertFailure s "fixture should have one attacker and bob's blockers"
  Spec.it s "CR 508.1d's own Example: the required creature attacks, and nothing else does" $ do
    -- Verbatim: "A player controls two creatures: one that 'attacks if able' and
    -- one with no abilities. An effect states 'No more than one creature can
    -- attack each turn.' The only legal attack is for just the creature that
    -- 'attacks if able' to attack. It's illegal to attack with the other
    -- creature, attack with both, or attack with neither."
    --
    -- Built as: a Kormus Bell animating alice's Swamp into a 1/1, a Synthetic
    -- Wetland Frenzy requiring Swamps to attack, and a Goblin Piker that is not a
    -- Swamp -- so exactly ONE requirement instance is minted, which is what the
    -- Example needs and what a Curse of the Nightly Hunt could not give. The
    -- Arbiter is bob's, so "an effect states" is the global sentence the Example
    -- describes rather than something alice's own board says.
    --
    -- The control on the same board WITHOUT the Arbiter is what proves the Frenzy
    -- is live independently of the bound: there attacking with both is legal and
    -- declining is not.
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    (gs, swampId, pikerId) <- exampleBoard s registry [silentArbiter]
    (control, controlSwamp, controlPiker) <- exampleBoard s registry []
    Spec.assertEqWith s "both of alice's creatures are offered" (Combat.legalAttackers S.alice gs) [swampId, pikerId]
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [swampId] gs) "the required Swamp alone is the only legal attack"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [pikerId] gs)) "attacking with the other creature is illegal"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [swampId, pikerId] gs)) "with both is illegal"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "with neither is illegal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [controlSwamp, controlPiker] control) "without the Arbiter both may attack"
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] control)) "and the Frenzy still forbids declining, so it is live on its own"
  Spec.it s "CR 508.1d two required creatures under a bound of one: the maximum is ONE, not two" $ do
    -- The board attackCeiling's CLOSED FORM gets wrong, and the reason its guard
    -- has to test the answer rather than the restriction. With a Curse of the
    -- Nightly Hunt over two Goblin Pikers the instance set is both of them, and
    -- the closed form hands back both -- a declaration the bound forbids, which
    -- no player could attain, so every declaration would be illegal at once.
    -- Enumerating instead finds a maximum of one, and either Piker attains it.
    --
    -- The case above does NOT prove this: there the closed form's answer is the
    -- single required Swamp, which is within a bound of one, so the shortcut is
    -- still exact. Two required creatures is the smallest board where the two
    -- readings disagree.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [piker, piker] [silentArbiter]
        (control, _, _) = cursing curse S.alice [piker, piker] []
    case mine of
      [first, second] -> do
        Spec.assertEqWith s "both are still offered" (Combat.legalAttackers S.alice gs) [first, second]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first] gs) "attacking with one attains the maximum"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [second] gs) "and so does the other one"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first, second] gs)) "both together is over the bound"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "and declining obeys neither requirement"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first] control)) "without the Arbiter one Piker no longer attains it"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [first, second] control) "and both together do"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 508.1d two requirements on ONE creature count twice" $ do
    -- CR 508.1d counts REQUIREMENTS being obeyed, not the creatures they name.
    -- Berserkers of Blood Ridge carries its own ("this creature attacks each
    -- combat if able") AND the Curse's, so it is worth two; the Piker is worth
    -- one. CR 508.1c's Silent Arbiter caps the declaration at one creature, so
    -- the maximum obtainable is two and only the Berserkers attains it.
    --
    -- Without the Arbiter the two readings agree: the maximizing declaration is
    -- both creatures either way, and multiplicity decides nothing. A SET-shaped
    -- restriction is what forces the choice between them, which is why the case
    -- above is this one's control rather than its equal -- there each Piker
    -- carries one requirement and the multiset changes no answer.
    --
    -- The Berserkers is declared FIRST deliberately. candidateAttackDeclarations
    -- folds so that the LATER candidate's singleton is enumerated first, and ties
    -- in attackCeilingGiven's fold go to the earlier entry -- so under a
    -- creature-counting reading `best` is the Piker alone, and the two
    -- discriminating assertions bite. With the Berserkers last they would agree
    -- with both readings.
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [berserkers, piker] [silentArbiter]
        (control, _, _) = cursing curse S.alice [berserkers, piker] []
    case mine of
      [bers, plain] -> do
        -- Anti-vacuity: both really are able attackers under the Arbiter, so
        -- nothing below is a CR 508.1a or CR 508.1c refusal in disguise. These
        -- three pass on both readings of CR 508.1d, which is what makes them a
        -- control rather than more of the same assertion.
        Spec.assertEqWith s "both are offered" (Combat.legalAttackers S.alice gs) [bers, plain]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [bers] gs) "attacking with the Berserkers attains the maximum"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "declining obeys nothing"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [bers, plain] gs)) "both together is over the Arbiter's bound"
        -- The proving assertion: one requirement where two were available.
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [plain] gs)) "but the Piker alone obeys one requirement where two were available"
        -- The second discriminator, over attackCeilingGiven's tie-break.
        let candidates = Combat.legalAttackers S.alice gs
        Spec.assertEqWith s "and the forced declaration names the Berserkers" (fmap fst (Combat.forcedAttackDeclaration (Combat.attackCeiling candidates gs) candidates)) [bers]
        -- Control: strip the Arbiter and the two readings agree again.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [bers, plain] control) "without the Arbiter, both together is legal"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [bers] control)) "and the Berserkers alone no longer attains the maximum"
      _ -> Spec.assertFailure s "fixture should have two creatures"
  Spec.it s "CR 509.1c a Lure under a bound of one: the maximum is ONE blocker" $ do
    -- The blocking twin of the case above, over blockCeiling's fold. Lure makes
    -- every creature able to block the enchanted attacker do so, which is all
    -- three of bob's; the bound allows one. So declining becomes illegal, exactly
    -- one blocker is legal, and two remain forbidden -- the requirement and the
    -- restriction each moving one of the three answers.
    lure <- S.printingOf s registry "Lure"
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = luring lure [piker] [silentArbiter, piker, piker]
        (control, plain, others) = luring lure [piker] [piker, piker]
    case (mine, theirs, plain, others) of
      ([a], [_, first, second], [b], [x, y]) -> do
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "declining is illegal under the Lure"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton first (Set.singleton a)) gs) "one blocker attains the maximum"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.fromList [(first, Set.singleton a), (second, Set.singleton a)]) gs)) "two are over the bound"
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton x (Set.singleton b)) control)) "without the Arbiter one blocker no longer attains it"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.fromList [(x, Set.singleton b), (y, Set.singleton b)]) control) "and both blocking does"
      _ -> Spec.assertFailure s "fixture should have one attacker and bob's blockers"
  Spec.it s "CR 508.1c whole cards: an over-large attack is refused in a real declare attackers step" $ do
    -- The gameplay-level attacking case, through the priority loop and CR 703.4i's
    -- turn-based action, with the interpreter that attacks with everything it is
    -- offered.
    --
    -- THREE boards, on attacksAloneSpec's terms, and no two share an observable:
    -- the Arbiter beside a Piker is refused outright and bob takes nothing; a lone
    -- Piker connects for two, which rules out "a lone attacker never gets
    -- through"; the Arbiter attacking by ITSELF connects for one, which rules out
    -- "the Arbiter is never a legal attacker".
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (pair, _, _) = S.combatBoardOf [silentArbiter, piker] []
        (lonePiker, _, _) = S.combatBoardOf [piker] []
        (loneArbiter, _, _) = S.combatBoardOf [silentArbiter] []
        refused = S.runCombat S.aggressiveAnswer pair
        connects = S.runCombat S.aggressiveAnswer lonePiker
        alone = S.runCombat S.aggressiveAnswer loneArbiter
    Spec.assertEqWith s "two offered, none declared" (S.attackerDeclarationsOf refused) []
    Spec.assertEqWith s "so bob takes nothing" (S.lifeOf S.bob refused) (Just 20)
    Spec.assertEqWith s "a lone Piker connects for two" (S.lifeOf S.bob connects) (Just 18)
    Spec.assertEqWith s "and the Arbiter by itself connects for one" (S.lifeOf S.bob alone) (Just 19)
  Spec.it s "CR 509.1b whole cards: an over-large block is refused in a real declare blockers step" $ do
    -- The gameplay-level blocking case, run through Combat.declareBlockers as
    -- Pawl.CombatSpec's Menace group runs its own. S.aggressiveAnswer blocks with everything, which
    -- on the first board is two creatures and therefore illegal, so
    -- declareBlockers falls back to the forced declaration -- the empty one, not
    -- a block repaired down to one creature.
    --
    -- THREE boards again: two candidate blockers under the Arbiter (nobody
    -- blocks), ONE candidate blocker under the same Arbiter (it blocks, so the
    -- Arbiter does not forbid blocking as such), and two candidates with no
    -- Arbiter (both block, so two blockers are not refused as such).
    silentArbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (crowded, mine, theirs) = attacking [piker] [silentArbiter, piker]
        (single, mineToo, theirsToo) = attacking [piker] [silentArbiter]
        (plain, mineThree, theirsThree) = attacking [piker] [piker, piker]
    case (mine, theirs, mineToo, theirsToo, mineThree, theirsThree) of
      ([a], [arbiter, other], [b], [loneArbiter], [c], [x, y]) -> do
        Spec.assertEqWith s "both of bob's are offered" (Combat.legalBlockers S.bob crowded) [arbiter, other]
        let refused = S.runPure S.aggressiveAnswer crowded Combat.declareBlockers
            blocked = S.runPure S.aggressiveAnswer single Combat.declareBlockers
            doubled = S.runPure S.aggressiveAnswer plain Combat.declareBlockers
        Spec.assertEqWith s "nobody blocks" (Combat.blockersOf a refused) Set.empty
        Spec.assertEqWith s "the lone Arbiter does block" (Combat.blockersOf b blocked) (Set.singleton loneArbiter)
        Spec.assertEqWith s "and without one, two Pikers do" (Combat.blockersOf c doubled) (Set.fromList [x, y])
      _ -> Spec.assertFailure s "fixture should have one attacker on each board"

-- CR 508.1d's Example board: alice controls a Kormus Bell animating her Swamp
-- into a 1/1, a Synthetic Wetland Frenzy requiring Swamps to attack, and a Goblin
-- Piker that is not a Swamp -- so the Swamp is the Example's creature that
-- "attacks if able" and the Piker is its creature with no abilities. `theirs` is
-- bob's side, which is where the Example's "an effect states" goes.
--
-- Named rather than inlined because both the Example and its no-Arbiter control
-- need it, and the two must differ in nothing else.
exampleBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  [Printing.Printing] ->
  m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
exampleBoard s registry theirs = do
  bell <- S.printingOf s registry "Kormus Bell"
  swamp <- S.printingOf s registry "Swamp"
  piker <- S.printingOf s registry "Goblin Piker"
  frenzy <- S.printingOf s registry "Synthetic Wetland Frenzy"
  let (gs0, ours, _) = S.combatBoardOf [bell, swamp, piker] theirs
      gs1 = snd (S.addCreature frenzy S.alice gs0)
  case ours of
    [_, swampId, pikerId] -> pure (gs1, swampId, pikerId)
    _ -> Spec.assertFailure s "fixture should have the Bell, the Swamp and the Piker"

vigilanceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vigilanceSpec s registry = Spec.describe s "Vigilance" $ do
  Spec.it s "CR 702.20b attacking doesn't tap a creature with vigilance, but does tap its neighbor" $ do
    -- Both creatures in ONE declaration, so a blanket "nothing taps" bug
    -- cannot pass: the Piker must still tap.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [windseekerCentaur, piker] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    case mine of
      [centaur, p] -> do
        Spec.assertEqWith s "both attacking" (length (declaredAttackers after)) 2
        Spec.assertEqWith s "the centaur is untapped" (tapStateOf centaur after) (Just TapState.Untapped)
        Spec.assertEqWith s "the piker is tapped" (tapStateOf p after) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 702.20b vigilance still attacks" $ do
    -- Vigilance is not a legality question: the creature is declared as an
    -- attacker exactly as normal. It simply skips CR 508.1f's tap.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [windseekerCentaur] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.alice))
    Spec.assertEqWith s "attacking" (declaredAttackers after) mine
  Spec.it s "CR 702.20b an untapped vigilant attacker can still be blocked" $ do
    -- It is attacking, so it is in the Combat record, tapped or not.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [windseekerCentaur] [piker]
        steps = do
          Combat.declareAttackers S.alice
          Combat.declareBlockers
        after = snd (Engine.runGamePure S.aggressiveAnswer gs steps)
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      attacker : _ -> Spec.assertEqWith s "blocked" (Combat.blockersOf attacker after) (Set.fromList theirs)

combatLegalitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
combatLegalitySpec s registry = Spec.describe s "CombatLegality" $ do
  Spec.it s "a Settled untapped creature may attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ -> Spec.assertBool s (Combat.canAttack S.alice oid gs) "may attack"
  Spec.it s "CR 302.6 a summoning sick creature may not attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ ->
        let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
         in Spec.assertBool s (not (Combat.canAttack S.alice oid sick)) "may not attack"
  Spec.it s "CR 508.1a a tapped creature may not attack" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
    case mine of
      [] -> Spec.assertFailure s "fixture should have an attacker"
      oid : _ ->
        let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
         in Spec.assertBool s (not (Combat.canAttack S.alice oid tapped)) "may not attack"
  Spec.it s "a land may not attack" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = (S.landsInPlay mountain 1) {GameState.activePlayer = S.alice}
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> Spec.assertBool s (not (Combat.canAttack S.alice oid gs)) "may not attack"
  Spec.it s "you may not attack with a creature you do not control" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      oid : _ -> Spec.assertBool s (not (Combat.canAttack S.alice oid gs)) "not alice's"
  -- CR 302.6 restricts attacking and tap abilities. It says NOTHING about
  -- blocking, and getting this wrong is the classic beginner bug.
  Spec.it s "CR 302.6 a summoning sick creature MAY block" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      oid : _ ->
        let sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)}
         in Spec.assertBool s (Combat.canBlock S.bob oid sick) "may block"
  Spec.it s "CR 509.1a a tapped creature may not block" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, theirs) = S.combatBoard piker 1 1
    case theirs of
      [] -> Spec.assertFailure s "fixture should have a blocker"
      oid : _ ->
        let tapped = gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}
         in Spec.assertBool s (not (Combat.canBlock S.bob oid tapped)) "may not block"
  Spec.it s "legalAttackers lists exactly the active player's creatures" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 2 3
    Spec.assertEqWith s "two" (Combat.legalAttackers S.alice gs) mine
  Spec.it s "CR 508.1a a player can attack with a creature they control but do not own" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, base) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
        gs0 = S.giveControl oid S.alice base
    Spec.assertBool s (elem oid (Combat.legalAttackers S.alice gs0)) "alice may attack with it"
    Spec.assertBool s (notElem oid (Combat.legalAttackers S.bob gs0)) "bob may not (not the controller, not active)"
  Spec.it s "combat starts empty and clears" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoard piker 1 0
        busy = case mine of
          [] -> gs
          oid : _ ->
            gs
              { GameState.combat =
                  Combat.Type.MkCombat
                    { Combat.Type.attackers = Map.singleton oid (AttackTarget.OfPlayer S.bob),
                      Combat.Type.blockers = Map.empty,
                      Combat.Type.struckFirst = Nothing,
                      Combat.Type.joinedUnder = Map.singleton oid S.alice,
                      Combat.Type.attacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      Combat.Type.declaredAttacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      -- Empty, because this board stands after the declare attackers
                      -- step ended: CR 500.1 scopes this half to the step, and
                      -- Combat.clearAttackedThisStep empties it as one ends.
                      Combat.Type.declaredAttackedThisStep = Set.empty,
                      Combat.Type.blockersDeclared = True,
                      Combat.Type.defender = Just S.bob
                    }
              }
    Spec.assertEqWith s "starts empty" (Combat.Type.attackers (GameState.combat gs)) Map.empty
    Spec.assertEqWith s "clears" (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy))) Map.empty
    -- CR 506.7c: the CR 511.3 reset re-arms CR 506.7b's boundary, so a CR 500.8
    -- second combat phase gets its own window rather than inheriting this one's.
    Spec.assertBool s (Turn.afterBlockersDeclared busy) "CR 506.7b's boundary is up while combat is live"
    Spec.assertBool s (not (Turn.afterBlockersDeclared (Combat.clearCombat busy))) "and down again once combat clears"

keywordSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
keywordSpec s registry = Spec.describe s "Keyword" $ do
  let gs0 = Setup.emptyGame S.bothPlayers
      -- Each M2a printing carries exactly its one keyword and no other.
      carriesOnly (name, keyword) =
        Spec.it s (name <> " carries exactly " <> show keyword) $ do
          printing <- S.printingOf s registry name
          let (oid, gs) = S.addCreature printing S.alice gs0
          Spec.assertEqWith s "keywords" (Projection.keywordsOf oid gs) (Map.singleton keyword 1)
          Spec.assertBool s (Projection.hasKeyword keyword oid gs) "hasKeyword"
  mapM_ carriesOnly S.m2aKeywords
  Spec.it s "a Piker has no keywords" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (oid, gs) = S.addCreature piker S.alice gs0
    Spec.assertEqWith s "none" (Projection.keywordsOf oid gs) Map.empty
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying oid gs)) "no flying"
  Spec.it s "a Mountain has no keywords" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs = S.landsInPlay mountain 1
    case Game.zoneMembers Zone.Battlefield S.alice gs of
      [] -> Spec.assertFailure s "fixture should have one Mountain"
      oid : _ -> Spec.assertEqWith s "none" (Projection.keywordsOf oid gs) Map.empty
  Spec.it s "an unknown id has no keywords" $
    Spec.assertEqWith s "none" (Projection.keywordsOf (ObjectId.MkObjectId 999) gs0) Map.empty
  -- Flying is on Bird Maiden and NOT on Nimble Birdsticker. If this
  -- passes while the reach case above also passes, the two keywords
  -- are genuinely distinct rather than one flag.
  Spec.it s "reach is not flying" $ do
    nimbleBirdsticker <- S.printingOf s registry "Nimble Birdsticker"
    let (oid, gs) = S.addCreature nimbleBirdsticker S.alice gs0
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying oid gs)) "no flying"

-- Run whole steps until the first-strike combat damage step has been dealt
-- (struckFirst is set) or combat ends, so a test can observe the board BETWEEN
-- the two combat damage steps.
runToFirstStrikeDone :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToFirstStrikeDone answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (Combat.Type.struckFirst (GameState.combat g))
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

firstStrikeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
firstStrikeSpec s registry = Spec.describe s "FirstStrike" $ do
  Spec.it s "CR 702.7b a first striker kills a vanilla blocker and lives" $ do
    -- The tiger (2/1 first strike) kills the Piker (2/1) in the first-strike
    -- step; the SBA between steps buries it before it can deal, so the tiger
    -- survives at zero damage.
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [sabretoothTiger] [piker]
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "the blocker is dead" (S.creaturesInPlay S.bob after) 0
    Spec.assertEqWith s "the first striker lives" (S.creaturesInPlay S.alice after) 1
  Spec.it s "CR 510.2 the control: two vanilla 2/1s trade" $ do
    -- With a Piker in the tiger's place there is one combat damage step and
    -- both die. So first strike is the sole cause above.
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] [piker]
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "alice's is dead" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "bob's is dead" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 702.4b a double striker deals twice to an unblocked player" $ do
    -- The raptor (2/1 double strike) deals 2 in each step: bob loses 4.
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let (gs, _, _) = S.combatBoardOf [ridgetopRaptor] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "bob took 4" (S.lifeOf S.bob after) (Just 16)
  Spec.it s "CR 702.7b the control: a first striker deals once to a player" $ do
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    let (gs, _, _) = S.combatBoardOf [sabretoothTiger] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 510.1b the control: a vanilla creature deals once to a player" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [piker] []
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "bob took 2" (S.lifeOf S.bob after) (Just 18)
  Spec.it s "CR 510.4 double strike kills a 3/3 across two steps; first strike does not" $ do
    -- The raptor deals 2 + 2 = 4 to the Ogre (3/3), killing it. A first
    -- striker deals 2 once, and the Ogre lives.
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    ogreSentry <- S.printingOf s registry "Ogre Sentry"
    let raptorVs = S.combatBoardOf [ridgetopRaptor] [ogreSentry]
        tigerVs = S.combatBoardOf [sabretoothTiger] [ogreSentry]
        afterRaptor = S.runCombat S.aggressiveAnswer (frst raptorVs)
        afterTiger = S.runCombat S.aggressiveAnswer (frst tigerVs)
    Spec.assertEqWith s "double strike kills the Ogre" (S.creaturesInPlay S.bob afterRaptor) 0
    Spec.assertEqWith s "first strike leaves the Ogre" (S.creaturesInPlay S.bob afterTiger) 1
  Spec.it s "CR 510.4 a striker killed in the first step does not deal in the second" $ do
    -- Raptor (double strike) and tiger (first strike) each block-kill the
    -- other in the first step. Neither is "remaining" for the second step, so
    -- no second-wave damage; both are simply dead.
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    let (gs, _, _) = S.combatBoardOf [ridgetopRaptor] [sabretoothTiger]
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "attacker dead" (S.creaturesInPlay S.alice after) 0
    Spec.assertEqWith s "blocker dead" (S.creaturesInPlay S.bob after) 0
  Spec.it s "CR 510.4 the mixed board: first strike once, vanilla once, double strike twice" $ do
    -- Tiger (first strike), raptor (double strike) and Piker (vanilla) all
    -- attack unblocked. First-strike step: tiger 2 + raptor 2 = 4. Second
    -- step: raptor 2 + Piker 2 = 4. bob: 20 - 8 = 12. The naive "strikers in
    -- step one, everyone else in step two" drops the raptor's second hit and
    -- lands bob at 14.
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = S.combatBoardOf [sabretoothTiger, ridgetopRaptor, piker] []
        mid = runToFirstStrikeDone S.aggressiveAnswer gs
        after = S.runCombat S.aggressiveAnswer gs
    Spec.assertEqWith s "after the first-strike step, bob took 4" (S.lifeOf S.bob mid) (Just 16)
    Spec.assertEqWith s "after both steps, bob took 8" (S.lifeOf S.bob after) (Just 12)

-- Attacks and blocks with everything, and casts whenever a cast is legal --
-- aggressiveAnswer's combat decisions with castAnswer's priority decision. The
-- end-of-combat group needs both: the attack has to happen for there to be an
-- attacking creature, and the spell has to be cast for the attacking-ness to be
-- observable.
attackAndCast :: Prompt.Prompt r -> r
attackAndCast p = case p of
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.aggressiveAnswer p

-- alice attacks with one Piker while holding a Kill Shot and exactly the three
-- Plains that pay for it; bob has nothing, so the attack is unblocked. Sits at
-- the declare attackers step like every combatBoardOf board, so the ENGINE
-- declares the attack and carries it forward -- the combat record this group
-- observes is never hand-written. S.addCreature is what puts the Plains out: it
-- is the "any printing, on the battlefield, untapped and Settled" helper its
-- haddock says it is, and lands need exactly that.
killShotBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
killShotBoard plains piker killShot =
  let (gs0, _, _) = S.combatBoardOf [piker] []
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature plains S.alice g))
      (withCard, _) = S.handOne killShot (addLands 3 gs0)
   in -- handOne parks its state in a precombat main phase; this board is mid-combat.
      withCard {GameState.phase = GameState.phase gs0, GameState.priority = GameState.priority gs0}

-- Run whole steps until the end of combat step is the current phase, WITHOUT
-- running it, so a test can play that one step itself under a different
-- answerer.
runToEndOfCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToEndOfCombat = S.runToStep (Phase.Combat CombatStep.EndOfCombat)

-- CR 511.3: creatures are removed from combat as the end of combat step ENDS, so
-- they are still attacking for the whole of that step -- including its priority
-- round (CR 511.1), where the active player may cast an instant. Kill Shot
-- ("Destroy target attacking creature") is what makes the window observable: it
-- has a legal target during the end of combat step and none after it.
endOfCombatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
endOfCombatSpec s registry = Spec.describe s "EndOfCombat" $ do
  Spec.it s "CR 511.3 whole card: Kill Shot destroys an attacker during the end of combat step" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    killShot <- S.printingOf s registry "Kill Shot"
    let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
        after = snd (Engine.runGamePure attackAndCast atEnd Engine.runStep)
    Spec.assertEqWith s "the step under test is the end of combat step" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
    Spec.assertBool s (not (Map.null (Combat.Type.attackers (GameState.combat atEnd)))) "the Piker is still attacking as the step begins"
    Spec.assertEqWith s "the attacker was destroyed" (S.creaturesInPlay S.alice after) 0
  Spec.it s "CR 511.3 the removal still happens, one step later: combat is empty once the step ends" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    killShot <- S.printingOf s registry "Kill Shot"
    let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
        after = snd (Engine.runGamePure S.aggressiveAnswer atEnd Engine.runStep)
    Spec.assertEqWith s "the combat phase is over" (GameState.phase after) Phase.PostcombatMain
    Spec.assertEqWith s "no attackers" (Combat.Type.attackers (GameState.combat after)) Map.empty
    -- CR 506.2's designation is scoped to the combat phase, and clearCombat
    -- resets it alongside the attackers.
    Spec.assertEqWith s "no defending player" (Combat.Type.defender (GameState.combat after)) Nothing
  Spec.it s "CR 511.3 the twin: the same Kill Shot has no target in the postcombat main phase" $ do
    -- The discriminator for the case above. If IsAttacking simply read True
    -- for every creature, or if combat were never cleared at all, this would
    -- kill the Piker too.
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    killShot <- S.printingOf s registry "Kill Shot"
    let atEnd = runToEndOfCombat S.aggressiveAnswer (killShotBoard plains piker killShot)
        postcombat = snd (Engine.runGamePure S.aggressiveAnswer atEnd Engine.runStep)
        after = snd (Engine.runGamePure attackAndCast postcombat Engine.runStep)
    Spec.assertEqWith s "the step under test is the postcombat main phase" (GameState.phase postcombat) Phase.PostcombatMain
    Spec.assertEqWith s "the Piker survives" (S.creaturesInPlay S.alice after) 1

-- The state out of a combatBoardOf triple.
frst :: (a, b, c) -> a
frst (a, _, _) = a

m2bExitSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
m2bExitSpec s registry = Spec.describe s "M2bExit" $ do
  Spec.it s "the milestone: first strike breaks the trade, double strike doubles the hit, no attacker no damage" $ do
    sabretoothTiger <- S.printingOf s registry "Sabretooth Tiger"
    piker <- S.printingOf s registry "Goblin Piker"
    ridgetopRaptor <- S.printingOf s registry "Ridgetop Raptor"
    let trade = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [sabretoothTiger] [piker]))
        doubled = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [ridgetopRaptor] []))
        quiet = S.runCombat S.aggressiveAnswer (frst (S.combatBoardOf [] []))
    Spec.assertEqWith s "first striker lives" (S.creaturesInPlay S.alice trade) 1
    Spec.assertEqWith s "its would-be killer is dead" (S.creaturesInPlay S.bob trade) 0
    Spec.assertEqWith s "double striker deals 4" (S.lifeOf S.bob doubled) (Just 16)
    Spec.assertEqWith s "an attacker-less turn deals nothing" (S.lifeOf S.bob quiet) (Just 20)

-- alice is mid-combat with three Pikers; bob holds a Ray of Command and exactly
-- the four Islands that pay for it, and controls nothing else. The board sits at
-- the declare attackers step like every combatBoardOf board, so the ENGINE
-- declares the attack and carries it forward: no test here writes the combat
-- record. S.addCreature is what puts the Islands out -- the "any printing, on the
-- battlefield, untapped and Settled" helper its haddock says it is.
rayBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, [ObjectId.ObjectId])
rayBoard island piker ray =
  let (gs0, mine, _) = S.combatBoardOf [piker, piker, piker] []
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature island S.bob g))
   in (snd (S.addHandCard ray S.bob (addLands 4 gs0)), mine)

-- Attack with everything except `homebody`, never block, cast whenever a cast is
-- offered, and aim every target at `victim`.
--
-- Blocks are DECLINED rather than routed, and that is what keeps the two legs
-- comparable: a stolen attacker arrives untapped (Ray of Command untaps it) and
-- hasty under its new controller, so an aggressive blocker answer would have bob
-- block with the very creature the case is about and hide the damage question
-- behind CR 509.1's routing.
steal :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
steal homebody victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block with everything, cast whenever a cast is offered, aim every target at
-- `victim`. The blocker-side twin of `steal`.
snatch :: ObjectId.ObjectId -> Prompt.Prompt r -> r
snatch victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if it leaves the battlefield, if
-- its controller changes, ..." -- and a creature so removed "stops being an
-- attacking, blocking, blocked, and/or unblocked creature".
--
-- Ray of Command is the pool's producer for the control-change clause: {3}{U},
-- INSTANT, "Untap target creature an opponent controls and gain control of it
-- until end of turn. That creature gains haste until end of turn." Act of Treason
-- has the same three effects and cannot reach this window at all, because it is a
-- sorcery -- which is why this clause was worked card-driven rather than built
-- speculatively. Ray of Command's third sentence, the delayed trigger that taps the
-- creature when its controller loses it, does not reach these legs: every one of
-- them stops at the end of combat step, well before the CR 514.2 sweep that ends
-- the control effect. Pawl.TriggerSpec's "RayOfCommand" group is what proves it.
--
-- Every leg runs whole steps through Engine.runStep, so the combat record under
-- test is the engine's own and the removal is observed where a player would see
-- it: at the CR 117.5 settle that follows the spell resolving. Each leg stops at
-- the end of combat step, where CR 511.3 says the record still reads live.
controlChangeRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
controlChangeRemovalSpec s registry = Spec.describe s "ControlChangeRemoval" $ do
  Spec.it s "CR 506.4 whole card: Ray of Command on an attacker removes THAT attacker from combat, and it deals no combat damage" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    killShot <- S.printingOf s registry "Kill Shot"
    case (rayBoard island piker rayOfCommand, S.spellTargetSlot killShot) of
      ((gs, [stolen, other, homebody]), Just attackingSlot) -> do
        let atEnd = runToEndOfCombat (steal homebody stolen) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
            legal = Target.legalRecipients Nothing S.noSource attackingSlot atEnd
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertEqWith s "bob really did gain control of it" (Projection.controllerOf stolen atEnd) (Just S.bob)
        Spec.assertBool s (Map.notMember stolen attackers) "CR 506.4: so it is no longer an attacking creature"
        Spec.assertBool s (Map.member other attackers) "the attacker bob left alone is untouched"
        -- The discriminating assertion: the unfixed engine keeps the stolen
        -- Piker in the record and deals its 2 alongside the other's.
        Spec.assertEqWith s "CR 510.1: bob takes only the surviving attacker's 2" (S.lifeOf S.bob atEnd) (Just 18)
        -- CR 508.1k through the door a card actually uses: Kill Shot's own
        -- committed target slot is Pool.Creatures narrowed by IsAttacking.
        Spec.assertBool s (not (Set.member (Recipient.ToCreature stolen) legal)) "Filter.IsAttacking no longer finds the stolen creature"
        Spec.assertBool s (Set.member (Recipient.ToCreature other) legal) "and still finds the one that is attacking"
      _ -> Spec.assertFailure s "fixture should have three Pikers and Kill Shot a 'target' slot"
  Spec.it s "CR 506.4 the twin: the same Ray of Command on a creature that is not in combat leaves combat intact" $ do
    -- The control leg, and the reason the case above is not passing for a
    -- trivial reason. The SAME card resolves, the SAME settle runs, and
    -- control really does change -- just not for a combatant. A sampler that
    -- cleared combat whenever it saw a control change, or whenever anything
    -- resolved, would take the attackers out here too.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    case rayBoard island piker rayOfCommand of
      (gs, [one, two, homebody]) -> do
        let atEnd = runToEndOfCombat (steal homebody homebody) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertEqWith s "bob gained control of the creature that stayed home" (Projection.controllerOf homebody atEnd) (Just S.bob)
        Spec.assertBool s (Map.member one attackers && Map.member two attackers) "both attackers are still attacking"
        Spec.assertEqWith s "so bob takes both hits" (S.lifeOf S.bob atEnd) (Just 16)
      _ -> Spec.assertFailure s "fixture should have three Pikers"
  Spec.it s "CR 506.4 a stolen BLOCKER is removed from combat, and CR 509.1h leaves the attacker blocked" $ do
    -- The blocker side of the same clause, and the interaction the
    -- Combat.blockers shape exists for: Game.removeFromCombat drops the
    -- blocker from the SET while the attacker's KEY survives, so the attacker
    -- stays blocked and (CR 510.1c) assigns no combat damage at all.
    --
    -- The theft has to land after blocks are declared, so the declare
    -- attackers step is played under an answerer that does not cast and only
    -- the declare blockers step onwards sees `snatch`.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    let (gs0, mine, theirs) = S.combatBoardOf [piker] [piker]
        addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature island S.alice g))
        gs = snd (S.addHandCard rayOfCommand S.alice (addLands 4 gs0))
    case (mine, theirs) of
      (attacker : _, blocker : _) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            atEnd = runToEndOfCombat (snatch blocker) atBlockers
            -- The control leg: the same board and the same blocks, with alice
            -- never casting. Two 2/1 Pikers then trade and both die.
            traded = runToEndOfCombat S.aggressiveAnswer atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so `snatch` is what declares the blocks and then casts" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        -- The discriminating assertion, and first because it is the one the
        -- unfixed engine fails: with the blocker still in the record the two
        -- Pikers trade, and the ids below stop resolving at all.
        Spec.assertBool s (S.onBattlefield attacker atEnd && S.onBattlefield blocker atEnd) "CR 510.1c: neither creature was dealt combat damage"
        Spec.assertEqWith s "alice really did gain control of the blocker" (Projection.controllerOf blocker atEnd) (Just S.alice)
        Spec.assertEqWith s "CR 506.4: it is blocking nothing" (Combat.blockersOf attacker atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked attacker atEnd) "CR 509.1h: but the attacker remains blocked"
        Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (not (S.onBattlefield attacker traded) && not (S.onBattlefield blocker traded)) "control leg: with no theft the two Pikers trade and both die"
      _ -> Spec.assertFailure s "fixture did not build an attacker and a blocker"

-- Labyrinth of Skophos' SECOND activated ability -- "{4}, {T}: Remove target
-- attacking or blocking creature from combat" -- read off the JSON-loaded
-- printing rather than hand-built, so every leg below exercises the codec's
-- parse of the committed card data (S.spellTargetSlot's posture, for an
-- activated ability rather than a spell). The first is the land's "{T}: Add
-- {C}".
removalAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
removalAbility printing = case Face.activatedAbilities (S.combinedFace printing) of
  [_, ability] -> Just ability
  _ -> Nothing

-- That ability's "target" slot: CR 601.2c's narrowing, reached for an
-- activated ability through CR 602.2b, which for this card is Pool.Creatures
-- under `Or [IsAttacking, IsBlocking]`.
removalTargetSlot :: ActivatedAbility.ActivatedAbility Card.Type.Card -> Maybe TargetSlot.TargetSlot
removalTargetSlot ability =
  Map.lookup
    (SlotName.MkSlotName (Text.pack "target"))
    (Modal.allTargetSlots (ActivatedAbility.modal ability))

-- alice is mid-combat with one creature per printing in `mine`; bob defends with
-- one per printing in `theirs`. `who` also controls a Labyrinth of Skophos and
-- the four lands that pay its {4}. S.addCreature is what puts all five out --
-- the "any printing, on the battlefield, untapped and Settled" helper its
-- haddock says it is, which is what a land needs.
skophosBoard ::
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  [Printing.Printing] ->
  [Printing.Printing] ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
skophosBoard labyrinth land who mine theirs =
  let (gs0, ours, yours) = S.combatBoardOf mine theirs
      addLands n g = if n <= (0 :: Int) then g else addLands (n - 1) (snd (S.addCreature land who g))
      (mazeId, gs1) = S.addCreature labyrinth who (addLands 4 gs0)
   in (gs1, ours, yours, mazeId)

-- Fire the Labyrinth's removal ability once, aim it at `victim`, and pay the {4}
-- with anything BUT the Labyrinth itself: CR 601.2g pays an activation's mana
-- before its components (Pawl.Engine.Activate), so tapping the land for its own {C}
-- would leave the {T} unpayable and revert the whole activation. Choosing around
-- that is the player's job, not the engine's.
--
-- Every other prompt falls through to S.aggressiveAnswer, so attacks and blocks
-- still happen.
--
-- STATEFUL, and it has to be, for GameSpec's illegalActivationAnswer reason: an
-- answerer that names the same Activate at every ask never lets the priority
-- loop terminate once the activation stops SUCCEEDING. A rejected one is a
-- no-op (CR 601.2c: an answer outside the legal target set reverts the whole
-- activation), so the cost goes unpaid, the land stays untapped, and the same
-- action is offered again forever. That is unreachable while the engine is
-- right, and this was written pure first: breaking Filter.IsBlocking on purpose
-- hung the suite instead of failing it, which is not a test.
mazeAnswer ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card ->
  ObjectId.ObjectId ->
  Prompt.Prompt r ->
  State.State Bool r
mazeAnswer mazeId ability victim p = case p of
  Prompt.ChooseAction _ _ actions -> do
    tried <- State.get
    if tried || notElem (A.Activate mazeId ability) actions
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate mazeId ability)
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (const (Set.singleton (Recipient.ToCreature victim))) sets)
  Prompt.ChooseManaSource _ _ candidates ->
    pure (Just (Maybe.fromMaybe (NonEmpty.head candidates) (List.find (/= mazeId) (NonEmpty.toList candidates))))
  _ -> pure (S.aggressiveAnswer p)

-- runToEndOfCombat's stateful twin, for the answerer above: the same bounded
-- walk of whole steps, threading the "have I activated yet" flag across them.
runToEndOfCombatWith ::
  (forall r. Prompt.Prompt r -> State.State Bool r) ->
  GameState.GameState ->
  GameState.GameState
runToEndOfCombatWith answer gs0 =
  let go n g s =
        if n <= (0 :: Int)
          || GameState.phase g == Phase.Combat CombatStep.EndOfCombat
          || not (S.inCombatPhase (GameState.phase g))
          then g
          else
            let ((_, g1), s1) = State.runState (Engine.runGame answer g Engine.runStep) s
             in go (n - 1) g1 s1
   in go 8 gs0 False

-- Attack with everything except `homebody`, and otherwise behave aggressively --
-- so the board carries an attacking creature, a blocking creature and a creature
-- that is neither, which is what the target filter has to tell apart.
stayHomeAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
stayHomeAnswer homebody p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (/= homebody) ids
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if ... an effect specifically
-- removes it from combat." The rule's one clause a card ASKS for, rather than a
-- condition the engine has to notice -- and Labyrinth of Skophos is the pool's
-- producer: "{T}: Add {C}. / {4}, {T}: Remove target attacking or blocking
-- creature from combat." (Land, Murders at Karlov Manor Commander; oracle text
-- checked against Scryfall.)
--
-- Every leg runs whole steps through Engine.runStep, so the combat record under
-- test is the engine's own: the fixture declares nothing by hand. The two damage
-- legs stop at the end of combat step, where CR 511.3 says the record still
-- reads live; the filter leg stops one step earlier, before anything dies.
--
-- Removal is removal only. Nothing here puts a creature back into combat, which
-- is what the rules say too -- the glossary's "removed from combat" entry has
-- the permanent take "no further involvement in that combat phase".
effectRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
effectRemovalSpec s registry = Spec.describe s "EffectRemoval" $ do
  Spec.it s "CR 506.4 whole card: Labyrinth of Skophos removes target ATTACKING creature, and it deals no combat damage" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
    case (removalAbility labyrinth, skophosBoard labyrinth island S.bob [piker] []) of
      (Just ability, (gs, [attacker], _, mazeId)) -> do
        let atEnd = runToEndOfCombatWith (mazeAnswer mazeId ability attacker) gs
            quiet = runToEndOfCombat S.aggressiveAnswer gs
            legal = fmap (\theSlot -> Target.legalRecipients Nothing S.noSource theSlot atEnd) (removalTargetSlot ability)
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertEqWith s "the ability really was activated: its {T} component was paid" (tapStateOf mazeId atEnd) (Just TapState.Tapped)
        Spec.assertBool s (Map.notMember attacker (Combat.Type.attackers (GameState.combat atEnd))) "CR 506.4: the Piker stopped being an attacking creature"
        -- The discriminating assertion: with the removal missing, the Piker
        -- stays in the record and deals its 2.
        Spec.assertEqWith s "CR 510.1: so bob takes nothing" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertEqWith s "and the card's own target filter no longer finds it" (fmap (Set.member (Recipient.ToCreature attacker)) legal) (Just False)
        Spec.assertBool s (Map.member attacker (Combat.Type.attackers (GameState.combat quiet))) "control leg: unactivated, the Piker is still attacking"
        Spec.assertEqWith s "and bob takes its 2" (S.lifeOf S.bob quiet) (Just 18)
      _ -> Spec.assertFailure s "fixture should give bob a Labyrinth with two abilities and alice one Piker"
  Spec.it s "CR 509.1h a removed BLOCKER leaves the attacker blocked, so nothing is dealt combat damage" $ do
    -- The blocker side of the same clause, and the interaction
    -- Game.removeFromCombat's two-way edit of Combat.blockers exists for: the
    -- blocker leaves the SET while the attacker's KEY survives, so the
    -- attacker stays blocked and (CR 510.1c) assigns no combat damage at all.
    --
    -- alice holds the Labyrinth and aims it at her opponent's blocker, so the
    -- removal has to land after blocks are declared: the declare attackers
    -- step is played under an answerer that never activates, and only the
    -- declare blockers step onwards sees `mazeAnswer`.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
    case (removalAbility labyrinth, skophosBoard labyrinth island S.alice [piker] [piker]) of
      (Just ability, (gs, [attacker], [blocker], mazeId)) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            atEnd = runToEndOfCombatWith (mazeAnswer mazeId ability blocker) atBlockers
            -- The control leg: the same board and the same blocks, with the
            -- ability never activated. Two 2/1 Pikers then trade.
            traded = runToEndOfCombat S.aggressiveAnswer atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the blocks are declared before the activation" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        Spec.assertEqWith s "the ability really was activated" (tapStateOf mazeId atEnd) (Just TapState.Tapped)
        Spec.assertBool s (S.onBattlefield attacker atEnd && S.onBattlefield blocker atEnd) "CR 510.1c: neither creature was dealt combat damage"
        Spec.assertEqWith s "CR 506.4: the removed creature is blocking nothing" (Combat.blockersOf attacker atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked attacker atEnd) "CR 509.1h: but the attacker remains blocked"
        Spec.assertEqWith s "so bob takes nothing either" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (not (S.onBattlefield attacker traded) && not (S.onBattlefield blocker traded)) "control leg: unactivated, the two Pikers trade and both die"
      _ -> Spec.assertFailure s "fixture should give alice a Labyrinth and an attacker, and bob a blocker"
  Spec.it s "CR 601.2c the card's filter admits the attacker and the blocker and rejects the creature that stayed home" $ do
    -- Or [IsAttacking, IsBlocking], and both halves are load-bearing: with
    -- IsAttacking alone the blocker would be rejected, and with no filter at
    -- all the homebody would be admitted.
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    labyrinth <- S.printingOf s registry "Labyrinth of Skophos"
    case (removalAbility labyrinth, skophosBoard labyrinth island S.alice [piker, piker] [piker]) of
      (Just ability, (gs, [attacker, homebody], [blocker], _)) -> do
        -- The combat damage step is the vantage point: blockers have been
        -- declared and nothing has died yet.
        let atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) (stayHomeAnswer homebody) gs
            legal = fmap (\theSlot -> Target.legalRecipients Nothing S.noSource theSlot atDamage) (removalTargetSlot ability)
            admits oid = fmap (Set.member (Recipient.ToCreature oid)) legal
        Spec.assertEqWith s "the fixture reached the combat damage step with blocks declared" (GameState.phase atDamage) (Phase.Combat CombatStep.CombatDamage)
        Spec.assertBool s (Set.member blocker (Combat.blockersOf attacker atDamage)) "the blocker really is blocking the attacker"
        Spec.assertEqWith s "IsAttacking admits the attacker" (admits attacker) (Just True)
        Spec.assertEqWith s "IsBlocking admits the blocker" (admits blocker) (Just True)
        Spec.assertEqWith s "and the creature in neither role is rejected" (admits homebody) (Just False)
      _ -> Spec.assertFailure s "fixture should give alice two Pikers and a Labyrinth, and bob a blocker"

-- alice is mid-combat with Opalescence, Living Plane and a Goblin Piker, plus one
-- Forest that Living Plane has made a 1/1 creature; bob defends with nothing but
-- the two Swamps that pay for the Doom Blade in his hand. The board sits at the
-- declare attackers step like every combatBoardOf board, so the ENGINE declares
-- the attack: no test here writes the combat record.
--
-- Returns alice's three combatBoardOf permanents in printing order alongside the
-- Forest, which is added separately because it is not one of them.
unmakeBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], ObjectId.ObjectId)
unmakeBoard opalescence livingPlane piker forest swamp doomBlade =
  let (gs0, mine, _) = S.combatBoardOf [opalescence, livingPlane, piker] []
      (land, gs1) = S.addCreature forest S.alice gs0
      addSwamps n g = if n <= (0 :: Int) then g else addSwamps (n - 1) (snd (S.addCreature swamp S.bob g))
   in (snd (S.addHandCard doomBlade S.bob (addSwamps 2 gs1)), mine, land)

-- alice attacks with `land` alone, nobody blocks, and whoever is offered a cast
-- takes it and aims every target at `victim`. The shape of `steal` above, with a
-- reason of its own for declining blocks: bob's own lands are 1/1 creatures while
-- Living Plane lives, so an aggressive blocker answer would put them in front of
-- the attacker and hide the question this asks behind CR 509.1's routing.
unmake :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unmake land victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareAttackers _ _ ids -> filter (== land) ids
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- alice attacks with one Goblin Piker and holds the Doom Blade and the two
-- Swamps that pay for it; bob defends with Opalescence, Living Plane and the
-- Forest that Living Plane has made a 1/1 creature. The mirror of unmakeBoard,
-- with the animator on the DEFENDING side so the creature that stops being one is
-- a blocker.
unblockBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
unblockBoard opalescence livingPlane piker forest swamp doomBlade =
  let (gs0, mine, theirs) = S.combatBoardOf [piker] [opalescence, livingPlane]
      (land, gs1) = S.addCreature forest S.bob gs0
      addSwamps n g = if n <= (0 :: Int) then g else addSwamps (n - 1) (snd (S.addCreature swamp S.alice g))
   in (snd (S.addHandCard doomBlade S.alice (addSwamps 2 gs1)), mine, theirs, land)

-- Attack with `attacker` alone and cast nothing. The declare attackers step of
-- the blocker leg is played under this, so alice's Swamps -- 1/1 creatures while
-- Living Plane lives, and therefore legal attackers -- stay untapped to pay for
-- the Doom Blade she casts a step later.
attackOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
attackOnly attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone and cast nothing: the control leg
-- of the blocker case, where Living Plane is left alone and the block resolves
-- into an ordinary trade.
blockOnly :: ObjectId.ObjectId -> Prompt.Prompt r -> r
blockOnly blocker p = case p of
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker (Set.singleton a)
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- Block the first attacker with `blocker` alone, cast whenever a cast is offered,
-- and aim every target at `victim`. Blocking with everything instead would put
-- Living Plane -- a 4/4 creature while Opalescence is out -- in front of the
-- attacker too, and killing it would then be a blocker LEAVING THE BATTLEFIELD,
-- which is a different clause of CR 506.4.
unblock :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
unblock blocker victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    a : _ -> Map.singleton blocker (Set.singleton a)
    [] -> Map.empty
  _ -> S.aggressiveAnswer p

-- CR 506.4: "A permanent is removed from combat if ... it's an attacking or
-- blocking creature that ... stops being a creature."
--
-- The clause has no one-card producer, and does not need one: the rule asks about
-- the permanent's creature-ness, not about how it was lost. Three pool cards make
-- it happen through the layer system alone, and every oracle text below was
-- checked against Scryfall.
--
--   * Living Plane ({2}{G}{G} World Enchantment, "All lands are 1/1 creatures
--     that are still lands") is what makes a Forest able to attack or block.
--   * Opalescence ({2}{W}{W} Enchantment, "Each other non-Aura enchantment is a
--     creature in addition to its other types and has base power and base
--     toughness each equal to its mana value") is what puts Living Plane itself
--     within reach of a creature-removal spell. Without it nothing in the pool
--     can touch an enchantment at instant speed, which is why this clause waited
--     on a producer rather than being built speculatively.
--   * Doom Blade ({1}{B} Instant, "Destroy target nonblack creature") kills the
--     green Living Plane in the priority round after the declaration.
--
-- CR 611.3b is what makes that enough: a static ability's continuous effect
-- "applies at all times that the permanent generating it is on the battlefield",
-- so Living Plane leaving takes the animation with it and the Forest stops being
-- a creature WITHOUT leaving the battlefield. That is what makes this the types
-- clause rather than CR 506.4's leaves-the-battlefield one, and each leg asserts
-- the Forest is still on the battlefield to pin it.
--
-- Every leg runs whole steps through Engine.runStep and stops at the end of
-- combat step, where CR 511.3 says the record still reads live.
typeChangeRemovalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
typeChangeRemovalSpec s registry = Spec.describe s "TypeChangeRemoval" $ do
  Spec.it s "CR 506.4 whole cards: an attacking Forest that stops being a creature is removed from combat" $ do
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    opalescence <- S.printingOf s registry "Opalescence"
    livingPlane <- S.printingOf s registry "Living Plane"
    doomBlade <- S.printingOf s registry "Doom Blade"
    case unmakeBoard opalescence livingPlane piker forest swamp doomBlade of
      (gs, [_, plane, _], land) -> do
        let atEnd = runToEndOfCombat (unmake land plane) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertEqWith s "the leg really reached the end of combat step, where the record still reads live (CR 511.3)" (GameState.phase atEnd) (Phase.Combat CombatStep.EndOfCombat)
        Spec.assertBool s (elem land (S.attackerDeclarationsOf atEnd)) "the Forest really was attacking: the declaration happened while it was a creature"
        Spec.assertBool s (not (S.onBattlefield plane atEnd)) "the Doom Blade really did kill Living Plane"
        Spec.assertBool s (not (Projection.isCreatureOf land atEnd)) "CR 611.3b: so the Forest stopped being a creature"
        Spec.assertBool s (S.onBattlefield land atEnd) "and is still on the battlefield, so this is the types clause and not the leaves-the-battlefield one"
        -- The discriminating assertion: the unfixed engine leaves the Forest
        -- in the record as an attacking creature, which CR 506.3 says a
        -- noncreature permanent cannot be.
        Spec.assertBool s (Map.notMember land attackers) "CR 506.4: it is no longer an attacking creature"
        Spec.assertEqWith s "CR 510.1: and bob takes nothing" (S.lifeOf S.bob atEnd) (Just 20)
      _ -> Spec.assertFailure s "fixture should give alice Opalescence, Living Plane and a Piker"
  Spec.it s "CR 506.4 the twin: the same Doom Blade on a creature that is not the animator leaves combat intact" $ do
    -- The control leg, and the reason the case above is not passing for a
    -- trivial reason. The SAME card resolves, the SAME settle runs, and a
    -- creature really does die -- just not the one the Forest's creature-ness
    -- hangs on. A sampler that cleared combat whenever anything died, or
    -- whenever anything resolved, would take the Forest out here too.
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    opalescence <- S.printingOf s registry "Opalescence"
    livingPlane <- S.printingOf s registry "Living Plane"
    doomBlade <- S.printingOf s registry "Doom Blade"
    case unmakeBoard opalescence livingPlane piker forest swamp doomBlade of
      (gs, [_, plane, homebody], land) -> do
        let atEnd = runToEndOfCombat (unmake land homebody) gs
            attackers = Combat.Type.attackers (GameState.combat atEnd)
        Spec.assertBool s (not (S.onBattlefield homebody atEnd)) "the Piker that stayed home died instead"
        Spec.assertBool s (S.onBattlefield plane atEnd) "Living Plane survives"
        Spec.assertBool s (Projection.isCreatureOf land atEnd) "so the Forest is still a creature"
        Spec.assertBool s (Map.member land attackers) "and still attacking"
        Spec.assertEqWith s "so bob takes its 1" (S.lifeOf S.bob atEnd) (Just 19)
      _ -> Spec.assertFailure s "fixture should give alice Opalescence, Living Plane and a Piker"
  Spec.it s "CR 509.1h a BLOCKER that stops being a creature leaves the attacker blocked, so nothing is dealt combat damage" $ do
    -- The blocker side of the same clause, through the same performer: the
    -- Forest leaves the SET while the attacker's KEY survives, so the Piker
    -- stays blocked and CR 510.1c gives it nobody to assign damage to.
    --
    -- This is the leg where the removal is observable as DAMAGE. An attacker
    -- that stops being a creature loses its power along with its card type, so
    -- Damage.attackerAssignment's Projection.powerOf already declines to
    -- assign anything for it; a stale BLOCKER is screened only for liveness
    -- (Damage's onBattlefield filter), so the unfixed engine marks the
    -- attacker's 2 on a land that is no longer a creature at all.
    --
    -- The kill has to land after blocks are declared, so the declare attackers
    -- step is played under an answerer that never casts and only the declare
    -- blockers step onwards sees `unblock`.
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    opalescence <- S.printingOf s registry "Opalescence"
    livingPlane <- S.printingOf s registry "Living Plane"
    doomBlade <- S.printingOf s registry "Doom Blade"
    case unblockBoard opalescence livingPlane piker forest swamp doomBlade of
      (gs, [attacker], [_, plane], land) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackOnly attacker) gs
            atEnd = runToEndOfCombat (unblock land plane) atBlockers
            -- The control leg: the same board and the same block, with alice
            -- never casting. The 2/1 Piker and the 1/1 Forest then trade.
            traded = runToEndOfCombat (blockOnly land) atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the block is declared before the kill" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        Spec.assertBool s (not (S.onBattlefield plane atEnd)) "the Doom Blade really did kill Living Plane"
        Spec.assertBool s (not (Projection.isCreatureOf land atEnd)) "CR 611.3b: so the Forest stopped being a creature"
        Spec.assertBool s (S.onBattlefield land atEnd) "and is still on the battlefield"
        -- The discriminating assertion: the unfixed engine leaves the Forest
        -- in the blocker set and marks the Piker's 2 on it.
        Spec.assertEqWith s "CR 510.1c: nothing was dealt combat damage" (S.damageOf land atEnd) (Just 0)
        Spec.assertEqWith s "CR 506.4: the Forest is blocking nothing" (Combat.blockersOf attacker atEnd) Set.empty
        Spec.assertBool s (Combat.isBlocked attacker atEnd) "CR 509.1h: but the attacker remains blocked"
        Spec.assertEqWith s "so bob takes nothing either" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (not (S.onBattlefield attacker traded) && not (S.onBattlefield land traded)) "control leg: with Living Plane left alone the Piker and the Forest trade"
      _ -> Spec.assertFailure s "fixture should give alice a Piker and bob Opalescence, Living Plane and a Forest"

-- Aims every target slot at one object. Liquimetal Coating's "target permanent"
-- and Wane's "target enchantment" both draw from Pool.Permanents, so the offered
-- recipients are Recipient.ToObject and the choice has to be answered rather than
-- forced by construction (Pawl.ProjectionSpec.aimAtObject's shape).
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- The right half of Wax // Wane (CR 709.4a), which is the half the cast has to
-- name: {W} "Destroy target enchantment".
waneName :: CardName.CardName
waneName = CardName.MkCardName (Text.pack "Wane")

isWaneCast :: A.Action -> Bool
isWaneCast a = case a of
  A.Cast _ name _ -> name == waneName
  _ -> False

-- alice attacks with two Llanowar Elves -- one announced at bob's Jace Beleren
-- (CR 508.1b), one at bob himself -- while holding a Plains and a Wax // Wane;
-- her Liquimetal Coating has already been activated on Jace and her March of the
-- Machines has animated him into a 3/3 artifact creature planeswalker. Returns
-- the state, the attacker aimed at Jace, the attacker aimed at bob, Jace, and
-- March.
--
-- The Coating is activated through the WHOLE-CARD path -- Activate.activateAbility
-- and Stack.resolveTop, Pawl.ProjectionSpec's "CR 613.8b whole cards" machinery --
-- rather than through a state fixture, because the activation is reachable: the
-- Coating is seated Settled and its only cost is {T}.
--
-- ONE-POWER attackers, and the choice is forced by the control leg. There Jace is
-- dealt damage TWICE -- once as the blocker of the attacker he blocks, once as the
-- planeswalker the other attacker is aimed at -- and CR 120.3c and CR 120.3e both
-- apply to each event, because he holds both card types (Pawl.DamageSpec's
-- CreatureAndPlaneswalker group). So the marks accumulate on a 3/3: two Goblin
-- Pikers would mark 4 and CR 704.5g would destroy him, turning the control leg
-- into a leaves-the-battlefield test. Llanowar Elves' mana ability is never
-- activated -- both answerers pass, and an attacking Elf is tapped anyway.
--
-- FIVE loyalty counters, where jaceBoard places three: the three legs then read 4,
-- 3 and 5, each distinct from the others, and none reaches CR 704.5i's zero. The
-- both-at-once leg's reading is the starting value itself, since nothing is ever
-- assigned to him there -- which is what the untouched 5 has to be distinguishable
-- from, and it is: 4 and 3 are the only other readings the fixture admits.
creaturePlaneswalkerBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
creaturePlaneswalkerBoard jace elves coating march plains waxWane =
  let (gs0, mine, theirs) = S.combatBoardOf [elves, elves] [jace]
      (marchId, gs1) = S.addCreature march S.alice gs0
      (coatingId, gs2) = S.addCreature coating S.alice gs1
      (_, gs3) = S.addCreature plains S.alice gs2
      (_, gs4) = S.addHandCard waxWane S.alice gs3
   in case (mine, theirs, Face.activatedAbilities (S.combinedFace coating)) of
        ([atJace, atBob], [jaceId], coat : _) ->
          let ready = (S.addCounter CounterKind.Loyalty 5 jaceId gs4) {GameState.priority = Just S.alice}
              coated =
                S.runPure (aimAtObject jaceId) ready $ do
                  Activate.activateAbility S.alice coatingId coat
                  Stack.resolveTop
           in Just (coated, atJace, atBob, jaceId, marchId)
        _ -> Nothing

-- Declare both attackers and announce CR 508.1b's targets by attacker: `atJace` at
-- the planeswalker, `atBob` at the defending player. Casts nothing, so the declare
-- attackers step played under this leaves the Wane for a later step -- attackOnly's
-- reason above.
attackJaceAndBob :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
attackJaceAndBob atJace atBob p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (\oid -> oid == atJace || oid == atBob) ids
  Prompt.ChooseAttackTarget _ _ oid options ->
    if oid == atJace
      then attackThePlaneswalker p
      else case filter (not . isPlaneswalkerTarget) (NonEmpty.toList options) of
        target : _ -> target
        [] -> NonEmpty.head options
  Prompt.ChooseAction {} -> A.Pass
  _ -> S.aggressiveAnswer p

-- Block the attacker aimed at bob with Jace, and cast nothing: the control leg,
-- where March of the Machines is left alone and Jace stays a creature.
--
-- The PLAYER-directed attacker and not the one aimed at Jace, so the two roles CR
-- 506.4d names are held against two different attackers -- which is what makes the
-- blocking half and the attacked half separately observable.
blockWithJace :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
blockWithJace jaceId atBob p = case p of
  Prompt.DeclareBlockers {} -> Map.singleton jaceId (Set.singleton atBob)
  Prompt.ChooseAction {} -> A.Pass
  _ -> S.aggressiveAnswer p

-- blockWithJace, plus: whoever is offered the cast takes Wane and aims it at
-- March of the Machines. The Wax half is never offered -- a lone Plains cannot pay
-- its {G} -- but the filter names the half anyway, since CR 709.3 makes which half
-- is cast a choice rather than a consequence of the board.
blockAndWane :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
blockAndWane jaceId atBob marchId p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject marchId))) sets
  Prompt.ChooseAction _ _ actions -> case filter isWaneCast actions of
    a : _ -> a
    [] -> A.Pass
  _ -> blockWithJace jaceId atBob p

-- The other half of the pair blockAndWane makes: the cast that strips BOTH card
-- types at once rather than one of them.
songName :: CardName.CardName
songName = CardName.MkCardName (Text.pack "Song of the Dryads")

isSongCast :: A.Action -> Bool
isSongCast a = case a of
  A.Cast _ name _ -> name == songName
  _ -> False

-- blockWithJace, plus: whoever is offered the cast takes Song of the Dryads and
-- enchants Jace with it. The recipient is FILTERED out of what the prompt offers
-- rather than built: "enchant permanent" is a Pool.Permanents slot, so a
-- hand-built Recipient.ToCreature of the same object would be a different
-- recipient and CR 608.2b's re-read at resolution would drop it with no error --
-- where a filter that matches nothing leaves the slot empty and reddens loudly.
--
-- Wax // Wane is in the same hand and both its halves are affordable once the
-- Forests are seated, so unlike blockAndWane the name filter is load-bearing here.
blockAndSong :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
blockAndSong jaceId atBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (Recipient.ToObject jaceId ==) . snd) sets
  Prompt.ChooseAction _ _ actions -> case filter isSongCast actions of
    a : _ -> a
    [] -> A.Pass
  _ -> blockWithJace jaceId atBob p

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
--     It is the only card in data/cards/ carrying SetCardType (grep the
--     constructor name), and it sets Land, which is why the mirror leg below is
--     still waiting on card data.
--   * Vedalken Orrery ({4} Artifact, "You may cast spells as though they had
--     flash") is alice's, and is what makes that cast reachable: CR 303.1 admits
--     an enchantment only in a main phase, and the block has to be declared first
--     (CR 601.3b for the permission, CR 702.8a for the window it carries).
--     March animates it too, exactly as it animates the Coating, which changes
--     nothing here: attackJaceAndBob declares only the two Elves as attackers.
--
-- The mirror case, where the permanent stops being a PLANESWALKER and stays a
-- creature, is unproven here rather than asserted (gap #1846): it needs an effect
-- that sets the card type to Creature, and the Song -- data/cards/'s lone
-- SetCardType -- sets Land. Kenrith's Transformation prints one and is not in the
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
        let (_, gs1) = S.addCreature forest S.alice gs0
            (_, gs2) = S.addCreature forest S.alice gs1
            (_, gs3) = S.addCreature forest S.alice gs2
            (_, gs4) = S.addCreature orrery S.alice gs3
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
        -- pawl derives attacked-ness at Combat.stillAttacked rather than storing
        -- it, and CR 506.4c keeps the ATTACKER in combat, so its Combat.attackers
        -- entry still names the planeswalker -- asserting that entry is gone would
        -- fail a correct engine.
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

isPlaneswalkerTarget :: AttackTarget.AttackTarget -> Bool
isPlaneswalkerTarget target = case target of
  AttackTarget.OfPlaneswalker _ -> True
  AttackTarget.OfPlayer _ -> False
  AttackTarget.OfBattle _ -> False

-- Announce every attack at the first planeswalker offered, and answer everything
-- else aggressively. The counterpart of S.aggressiveAnswer, which takes the head
-- of the same list and so always attacks the defending player: the pair is what
-- makes CR 508.1b's announcement a REAL choice here rather than a prompt whose
-- answer the engine could have supplied itself.
--
-- Falls back to the head when no planeswalker is offered, which keeps it total
-- and makes it the same interpreter as S.aggressiveAnswer on a board without one.
attackThePlaneswalker :: Prompt.Prompt r -> r
attackThePlaneswalker p = case p of
  Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalkerTarget (NonEmpty.toList options) of
    target : _ -> target
    [] -> NonEmpty.head options
  _ -> S.aggressiveAnswer p

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
announcementsFor gs = State.execState (Engine.runGame announcementLog gs (Combat.declareAttackers S.alice)) []

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
-- "That planeswalker's controller" and "the defending player" are one seat here.
-- That is not the two-player collapse that hides a bug: CR 702.19c names the
-- planeswalker's controller precisely because the attack was declared against
-- that player's planeswalker (CR 508.1b), so the two are the same player on every
-- board pawl can build.
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
      let (confiscateId, gs1) = S.addCreature confiscate S.bob gs0
          (boltId, gs2) = S.addHandCard bolt S.alice gs1
          gs3 = S.addCounter CounterKind.Loyalty 3 jaceId (S.attachTo confiscateId (Recipient.ToObject jaceId) gs2)
          board =
            gs3
              { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
                GameState.priority = Just S.alice,
                -- CR 506.2a / CR 507.1's choice, stated rather than run: bob
                -- defends, which is what puts carol's Jace among the attackable
                -- planeswalkers (CR 306.6 reads the CONTROLLER).
                GameState.combat = (GameState.combat gs3) {Combat.Type.defender = Just S.bob}
              }
          declared = snd (Engine.runGamePure attackThePlaneswalker board (Combat.declareAttackers S.alice))
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

-- A declare-attackers board with a real taxing permanent -- Ghostly Prison, or
-- Sphere of Safety -- under `who`'s control and `lands` untapped Forests under
-- alice's. `cursing`'s twin on the cost side of CR 508.1d: alice is active with
-- one creature per printing in `mine`, and the taxing permanent's controller is
-- the only thing that decides whether her attacks are taxed at all.
--
-- The Forests are real Forests, so CR 305.6's intrinsic ability is what pays. A
-- fixture that seeded a mana pool instead would prove nothing about CR 508.1i's
-- window -- the whole of what that rule gives the player is the chance to make
-- the mana -- and would not survive the step boundary in the gameplay-level case
-- (CR 500.5). Their ids come back so a test can read the payment off the board.
imprisoning :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> [Printing.Printing] -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
imprisoning prison forest who mine lands =
  let (gs, ours, _) = S.combatBoardOf mine []
      (forests, board) = addForests forest lands (snd (S.addCreature prison who gs))
   in (board, ours, forests)

-- `n` untapped Forests under alice's control, ids first. Not S.landsInPlay, which
-- builds a whole fresh game: these go onto a board that already exists.
addForests :: Printing.Printing -> Int -> GameState.GameState -> ([ObjectId.ObjectId], GameState.GameState)
addForests forest n gs =
  let add (ids, g) _ = let (oid, g1) = S.addCreature forest S.alice g in (ids <> [oid], g1)
   in List.foldl' add ([], gs) [1 .. n]

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

-- CR 508.1d's cost clause and CR 508.1h-508.1j, proved by Ghostly Prison
-- ("Creatures can't attack you unless their controller pays {2} for each creature
-- they control that's attacking you") -- the pool's first cost to attack, and the
-- first board on which a legal declaration can leave the active player unable to
-- comply with CR 508.1 -- and by Sphere of Safety, which is the same sentence
-- widened to the planeswalkers its controller controls and with a {X} that counts
-- the board where the Prison has a constant.
--
-- Every MANA case here is arithmetic rather than a threshold: a Forest makes one
-- mana, so "how many Forests were tapped" reads the total cost off the board
-- directly. The non-mana cases at the end of the group read the same lands the
-- other way -- Exalted Dragon sacrifices one rather than tapping it (CR 508.1h),
-- so a land that is GONE is that toll's trace and a land that is TAPPED is the
-- other's.
attackCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attackCostSpec s registry = Spec.describe s "AttackCosts" $ do
  Spec.it s "CR 508.1h/508.1j attacking under a Ghostly Prison costs {2}, and the mana is paid" $ do
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker] 2
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the Piker really was declared" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allTapped forests after) "CR 508.1j: both Forests paid for it"
  Spec.it s "CR 508.1 the same board WITHOUT the Prison pays nothing" $ do
    -- The control for the test above, and the reason it is not vacuous: attacking
    -- is free by default (CR 508.1f: "tapping a creature when it's declared as an
    -- attacker isn't a cost"), so the Prison is what tapped the Forests.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] []
        (forests, board) = addForests forest 2 gs
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the Piker still attacks" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allUntapped forests after) "and no Forest was tapped"
  Spec.it s "CR 508.1h the total scales with the declaration: two attackers owe {4}" $ do
    -- CR 508.1h totals the WHOLE declaration, which is what makes Ghostly Prison's
    -- "for each creature they control that's attacking you" a multiplication.
    -- Ghostly Prison's own Two-Headed Giant ruling states the same arithmetic from
    -- the other end: "you still only have to pay once per creature."
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker, piker] 4
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "both Pikers were declared" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allTapped forests after) "CR 508.1h: all four Forests went"
  Spec.it s "CR 508.1j partial payments are not allowed: three Forests do not buy two attacks" $ do
    -- The same board one Forest short. CR 508.1's preamble -- "the declaration is
    -- illegal; the game returns to the moment before the declaration" -- so it is
    -- not that one Piker attacks and the other does not: NEITHER does, and the
    -- three Forests that could have paid for one are untapped again.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker, piker] 3
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
    Spec.assertEqWith s "and nothing is attacking" (Combat.Type.attackers (GameState.combat after)) Map.empty
    Spec.assertBool s (allUntapped forests after) "the Forests are untapped again"
    Spec.assertBool s (allUntapped mine after) "CR 508.1f's tapping was undone too"
  Spec.it s "CR 508.1 the rewound declaration is made again: two Pikers under a Ghostly Prison become one" $ do
    -- The case directly above's board, answered by a player rather than by a
    -- machine that repeats itself. CR 508.1's preamble returns the game to the
    -- moment before the declaration, and the declaration is still owed -- so the
    -- active player declares again, normally the smaller attack they can afford.
    -- Where the case above ends with nothing attacking, this one ends with one
    -- Piker attacking and one Forest to spare.
    --
    -- THREE Forests is load-bearing: at two the leftover Forest cannot be read,
    -- and at four the first declaration is affordable and the two readings of the
    -- preamble agree.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker, piker] 3
        ((_, after), asked) = State.runState (Engine.runGame retryAttackAnswer gs (Combat.declareAttackers S.alice)) 0
        declared = S.attackerDeclarationsOf after
    Spec.assertEqWith s "CR 508.1: exactly one Piker attacks, where the rewind alone left none" (length declared) 1
    Spec.assertBool s (all (\oid -> List.elem oid mine) declared) "and it is one of alice's two"
    Spec.assertEqWith s "CR 508.1j: the second declaration owes {2}, so one Forest is left" (length (filter (\oid -> tapStateOf oid after == Just TapState.Untapped) forests)) 1
    Spec.assertEqWith s "CR 508.1's preamble asked for a fresh declaration" asked 2
  Spec.it s "CR 109.5 a Ghostly Prison its own controller is attacking WITH taxes nothing" $ do
    -- The direction, which is the whole of the "you": alice controls the Prison
    -- and attacks bob, so nothing is attacking alice and no cost is owed. An
    -- engine that taxed every attack while any Prison was on the battlefield
    -- would tap her Forests here.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.alice [piker] 2
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the Piker attacks" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allUntapped forests after) "and paid nothing"
  Spec.it s "Ghostly Prison's ruling: a creature that can't attack you can still attack a planeswalker you control" $ do
    -- "Unless some effect explicitly says otherwise, a creature that can't attack
    -- you can still attack a planeswalker you control" (Ghostly Prison, 2014-02-01).
    --
    -- ONE board, two interpreters: attacking Jace is free and attacking bob costs
    -- {2}. An engine that read the DEFENDING PLAYER rather than what each creature
    -- was announced as attacking could not produce both lines.
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker]
        withPrison = snd (S.addCreature prison S.bob gs)
        (forests, board) = addForests forest 2 withPrison
        atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.alice)
        atBob = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith
      s
      "attacking Jace: the record names the planeswalker"
      (Map.elems (Combat.Type.attackers (GameState.combat atJace)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (allUntapped forests atJace) "attacking Jace: nothing was paid"
    Spec.assertEqWith s "attacking bob: the Piker was declared" (S.attackerDeclarationsOf atBob) mine
    Spec.assertBool s (allTapped forests atBob) "attacking bob: the {2} was paid"
  Spec.it s "CR 306.6 Sphere of Safety taxes the attack on a planeswalker that Ghostly Prison lets through" $ do
    -- The contrast with the case directly above, on the same board shape: Sphere
    -- of Safety prints "you OR PLANESWALKERS YOU CONTROL", which is the "unless
    -- some effect explicitly says otherwise" that Ghostly Prison's own ruling
    -- leaves room for. bob controls one enchantment (the Sphere), so X = 1 and
    -- attacking Jace costs {1}.
    --
    -- The discriminating half is the POSITIVE one -- the Forest went -- because a
    -- creature refused an attack for an unrelated reason looks exactly like one
    -- refused by this gate. The record naming the planeswalker is asserted
    -- alongside it so that a Forest tapped for an attack on bob cannot pass.
    sphere <- S.printingOf s registry "Sphere of Safety"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, _, jaceId) = jaceBoard jace [piker]
        withSphere = snd (S.addCreature sphere S.bob gs)
        (forests, board) = addForests forest 1 withSphere
        atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.alice)
    Spec.assertEqWith
      s
      "the record names the planeswalker"
      (Map.elems (Combat.Type.attackers (GameState.combat atJace)))
      [AttackTarget.OfPlaneswalker jaceId]
    Spec.assertBool s (allTapped forests atJace) "and the {X} was paid for it"
  Spec.it s "CR 508.1h Sphere of Safety's share is the enchantment count, not a constant" $ do
    -- ONE card, two boards differing by exactly one inert enchantment. Megrim is
    -- a bare {2}{B} Enchantment whose only ability triggers on a discard, so it
    -- changes the count and nothing else about the combat.
    --
    -- No constant can produce both lines: with the Sphere alone X = 1 and one
    -- Forest is the whole toll, and with Megrim beside it X = 2 and the same
    -- single Piker owes two. An engine that had kept a literal share would fail
    -- one line or the other whatever literal it picked.
    sphere <- S.printingOf s registry "Sphere of Safety"
    megrim <- S.printingOf s registry "Megrim"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (one, mine1, f1) = imprisoning sphere forest S.bob [piker] 1
        after1 = S.runPure S.aggressiveAnswer one (Combat.declareAttackers S.alice)
        (two0, mine2, f2) = imprisoning sphere forest S.bob [piker] 2
        two = snd (S.addCreature megrim S.bob two0)
        after2 = S.runPure S.aggressiveAnswer two (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "X = 1: the Piker was declared" (S.attackerDeclarationsOf after1) mine1
    Spec.assertBool s (allTapped f1 after1) "X = 1: the one Forest paid"
    Spec.assertEqWith s "X = 2: the same Piker was declared" (S.attackerDeclarationsOf after2) mine2
    Spec.assertBool s (allTapped f2 after2) "X = 2: both Forests paid"
  Spec.it s "CR 508.1d a Curse of the Nightly Hunt does not force an attack a Ghostly Prison taxes" $ do
    -- THE COST CLAUSE: "if a creature can't attack unless a player pays a cost,
    -- that player is not required to pay that cost, even if attacking with that
    -- creature would increase the number of requirements being obeyed."
    --
    -- Both worlds on one line each. Without the Prison the Curse makes declining
    -- illegal (that is AttackRequirements' first case); with it, declining is
    -- legal again, and the requirement has not gone anywhere -- attacking with the
    -- Piker is still a legal declaration, it is just no longer a forced one.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    piker <- S.printingOf s registry "Goblin Piker"
    let (cursed, mine, _) = cursing curse S.alice [piker] []
        taxed = snd (S.addCreature prison S.bob cursed)
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] cursed)) "without the Prison, declining is illegal"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] taxed) "CR 508.1d: with the Prison, declining is legal"
    case mine of
      a : _ -> Spec.assertBool s (Combat.legalAttackDeclaration S.alice [a] taxed) "and attacking anyway is still legal"
      _ -> Spec.assertFailure s "fixture should have a creature"
  Spec.it s "CR 508.1d the cost clause excuses a requirement only when EVERY attack costs" $ do
    -- ANY free attack, not ALL attacks free. A creature that could attack Jace for
    -- nothing is not one that "can't attack unless a player pays a cost", so the
    -- Curse still forces it onto the battlefield's other side -- against Jace,
    -- since the free announcement is the only one the cost clause leaves in
    -- Combat.attackCeilingGiven's reach.
    --
    -- The two boards differ ONLY by the planeswalker, which is what makes this the
    -- test for that clause being applied per (creature, target) PAIR: with no
    -- planeswalker every announcement is taxed and the answers coincide.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (withJace, _, _) = jaceBoard jace [piker]
        (plain, _, _) = S.combatBoardOf [piker] []
        taxedWithJace = snd (S.addCreature prison S.bob (cursingBoard curse S.alice withJace))
        taxedPlain = snd (S.addCreature prison S.bob (cursingBoard curse S.alice plain))
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] taxedWithJace)) "the free attack on Jace keeps the requirement"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] taxedPlain) "with no planeswalker every attack costs, so declining is legal"
  Spec.it s "CR 508.1d whole cards: the Curse forces the attack, and the Prison unforces it" $ do
    -- The gameplay-level case, run through Engine.runStep -- the priority loop and
    -- the CR 703.4i turn-based action, not a direct call -- with an interpreter
    -- that declines to attack. It has the mana to pay twice over, so the Forests
    -- are the discriminator rather than the affordability: WITH the Prison the
    -- engine must not reach for them.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (cursed, _, _) = cursing curse S.alice [piker] []
        (forests, forced) = addForests forest 2 cursed
        taxed = snd (S.addCreature prison S.bob forced)
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
        forcedRun = S.runCombat declining forced
        taxedRun = S.runCombat declining taxed
    Spec.assertEqWith s "without the Prison the Curse forces the attack and bob takes two" (S.lifeOf S.bob forcedRun) (Just 18)
    Spec.assertBool s (allUntapped forests forcedRun) "and it cost nothing"
    Spec.assertEqWith s "CR 508.1d: with the Prison, declining stands and bob takes nothing" (S.lifeOf S.bob taxedRun) (Just 20)
    Spec.assertEqWith s "nothing attacked" (S.attackerDeclarationsOf taxedRun) []
    Spec.assertBool s (allUntapped forests taxedRun) "and no mana was spent"

  Spec.it s "CR 508.1c an Oppressive Rays taxes the attack whoever it is aimed at" $ do
    -- The third scope arm (Pawl.Types.AttackCostScope). Oppressive Rays enchants
    -- the ATTACKING creature rather than protecting a player, so its {3} is owed
    -- on an attack against bob and on an attack against bob's Jace Beleren alike
    -- -- where Ghostly Prison's own ruling exempts the second, which is the pair
    -- of lines above.
    --
    -- The Aura goes under BOB, whose creature it is not on: the taxed player is
    -- the attacker's controller under this arm, never the source's.
    rays <- S.printingOf s registry "Oppressive Rays"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    jace <- S.printingOf s registry "Jace Beleren"
    let (gs, mine, jaceId) = jaceBoard jace [piker]
    case mine of
      [attacker] -> do
        let (aura, withAura) = S.addCreature rays S.bob gs
            (forests, board) = addForests forest 3 (S.attach aura attacker withAura)
            atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.alice)
            atBob = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
        Spec.assertEqWith
          s
          "attacking Jace: the record names the planeswalker"
          (Map.elems (Combat.Type.attackers (GameState.combat atJace)))
          [AttackTarget.OfPlaneswalker jaceId]
        Spec.assertBool s (allTapped forests atJace) "attacking Jace: the {3} was paid all the same"
        Spec.assertEqWith s "attacking bob: the Piker was declared" (S.attackerDeclarationsOf atBob) mine
        Spec.assertBool s (allTapped forests atBob) "attacking bob: the {3} was paid"
      _ -> Spec.assertFailure s "fixture should have one attacker"

  Spec.it s "CR 508.1h a cost to attack that is not mana: an Exalted Dragon sacrifices a land" $ do
    -- CR 508.1h's list past its first item -- "costs may include paying mana,
    -- tapping permanents, sacrificing permanents, discarding cards, and so on" --
    -- proved by Exalted Dragon ("This creature can't attack unless you sacrifice a
    -- land"), the pool's first cost to attack that is not mana.
    --
    -- The lands are Forests and are UNTAPPED throughout, which is what separates
    -- the two kinds of toll on one board: an engine that charged mana here would
    -- tap one, and an engine that charged nothing would leave both standing.
    dragon <- S.printingOf s registry "Exalted Dragon"
    forest <- S.printingOf s registry "Forest"
    let (gs, mine, _) = S.combatBoardOf [dragon] []
        (forests, board) = addForests forest 2 gs
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "CR 508.1j: one of the two lands was sacrificed" (stillThere forests after) 1
    Spec.assertEqWith s "and the Dragon really was declared" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allUntapped (filter (\oid -> Set.member oid (GameState.battlefield after)) forests) after) "the surviving land was not tapped: this toll is not mana"
  Spec.it s "CR 508.1 the same board with an untaxed attacker sacrifices nothing" $ do
    -- The control for the case above, differing in the attacking creature alone:
    -- Exalted Dragon's subject is ITSELF, so a Goblin Piker on the same two-Forest
    -- board attacks for free.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] []
        (forests, board) = addForests forest 2 gs
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "both lands are still there" (stillThere forests after) 2
    Spec.assertEqWith s "and the Piker attacked all the same" (S.attackerDeclarationsOf after) mine
  Spec.it s "CR 508.1j partial payments are not allowed: two Dragons and one land sacrifice nothing" $ do
    -- CR 508.1h totals the whole declaration, so two Dragons owe two lands. One
    -- land cannot pay for both, and CR 508.1j's "partial payments are not allowed"
    -- is what makes the answer NEITHER attacks rather than one of them does: the
    -- land that was already sacrificed while the toll was being paid comes back.
    dragon <- S.printingOf s registry "Exalted Dragon"
    forest <- S.printingOf s registry "Forest"
    let (gs, mine, _) = S.combatBoardOf [dragon, dragon] []
        (forests, board) = addForests forest 1 gs
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.alice)
    Spec.assertEqWith s "the land is still on the battlefield" (stillThere forests after) 1
    Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
    Spec.assertEqWith s "and nothing is attacking" (Combat.Type.attackers (GameState.combat after)) Map.empty
    Spec.assertBool s (allUntapped mine after) "CR 508.1f's tapping was undone too"

-- Declares every candidate the FIRST time CR 508.1a is asked and the first
-- candidate alone the second, counting the asks in its state. Stateful because a
-- pure `Prompt r -> r` cannot tell the two asks apart: it repeats the answer CR
-- 508.1's preamble just rewound, which is the path the case beside this one's
-- covers. Pinned by position rather than by affordability, so a mutation that
-- breaks the retry cannot be repaired by the answerer looking for a legal answer.
retryAttackAnswer :: Prompt.Prompt r -> State.State Natural r
retryAttackAnswer p = case p of
  Prompt.DeclareAttackers _ _ candidates -> do
    asked <- State.get
    State.put (asked + 1)
    pure (if asked == 0 then candidates else take 1 candidates)
  _ -> pure (S.identityAnswer p)

-- retryAttackAnswer's twin for CR 509.1a: every candidate blocks the first
-- attacker on the first ask, and only `keep` does on the second.
retryBlockAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Natural r
retryBlockAnswer keep p = case p of
  Prompt.DeclareBlockers _ _ mine attackers -> do
    asked <- State.get
    State.put (asked + 1)
    pure $ case attackers of
      [] -> Map.empty
      a : _ ->
        let blocking = if asked == 0 then mine else filter (\oid -> oid == keep) mine
         in Map.fromList (fmap (\b -> (b, Set.singleton a)) blocking)
  _ -> pure (S.identityAnswer p)

-- `n` untapped Forests under `who`'s control, ids first. addForests with the
-- payer as an argument: CR 509.1f's payer is the DEFENDING player, where CR
-- 508.1j's is the active one, so a cost to block is paid out of bob's lands.
addForestsFor :: PlayerId.PlayerId -> Printing.Printing -> Int -> GameState.GameState -> ([ObjectId.ObjectId], GameState.GameState)
addForestsFor who forest n gs =
  let add (ids, g) _ = let (oid, g1) = S.addCreature forest who g in (ids <> [oid], g1)
   in List.foldl' add ([], gs) [1 .. n]

-- An Oppressive Rays under ALICE's control attached to `victim`, which is bob's
-- creature. The Aura's controller is deliberately not the payer: CR 509.1a makes
-- every chosen blocker one the defending player controls, so "its controller
-- pays" resolves to bob however the Aura got there.
raying :: Printing.Printing -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
raying rays victim gs =
  let (aura, withAura) = S.addCreature rays S.alice gs
   in S.attach aura victim withAura

-- Who is blocking `attacker`, as CR 509.1g's record states it.
blockersOf :: ObjectId.ObjectId -> GameState.GameState -> Set.Set ObjectId.ObjectId
blockersOf attacker gs = Map.findWithDefault Set.empty attacker (Combat.Type.blockers (GameState.combat gs))

-- CR 509.1c's cost clause and CR 509.1d-509.1f, proved by Oppressive Rays
-- ("Enchanted creature can't attack or block unless its controller pays {3}") --
-- the pool's first cost to block, and the first board on which a legal
-- declaration can leave the defending player unable to comply with CR 509.1.
--
-- attackCostSpec's twin, and every mana case here reads the toll the same way: a
-- Forest makes one mana, so "how many Forests were tapped" is the total cost read
-- off the board. The non-mana cases at the end of the group read a land that is
-- GONE instead, CR 509.1d's list being as wide as CR 508.1h's.
--
-- Not implemented: Oppressive Rays' third line, "activated abilities of enchanted
-- creature cost {3} more to activate", which the card's JSON omits -- nothing
-- raises an activation cost (#1242). pawl's Oppressive Rays is WEAKER than
-- printed there, and weaker against the Aura's own controller, which is why the
-- card is still the right producer for the two combat lines.
blockCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blockCostSpec s registry = Spec.describe s "BlockCosts" $ do
  Spec.it s "CR 509.1d/509.1f blocking under an Oppressive Rays costs {3}, and the mana is paid" $ do
    rays <- S.printingOf s registry "Oppressive Rays"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      ([attacker], [blocker]) -> do
        let (forests, board) = addForestsFor S.bob forest 3 (raying rays blocker gs)
            after = S.runPure S.aggressiveAnswer board Combat.declareBlockers
        Spec.assertEqWith s "the block really was declared" (blockersOf attacker after) (Set.singleton blocker)
        Spec.assertBool s (allTapped forests after) "CR 509.1f: all three Forests paid for it"
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  Spec.it s "CR 509.1 the same board WITHOUT the Aura pays nothing" $ do
    -- The control for the case above, and the reason it is not vacuous: blocking
    -- is free by default, so the Aura is what tapped the Forests.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      ([attacker], [blocker]) -> do
        let (forests, board) = addForestsFor S.bob forest 3 gs
            after = S.runPure S.aggressiveAnswer board Combat.declareBlockers
        Spec.assertEqWith s "the same block was declared" (blockersOf attacker after) (Set.singleton blocker)
        Spec.assertBool s (allUntapped forests after) "and no Forest was tapped"
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  Spec.it s "CR 509.1f partial payments are not allowed: two Forests do not buy the block" $ do
    -- The same board one Forest short. CR 509.1's preamble -- "the declaration is
    -- illegal; the game returns to the moment before the declaration" -- so the
    -- creature does not block at all, and the two Forests that could have paid
    -- part of the toll are untapped.
    rays <- S.printingOf s registry "Oppressive Rays"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      ([attacker], [blocker]) -> do
        let (forests, board) = addForestsFor S.bob forest 2 (raying rays blocker gs)
            after = S.runPure S.aggressiveAnswer board Combat.declareBlockers
        Spec.assertEqWith s "nothing is blocking the attacker" (blockersOf attacker after) Set.empty
        Spec.assertBool s (allUntapped forests after) "and the Forests are untapped"
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  Spec.it s "CR 509.1 the rewound declaration is made again: the taxed blocker is dropped and the free one blocks" $ do
    -- attackCostSpec's retry case on the blocking side, CR 509.1's preamble being
    -- word for word CR 508.1's. Two blockers, exactly one of them enchanted, and
    -- two Forests -- so the pair costs {3} and cannot be paid, while the untaxed
    -- blocker alone costs nothing.
    --
    -- TWO blockers, one untaxed, is load-bearing: with a single taxed blocker
    -- there is no smaller legal declaration and both readings of the preamble
    -- agree on Set.empty.
    rays <- S.printingOf s registry "Oppressive Rays"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker, piker]
    case (mine, theirs) of
      ([attacker], [taxed, free]) -> do
        let (forests, board) = addForestsFor S.bob forest 2 (raying rays taxed gs)
            ((_, after), asked) = State.runState (Engine.runGame (retryBlockAnswer free) board Combat.declareBlockers) 0
        Spec.assertEqWith s "CR 509.1: the untaxed blocker blocks, where the rewind alone left the attacker unblocked" (blockersOf attacker after) (Set.singleton free)
        Spec.assertBool s (allUntapped forests after) "CR 509.1f: the second declaration owes nothing, so no Forest was tapped"
        Spec.assertEqWith s "CR 509.1's preamble asked for a fresh declaration" asked 2
      _ -> Spec.assertFailure s "fixture should have one attacker and two blockers"
  Spec.it s "CR 509.1d the total is per CREATURE, not per pair: a Palace Guard blocking two owes {3} once" $ do
    -- CR 509.1d totals the cost over the CHOSEN CREATURES, and Palace Guard blocks
    -- any number of attackers -- so one taxed creature blocking two attackers owes
    -- its share once. Three Forests are exactly {3}: an engine that charged per
    -- PAIR would owe {6}, fail CR 509.1f and block nothing, which is what the
    -- first assertion reads.
    rays <- S.printingOf s registry "Oppressive Rays"
    forest <- S.printingOf s registry "Forest"
    palaceGuard <- S.printingOf s registry "Palace Guard"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker, piker] [palaceGuard]
    case (mine, theirs) of
      ([first, second], [guard]) -> do
        let (forests, board) = addForestsFor S.bob forest 3 (raying rays guard gs)
            blockBoth :: Prompt.Prompt r -> r
            blockBoth p = case p of
              Prompt.DeclareBlockers _ _ blockers attackers -> Map.fromList (fmap (\b -> (b, Set.fromList attackers)) blockers)
              _ -> S.aggressiveAnswer p
            after = S.runPure blockBoth board Combat.declareBlockers
        Spec.assertEqWith
          s
          "the Guard is blocking both attackers"
          (blockersOf first after, blockersOf second after)
          (Set.singleton guard, Set.singleton guard)
        Spec.assertBool s (allTapped forests after) "and the {3} was paid once"
      _ -> Spec.assertFailure s "fixture should have two attackers and a Palace Guard"
  Spec.it s "CR 509.1c a Prized Unicorn does not force a block an Oppressive Rays taxes" $ do
    -- THE COST CLAUSE: "if a creature can't block unless a player pays a cost,
    -- that player is not required to pay that cost, even if blocking with that
    -- creature would increase the number of requirements being obeyed."
    --
    -- Both worlds on one line each. Without the Aura the Unicorn makes declining
    -- illegal (that is BlockRequirements' own case); with it, declining is legal
    -- again, and the requirement has not gone anywhere -- blocking with the taxed
    -- creature is still a legal declaration, it is just no longer a forced one.
    rays <- S.printingOf s registry "Oppressive Rays"
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [prizedUnicorn] [piker]
    case (mine, theirs) of
      ([unicorn], [blocker]) -> do
        let taxed = raying rays blocker gs
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "without the Aura, declining is illegal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty taxed) "CR 509.1c: with the Aura, declining is legal"
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton unicorn)) taxed) "and blocking anyway is still legal"
      _ -> Spec.assertFailure s "fixture should have a Unicorn and a blocker"
  Spec.it s "CR 509.1c whole cards: the Unicorn forces the block, and the Aura unforces it" $ do
    -- The gameplay-level case, run through Engine.runStep -- the priority loop and
    -- the CR 703.4j turn-based action, not a direct call -- with an interpreter
    -- that declines to block. bob has the mana to pay twice over, so the Forests
    -- are the discriminator rather than the affordability: WITH the Aura the
    -- engine must not reach for them.
    rays <- S.printingOf s registry "Oppressive Rays"
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [prizedUnicorn] [piker]
    case (mine, theirs) of
      ([unicorn], [blocker]) -> do
        let (forests, forced) = addForestsFor S.bob forest 6 gs
            taxed = raying rays blocker forced
            declining :: Prompt.Prompt r -> r
            declining p = case p of
              Prompt.DeclareBlockers {} -> Map.empty
              _ -> S.aggressiveAnswer p
            forcedRun = S.runCombat declining forced
            taxedRun = S.runCombat declining taxed
        Spec.assertEqWith s "without the Aura the Unicorn is blocked and bob takes nothing" (S.lifeOf S.bob forcedRun) (Just 20)
        Spec.assertBool s (allUntapped forests forcedRun) "and the forced block cost nothing"
        Spec.assertEqWith s "CR 509.1c: with the Aura, declining stands and bob takes two" (S.lifeOf S.bob taxedRun) (Just 18)
        Spec.assertEqWith s "nothing blocked" (blockersOf unicorn taxedRun) Set.empty
        Spec.assertBool s (allUntapped forests taxedRun) "and no mana was spent"
      _ -> Spec.assertFailure s "fixture should have a Unicorn and a blocker"

  Spec.it s "CR 509.1d a cost to block that is not mana sacrifices a land" $ do
    -- CR 509.1d's list past its first item, attackCostSpec's Exalted Dragon case
    -- on the blocking side. SYNTHETIC, and legitimately so: Hollow Warrior is the
    -- printing ("can't attack or block unless you tap an untapped creature you
    -- control not declared as an attacking or blocking creature this combat"), and
    -- its criterion asks which creatures were DECLARED this combat, which no
    -- Pawl.Types.Filter says -- Not IsAttacking admits a creature removed from
    -- combat, and a fellow chosen blocker is not blocking yet when CR 509.1f pays
    -- (#2024). Transcribing it that way would make pawl's card WEAKER than
    -- printed, so the toll is proved on a card that owes nothing to that criterion.
    --
    -- bob pays, and bob's lands are the ones that go: CR 509.1a makes every chosen
    -- blocker one the defending player controls.
    tithe <- S.printingOf s registry "Synthetic Blocking Tithe"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker]
    case (mine, theirs) of
      ([attacker], [blocker]) -> do
        let (forests, board) = addForestsFor S.bob forest 2 (raying tithe blocker gs)
            after = S.runPure S.aggressiveAnswer board Combat.declareBlockers
        Spec.assertEqWith s "CR 509.1f: one of the two lands was sacrificed" (stillThere forests after) 1
        Spec.assertEqWith s "and the block really was declared" (blockersOf attacker after) (Set.singleton blocker)
        Spec.assertBool s (allUntapped (filter (\oid -> Set.member oid (GameState.battlefield after)) forests) after) "the surviving land was not tapped: this toll is not mana"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1f partial payments are not allowed: two taxed blockers and one land sacrifice nothing" $ do
    -- CR 509.1d totals over the chosen creatures, so two taxed blockers owe two
    -- lands and one land cannot pay. The land sacrificed while the toll was being
    -- paid comes back, which on THIS side is the payment's own doing: CR 509.1's
    -- preamble leaves declareBlockers nothing of its own to rewind, the record not
    -- being written until the toll is paid.
    tithe <- S.printingOf s registry "Synthetic Blocking Tithe"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker, piker]
    case (mine, theirs) of
      ([attacker], [one, two]) -> do
        let (forests, board) = addForestsFor S.bob forest 1 (raying tithe two (raying tithe one gs))
            after = S.runPure S.aggressiveAnswer board Combat.declareBlockers
        Spec.assertEqWith s "the land is still on the battlefield" (stillThere forests after) 1
        Spec.assertEqWith s "and nothing blocked" (blockersOf attacker after) Set.empty
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"

-- CR 305.7 as the FIVE readers in this module read it, which is one shared gate:
-- Pawl.Engine.Projection.liveAfterLayers. Ashaya, Soul of the Wild makes its
-- controller's nontoken creatures Forest LANDS at layer 4, and Blood Moon then
-- depends on that (CR 613.8a) and SETS every nonbasic land's subtype to Mountain,
-- which by CR 305.7 takes the animated permanent's rules text -- its combat
-- sentence included.
--
-- What each case here discriminates is the gate's READING, not the strip: the gate
-- has to judge Blood Moon's "nonbasic land" against the FINISHED projection, which
-- CR 613.10 and CR 613.11 permit because every reader in this module runs after
-- the layers. Judged against BASE characteristics the animated creature is no land
-- at all and keeps its sentence, and that is the only difference the boards below
-- turn on.
--
-- FOUR boards a case, differing in nothing but which of the two permanents is
-- present, and each of the three controls is load-bearing. Ashaya alone ADDS a
-- land type, and CR 305.7's last sentence keeps the rules text of a land that
-- gains types in addition to its own; Blood Moon alone names no creature. Only
-- the conjunction strips.
--
-- ONE CASE PER READER, named in the case's own comment, because each reader keeps
-- its own copy of the `null setEffs || ...` guard: one case for the gate would
-- leave a change to any single copy regressing silently.
--
-- Pawl.Engine.BlockCost is the reader with no case, and its own comment says why:
-- discriminating it wants a cost to block printed on a nontoken creature, since
-- Ashaya animates creatures and Oppressive Rays is an Aura (#1999).
-- Pawl.Engine.PlayerEffect's share is pinned in Pawl.PlayerEffectSpec and
-- Pawl.Engine.SacrificeRestriction's in Pawl.SacrificeRestrictionSpec.
--
-- A BOARD THAT CANNOT DISCRIMINATE, recorded because it looks like the obvious
-- one: Glacial Crasher ("this creature can't attack unless you control a
-- Mountain") is no witness for the restriction reader, since Blood Moon makes
-- every nonbasic land a Mountain and the gate is satisfied whether or not the
-- Crasher's own sentence survived.
landSubtypeStripSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
landSubtypeStripSpec s registry = Spec.describe s "LandSubtypeStrip" $ do
  Spec.it s "CR 305.7 an animated Palace Guard set to Mountain blocks only one attacker" $ do
    -- Pawl.Engine.BlockPermission.additionalBlocks. Palace Guard says nothing but
    -- "this creature can block any number of creatures", so THREE attackers are
    -- what separate its unbounded arity from the one CR 509.1a gives every
    -- creature. Ashaya goes under BOB, whose blocker it has to animate; Blood Moon
    -- goes there too and its controller never matters, since its sentence names
    -- lands globally rather than "lands you control".
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    palaceGuard <- S.printingOf s registry "Palace Guard"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = attacking [piker, piker, piker] [palaceGuard]
        with extras = withPermanents S.bob extras base
    case (mine, theirs) of
      ([first, second, third], [guard]) -> do
        let three = Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.fromList [first, second, third]))
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "all three until Ashaya and Blood Moon are both on the battlefield, and then not three"
          (three base, three (with [ashaya]), three (with [bloodMoon]), three stripped)
          (True, True, True, False)
        -- The anti-vacuity leg: the Guard lost its arity, not its ability to
        -- block. Without this, a strip that made it no legal blocker at all --
        -- or a fixture that stopped offering it -- would pass the line above.
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton guard (Set.singleton first)) stripped) "and one attacker is still legal"
      _ -> Spec.assertFailure s "fixture should have three attackers and a Palace Guard"
  Spec.it s "CR 305.7 an animated Prized Unicorn set to Mountain no longer forces a block" $ do
    -- Pawl.Engine.BlockRequirement.instances. blockRequirementSpec's Humility case
    -- with CR 613.1f's layer-6 removal swapped for CR 305.7's layer-4 route:
    -- Humility reaches the Unicorn's own ability directly, where this reaches it
    -- only through the animation, which is exactly the difference between reading
    -- base characteristics and reading the projection.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    prizedUnicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = attacking [prizedUnicorn] [piker]
        with extras = withPermanents S.alice extras base
    case (mine, theirs) of
      ([unicorn], [blocker]) -> do
        let declining = Combat.legalBlockDeclaration S.bob Map.empty
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "declining stays illegal until Ashaya and Blood Moon are both on the battlefield"
          (declining base, declining (with [ashaya]), declining (with [bloodMoon]), declining stripped)
          (False, False, False, True)
        -- The same anti-vacuity leg blockRequirementSpec's Humility case carries:
        -- the combat is still live under the strip, so declining became legal
        -- because the requirement went away rather than because there was nothing
        -- to block.
        Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blocker (Set.singleton unicorn)) stripped) "and blocking the Unicorn is still legal"
      _ -> Spec.assertFailure s "fixture should have a Unicorn and a blocker"
  Spec.it s "CR 305.7 an animated Bonded Construct set to Mountain may attack alone" $ do
    -- Pawl.Engine.CombatRestriction.inForce, whose `keepsAbilities` holds the gate
    -- for the rows `restricted` then selects from. The Construct is an ARTIFACT
    -- creature and Ashaya animates creatures, so the animation reaches it; a Silent
    -- Arbiter would do as well for the strip and worse for the reading, its
    -- sentence naming no creature to be judged.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    let (base, mine, _) = S.combatBoardOf [bondedConstruct] []
        with extras = withPermanents S.alice extras base
    case mine of
      [construct] -> do
        let alone = Combat.legalAttackDeclaration S.alice [construct]
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "attacking alone stays illegal until Ashaya and Blood Moon are both on the battlefield"
          (alone base, alone (with [ashaya]), alone (with [bloodMoon]), alone stripped)
          (False, False, False, True)
        -- Anchors, so a failure above says which half moved. Both readings of the
        -- gate agree about these -- they are the layer fold's answer, not the
        -- gate's -- so they cannot make the line above pass.
        Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf construct stripped)) "the Construct is a Mountain"
        Spec.assertBool s (Projection.isCreatureOf construct stripped) "and still a creature (CR 305.7: setting a subtype removes no card type)"
      _ -> Spec.assertFailure s "fixture should have one Construct"
  Spec.it s "CR 305.7 animated Berserkers of Blood Ridge set to Mountain need not attack" $ do
    -- Pawl.Engine.AttackRequirement.instances. Berserkers of Blood Ridge {4}{R}
    -- 4/4 says nothing but "this creature attacks each combat if able", and the
    -- gate needs the requirement printed on a CREATURE: Curse of the Nightly Hunt
    -- is an Aura, and Ashaya animates nontoken creatures, so the Curse can never
    -- reach this gate. Otarian Juggernaut prints the requirement on a creature
    -- too (conditionalAttackRequirementSpec below), but gates it on a threshold,
    -- so the Berserkers is still the one that isolates the strip.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    let (base, mine, _) = S.combatBoardOf [berserkers] []
        with extras = withPermanents S.alice extras base
    case mine of
      [required] -> do
        let declining = Combat.legalAttackDeclaration S.alice []
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "declining stays illegal until Ashaya and Blood Moon are both on the battlefield"
          (declining base, declining (with [ashaya]), declining (with [bloodMoon]), declining stripped)
          (False, False, False, True)
        -- The anti-vacuity leg: attacking is still legal under the strip, so
        -- declining became legal because the requirement went away rather than
        -- because a Mountain creature-land may no longer attack.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [required] stripped) "and attacking with them is still legal"
      _ -> Spec.assertFailure s "fixture should have one Berserkers"
  Spec.it s "CR 305.7 whole cards: the strip unforces the Berserkers' attack in a real declare attackers step" $ do
    -- The gameplay-level case for the same reader, through Engine.runStep -- the
    -- priority loop and CR 703.4i's turn-based action -- with an interpreter that
    -- declines to attack. Both worlds are read off bob's life: unstripped the
    -- rules force the 4/4 through, stripped the declination stands.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    let (base, mine, _) = S.combatBoardOf [berserkers] []
        stripped = withPermanents S.alice [ashaya, bloodMoon] base
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
        forced = S.runCombat declining base
        after = S.runCombat declining stripped
    Spec.assertEqWith s "unstripped, the requirement forces the attack and bob takes four" (S.lifeOf S.bob forced) (Just 16)
    Spec.assertEqWith s "and they really were declared" (S.attackerDeclarationsOf forced) mine
    Spec.assertEqWith s "stripped, bob takes nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "and nothing was declared" (S.attackerDeclarationsOf after) []
  Spec.it s "CR 305.7 an animated Windborn Muse set to Mountain taxes nothing" $ do
    -- Pawl.Engine.AttackCost.costsOn. Windborn Muse {3}{W} 2/3 prints Ghostly
    -- Prison's sentence on a CREATURE, which is what lets Ashaya animate it -- the
    -- Prison itself is an enchantment and can never reach this gate. Both go under
    -- BOB, since by CR 109.5 the cost's "you" is the Muse's own controller and only
    -- an attack on that player is taxed. alice's Forests are BASIC, so Blood Moon
    -- leaves the payment's source alone and the tax is the only thing that moves.
    --
    -- The tax is read off the board rather than asserted as a number: a Forest
    -- makes one mana and the tax is {2}, so "were the Forests tapped" is CR
    -- 508.1j's payment exactly.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    windbornMuse <- S.printingOf s registry "Windborn Muse"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (board, mine, _) = S.combatBoardOf [piker] []
        (forests, base) = addForests forest 2 (snd (S.addCreature windbornMuse S.bob board))
        with extras = withPermanents S.bob extras base
        declared b = S.runPure S.aggressiveAnswer b (Combat.declareAttackers S.alice)
        paid b = allTapped forests (declared b)
        stripped = with [ashaya, bloodMoon]
    Spec.assertEqWith
      s
      "the {2} is paid until Ashaya and Blood Moon are both on the battlefield"
      (paid base, paid (with [ashaya]), paid (with [bloodMoon]), paid stripped)
      (True, True, True, False)
    -- Two anti-vacuity legs, and the first is the one that matters: nothing was
    -- paid because nothing was owed, not because the declaration was refused for
    -- want of mana (CR 508.1's preamble undoes an unpayable declaration whole).
    Spec.assertEqWith s "the Piker attacked anyway" (S.attackerDeclarationsOf (declared stripped)) mine
    Spec.assertBool s (allUntapped forests (declared stripped)) "and not one Forest went"

-- alice attacks with one creature per printing in `mine`; bob defends with a
-- Goblin Piker, holds Curtain of Light and the two Plains that pay its {1}{W},
-- and has one card left in his library so the spell's draw is not a CR 104.3c
-- loss. Returns the attackers, the blocker and the spell.
curtainBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  [Printing.Printing] ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
curtainBoard plains piker curtain mine =
  let (gs0, ours, yours) = S.combatBoardOf mine [piker]
      paid = snd (S.addCreature plains S.bob (snd (S.addCreature plains S.bob gs0)))
      (curtainId, withCard) = S.addHandCard curtain S.bob paid
      stocked = snd (S.addLibraryCard plains S.bob withCard)
   in (stocked, ours, yours, curtainId)

-- Decline every block, cast whatever is castable, and aim every target at
-- `victim`. The cast is bob's: Curtain of Light is the only card in his hand.
--
-- Blocks are DECLINED so that the only route into CR 509.1h's status is the
-- spell -- an aggressive block would confer it by declaration and hide the case.
castCurtain :: ObjectId.ObjectId -> Prompt.Prompt r -> r
castCurtain victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victim))) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- castCurtain's paired control: the same declined blocks, and no cast. The ONE
-- difference between the two answerers is whether the spell is cast.
declineBlocks :: Prompt.Prompt r -> r
declineBlocks p = case p of
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- CR 509.1i's blocker-side event, which CR 509.3d's "becomes blocked by a
-- creature" reads: recorded by the declaration and by nothing else.
blockerWasDeclared :: GameEvent.GameEvent -> Bool
blockerWasDeclared e = case e of
  GameEvent.BlockerDeclared _ -> True
  _ -> False

-- CR 509.1h's escape clause: "an effect says that it becomes blocked". Curtain
-- of Light is the pool's producer -- {1}{W} INSTANT, "Cast this spell only
-- during combat after blockers are declared. Target unblocked attacking creature
-- becomes blocked. Draw a card."
--
-- Its window is CastingRestriction.AfterBlockersDeclared, read through
-- Turn.afterBlockersDeclared; castingWindowSpec below is where CR 506.7b's
-- boundary is proved step by step.
--
-- Sacred Prey ("Whenever this creature becomes blocked, you gain 1 life") is the
-- observer for CR 509.3c, which says a "becomes blocked" ability triggers on the
-- effect exactly as it does on the declaration. Its 1 life and the Prey's 1
-- power are the two numbers every leg is read off, and they move different
-- players' totals.
--
-- THREE readings of the board are told apart, because two of them agree about
-- bob's life total: became blocked by the effect (blocked, nothing blocking it,
-- both creatures alive), blocked by the declaration (blocked, the Piker in the
-- set, both creatures dead), and never blocked (unblocked, bob down 1).
becomesBlockedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becomesBlockedSpec s registry = Spec.describe s "BecomesBlocked" $ do
  Spec.it s "CR 509.1h whole card: Curtain of Light blocks an unblocked attacker, with nothing blocking it" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    prey <- S.printingOf s registry "Sacred Prey"
    curtain <- S.printingOf s registry "Curtain of Light"
    case curtainBoard plains piker curtain [prey] of
      (gs, [attacker], [blocker], _) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            cast = runToEndOfCombat (castCurtain attacker) atBlockers
            -- The control: the same board, the same declined blocks, nothing
            -- cast. The Prey is unblocked and bob takes its 1.
            uncast = runToEndOfCombat declineBlocks atBlockers
            -- The other reading: blocked by the DECLARATION rather than by the
            -- effect. Same board again, and the only change is that bob blocks.
            declared = runToEndOfCombat S.aggressiveAnswer atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the spell is cast after the declaration" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        -- The discriminating assertions: blocked, and blocked by NOTHING.
        Spec.assertBool s (Combat.isBlocked attacker cast) "CR 509.1h: the effect made it a blocked creature"
        Spec.assertEqWith s "and no creature is blocking it" (Combat.blockersOf attacker cast) Set.empty
        Spec.assertEqWith s "CR 510.1c: so it assigns no combat damage and bob takes nothing" (S.lifeOf S.bob cast) (Just 20)
        Spec.assertEqWith s "CR 509.3c: the Prey's becomes-blocked trigger fired" (S.lifeOf S.alice cast) (Just 21)
        Spec.assertBool s (S.onBattlefield attacker cast && S.onBattlefield blocker cast) "nothing was dealt damage either way"
        Spec.assertEqWith s "and bob drew the card the spell says to draw" (length (Game.zoneMembers Zone.Library S.bob cast)) 0
        -- CR 509.3d: "it won't trigger if the creature becomes blocked by an
        -- effect rather than a creature". The event that condition reads is the
        -- one the declaration leg below does record, so this pair is what says
        -- the effect records the attacking side and only that.
        Spec.assertBool s (not (any (blockerWasDeclared . snd) (GameState.events cast))) "no blocker was declared for it"
        -- Never blocked: the trigger is silent and the Prey connects.
        Spec.assertBool s (not (Combat.isBlocked attacker uncast)) "control: with no spell the attacker is unblocked"
        Spec.assertEqWith s "control: so bob takes the Prey's 1" (S.lifeOf S.bob uncast) (Just 19)
        Spec.assertEqWith s "control: and alice gains nothing" (S.lifeOf S.alice uncast) (Just 20)
        Spec.assertEqWith s "control: bob's library is untouched" (length (Game.zoneMembers Zone.Library S.bob uncast)) 1
        -- Blocked by the declaration: the same status from the other writer,
        -- and the board tells them apart by the set and by who is left alive.
        Spec.assertBool s (Combat.isBlocked attacker declared) "declaration leg: blocked as well"
        Spec.assertEqWith s "declaration leg: but the Piker is what is blocking it" (Combat.blockersOf attacker declared) (Set.singleton blocker)
        Spec.assertEqWith s "declaration leg: the same trigger fires" (S.lifeOf S.alice declared) (Just 21)
        Spec.assertBool s (not (S.onBattlefield attacker declared) && not (S.onBattlefield blocker declared)) "declaration leg: and the two creatures trade"
        Spec.assertBool s (any (blockerWasDeclared . snd) (GameState.events declared)) "declaration leg: and CR 509.3d's event IS recorded there"
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  Spec.it s "CR 509.1h a creature already blocked is not a legal target" $ do
    -- CR 509.1h again, read through the card's own committed target slot:
    -- Pool.Creatures under `And [IsAttacking, Not IsBlocked]`. The pair differs
    -- in exactly one thing -- both attackers are alice's, both are attacking,
    -- and bob's one Piker blocks the first of them.
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    prey <- S.printingOf s registry "Sacred Prey"
    curtain <- S.printingOf s registry "Curtain of Light"
    case (curtainBoard plains piker curtain [prey, prey], S.spellTargetSlot curtain) of
      ((gs, [first, second], _, _), Just slot) -> do
        let atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer gs
            legal = Target.legalRecipients Nothing S.noSource slot atDamage
        Spec.assertBool s (Combat.isBlocked first atDamage) "the declaration blocked the first attacker"
        Spec.assertBool s (not (Combat.isBlocked second atDamage)) "and left the second unblocked"
        Spec.assertBool s (not (Set.member (Recipient.ToCreature first) legal)) "Not IsBlocked refuses the blocked attacker"
        Spec.assertBool s (Set.member (Recipient.ToCreature second) legal) "and admits the unblocked one"
      _ -> Spec.assertFailure s "fixture should have two attackers and Curtain of Light a 'target' slot"

-- CR 506.7b's window, "only during combat after blockers are declared", proved
-- step by step on ONE board.
--
-- Every leg comes from the same curtainBoard: the same two seats, the same two
-- Plains paying the same {1}{W}, the same empty stack, and the same unblocked
-- attacking Prey standing as CR 601.2c's legal target. So no leg can refuse the
-- cast for want of mana, of a target, or of a timing window -- the only thing
-- that moves is where in the combat phase the board sits, which is exactly what
-- CR 506.7b is about.
--
-- The pair that carries the rule is `beforeDeclaration` against `declared`:
-- identical states but for CR 509.1's turn-based action having run. Everything
-- after that pair moves GameState.phase on the DECLARED board, which is how the
-- combat damage and end of combat steps -- the two the old transcription's
-- declare-blockers-step window left out -- get read.
--
-- What is NOT provable here is CR 506.7f, and by construction rather than by
-- omission: the pool's only route to a skipped declare blockers step is CR
-- 508.8's empty attack, which leaves no attacking creature, and with no
-- attacking creature Curtain of Light has no legal target -- so such a board
-- refuses the cast whatever the gate answers. The gate implements CR 506.7f
-- all the same, since it reads a record only the step itself writes.
castingWindowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
castingWindowSpec s registry = Spec.describe s "CastingWindow" $ do
  Spec.it s "CR 506.7b the window opens at the declaration and runs to the end of the combat phase" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    prey <- S.printingOf s registry "Sacred Prey"
    curtain <- S.printingOf s registry "Curtain of Light"
    mountain <- S.printingOf s registry "Mountain"
    bolt <- S.printingOf s registry "Lightning Bolt"
    let (gs0, _, _, curtainId) = curtainBoard plains piker curtain [prey]
        (boltId, gs) = S.addHandCard bolt S.bob (snd (S.addCreature mountain S.bob gs0))
        -- The declare blockers step reached but not yet run: attackers are
        -- declared, CR 509.1's turn-based action is not.
        beforeDeclaration = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
        -- That one action, and nothing else. Blocks are DECLINED so the Prey
        -- stays an unblocked attacking creature and every later leg keeps the
        -- same legal target.
        declared = S.runPure declineBlocks beforeDeclaration Combat.declareBlockers
        inStep step = declared {GameState.phase = Phase.Combat step}
        -- The other side of CR 506.7b, on the board that has NOT declared: a
        -- reachable priority window one step earlier.
        atAttackers = beforeDeclaration {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}
    -- Anti-vacuity: bob's unrestricted instant is castable on every one of these
    -- boards, so a leg that refuses the Curtain is refusing the Curtain.
    Spec.assertBool s (all (S.castable S.bob boltId) [beforeDeclaration, declared, inStep CombatStep.CombatDamage, inStep CombatStep.EndOfCombat, atAttackers]) "bob can pay for and legally cast an unrestricted instant on every leg"
    Spec.assertBool s (not (Turn.afterBlockersDeclared beforeDeclaration)) "the declaration has not happened yet"
    Spec.assertBool s (Turn.afterBlockersDeclared declared) "and it has after CR 509.1's turn-based action"
    -- Before the point CR 506.7b names.
    Spec.assertBool s (not (S.castable S.bob curtainId atAttackers)) "refused in the declare attackers step"
    Spec.assertBool s (not (S.castable S.bob curtainId beforeDeclaration)) "refused in the declare blockers step before blockers are declared"
    -- After it. The first was already reachable under the declare-blockers-step
    -- window this replaced; the last two are what that window lost.
    Spec.assertBool s (S.castable S.bob curtainId declared) "castable once blockers are declared"
    Spec.assertBool s (S.castable S.bob curtainId (inStep CombatStep.CombatDamage)) "castable in the combat damage step"
    Spec.assertBool s (S.castable S.bob curtainId (inStep CombatStep.EndOfCombat)) "castable in the end of combat step"

-- Glory-Bound Initiate {1}{W} Creature -- Human Warrior 3/1, "You may exert this
-- creature as it attacks. When you do, it gets +1\/+3 and gains lifelink until
-- end of turn." The pool's producer for Keyword.Exert, and so for CR 508.1g's
-- optional-cost step, for GameEvent.Exerted and for
-- TriggerCondition.SelfExerted's CR 607.2h linked trigger.
--
-- Every case runs a PAIR of boards differing in exactly one thing -- the answer
-- to Prompt.ChooseExert -- so no assertion can pass because the board could not
-- have shown the difference. The Goblin Piker beside the Initiate is there for
-- two reasons: it makes Prompt.DeclareAttackers a real choice rather than one the
-- engine could elide, and it is an attacker WITHOUT exert on the same board, so
-- "the exerted creature stays tapped" is measured against a creature that does
-- not.
exertSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exertSpec s registry = Spec.describe s "Exert" $ do
  Spec.it s "CR 508.1g / 701.43d exerting an attacker fires its linked trigger" $ do
    initiate <- S.printingOf s registry "Glory-Bound Initiate"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [initiate, piker] []
        exerted = S.runCombat (exertAnswer OptionalDecision.Exercises) gs
        declined = S.runCombat (exertAnswer OptionalDecision.Declines) gs
    case mine of
      [] -> Spec.assertFailure s "the fixture should have put two attackers on the board"
      initiateId : _ -> do
        -- The pair pins BOTH halves of "+1/+3": the misreading +3/+1 would leave a
        -- 6/2 here and take bob to 12 below, so neither number is a coincidence
        -- with the other.
        Spec.assertEqWith s "CR 701.43d the exerted Initiate is 4/4" (S.powerToughnessOf initiateId exerted) (Just (4, 4))
        Spec.assertEqWith s "the declined Initiate is still 3/1" (S.powerToughnessOf initiateId declined) (Just (3, 1))
        -- The Piker's 2 damage is in both totals, which is what makes the
        -- difference between them the Initiate's power alone.
        Spec.assertEqWith s "bob took 4 from the exerted Initiate and 2 from the Piker" (S.lifeOf S.bob exerted) (Just 14)
        Spec.assertEqWith s "bob took 3 and 2 without the exert" (S.lifeOf S.bob declined) (Just 15)
        -- CR 702.15b: the lifelink half of the same trigger, and the second thing
        -- that separates the two boards.
        Spec.assertEqWith s "the granted lifelink gained alice 4" (S.lifeOf S.alice exerted) (Just 24)
        Spec.assertEqWith s "no lifelink without the exert" (S.lifeOf S.alice declined) (Just 20)
        -- CR 701.43a's own event, which is what the linked trigger matched.
        Spec.assertBool s (elem (GameEvent.Exerted initiateId) (S.eventsOf exerted)) "CR 701.43a the exert recorded its event"
        Spec.assertBool s (notElem (GameEvent.Exerted initiateId) (S.eventsOf declined)) "and a declined exert records none"
  Spec.it s "CR 701.43a / 701.43b an exerted attacker misses one untap step, then untaps" $ do
    initiate <- S.printingOf s registry "Glory-Bound Initiate"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [initiate, piker] []
        -- CR 502.3's turn-based action, called directly: that is the narrowest
        -- path to the prohibition, and alice is both the exerting player and the
        -- permanent's controller, so it is her untap step under either reading of
        -- CR 701.43a. The case below is the one that tells the two apart.
        untap g = S.runPure S.identityAnswer g (Engine.untapAll S.alice)
        exerted = S.runPure (exertAnswer OptionalDecision.Exercises) gs (Combat.declareAttackers S.alice)
        declined = S.runPure (exertAnswer OptionalDecision.Declines) gs (Combat.declareAttackers S.alice)
    case mine of
      initiateId : pikerId : _ -> do
        -- CR 508.1f: the declaration taps both, whichever way CR 508.1g was
        -- answered -- so the untap step below starts from one board state.
        Spec.assertEqWith s "CR 508.1f the exerted attacker is tapped by the declaration" (tapStateOf initiateId exerted) (Just TapState.Tapped)
        Spec.assertEqWith s "and so is the declined one" (tapStateOf initiateId declined) (Just TapState.Tapped)
        Spec.assertEqWith s "CR 701.43a the exerted attacker does not untap" (tapStateOf initiateId (untap exerted)) (Just TapState.Tapped)
        Spec.assertEqWith s "the declined attacker untaps" (tapStateOf initiateId (untap declined)) (Just TapState.Untapped)
        -- The prohibition rides the CREATURE and not the declaration: the Piker
        -- attacked in the same declaration and untaps.
        Spec.assertEqWith s "the Piker that attacked beside it untaps" (tapStateOf pikerId (untap exerted)) (Just TapState.Untapped)
        -- CR 701.43b: "each effect causing it not to untap expires during the same
        -- untap step", so ONE step is all it costs.
        Spec.assertEqWith s "CR 701.43b and untaps at the next untap step" (tapStateOf initiateId (untap (untap exerted))) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "the fixture should have put two attackers on the board"
  -- The case the reading turns on. CR 701.43a says "your next untap step" of the
  -- EXERTING player, so a control change between the declaration and the step
  -- separates it from the untap step of whoever holds the permanent then -- and
  -- bob's comes first, since alice exerted on her own turn.
  --
  -- S.giveControl is the fixture; the printed board it stands in for is bob's
  -- Garland, Royal Kidnapper, which gains control of a creature the monarch
  -- controls "for as long as they're the monarch" -- a duration that outlasts the
  -- turn the creature was taken on.
  Spec.it s "CR 701.43a the rider is the EXERTING player's untap step, not the new controller's" $ do
    initiate <- S.printingOf s registry "Glory-Bound Initiate"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [initiate, piker] []
        exerted = S.runPure (exertAnswer OptionalDecision.Exercises) gs (Combat.declareAttackers S.alice)
        untapFor pid g = S.runPure S.identityAnswer g (Engine.untapAll pid)
    case mine of
      initiateId : pikerId : _ -> do
        -- Both attackers change hands, so the two differ in exactly one thing:
        -- alice exerted the Initiate and did not exert the Piker.
        let stolen = S.giveControl pikerId S.bob (S.giveControl initiateId S.bob exerted)
            bobUntapped = untapFor S.bob stolen
        Spec.assertEqWith s "CR 701.43a alice's exert says nothing about bob's untap step" (tapStateOf initiateId bobUntapped) (Just TapState.Untapped)
        Spec.assertEqWith s "and the Piker beside it untaps too" (tapStateOf pikerId bobUntapped) (Just TapState.Untapped)
        -- CR 701.43b's expiry is keyed to the same player, so it happens at
        -- alice's untap step even though bob is holding the permanent through it
        -- -- and a creature handed back afterwards untaps on schedule.
        let aliceUntapped = untapFor S.alice stolen
            handedBack = S.giveControl initiateId S.alice aliceUntapped
        Spec.assertEqWith s "CR 502.3 alice's untap step does not reach a permanent bob controls" (tapStateOf initiateId aliceUntapped) (Just TapState.Tapped)
        Spec.assertEqWith s "CR 701.43b the rider expired at that step all the same" (tapStateOf initiateId (untapFor S.alice handedBack)) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "the fixture should have put two attackers on the board"

-- S.aggressiveAnswer with Prompt.ChooseExert pinned, on Support's `attackTo`
-- pattern: the rank-1 signature partially applies to the `forall r. Prompt r ->
-- r` runCombat and runPure want. Pinned EXPLICITLY rather than left to the
-- fallthrough, which is a searching answerer and could repair a mutation.
exertAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
exertAnswer decision p = case p of
  Prompt.ChooseExert {} -> decision
  _ -> S.aggressiveAnswer p

-- CR 508.1d's OBJECT axis -- what a required creature has to attack, rather than
-- merely that it must attack -- proved by Alluring Siren ("{T}: Target creature
-- an opponent controls attacks you this turn if able"). The subject-only shape is
-- Curse of the Nightly Hunt's, in attackCostSpec above and in Pawl.CombatSpec.
--
-- bob controls a Jace Beleren, so CR 508.1b offers alice's attacker TWO
-- announcements and the narrowing has something to narrow. That is the Siren's
-- own ruling: "if the targeted creature would be able to attack either you or a
-- planeswalker you control, it must attack you, not the planeswalker."
--
-- alice also controls a second creature the Siren never touched, which is what
-- makes the target prompt a real choice (one candidate would short-circuit it)
-- and what keeps "every creature must attack bob" from passing for the answer.
alluringSirenSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
alluringSirenSpec s registry = Spec.describe s "AlluringSiren" $ do
  Spec.it s "CR 508.1d the requirement is obeyed by attacking bob and not by attacking bob's Jace" $ do
    siren <- S.printingOf s registry "Alluring Siren"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    case sirenBoard siren jace piker centaur of
      Nothing -> Spec.assertFailure s "fixture should build"
      Just (control, lured, free, jaceId, activated) -> do
        -- THE OBJECT AXIS. Attacking Jace is a declaration that attacks with the
        -- required creature and still disobeys the requirement, which is the one
        -- thing the subject-only carrier could not say.
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclarationAs S.alice [(lured, AttackTarget.OfPlaneswalker jaceId)] activated))
          "CR 508.1d: announcing Jace does not obey 'attacks you if able'"
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(lured, AttackTarget.OfPlayer S.bob)] activated)
          "announcing bob does"
        -- The subject axis, still live: declining is illegal under the same
        -- requirement.
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] activated)) "declining to attack obeys nothing"
        -- The creature the Siren never named is unconstrained on both axes, so
        -- "attacking Jace is illegal" is about the requirement rather than about
        -- the board.
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(lured, AttackTarget.OfPlayer S.bob), (free, AttackTarget.OfPlaneswalker jaceId)] activated)
          "and the unnamed creature may still attack Jace alongside it"
        -- The control board differs in exactly one thing: the Siren's ability
        -- never resolved.
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(lured, AttackTarget.OfPlaneswalker jaceId)] control)
          "without the Siren's ability, that same creature may attack Jace"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] control) "and may decline altogether"
  Spec.it s "CR 514.2 the stored requirement lasts exactly the turn" $ do
    -- "This turn" is Duration.UntilEndOfTurn, which Expiry.arm turns into
    -- Expiry.AtCleanup -- so the cleanup step's sweep is what has to drop the
    -- stored row. Read off the store, since a second declare attackers step in
    -- the same turn is the only other place it would show, and this fixture has
    -- none.
    siren <- S.printingOf s registry "Alluring Siren"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    case sirenBoard siren jace piker centaur of
      Nothing -> Spec.assertFailure s "fixture should build"
      Just (control, lured, _, _, activated) -> do
        Spec.assertEqWith s "stored once the ability has resolved" (fmap ActiveAttackRequirement.attacker (GameState.attackRequirements activated)) [lured]
        Spec.assertEqWith s "and gone at cleanup (CR 514.2)" (GameState.attackRequirements (Expiry.dropAtCleanup activated)) []
        Spec.assertEqWith s "nothing was stored without the ability" (GameState.attackRequirements control) []
  Spec.it s "CR 508.1d whole cards: a real declare attackers step sends the lured creature at bob" $ do
    -- The gameplay-level case, through Combat.declareAttackers rather than the
    -- legality predicate, with an interpreter that attacks with everything and
    -- announces JACE for every attacker. CR 508.1d makes that declaration illegal,
    -- so the engine replaces it, and what the combat record then says the lured
    -- creature is attacking is the whole assertion.
    siren <- S.printingOf s registry "Alluring Siren"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    case sirenBoard siren jace piker centaur of
      Nothing -> Spec.assertFailure s "fixture should build"
      Just (control, lured, _, jaceId, activated) -> do
        let after = S.runPure (announcing jaceId) activated (Combat.declareAttackers S.alice)
            without = S.runPure (announcing jaceId) control (Combat.declareAttackers S.alice)
            announced gs oid = Map.lookup oid (Combat.Type.attackers (GameState.combat gs))
        Spec.assertEqWith s "the lured creature attacks bob, not the Jace it was announced against" (announced after lured) (Just (AttackTarget.OfPlayer S.bob))
        -- The SAME interpreter, the same board bar the Siren's resolution: without
        -- the requirement its announcement stands, so the redirect above is CR
        -- 508.1d and not a rule about announcements.
        Spec.assertEqWith s "without the Siren's ability the announcement stands" (announced without lured) (Just (AttackTarget.OfPlaneswalker jaceId))

-- Attacks with everything (aggressiveAnswer) and announces the PLANESWALKER for
-- every attacker, which CR 508.1d then has to refuse.
announcing :: ObjectId.ObjectId -> Prompt.Prompt r -> r
announcing jaceId p = case p of
  Prompt.ChooseAttackTarget {} -> AttackTarget.OfPlaneswalker jaceId
  _ -> S.aggressiveAnswer p

-- CR 601.2c: aim the Siren's ability at one particular creature. The offered set
-- is FILTERED rather than rebuilt, so the target the engine re-reads at
-- resolution (CR 608.2b) is the one it offered.
aimingAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- alice's two creatures against bob's Jace and bob's Siren, returned twice: once
-- untouched, and once with the Siren's ability resolved on alice's FIRST creature.
-- The two differ in that resolution and what paying for it did to bob's own side
-- (the Siren is tapped); nothing on alice's side of the board moves, which is
-- what every paired assertion below rests on.
sirenBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
sirenBoard siren jace piker centaur =
  let (gs0, mine, theirs) = S.combatBoardOf [piker, centaur] [jace, siren]
   in case (mine, theirs, Face.activatedAbilities (S.combinedFace siren)) of
        ([lured, free], [jaceId, sirenId], ability : _) ->
          let control = S.addCounter CounterKind.Loyalty 3 jaceId gs0
              ready = control {GameState.priority = Just S.bob}
              activated = snd (Engine.runGamePure (aimingAt lured) ready (Activate.activateAbility S.bob sirenId ability))
           in Just (control, lured, free, jaceId, snd (Engine.runGamePure (aimingAt lured) activated Stack.resolveTop))
        _ -> Nothing

-- CR 508.1d's second shape -- "or that it attacks if some condition is met" --
-- proved by Otarian Juggernaut, whose whole threshold line is one CR 604.2 "as
-- long as" clause: "as long as there are seven or more cards in your graveyard,
-- this creature gets +3/+0 and attacks each combat if able". The unconditional
-- shape is Berserkers of Blood Ridge's, in boundedDeclarationSpec above.
--
-- The two boards differ in ONE thing, the number of cards in alice's graveyard,
-- and the threshold falls between them. The +3/+0 rides the same clause, so the
-- damage a forced attack deals is 5 rather than 2 and the two readings of the
-- rule cannot reach the same life total.
conditionalAttackRequirementSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
conditionalAttackRequirementSpec s registry = Spec.describe s "ConditionalAttackRequirement" $ do
  Spec.it s "CR 508.1d a threshold requirement forces the attack only once the condition holds" $ do
    juggernaut <- S.printingOf s registry "Otarian Juggernaut"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, _) = S.combatBoardOf [juggernaut] []
        filling n gs = List.foldl' (\g _ -> snd (S.addGraveyardCard piker S.alice g)) gs [1 .. n :: Int]
        under = filling 6 base
        over = filling 7 base
        declining :: Prompt.Prompt r -> r
        declining p = case p of
          Prompt.DeclareAttackers {} -> []
          _ -> S.aggressiveAnswer p
    case mine of
      [required] -> do
        -- The gameplay-level assertions, through Engine.runStep's declare
        -- attackers step with an interpreter that declines to attack: over the
        -- threshold the rules force the 2/3 through as a 5/3, under it the
        -- declination stands.
        let forced = S.runCombat declining over
            declined = S.runCombat declining under
        Spec.assertEqWith s "seven cards in the graveyard: the requirement forces the attack and bob takes five" (S.lifeOf S.bob forced) (Just 15)
        Spec.assertEqWith s "six cards: the declination stands and bob takes nothing" (S.lifeOf S.bob declined) (Just 20)
        Spec.assertEqWith s "and the Juggernaut really was declared over the threshold" (S.attackerDeclarationsOf forced) mine
        Spec.assertEqWith s "and really was not under it" (S.attackerDeclarationsOf declined) []
        -- The same two answers off Combat.legalAttackDeclaration directly, which
        -- is the narrowest path to CR 508.1d's maximization.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] under) "under the threshold declining obeys everything there is to obey"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] over)) "over it declining obeys nothing"
        -- Anti-vacuity: the Juggernaut is an able attacker on BOTH boards, so the
        -- six-card board's legal declination is the condition being false rather
        -- than a CR 508.1a or CR 508.1c refusal in disguise.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [required] under) "and attacking with it is legal either way"
      _ -> Spec.assertFailure s "fixture should have one Juggernaut"

-- CR 608.2d's random half, proved by Ruhan of the Fomori ("At the beginning of
-- combat on your turn, choose an opponent at random. Ruhan attacks that player
-- this combat if able."). Two effects in one resolution: the first binds the
-- opponent randomness named into a slot, the second reads it back through
-- PlayerRef.InSlot as the requirement's defender.
--
-- THREE SEATS, and that is load-bearing: CR 102.2 leaves a two-player game
-- exactly one opponent, so the pick is elided and every implementation agrees.
--
-- The pair of boards differs in ONE thing -- which opponent alice named as the
-- defending player at CR 507.1, before the trigger resolved -- and the random
-- pick is pinned to carol on both. Only one opponent is attackable at a time (CR
-- 506.2a), so the assertion is whether the requirement can be obeyed:
--
--   * defender carol: the requirement names the attackable seat, so CR 508.1d
--     forbids declining.
--   * defender bob: it names a seat that cannot be attacked at all, so the
--     maximum is zero and declining is legal.
--
-- An engine that rolled the head of the offer itself (bob) rather than honouring
-- the answer flips BOTH, and one that never landed the bind makes declining legal
-- on both. Pinned to NonEmpty.last for exactly that reason: Replay.defaultAnswer
-- -- which S.identityAnswer falls through to -- answers this prompt with the
-- HEAD, so a head-pinned board could not tell the two apart.
randomOpponentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
randomOpponentSpec s registry = Spec.describe s "RandomOpponent" $ do
  Spec.it s "CR 608.2d the opponent randomness named is the one the requirement makes Ruhan attack" $ do
    ruhan <- S.printingOf s registry "Ruhan of the Fomori"
    let (board, mine, _, _) = S.threePlayerCombat [ruhan] [] []
        atCarol = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (ruhanAnswer S.carol) board
        atBob = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (ruhanAnswer S.bob) board
    case mine of
      [ruhanId] -> do
        -- THE GAMEPLAY ASSERTION. carol is the attackable seat and the
        -- requirement names her, so CR 508.1d refuses the empty declaration.
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclaration S.alice [] atCarol))
          "CR 508.1d: with carol defending, declining disobeys the requirement randomness bound"
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(ruhanId, AttackTarget.OfPlayer S.carol)] atCarol)
          "and attacking carol obeys it"
        -- The paired board, one thing different: bob defends, so the requirement
        -- names a seat CR 506.2a leaves unattackable and the maximum is zero.
        Spec.assertBool
          s
          (Combat.legalAttackDeclaration S.alice [] atBob)
          "CR 508.1d: with bob defending, the requirement cannot be obeyed and declining is legal"
        -- Supporting, and LAST so it cannot absorb a mutation the two above
        -- should catch: the requirement really was stored against carol, not bob.
        Spec.assertEqWith
          s
          "the stored requirement names carol"
          (fmap ActiveAttackRequirement.defender (GameState.attackRequirements atCarol))
          [S.carol]
        Spec.assertEqWith
          s
          "and the attacker is Ruhan"
          (fmap ActiveAttackRequirement.attacker (GameState.attackRequirements atCarol))
          [ruhanId]
      _ -> Spec.assertFailure s "fixture should have one Ruhan"
  Spec.it s "CR 104.3a the offer is alice's opponents and never alice herself" $ do
    ruhan <- S.printingOf s registry "Ruhan of the Fomori"
    let (board, _, _, _) = S.threePlayerCombat [ruhan] [] []
        logging :: Prompt.Prompt r -> State.State [[PlayerId.PlayerId]] r
        logging p = case p of
          Prompt.RandomOpponent offered -> do
            State.modify' (NonEmpty.toList offered :)
            pure (ruhanAnswer S.carol p)
          _ -> pure (ruhanAnswer S.carol p)
        offers = reverse (State.execState (Engine.runGame logging board Engine.runStep) [])
    -- Recorded off the prompt, since the candidate list is not readable off the
    -- resulting board. The engine never rolls -- it offers and filters back -- so
    -- WHAT it offered is the part of that posture a test can see.
    Spec.assertEqWith s "asked once, offering both opponents and not alice" offers [[S.bob, S.carol]]

-- Pins BOTH of the beginning-of-combat step's questions: which opponent alice
-- names as the defending player (CR 507.1), and which opponent randomness names
-- (CR 608.2d). The defender is FILTERED out of the offered candidates rather than
-- built, so an answer the engine never offered cannot slip through; the random
-- pick is NonEmpty.last, which on this board is carol and is never the value
-- Replay.defaultAnswer would supply.
ruhanAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
ruhanAnswer defending p = case p of
  Prompt.ChooseDefender _ _ candidates ->
    Maybe.fromMaybe (NonEmpty.head candidates) (List.find (== defending) (NonEmpty.toList candidates))
  Prompt.RandomOpponent offered -> NonEmpty.last offered
  _ -> S.identityAnswer p

-- Declines CR 508.1a's declaration the first time and declares everything the
-- second, counting the asks. The first answer is ILLEGAL rather than
-- unaffordable, which is CR 508.1's preamble reached through CR 508.1d instead of
-- through CR 508.1j.
declineThenAttackAnswer :: Prompt.Prompt r -> State.State Natural r
declineThenAttackAnswer p = case p of
  Prompt.DeclareAttackers _ _ candidates -> do
    asked <- State.get
    State.put (asked + 1)
    pure (if asked == 0 then [] else candidates)
  _ -> pure (S.identityAnswer p)

-- declineThenAttackAnswer's twin for CR 509.1a: no blocks the first time, every
-- candidate on the first attacker the second.
declineThenBlockAnswer :: Prompt.Prompt r -> State.State Natural r
declineThenBlockAnswer p = case p of
  Prompt.DeclareBlockers _ _ mine attackers -> do
    asked <- State.get
    State.put (asked + 1)
    pure $ case attackers of
      [] -> Map.empty
      a : _ ->
        if asked == 0
          then Map.empty
          else Map.fromList (fmap (\b -> (b, Set.singleton a)) mine)
  _ -> pure (S.identityAnswer p)

-- CR 508.1's and CR 509.1's preambles reached through the ILLEGAL-declaration
-- clauses (CR 508.1c/508.1d, CR 509.1b/509.1c) rather than through the payment
-- clauses -- both end "the declaration ... is illegal", so both take the same
-- rewind, and attackCostSpec's and blockCostSpec's retry cases are the payment
-- half of the same behaviour.
--
-- Each board is chosen so the ceiling's own declaration is SMALLER than the one
-- the player then makes: a requirement over one creature leaves several
-- declarations attaining CR 508.1d's maximum, and forcedAttackDeclaration takes
-- the smallest. An engine that substituted the ceiling rather than asking again
-- would send one creature where two were declared.
declarationRetrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
declarationRetrySpec s registry = Spec.describe s "DeclarationRetry" $ do
  Spec.it s "CR 508.1d an illegal declaration is rewound and asked again, not replaced by the ceiling's" $ do
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [berserkers, piker] []
        ((_, after), asked) = State.runState (Engine.runGame declineThenAttackAnswer gs (Combat.declareAttackers S.alice)) 0
    Spec.assertEqWith s "CR 508.1: both creatures attack, where the ceiling's declaration sends the Berserkers alone" (Set.fromList (S.attackerDeclarationsOf after)) (Set.fromList mine)
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "declining really is illegal, so the first answer really was rewound"
    Spec.assertBool s (all (\oid -> Combat.legalAttackDeclaration S.alice [oid] gs) (take 1 mine)) "and the Berserkers alone attains the maximum, so the ceiling stops there"
    Spec.assertEqWith s "CR 508.1's preamble asked for a fresh declaration" asked 2
  Spec.it s "CR 509.1c an illegal declaration is rewound and asked again, not replaced by the ceiling's" $ do
    screen <- S.printingOf s registry "Razorgrass Screen"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [screen, piker]
    case mine of
      [attacker] -> do
        let ((_, after), asked) = State.runState (Engine.runGame declineThenBlockAnswer gs Combat.declareBlockers) 0
        Spec.assertEqWith s "CR 509.1: both creatures block, where the ceiling's declaration sends the Screen alone" (blockersOf attacker after) (Set.fromList theirs)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "declining really is illegal, so the first answer really was rewound"
        Spec.assertEqWith s "CR 509.1's preamble asked for a fresh declaration" asked 2
      _ -> Spec.assertFailure s "fixture should have one attacker"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  combatLegalitySpec s registry
  landSubtypeStripSpec s registry
  keywordSpec s registry
  firstStrikeSpec s registry
  endOfCombatSpec s registry
  m2bExitSpec s registry
  vigilanceSpec s registry
  attacksAloneSpec s registry
  boundedDeclarationSpec s registry
  controlChangeRemovalSpec s registry
  becomesBlockedSpec s registry
  castingWindowSpec s registry
  typeChangeRemovalSpec s registry
  creaturePlaneswalkerCombatSpec s registry
  effectRemovalSpec s registry
  putOntoBattlefieldAttackingSpec s registry
  towershellSpec s registry
  planeswalkerAttackSpec s registry
  trampleOverPlaneswalkersSpec s registry
  sharedBlockerSpec s registry
  lastKnownDefendingPlayerSpec s registry
  attackCostSpec s registry
  alluringSirenSpec s registry
  conditionalAttackRequirementSpec s registry
  randomOpponentSpec s registry
  declarationRetrySpec s registry
  blockCostSpec s registry
  exertSpec s registry

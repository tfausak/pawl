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
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone

-- The helpers below are duplicated from Pawl.CombatSpec rather than hoisted into
-- Pawl.Support, which every spec in the tree imports and so rebuilds.

-- The ATTACKING creatures (CR 508.1k), which since #2024 is a different question
-- from Combat.declaredAttackers -- CR 506.4 removal ends one and leaves the
-- other standing.
attackersOf :: GameState.GameState -> [ObjectId.ObjectId]
attackersOf gs = Map.keys (Combat.Type.attackers (GameState.combat gs))

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Declare attackers with everything, then hand back the state and the ids.
attacking :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
attacking mine theirs =
  let (gs, ours, yours) = S.combatBoardOf mine theirs
      after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
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
        -- The witness, pinned: attackCeilingGiven's search reaches size one only
        -- over the creatures CR 506.5 leaves, and here it leaves none -- so the
        -- declaration it hands back is the empty one, and not the Construct's
        -- illegal attack. The control's is the Piker's attack, which is what
        -- keeps this from passing on a ceiling that answers empty everywhere.
        let offered = Combat.legalAttackers S.alice gs
            controlOffered = Combat.legalAttackers S.alice control
        Spec.assertEqWith s "and the forced declaration is empty" (fmap fst (Combat.forcedAttackDeclaration (Combat.attackCeiling offered gs) offered)) []
        Spec.assertEqWith s "while the control's names its Piker" (fmap fst (Combat.forcedAttackDeclaration (Combat.attackCeiling controlOffered control) controlOffered)) controlOffered
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
  Spec.it s "CR 508.1d a wide board carrying both shapes still answers, and answers correctly" $ do
    -- #714's board: a set-shaped restriction in force (the Construct's) alongside
    -- an attack requirement, which is the pair that used to take CR 508.1d's
    -- maximum off its closed form and onto an enumeration of every declaration --
    -- O((1 + targets) ^ candidates), so twenty-four candidates was tens of
    -- millions of declarations and the step did not finish.
    --
    -- Twenty-four is chosen to be past the old search's reach rather than for any
    -- rules reason. What it proves about SPEED it proves only by finishing inside
    -- the suite's timeout, which is a weak instrument; the assertions below are
    -- the load-bearing half, and they are the same three answers the two-creature
    -- board above gives.
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    bondedConstruct <- S.printingOf s registry "Bonded Construct"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice (bondedConstruct : replicate 23 piker) []
    case mine of
      construct : pikers@(_ : _) -> do
        Spec.assertEqWith s "all twenty-four are offered" (Combat.legalAttackers S.alice gs) mine
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice mine gs) "attacking with every one of them obeys every requirement"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice (construct : drop 1 pikers) gs)) "leaving one Piker home obeys one requirement fewer"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [construct] gs)) "and the Construct alone is still illegal on the restriction"
      _ -> Spec.assertFailure s "fixture should have twenty-four creatures"
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
-- Pacifism's per-creature one and Bonded Construct's set-shaped one. Caverns of
-- Despair is the same restriction at a bound of TWO, which is the smallest bound
-- that makes CR 508.1d choose WHICH creatures rather than merely how many.
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
        -- The TIE-BREAK, pinned: both Pikers attain the maximum and CR 508.1d
        -- chooses neither, so which one `best` names is pawl's own call. It is the
        -- LATER candidate, and the assertion is here because it is the only thing
        -- fencing that choice -- the case below discriminates on WEIGHT and so
        -- cannot see it.
        Spec.assertEqWith s "and the forced declaration names the second Piker" (fmap fst (Combat.forcedAttackDeclaration (Combat.attackCeiling mine gs) mine)) [second]
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
    -- The Berserkers is declared FIRST deliberately. attackCeilingGiven breaks a
    -- tie towards the LATER candidate (pinned by the case above), so under a
    -- creature-counting reading -- where the two tie at one apiece -- `best` would
    -- be the Piker alone, and the two discriminating assertions bite. With the
    -- Berserkers last they would agree with both readings.
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
  Spec.it s "CR 508.1d under a bound of TWO the maximum takes the HEAVIEST creatures, not any two" $ do
    -- The case that separates CR 508.1d's maximum from "as many creatures as the
    -- bound allows". Caverns of Despair ("{2}{R}{R} World Enchantment, No more
    -- than two creatures can attack each combat. / No more than two creatures can
    -- block each combat.") allows two, a Curse of the Nightly Hunt requires all
    -- three of alice's creatures, and Berserkers of Blood Ridge carries its own
    -- requirement on top of the Curse's -- so the weights are two, one and one,
    -- and the maximum is THREE requirements rather than the two that any pair of
    -- Pikers obeys.
    --
    -- A bound of one cannot see this: there the only sizes are zero and one, so
    -- ORDERING the weights decides nothing and picking the largest is the same as
    -- picking any. Two is the smallest bound at which the choice among sizes and
    -- the choice among creatures come apart, which is why this case wants a second
    -- printing rather than another Arbiter board.
    caverns <- S.printingOf s registry "Caverns of Despair"
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    berserkers <- S.printingOf s registry "Berserkers of Blood Ridge"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = cursing curse S.alice [berserkers, piker, piker] [caverns]
        (control, _, _) = cursing curse S.alice [berserkers, piker, piker] []
    case mine of
      [bers, first, second] -> do
        Spec.assertEqWith s "all three are still offered" (Combat.legalAttackers S.alice gs) mine
        -- The proving assertion: two Pikers is a full declaration under the bound
        -- and still illegal, because the pair obeying the MOST is the Berserkers
        -- and either Piker.
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [first, second] gs)) "two Pikers fill the bound and obey two of three"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [bers, first] gs) "the Berserkers with a Piker attains the maximum"
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [bers, second] gs) "and so does the Berserkers with the other one"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [bers] gs)) "the Berserkers alone leaves a requirement obeyable"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] gs)) "and declining obeys none of the three"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice mine gs)) "while all three together is over the bound"
        Spec.assertEqWith s "and the forced declaration is the Berserkers and the later Piker" (fmap fst (Combat.forcedAttackDeclaration (Combat.attackCeiling mine gs) mine)) [bers, second]
        -- Control: strip the Caverns and the bound stops choosing, so all three
        -- attack and every proper subset is illegal.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice mine control) "without the Caverns all three attack"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [bers, first] control)) "and the pair no longer attains the maximum"
      _ -> Spec.assertFailure s "fixture should have three creatures"
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
        let refused = S.runPure S.aggressiveAnswer crowded (Combat.declareBlockers S.manaPerformer)
            blocked = S.runPure S.aggressiveAnswer single (Combat.declareBlockers S.manaPerformer)
            doubled = S.runPure S.aggressiveAnswer plain (Combat.declareBlockers S.manaPerformer)
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
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
    case mine of
      [centaur, p] -> do
        Spec.assertEqWith s "both attacking" (length (attackersOf after)) 2
        Spec.assertEqWith s "the centaur is untapped" (tapStateOf centaur after) (Just TapState.Untapped)
        Spec.assertEqWith s "the piker is tapped" (tapStateOf p after) (Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 702.20b vigilance still attacks" $ do
    -- Vigilance is not a legality question: the creature is declared as an
    -- attacker exactly as normal. It simply skips CR 508.1f's tap.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [windseekerCentaur] [piker]
        after = snd (Engine.runGamePure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice))
    Spec.assertEqWith s "attacking" (attackersOf after) mine
  Spec.it s "CR 702.20b an untapped vigilant attacker can still be blocked" $ do
    -- It is attacking, so it is in the Combat record, tapped or not.
    windseekerCentaur <- S.printingOf s registry "Windseeker Centaur"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [windseekerCentaur] [piker]
        steps = do
          Combat.declareAttackers S.manaPerformer S.alice
          Combat.declareBlockers S.manaPerformer
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
                      -- CR 802.2a: the seat each attack names, which is what a
                      -- declaration would have recorded.
                      Combat.Type.attackedUnder = Map.singleton oid S.bob,
                      Combat.Type.attackedControlledBy = Map.empty,
                      Combat.Type.joinedUnder = Map.singleton oid S.alice,
                      Combat.Type.attacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      Combat.Type.declaredAttacked = Set.singleton (AttackTarget.OfPlayer S.bob),
                      -- Empty, because this board stands after the declare attackers
                      -- step ended: CR 500.1 scopes this half to the step, and
                      -- Combat.clearAttackedThisStep empties it as one ends.
                      Combat.Type.declaredAttackedThisStep = Set.empty,
                      -- CR 508.1a / 509.1a: this board is hand-built rather
                      -- than declared, so nothing was declared on it.
                      Combat.Type.declaredAttackers = Set.empty,
                      Combat.Type.declaredBlockers = Set.empty,
                      Combat.Type.blockersDeclared = True,
                      Combat.Type.attackingNothing = Set.empty,
                      Combat.Type.defenders = [S.bob]
                    }
              }
    Spec.assertEqWith s "starts empty" (Combat.Type.attackers (GameState.combat gs)) Map.empty
    Spec.assertEqWith s "clears" (Combat.Type.attackers (GameState.combat (Combat.clearCombat busy))) Map.empty
    -- CR 511.3 on the two CR 508.1a / 509.1a records too, which is what scopes
    -- Filter.DeclaredAttackerThisCombat to THIS combat rather than the turn:
    -- CR 506.7c's second combat phase asks the question from empty.
    Spec.assertEqWith
      s
      "CR 511.3 clears the declared-attacker record"
      (Combat.Type.declaredAttackers (GameState.combat (Combat.clearCombat busy {GameState.combat = (GameState.combat busy) {Combat.Type.declaredAttackers = Set.fromList mine}})))
      Set.empty
    Spec.assertEqWith
      s
      "CR 511.3 clears the declared-blocker record"
      (Combat.Type.declaredBlockers (GameState.combat (Combat.clearCombat busy {GameState.combat = (GameState.combat busy) {Combat.Type.declaredBlockers = Set.fromList mine}})))
      Set.empty
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
    Spec.assertEqWith s "no defending player" (Combat.Type.defenders (GameState.combat after)) []
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
-- Ray of Command is this group's producer for the control-change clause: {3}{U},
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
removalAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
removalAbility printing = case Face.activatedAbilities (S.combinedFace printing) of
  [_, ability] -> Just ability
  _ -> Nothing

-- That ability's "target" slot: CR 601.2c's narrowing, reached for an
-- activated ability through CR 602.2b, which for this card is Pool.Creatures
-- under `Or [IsAttacking, IsBlocking]`.
removalTargetSlot :: ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe TargetSlot.TargetSlot
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
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
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
-- TARGETED producer: "{T}: Add {C}. / {4}, {T}: Remove target attacking or
-- blocking creature from combat." (Land, Murders at Karlov Manor Commander;
-- oracle text checked against Scryfall.) The Save Point group below is the
-- swept-set half of the same opcode.
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

-- Save Point's only activated ability, read off the JSON-loaded printing for
-- removalAbility's reason.
savePointAbility :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
savePointAbility printing = case Face.activatedAbilities (S.combinedFace printing) of
  [ability] -> Just ability
  _ -> Nothing

-- mazeAnswer's shape, and stateful for mazeAnswer's reason -- but with no target
-- and no mana to choose: Save Point's whole cost is sacrificing itself, so the
-- activation either happens or the rider refused it, and nothing else can
-- explain a leg where it did not.
savePointAnswer ::
  ObjectId.ObjectId ->
  ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) ->
  Prompt.Prompt r ->
  State.State Bool r
savePointAnswer pointId ability p = case p of
  Prompt.ChooseAction _ _ actions -> do
    tried <- State.get
    if tried || notElem (A.Activate pointId ability) actions
      then pure A.Pass
      else do
        State.put True
        pure (A.Activate pointId ability)
  _ -> pure (S.aggressiveAnswer p)

-- CR 506.4 read over a SET rather than one named permanent, which is what
-- Effect.RemoveFromCombat's ObjectRef payload buys and what Labyrinth of Skophos
-- above cannot show: its ability names one target, so a fold over a one-element
-- group and a reader that takes the head agree on every board it builds.
--
-- Save Point, {1}{W} Enchantment (Unknown Event, a Mystery-Booster-style
-- playtest card; oracle text checked against Scryfall): "When this enchantment
-- enters, draw a card. / Sacrifice this enchantment: Remove each creature from
-- combat and untap each creature that attacked this turn. There is an additional
-- combat phase after this one. Activate only during combat before combat damage
-- has been dealt."
--
-- "Each creature" is CR 109.2's unqualified description -- every creature on the
-- battlefield, both players' -- so ObjectRef.EachMatching carries it with no
-- context-relative atom in the filter at all.
--
-- THE BOARD: alice attacks with two Goblin Pikers and bob blocks the first with
-- one of his own. TWO attackers, because one cannot tell a sweep from a reader
-- that removed the head and stopped; a blocker, so CR 509.1h's asymmetry is on
-- the board; and bob's life is the falsifier -- the UNBLOCKED Piker is the one a
-- head-only reader would leave in combat, so 20 against 18 separates the sweep
-- from both a partial removal and a no-op.
--
-- The activation happens in the declare blockers step, which is inside the
-- rider's window (Pawl.ActivateSpec's Save Point case is what pins the window
-- itself).
savePointSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
savePointSpec s registry = Spec.describe s "Save Point" $ do
  Spec.it s "CR 506.4/109.2 whole card: Save Point removes EVERY creature from combat, so no combat damage is dealt" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    savePoint <- S.printingOf s registry "Save Point"
    let (gs0, ours, theirs) = S.combatBoardOf [piker, piker] [piker]
    case (savePointAbility savePoint, ours, theirs) of
      (Just ability, [blocked, unblocked], [blocker]) -> do
        let (pointId, staged) = S.addCreature savePoint S.alice gs0
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer staged
            atEnd = runToEndOfCombatWith (savePointAnswer pointId ability) atBlockers
            idle = runToEndOfCombat S.aggressiveAnswer atBlockers
            -- atBlockers is the declare blockers step BEFORE its turn-based
            -- action, so the blocks the legs below run under are read one step
            -- on, where nothing has died yet.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer atBlockers
        Spec.assertBool s (Set.member blocker (Combat.blockersOf blocked atDamage)) "the fixture really blocked the first Piker"
        Spec.assertBool s (not (Combat.isBlocked unblocked atDamage)) "and left the second one unblocked"
        Spec.assertBool s (not (S.onBattlefield pointId atEnd)) "the ability really was activated: Save Point sacrificed itself"
        -- The sweep, at gameplay level. A reader that removed only the head of
        -- the group would leave the UNBLOCKED Piker attacking and bob would take
        -- its 2, which is the same 18 a no-op leaves.
        Spec.assertEqWith s "CR 510.1a: bob takes nothing, so the unblocked Piker left combat too" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertEqWith s "CR 506.4: nothing is an attacking creature any more" (Combat.Type.attackers (GameState.combat atEnd)) Map.empty
        Spec.assertEqWith s "CR 506.4: and the blocker is blocking nothing" (Combat.blockersOf blocked atEnd) Set.empty
        Spec.assertBool s (all (`S.onBattlefield` atEnd) [blocked, unblocked, blocker]) "so the blocked pair never traded"
        Spec.assertEqWith s "control leg: unactivated, bob takes the unblocked Piker's 2" (S.lifeOf S.bob idle) (Just 18)
        Spec.assertBool s (not (S.onBattlefield blocked idle) && not (S.onBattlefield blocker idle)) "control leg: and the blocked pair trades"
      _ -> Spec.assertFailure s "fixture should give alice two Pikers and a Save Point, and bob a blocker"
  Spec.it s "CR 701.26b/500.8 the same activation untaps both attackers and adds a combat phase after this one" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    savePoint <- S.printingOf s registry "Save Point"
    let (gs0, ours, theirs) = S.combatBoardOf [piker, piker] [piker]
    case (savePointAbility savePoint, ours, theirs) of
      (Just ability, [blocked, unblocked], [_]) -> do
        let (pointId, staged) = S.addCreature savePoint S.alice gs0
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer staged
            atEnd = runToEndOfCombatWith (savePointAnswer pointId ability) atBlockers
            idle = runToEndOfCombat S.aggressiveAnswer atBlockers
            nextPhase g = GameState.phase (S.runPure S.identityAnswer g Engine.runStep)
        Spec.assertEqWith s "CR 508.1f: attacking tapped them both" (fmap (`tapStateOf` atBlockers) [blocked, unblocked]) [Just TapState.Tapped, Just TapState.Tapped]
        Spec.assertEqWith s "CR 701.26b: both are untapped, so the sweep reached the blocked one and the unblocked one" (fmap (`tapStateOf` atEnd) [blocked, unblocked]) [Just TapState.Untapped, Just TapState.Untapped]
        Spec.assertEqWith s "CR 500.8: a second combat phase follows this one" (nextPhase atEnd) (Phase.Combat CombatStep.BeginningOfCombat)
        Spec.assertEqWith s "control leg: unactivated, the surviving attacker stays tapped" (tapStateOf unblocked idle) (Just TapState.Tapped)
        Spec.assertEqWith s "control leg: and the postcombat main phase follows" (nextPhase idle) Phase.PostcombatMain
      _ -> Spec.assertFailure s "fixture should give alice two Pikers and a Save Point, and bob a blocker"

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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  combatLegalitySpec s registry
  keywordSpec s registry
  firstStrikeSpec s registry
  endOfCombatSpec s registry
  m2bExitSpec s registry
  vigilanceSpec s registry
  attacksAloneSpec s registry
  boundedDeclarationSpec s registry
  controlChangeRemovalSpec s registry
  typeChangeRemovalSpec s registry
  effectRemovalSpec s registry
  savePointSpec s registry

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Combat over costs and requirements to attack or block (CR 508.1,
-- CR 509.1): attack and block costs, entering blocking, exert, Alluring Siren,
-- Public Enemy, a random opponent, and the declaration retry. Split out of
-- Pawl.CombatEffectSpec, which keeps the machinery.
module Pawl.CombatCostSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Pawl.CombatEffectSpec (addForests, attackThePlaneswalker, attackersOf, attacking, cursing, cursingBoard, imprisoning, runToEndOfCombat, tapStateOf, withPermanents)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import Pawl.PlaneswalkerCombatSpec (allTapped, allUntapped, announcesWay, atLife, jaceBoard, stillThere)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BecameBlocking as BecameBlocking
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

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
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "the Piker really was declared" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allTapped forests after) "CR 508.1j: both Forests paid for it"
  Spec.it s "CR 508.1j the way the attacker's controller announced is the way the toll is paid" $ do
    -- The choice rule 118.13 does not state a moment for. Its three moments are a
    -- cast or an activation (118.13a), a cost paid during a resolution (118.13b)
    -- and a special action (118.13c); CR 508.1j's toll is none of them, and the
    -- choice is still the payer's, so Cost.announceToll asks immediately before
    -- CR 508.1i's window opens.
    --
    -- Norn's Annex is the card, {3}{W/P}{W/P} Artifact, "Creatures can't attack
    -- you or planeswalkers you control unless their controller pays {W/P} for
    -- each of those creatures" -- the only printing whose attack- or
    -- block-declaration cost holds a symbol payable in more than one way.
    --
    -- ONE Plains and twenty life, so both of CR 107.4f's routes are payable and
    -- the two legs differ in nothing but the answer. The Plains is what makes it
    -- a real prompt: with no white source `announcesWay` would fall through to
    -- the single life offer and both legs would read the same.
    annex <- S.printingOf s registry "Norn's Annex"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, lands) = imprisoning annex plains S.bob [piker] 1
        legOf way = S.runPure (announcesWay way) gs (Combat.declareAttackers S.manaPerformer S.alice)
        manaLeg = legOf PhyrexianPayment.PaysMana
        lifeLeg = legOf PhyrexianPayment.PaysLife
    Spec.assertEqWith s "CR 107.4f the life route was announced, so alice paid 2 life" (S.lifeOf S.alice lifeLeg) (Just 18)
    Spec.assertBool s (allUntapped lands lifeLeg) "and the Plains is still untapped"
    Spec.assertEqWith s "CR 107.4f the mana route was announced on the same board, so alice's life is untouched" (S.lifeOf S.alice manaLeg) (Just 20)
    Spec.assertBool s (allTapped lands manaLeg) "and the Plains paid instead"
    -- Both legs PAID, so the difference above is the announcement and not one leg
    -- failing CR 508.1j and rewinding the declaration.
    Spec.assertEqWith s "CR 508.1k the mana leg's Piker attacks" (S.attackerDeclarationsOf manaLeg) mine
    Spec.assertEqWith s "and so does the life leg's" (S.attackerDeclarationsOf lifeLeg) mine
  Spec.it s "CR 508.1h two taxed attackers at 3 life: neither {W/P} may take the life route" $ do
    -- What makes the announcement a question about the WHOLE toll rather than
    -- about one charge: CR 508.1h totals the shares, and CR 118.3 makes the total
    -- one demand on one life total. Two Pikers under Norn's Annex owe {W/P}{W/P},
    -- and alice has no white source and 3 life -- enough for ONE life route and
    -- not for two, so neither may be offered and the toll is unpayable.
    --
    -- The declaration is then rewound by CR 508.1's preamble and remade, and
    -- with `aggressiveAnswer` naming the same two creatures every time it ends
    -- in no attack at all. A per-charge measure would offer each {W/P} its life
    -- route on the strength of the 3 life the other one is also spending, and
    -- alice would end the step at -1.
    annex <- S.printingOf s registry "Norn's Annex"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _, _) = imprisoning annex plains S.bob [piker, piker] 0
        after = S.runPure (announcesWay PhyrexianPayment.PaysLife) (atLife S.alice 3 gs) (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "CR 118.3 alice's 3 life pays neither symbol, so nothing was spent" (S.lifeOf S.alice after) (Just 3)
    Spec.assertEqWith s "CR 508.1 and the rewound declaration ends with no attacker" (S.attackerDeclarationsOf after) []
  Spec.it s "CR 508.1 the same board WITHOUT the Prison pays nothing" $ do
    -- The control for the test above, and the reason it is not vacuous: attacking
    -- is free by default (CR 508.1f: "tapping a creature when it's declared as an
    -- attacker isn't a cost"), so the Prison is what tapped the Forests.
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, _) = S.combatBoardOf [piker] []
        (forests, board) = addForests forest 2 gs
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "the Piker still attacks" (S.attackerDeclarationsOf after) mine
    Spec.assertBool s (allUntapped forests after) "and no Forest was tapped"
  Spec.it s "CR 508.1j Hollow Warrior cannot tap a creature declared alongside it" $ do
    -- The attacking half of Hollow Warrior's criterion. It needs VIGILANCE to be
    -- observable at all: CR 508.1f taps every chosen creature before CR 508.1h-j
    -- determine and pay, so on a vanilla board Not IsTapped already excludes the
    -- co-attackers and the two readings of the criterion agree. Windseeker
    -- Centaur is a vanilla 2/2 with vigilance, so it is untapped at CR 508.1j and
    -- CR 508.1k has not yet made it attacking -- which is exactly the creature
    -- Not IsAttacking would wrongly admit.
    --
    -- The pair is the proof, not the first board alone: the SAME two creatures
    -- with only the Warrior declared must pay and attack, so the negative below
    -- cannot be an unpayable toll for some unrelated reason.
    warrior <- S.printingOf s registry "Hollow Warrior"
    centaur <- S.printingOf s registry "Windseeker Centaur"
    let (gs, mine, _) = S.combatBoardOf [warrior, centaur] []
    case mine of
      [hollow, vigilant] -> do
        let -- Declares the Warrior alone, leaving the Centaur home to pay.
            warriorOnly :: Prompt.Prompt r -> r
            warriorOnly p = case p of
              Prompt.DeclareAttackers {} -> [hollow]
              _ -> S.aggressiveAnswer p
            bothFought = S.runCombat S.aggressiveAnswer gs
            aloneFought = S.runCombat warriorOnly gs
            bothDeclared = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)
            aloneDeclared = S.runPure warriorOnly gs (Combat.declareAttackers S.manaPerformer S.alice)
        Spec.assertEqWith s "CR 508.1j: with both declared there is nobody left to tap, so neither attacks and bob takes nothing" (S.lifeOf S.bob bothFought) (Just 20)
        Spec.assertEqWith s "and the record agrees: nothing is attacking" (attackersOf bothDeclared) []
        Spec.assertBool s (allUntapped [vigilant] bothDeclared) "CR 508.1j is all-or-nothing: the Centaur was not tapped for a declaration that failed"
        Spec.assertEqWith s "the control: the Centaur stays home, pays, and the Warrior's four go through" (S.lifeOf S.bob aloneFought) (Just 16)
        Spec.assertEqWith s "and only the Warrior attacked" (attackersOf aloneDeclared) [hollow]
        Spec.assertBool s (not (allUntapped [vigilant] aloneDeclared)) "CR 701.26a: the Centaur is what paid the toll"
      _ -> Spec.assertFailure s "fixture should have a Warrior and a Centaur"
  Spec.it s "CR 508.1h the total scales with the declaration: two attackers owe {4}" $ do
    -- CR 508.1h totals the WHOLE declaration, which is what makes Ghostly Prison's
    -- "for each creature they control that's attacking you" a multiplication.
    -- Ghostly Prison's own Two-Headed Giant ruling states the same arithmetic from
    -- the other end: "you still only have to pay once per creature."
    prison <- S.printingOf s registry "Ghostly Prison"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, forests) = imprisoning prison forest S.bob [piker, piker] 4
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)
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
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)
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
        ((_, after), asked) = State.runState (Engine.runGame retryAttackAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)) 0
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
        after = S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)
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
        atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.manaPerformer S.alice)
        atBob = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
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
        atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.manaPerformer S.alice)
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
        after1 = S.runPure S.aggressiveAnswer one (Combat.declareAttackers S.manaPerformer S.alice)
        (two0, mine2, f2) = imprisoning sphere forest S.bob [piker] 2
        two = snd (S.addCreature megrim S.bob two0)
        after2 = S.runPure S.aggressiveAnswer two (Combat.declareAttackers S.manaPerformer S.alice)
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
            atJace = S.runPure attackThePlaneswalker board (Combat.declareAttackers S.manaPerformer S.alice)
            atBob = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
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
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
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
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
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
        after = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "the land is still on the battlefield" (stillThere forests after) 1
    Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
    Spec.assertEqWith s "and nothing is attacking" (Combat.Type.attackers (GameState.combat after)) Map.empty
    Spec.assertBool s (allUntapped mine after) "CR 508.1f's tapping was undone too"
  Spec.it s "CR 508.1j the payer orders the two taxing permanents: Hollow Warrior before Exalted Dragon" $ do
    -- CR 508.1j's "in any order" read across TAXING PERMANENTS rather than across
    -- one permanent's parts: an Exalted Dragon ("sacrifice a land") and a Hollow
    -- Warrior ("tap an untapped creature you control") attacking together owe two
    -- charges, and the payer says which is paid first.
    --
    -- Dryad Arbor is the whole board: alice's ONLY land and, once CR 508.1f has
    -- tapped the two attackers, her only untapped creature. So each charge has
    -- exactly one candidate and neither component prompts -- what the payer picks
    -- is nothing but the order, and no answer to a later prompt can repair it.
    --
    -- Warrior first taps the Arbor and the Dragon then sacrifices it, a land being
    -- no less a land for being tapped. Dragon first sacrifices it and leaves the
    -- Warrior nothing untapped to tap, so CR 508.1j's "partial payments are not
    -- allowed" rewinds the whole declaration.
    dragon <- S.printingOf s registry "Exalted Dragon"
    warrior <- S.printingOf s registry "Hollow Warrior"
    arbor <- S.printingOf s registry "Dryad Arbor"
    let (gs, mine, _) = S.combatBoardOf [dragon, warrior] []
        (tree, board) = S.addCreature arbor S.alice gs
        -- Declares the two taxed creatures and leaves the Arbor home; the Arbor
        -- is a legal attacker too, and one that attacked would be tapped and so
        -- out of the Warrior's reach.
        ordering :: [Natural] -> Prompt.Prompt r -> r
        ordering order p = case p of
          Prompt.DeclareAttackers {} -> mine
          Prompt.OrderCombatTolls {} -> order
          _ -> S.identityAnswer p
        warriorFirst = S.runPure (ordering [1, 0]) board (Combat.declareAttackers S.manaPerformer S.alice)
        dragonFirst = S.runPure (ordering [0, 1]) board (Combat.declareAttackers S.manaPerformer S.alice)
    Spec.assertEqWith s "the payer's order: both were declared" (S.attackerDeclarationsOf warriorFirst) mine
    Spec.assertEqWith s "and the Arbor paid both charges" (stillThere [tree] warriorFirst) 0
    Spec.assertEqWith s "the gathered order: nothing was declared" (S.attackerDeclarationsOf dragonFirst) []
    Spec.assertEqWith s "and CR 508.1j gave the Arbor back" (stillThere [tree] dragonFirst) 1
    Spec.assertBool s (allUntapped [tree] dragonFirst) "the Warrior's tap was rewound with it"

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
-- Pawl.Support.runCombat with a STATEFUL answerer, which the Hollow Warrior
-- rewind case below needs and a pure Prompt r -> r cannot give: CR 509.1's
-- preamble asks for a FRESH declaration, and an answerer that cannot tell the
-- second prompt from the first can only repeat itself into the CR 509.1c
-- degradation. The Int is the number of declare-blockers prompts answered.
runCombatCounting :: (forall r. Prompt.Prompt r -> State.State Int r) -> GameState.GameState -> GameState.GameState
runCombatCounting answer gs0 =
  let inCombat p = case p of
        Phase.Combat _ -> True
        _ -> False
      go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || not (inCombat (GameState.phase g))
          then pure g
          else Engine.runGame answer g Engine.runStep >>= (go (n - 1) . snd)
   in State.evalState (go 24 gs0) 0

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
-- Oppressive Rays' third line, "activated abilities of enchanted creature cost
-- {3} more to activate", is transcribed too; Pawl.PlayerEffectSpec's Oppressive
-- Rays group is where it is proved.
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
            after = S.runPure S.aggressiveAnswer board (Combat.declareBlockers S.manaPerformer)
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
            after = S.runPure S.aggressiveAnswer board (Combat.declareBlockers S.manaPerformer)
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
            after = S.runPure S.aggressiveAnswer board (Combat.declareBlockers S.manaPerformer)
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
            ((_, after), asked) = State.runState (Engine.runGame (retryBlockAnswer free) board (Combat.declareBlockers S.manaPerformer)) 0
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
            after = S.runPure blockBoth board (Combat.declareBlockers S.manaPerformer)
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
    -- on the blocking side. SYNTHETIC, and it stays so now that Hollow Warrior is
    -- in the pool: this case wants a non-mana block toll on an AURA subject, which
    -- Hollow Warrior (Affected.Matching Filter.IsSource) does not give, and a
    -- SACRIFICE rather than a tap.
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
            after = S.runPure S.aggressiveAnswer board (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "CR 509.1f: one of the two lands was sacrificed" (stillThere forests after) 1
        Spec.assertEqWith s "and the block really was declared" (blockersOf attacker after) (Set.singleton blocker)
        Spec.assertBool s (allUntapped (filter (\oid -> Set.member oid (GameState.battlefield after)) forests) after) "the surviving land was not tapped: this toll is not mana"
      _ -> Spec.assertFailure s "fixture should have an attacker and a blocker"
  Spec.it s "CR 509.1f Hollow Warrior cannot tap itself to pay for its own block" $ do
    -- Hollow Warrior: "This creature can't attack or block unless you tap an
    -- untapped creature you control not declared as an attacking or blocking
    -- creature this combat." Its 2004-10-04 ruling is this case in words: "You
    -- have to tap a different creature which is not being declared as an attacker
    -- or blocker."
    --
    -- bob's ONLY creature is the Warrior, and that is the discrimination rather
    -- than a convenience. CR 509 has no analogue of CR 508.1f's tapping, so the
    -- Warrior is untapped when CR 509.1f pays whatever the criterion says, and CR
    -- 509.1g has not yet made it a blocking creature. Written Not IsBlocking the
    -- criterion would therefore admit the Warrior ITSELF -- one candidate for one
    -- tap, so Cost.tapCandidates elides the choice -- and the block would stand.
    -- A second untapped creature under bob lets both readings pay, and collapses
    -- the case to green.
    --
    -- A 2/1 attacker so the life delta is unambiguous, and a 4/4 blocker so a
    -- block that stands really does stop it.
    warrior <- S.printingOf s registry "Hollow Warrior"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [piker] [warrior]
    case (mine, theirs) of
      ([attacker], [blocker]) -> do
        let fought = S.runCombat S.aggressiveAnswer gs
            declared = S.runPure S.aggressiveAnswer (S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)) (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "CR 509.1f: the toll cannot be paid, so nothing blocks and bob takes the Piker's two" (S.lifeOf S.bob fought) (Just 18)
        Spec.assertEqWith s "and the record says so: no blocker was declared" (blockersOf attacker declared) Set.empty
        Spec.assertBool s (allUntapped [blocker] declared) "CR 509.1f is all-or-nothing: the Warrior did not tap itself"
      _ -> Spec.assertFailure s "fixture should have a Piker and a Warrior"
  Spec.it s "CR 509.1 the rewind unwrites the declaration a failed toll named" $ do
    -- The case that separates CR 509.1's preamble from a record that merely
    -- accumulates. bob's ONLY creatures are two Hollow Warriors, so the first
    -- declaration -- both of them -- owes two taps and has nobody to tap, and
    -- CR 509.1f leaves it unpaid. The preamble puts the game back to before it,
    -- which since #2024 includes unwriting Combat.declaredBlockers.
    --
    -- The second declaration is where that matters: with only the first Warrior
    -- blocking, the second is untapped and no longer declared, so it can pay.
    -- A rewind that left the first declaration's ids standing would exclude it,
    -- the toll would fail again, and CR 509.1c's degradation would end in no
    -- blocks at all.
    --
    -- A STATEFUL answerer, because a pure one cannot make a second, different
    -- declaration: it answers both prompts the same way and the case collapses.
    warrior <- S.printingOf s registry "Hollow Warrior"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = S.combatBoardOf [piker] [warrior, warrior]
    case (mine, theirs) of
      ([attacker], [first, second]) -> do
        let answering :: Prompt.Prompt r -> State.State Int r
            answering p = case p of
              Prompt.DeclareBlockers _ _ _ attackers -> do
                seen <- State.get
                State.put (seen + 1)
                pure $ case attackers of
                  [] -> Map.empty
                  a : _ ->
                    if seen == (0 :: Int)
                      then Map.fromList [(first, Set.singleton a), (second, Set.singleton a)]
                      else Map.singleton first (Set.singleton a)
              _ -> pure (S.aggressiveAnswer p)
            fought = runCombatCounting answering gs
            declared = State.evalState (fmap snd (Engine.runGame answering (S.runPure S.aggressiveAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)) (Combat.declareBlockers S.manaPerformer))) 0
        Spec.assertEqWith s "CR 509.1: the second declaration pays and blocks, so bob takes nothing" (S.lifeOf S.bob fought) (Just 20)
        Spec.assertEqWith s "and the block that stands is the second declaration's, not the first" (blockersOf attacker declared) (Set.singleton first)
        Spec.assertBool s (not (allUntapped [second] declared)) "CR 509.1f: the Warrior the rewind released is what paid"
      _ -> Spec.assertFailure s "fixture should have a Piker and two Warriors"
  Spec.it s "CR 509.1f partial payments are not allowed: two taxed blockers and one land sacrifice nothing" $ do
    -- CR 509.1d totals over the chosen creatures, so two taxed blockers owe two
    -- lands and one land cannot pay. The land sacrificed while the toll was being
    -- paid comes back: Cost.payToll restores what a half-paid toll spent, and CR
    -- 509.1's preamble restore beside it puts back the one thing declareBlockers
    -- writes ahead of the payment (Combat.declaredBlockers).
    tithe <- S.printingOf s registry "Synthetic Blocking Tithe"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, mine, theirs) = attacking [piker] [piker, piker]
    case (mine, theirs) of
      ([attacker], [one, two]) -> do
        let (forests, board) = addForestsFor S.bob forest 1 (raying tithe two (raying tithe one gs))
            after = S.runPure S.aggressiveAnswer board (Combat.declareBlockers S.manaPerformer)
        Spec.assertEqWith s "the land is still on the battlefield" (stillThere forests after) 1
        Spec.assertEqWith s "and nothing blocked" (blockersOf attacker after) Set.empty
      _ -> Spec.assertFailure s "fixture should have an attacker and two blockers"

-- Block the first attacker with ONE named creature, and pay a tap toll with one
-- named permanent, filtered against what the prompt actually offers.
--
-- Not S.aggressiveAnswer, which blocks with every creature bob controls: that
-- declares the spare a blocker too, and Hollow Warrior's criterion excludes a
-- creature declared as a blocker this combat -- Combat.declaredBlockers is
-- written ahead of CR 509.1f's payment. The toll would then have nobody to tap
-- on any of the four boards below, and the case would read the same under both
-- readings of the gate.
blockingWith :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
blockingWith who tapping p = case p of
  Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.singleton who (Set.singleton a)
  -- Ashaya's own text is "NONTOKEN creatures you control", not "other", so on
  -- the boards that carry it Ashaya is itself an untapped undeclared creature
  -- bob controls and the count-1 toll has two candidates. Pinned by identity
  -- rather than by position, so the case reads the same whichever order
  -- Cost.tapCandidates hands them over in.
  Prompt.ChooseTaps _ _ _ candidates _ -> Set.fromList (filter (== tapping) candidates)
  _ -> S.aggressiveAnswer p

-- CR 305.7 as the SIX readers in this module read it, which is one shared gate:
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
-- No reader in this module is left without one, Hollow Warrior having supplied
-- the last thing missing: a cost to block printed on a nontoken creature, which
-- is what Ashaya can animate. Pawl.Engine.PlayerEffect's share is pinned in
-- Pawl.PlayerEffectSpec and Pawl.Engine.SacrificeRestriction's in
-- Pawl.SacrificeRestrictionSpec.
--
-- A BOARD THAT CANNOT DISCRIMINATE, recorded because it looks like the obvious
-- one: Glacial Crasher ("this creature can't attack unless you control a
-- Mountain") is no witness for the restriction reader, since Blood Moon makes
-- every nonbasic land a Mountain and the gate is satisfied whether or not the
-- Crasher's own sentence survived. The block-cost case has a second such trap of
-- its own, which is why it carries the blockingWith answerer above rather than
-- S.aggressiveAnswer.
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
        declared b = S.runPure S.aggressiveAnswer b (Combat.declareAttackers S.manaPerformer S.alice)
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
  Spec.it s "CR 305.7 an animated Hollow Warrior set to Mountain costs nothing to block" $ do
    -- Pawl.Engine.BlockCost.costsOn, this module's sixth reader of the shared
    -- gate and the last to get a case. Discriminating it wants a cost to BLOCK
    -- printed on a nontoken creature, since Ashaya animates nontoken creatures.
    -- Hollow Warrior {4} 4/4 ("This creature can't attack or block unless you
    -- tap an untapped creature you control not declared as an attacking or
    -- blocking creature this combat") is the only such printing in
    -- data/cards/: the other two files carrying a Face.blockCosts entry,
    -- oppressive-rays.json and synthetic-blocking-tithe.json, are Auras
    -- whose subject is the enchanted creature, which Ashaya can never reach.
    --
    -- Both extras go under BOB, since CR 509.1a chooses blockers from the
    -- DEFENDING player's creatures and Ashaya animates its own controller's.
    --
    -- The toll is read off the spare Piker's tap state, which is CR 509.1f's
    -- payment exactly. The spare is part of the fixture rather than one of the
    -- extras because it has to stand on all four boards: without an eligible
    -- creature the toll is unpayable and the base leg reads False for the wrong
    -- reason.
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    bloodMoon <- S.printingOf s registry "Blood Moon"
    warrior <- S.printingOf s registry "Hollow Warrior"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = attacking [piker] [warrior, piker]
        with extras = withPermanents S.bob extras base
    case (mine, theirs) of
      ([attacker], [blocker, spare]) -> do
        let declared b = S.runPure (blockingWith blocker spare) b (Combat.declareBlockers S.manaPerformer)
            paid b = allTapped [spare] (declared b)
            stripped = with [ashaya, bloodMoon]
        Spec.assertEqWith
          s
          "the tap is paid until Ashaya and Blood Moon are both on the battlefield"
          (paid base, paid (with [ashaya]), paid (with [bloodMoon]), paid stripped)
          (True, True, True, False)
        -- Two anti-vacuity legs, and the first is the one that matters: nothing
        -- was tapped because nothing was owed, not because the declaration was
        -- refused for want of a creature to tap (CR 509.1's preamble unwrites an
        -- unpayable declaration whole).
        Spec.assertEqWith s "the Warrior blocked anyway" (blockersOf attacker (declared stripped)) (Set.singleton blocker)
        Spec.assertBool s (allUntapped [spare] (declared stripped)) "and the spare never went"
      _ -> Spec.assertFailure s "fixture should have an attacker, a Warrior and a spare"

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

-- CR 509.1g's blocker-side event as a DECLARATION recorded it. CR 509.4's entry
-- records the same event with the flag set, and the flag is exactly what keeps
-- CR 509.3b off it, so a test asking "was anything declared" has to read the
-- flag rather than the constructor.
blockerWasDeclared :: GameEvent.GameEvent -> Bool
blockerWasDeclared e = case e of
  GameEvent.BecameBlocking b -> not (BecameBlocking.putOntoBattlefield b)
  _ -> False

-- Its complement: CR 509.4's own producer, which CR 509.3d fires off.
blockerEnteredBlocking :: GameEvent.GameEvent -> Bool
blockerEnteredBlocking e = case e of
  GameEvent.BecameBlocking b -> BecameBlocking.putOntoBattlefield b
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
        Spec.assertBool s (not (any (blockerWasDeclared . LoggedEvent.event) (GameState.events cast))) "no blocker was declared for it"
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
        Spec.assertBool s (any (blockerWasDeclared . LoggedEvent.event) (GameState.events declared)) "declaration leg: and CR 509.3d's event IS recorded there"
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

-- alice attacks with a Spined Thopter and a Sacred Prey; bob defends with NO
-- creature at all, holds Flash Foliage and the three Forests that pay its
-- {2}{G}, and has one card left in his library so the spell's draw is not a CR
-- 104.3c loss. Returns the two attackers and the spell.
--
-- No blocker for bob is the point: it removes the declaration route entirely, so
-- nothing on this board can confer blocking status except the spell.
foliageBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], ObjectId.ObjectId)
foliageBoard forest thopter prey foliage =
  let (gs0, ours, _) = S.combatBoardOf [thopter, prey] []
      lands = List.foldl' (\g _ -> snd (S.addCreature forest S.bob g)) gs0 [1 :: Int, 2, 3]
      (foliageId, withCard) = S.addHandCard foliage S.bob lands
      stocked = snd (S.addLibraryCard forest S.bob withCard)
   in (stocked, ours, foliageId)

-- foliageBoard with ONE attacker of the caller's choosing and everything else
-- held fixed: the three Forests that pay {2}{G}, Flash Foliage in hand, a card
-- left in the library for its draw, and no creature for bob, so the token's
-- arrival is the only way anything becomes a blocking creature. Returns the
-- attacker.
soloFoliageBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId])
soloFoliageBoard forest attacker foliage =
  let (gs0, ours, _) = S.combatBoardOf [attacker] []
      lands = List.foldl' (\g _ -> snd (S.addCreature forest S.bob g)) gs0 [1 :: Int, 2, 3]
      (_, withCard) = S.addHandCard foliage S.bob lands
      stocked = snd (S.addLibraryCard forest S.bob withCard)
   in (stocked, ours)

-- soloFoliageBoard with bob's own JACE BELEREN added and FIVE loyalty counters
-- on him. Everything else is that fixture's -- one attacker, three Forests for
-- Flash Foliage's {2}{G}, the spell in hand, one card left in the library so its
-- draw is not a CR 104.3c loss, and no creature for bob, so nothing can become a
-- blocker except the token.
--
-- ONE attacker is the point: which of CR 508.1b's subjects it is announced at is
-- then the ONLY legal target the board could offer, so a leg that announces it
-- at Jace offers Flash Foliage no target at all rather than a different one.
--
-- Jace is what makes CR 509.1a's and CR 802.4a's three-way subject list
-- observable at all: bob is the defending player either way (CR 508.5), and only
-- a planeswalker on the board separates "attacking you" from it.
--
-- Returns the state, the attacker and Jace.
planeswalkerFoliageBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
planeswalkerFoliageBoard forest jace attacker foliage =
  let (gs0, mine, theirs) = S.combatBoardOf [attacker] [jace]
      lands = List.foldl' (\g _ -> snd (S.addCreature forest S.bob g)) gs0 [1 :: Int, 2, 3]
      (_, withCard) = S.addHandCard foliage S.bob lands
      stocked = snd (S.addLibraryCard forest S.bob withCard)
   in case (mine, theirs) of
        ([attackerId], [jaceId]) -> Just (S.addCounter CounterKind.Loyalty 5 jaceId stocked, attackerId, jaceId)
        _ -> Nothing

-- Decline every block -- bob has nothing to declare anyway -- cast whatever is
-- castable, and aim the target at `victim`. The offered set is FILTERED rather
-- than replaced, so a leg whose target the card's own slot does not admit takes
-- no target at all instead of quietly succeeding on a hand-built recipient.
castFoliage :: ObjectId.ObjectId -> Prompt.Prompt r -> r
castFoliage victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, rs) -> Set.filter (== Recipient.ToCreature victim) rs) sets
  Prompt.ChooseAction {} -> S.castAnswer p
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

-- alice attacks with one Icehide Golem; bob defends with Aetherplasm on the
-- battlefield and one Cabal Evangel in hand. Returns the attacker, Aetherplasm
-- and the Evangel's HAND id.
--
-- Every element is load-bearing. Both 2/2s so the trade is lethal in BOTH
-- directions -- a Golem that survived its blocker would leave the count only
-- half moved. Aetherplasm's printed 1/1 is what makes the declined leg differ:
-- left blocking, it dies to the Golem and the Golem lives. The Evangel is
-- vanilla so no trigger of any kind rides on its arrival (CR 509.3d would let
-- flanking kill the Golem by a second route), and it is bob's only card so the
-- library is never drawn from. No lands: the trigger costs nothing. Two seats:
-- nothing here reads APNAP or a second opponent. ONE attacker, so CR 510.1c's
-- divided assignment is never entered and no Prompt.AssignCombatDamage is
-- raised.
aetherBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], ObjectId.ObjectId)
aetherBoard golem plasm evangel =
  let (gs0, ours, theirs) = S.combatBoardOf [golem] [plasm]
      (evangelId, withCard) = S.addHandCard evangel S.bob gs0
   in (withCard, ours, theirs, evangelId)

-- Declare Aetherplasm blocking the Golem, answer each printed "may" by its
-- CLAUSE ORDINAL, and take the Evangel out of hand. The declaration is spelled
-- out rather than left to aggressiveAnswer so the pairing the trigger binds is
-- the test's and not the default's.
--
-- ChooseCardInHand is a real choice on the leg that takes clause 0: Aetherplasm
-- is back in hand beside the Evangel and both are creature cards, so the offer
-- carries two candidates. Pinned by FILTERING the offered set, so a leg where
-- the Evangel is not offered takes the fallback rather than succeeding on a
-- hand-built id.
aetherAnswer ::
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  ObjectId.ObjectId ->
  (ClauseIndex.ClauseIndex -> OptionalDecision.OptionalDecision) ->
  Prompt.Prompt r ->
  r
aetherAnswer plasm golem evangel decide p = case p of
  Prompt.DeclareBlockers {} -> Map.singleton plasm (Set.singleton golem)
  Prompt.ChooseOptional _ _ _ _ cIdx -> decide cIdx
  Prompt.ChooseCardInHand _ _ _ offered ->
    Maybe.fromMaybe (NonEmpty.head offered) (List.find (== evangel) (NonEmpty.toList offered))
  _ -> S.aggressiveAnswer p

-- Declare one blocker against one attacker and answer everything else the
-- default way. The declaration twin of aetherAnswer's own, for the leg that has
-- no trigger to answer.
declareBlock :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
declareBlock blocker attacker p = case p of
  Prompt.DeclareBlockers {} -> Map.singleton blocker (Set.singleton attacker)
  _ -> S.aggressiveAnswer p

-- The ids Combat.blockers holds for an attacker, narrowed to the ones still on
-- the battlefield.
--
-- The narrowing is not decoration. CR 506.4 removes a permanent from combat as
-- it leaves the battlefield, but this engine spells that lazily: nothing prunes
-- the blocker's id on the way out, and Pawl.Engine.Damage's liveness filter is
-- what makes the assignment right (Pawl.Engine.Departure's comment states the
-- reason -- a blocker DESTROYED mid-combat leaves an identical stale id no
-- pruner could see). Aetherplasm is the first board here whose blocker leaves
-- the battlefield ALIVE, so it is the first to read one.
liveBlockersOf :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
liveBlockersOf attacker gs = filter (`S.onBattlefield` gs) (Set.toList (Combat.blockersOf attacker gs))

-- The battlefield objects a player OWNS that are copies of a card with this
-- name. Owner and not controller (CR 108.3), which is the question
-- Game.zoneMembers answers -- and the two coincide on every board below, bob
-- owning and controlling everything he puts out.
--
-- By name because CR 400.7 mints a NEW object for a card that changes zones, so
-- the hand id a fixture hands back cannot name the permanent it becomes.
battlefieldNamed :: CardName.CardName -> PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
battlefieldNamed wanted pid gs =
  filter (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just wanted) (Game.zoneMembers Zone.Battlefield pid gs)

-- CR 509.4: "If a creature is put onto the battlefield blocking, its controller
-- chooses which attacking creature it's blocking as it enters the battlefield
-- (unless the effect that put it onto the battlefield specifies what it's
-- blocking). A creature put onto the battlefield this way is 'blocking' but, for
-- the purposes of trigger events and effects, it never 'blocked'."
--
-- Flash Foliage is the pool's Create-side producer -- {2}{G} INSTANT, "Cast this spell only
-- during combat after blockers are declared. / Create a 1/1 green Saproling
-- creature token that's blocking target creature attacking you. / Draw a card."
-- It is CR 509.4's parenthetical case: the effect specifies what the token
-- blocks, so nothing is prompted.
--
-- THE FLIER IS THE RULE, not decoration. A 1/1 Saproling has neither flying nor
-- reach, so CR 702.9b makes it an illegal blocker for the Spined Thopter and CR
-- 509.1b would throw out any declaration naming that pair -- which the pair of
-- legalBlockDeclaration readings below asserts on the very board where the token
-- IS blocking it. That is CR 509.4b ("A creature that's put onto the battlefield
-- blocking isn't affected by requirements or restrictions that apply to the
-- declaration of blockers") made observable.
--
-- The two attackers have DIFFERENT POWER (2 and 1), so bob's life at end of
-- combat is the discriminating quantity: 19 correct, 17 both when the rider is
-- dropped and when the entry is routed through CR 509.1's legality. Those two
-- are told apart by the second leg, which the legality reading leaves green (its
-- attacker has no flying) and the dropped rider reddens at alice-21.
--
-- The second leg is also what catches an implementation that IGNORES the slot
-- and attaches the token to some other attacker: measured against
-- Map.lookupMin of Combat.attackers it leaves leg one green, the Thopter
-- holding the lower object id, and reddens leg two's alice-21.
--
-- Sacred Prey ("Whenever this creature becomes blocked, you gain 1 life") is CR
-- 509.3c's observer, and its trigger is what proves the THIRD producer of
-- GameEvent.AttackerBlocked: an attacker that was unblocked becomes blocked when
-- a creature is put onto the battlefield blocking it.
putOntoBattlefieldBlockingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
putOntoBattlefieldBlockingSpec s registry = Spec.describe s "PutOntoBattlefieldBlocking" $ do
  Spec.it s "CR 509.4 whole card: Flash Foliage's Saproling blocks the flier it could never have been declared against" $ do
    forest <- S.printingOf s registry "Forest"
    thopter <- S.printingOf s registry "Spined Thopter"
    prey <- S.printingOf s registry "Sacred Prey"
    foliage <- S.printingOf s registry "Flash Foliage"
    case foliageBoard forest thopter prey foliage of
      (gs, [flier, ground], _) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            -- The same answerer twice: once stopped before combat damage, where
            -- the token is still alive and the combat record can be read against
            -- it, and once run to the end, where the life totals are.
            blocking = S.runToStep (Phase.Combat CombatStep.CombatDamage) (castFoliage flier) atBlockers
            atEnd = runToEndOfCombat (castFoliage flier) atBlockers
        Spec.assertEqWith s "the leg hands over at the declare blockers step, so the spell is cast after the declaration" (GameState.phase atBlockers) (Phase.Combat CombatStep.DeclareBlockers)
        -- GAMEPLAY FIRST: the Thopter's 2 never reached bob, so the token really
        -- did block it, and only the Prey's 1 got through.
        Spec.assertEqWith s "CR 510.1c: the Thopter's 2 was assigned to the token, so bob takes only the Prey's 1" (S.lifeOf S.bob atEnd) (Just 19)
        -- Identity, still at gameplay level: the Prey is the OTHER attacker, and
        -- its becomes-blocked trigger stayed silent, so the token attached to
        -- the attacker the target slot named rather than to whichever came first.
        Spec.assertEqWith s "and the Prey was not the one blocked, so its CR 509.3c trigger never fired" (S.lifeOf S.alice atEnd) (Just 20)
        case S.tokensOf blocking of
          [saproling] -> do
            Spec.assertEqWith s "CR 509.4: the Saproling is blocking the Thopter" (Combat.blockersOf flier blocking) (Set.singleton saproling)
            Spec.assertEqWith s "and nothing is blocking the Prey" (Combat.blockersOf ground blocking) Set.empty
            -- CR 509.4b, on the very board the token is blocking the flier on.
            -- The pair differs in exactly one thing: which attacker the
            -- hypothetical declaration names.
            Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob (Map.singleton saproling (Set.singleton flier)) blocking)) "CR 702.9b / CR 509.1b: declaring that same Saproling as a blocker for the flier would have been illegal"
            Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton saproling (Set.singleton ground)) blocking) "control: and legal against the attacker without flying, so the refusal above is the flying"
            Spec.assertEqWith s "CR 506.4: the token joined combat under bob" (Map.lookup saproling (Combat.Type.joinedUnder (GameState.combat blocking))) (Just S.bob)
            Spec.assertEqWith s "CR 509.4b: and it is not tapped, CR 509.1a's condition belonging to the declaration" (tapStateOf saproling blocking) (Just TapState.Untapped)
          other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 509.3a and CR 509.3b's last sentence, at event level: the entry
        -- records no declaration.
        Spec.assertBool s (not (any (blockerWasDeclared . LoggedEvent.event) (GameState.events atEnd))) "CR 509.3a / CR 509.3b: no blocker was declared"
        -- Both traded: the Thopter's 1 toughness took the Saproling's 1, and the
        -- Saproling's 1 toughness took the Thopter's 2. That is CR 510.1c damage
        -- being assigned in both directions, which only a real blocker gets.
        Spec.assertBool s (not (S.onBattlefield flier atEnd)) "CR 510.1c: the Thopter took the Saproling's 1 and died"
        Spec.assertBool s (S.onBattlefield ground atEnd) "and the unblocked Prey is untouched"
        Spec.assertEqWith s "and bob drew the card the spell says to draw" (length (Game.zoneMembers Zone.Library S.bob atEnd)) 0
      _ -> Spec.assertFailure s "fixture should have two attackers"
  Spec.it s "CR 509.3c the attacker it is put onto the battlefield blocking becomes blocked" $ do
    -- The same board and the same answerer; the ONE difference is which
    -- attacker the target slot names. Now the Prey is the one blocked, so CR
    -- 509.3c's third producer fires and alice gains 1.
    forest <- S.printingOf s registry "Forest"
    thopter <- S.printingOf s registry "Spined Thopter"
    prey <- S.printingOf s registry "Sacred Prey"
    foliage <- S.printingOf s registry "Flash Foliage"
    case foliageBoard forest thopter prey foliage of
      (gs, [flier, ground], _) -> do
        let atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            blocking = S.runToStep (Phase.Combat CombatStep.CombatDamage) (castFoliage ground) atBlockers
            atEnd = runToEndOfCombat (castFoliage ground) atBlockers
            -- The control: the same board, the same declined blocks, nothing
            -- cast. Both attackers connect.
            uncast = runToEndOfCombat declineBlocks atBlockers
        Spec.assertEqWith s "CR 509.3c: the Prey was an unblocked creature and a creature put onto the battlefield blocking it made it blocked" (S.lifeOf S.alice atEnd) (Just 21)
        Spec.assertEqWith s "CR 510.1c: so the Prey's 1 was assigned to the token and bob takes only the Thopter's 2" (S.lifeOf S.bob atEnd) (Just 18)
        Spec.assertEqWith s "CR 509.4: the Saproling is blocking the Prey" (Combat.blockersOf ground blocking) (Set.fromList (S.tokensOf blocking))
        Spec.assertEqWith s "and nothing is blocking the Thopter" (Combat.blockersOf flier blocking) Set.empty
        Spec.assertBool s (not (any (blockerWasDeclared . LoggedEvent.event) (GameState.events atEnd))) "CR 509.3a / CR 509.3b: no blocker was declared here either"
        Spec.assertBool s (S.onBattlefield flier atEnd) "the unblocked Thopter is untouched"
        Spec.assertBool s (not (S.onBattlefield ground atEnd)) "and the Prey traded with the Saproling"
        -- The control leg, differing from the first only in whether the spell is
        -- cast: nothing is blocked, nothing triggers, and both attackers connect.
        Spec.assertEqWith s "control: with no spell bob takes 2 and 1" (S.lifeOf S.bob uncast) (Just 17)
        Spec.assertEqWith s "control: and the Prey's trigger is silent" (S.lifeOf S.alice uncast) (Just 20)
        Spec.assertEqWith s "control: nothing is blocking anything" (Combat.Type.blockers (GameState.combat uncast)) Map.empty
        Spec.assertEqWith s "control: bob's library is untouched" (length (Game.zoneMembers Zone.Library S.bob uncast)) 1
      _ -> Spec.assertFailure s "fixture should have two attackers"
  -- CR 509.1a / CR 802.4a: "attacking you" is CR 508.1b's PLAYER and not CR
  -- 508.5's defending player. Both rules write the subject list as three
  -- separate things -- "attacking that player, a planeswalker they control, or a
  -- battle they protect" -- so a creature attacking bob's planeswalker is not a
  -- creature attacking bob, however squarely bob is its defending player.
  --
  -- Flash Foliage's target slot is Filter.IsAttackingPlayer You, and this is the
  -- board that pays for it. Spelled And [IsAttacking, ControlledBy Opponent] --
  -- what the card carried before -- the Thopter is a legal target on the
  -- planeswalker leg, the spell resolves, a Saproling enters blocking it and bob
  -- draws, none of which printed Flash Foliage can do here: with no legal target
  -- it cannot be cast at all (CR 601.2c). Answering the atom off
  -- Pawl.Engine.Defender.playerOfAttacker (CR 508.5) reintroduces exactly that,
  -- that function folding all three of the rule's subjects onto one player.
  --
  -- A PAIR of legs off ONE board differing in exactly one thing: which of CR
  -- 508.1b's subjects the lone attacker was announced at. The control leg is what
  -- keeps the refusal from being an atom that answers False for everything.
  Spec.it s "CR 509.1a / 802.4a whole card: Flash Foliage cannot name a creature attacking a planeswalker you control" $ do
    forest <- S.printingOf s registry "Forest"
    jace <- S.printingOf s registry "Jace Beleren"
    thopter <- S.printingOf s registry "Spined Thopter"
    foliage <- S.printingOf s registry "Flash Foliage"
    case planeswalkerFoliageBoard forest jace thopter foliage of
      Just (gs, attacker, jaceId) -> do
        -- attackThePlaneswalker and S.aggressiveAnswer differ on exactly CR
        -- 508.1b's announcement and agree on every other prompt.
        let atJace = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) attackThePlaneswalker gs
            atBob = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            refused = runToEndOfCombat (castFoliage attacker) atJace
            allowed = runToEndOfCombat (castFoliage attacker) atBob
        Spec.assertEqWith s "the leg really did announce the attack at Jace (CR 508.1b)" (Map.lookup attacker (Combat.Type.attackers (GameState.combat atJace))) (Just (AttackTarget.OfPlaneswalker jaceId))
        Spec.assertEqWith s "and the control leg at bob himself" (Map.lookup attacker (Combat.Type.attackers (GameState.combat atBob))) (Just (AttackTarget.OfPlayer S.bob))
        -- GAMEPLAY FIRST, on the quantity the two readings of the slot differ
        -- on: the Thopter's 2 reached Jace, so nothing blocked it. Under the old
        -- spelling a Saproling did and this reads 5.
        Spec.assertEqWith s "CR 306.8 / 509.1a: the attacker is attacking JACE and not bob, so Flash Foliage cannot block it and its 2 comes off loyalty" (S.counterOf CounterKind.Loyalty jaceId refused) 3
        -- Still gameplay, and the reason: the spell was never cast, so its draw
        -- never happened either.
        Spec.assertEqWith s "CR 601.2c: with no legal target it was not cast, so bob never drew" (length (Game.zoneMembers Zone.Library S.bob refused)) 1
        Spec.assertEqWith s "and no token was created" (S.tokensOf refused) []
        Spec.assertEqWith s "so nothing is blocking anything" (Combat.Type.blockers (GameState.combat refused)) Map.empty
        -- Anti-vacuity, and the narrowness of the atom: the creature IS an
        -- attacking creature on the refused leg. What it is not is one attacking
        -- bob.
        Spec.assertBool s (Map.member attacker (Combat.Type.attackers (GameState.combat refused))) "CR 508.1k: it is an attacking creature all the same"
        -- The other leg: the SAME spell, the SAME board, announced at bob, goes
        -- through.
        Spec.assertEqWith s "control: announced at bob, the token blocks it and bob takes nothing" (S.lifeOf S.bob allowed) (Just 20)
        Spec.assertEqWith s "control: and bob drew the card the spell says to draw" (length (Game.zoneMembers Zone.Library S.bob allowed)) 0
        Spec.assertEqWith s "control: Jace was never attacked on that leg, so his loyalty is untouched" (S.counterOf CounterKind.Loyalty jaceId allowed) 5
      Nothing -> Spec.assertFailure s "fixture should have one attacker and one planeswalker"
  -- CR 509.3d's third sentence -- "In addition, it will trigger if a creature is
  -- put onto the battlefield blocking that creature" -- the one form of CR 509.3
  -- that CR 509.4's "never blocked" leaves standing. Rules 509.3a and 509.3b
  -- each end "It won't trigger if the creature is put onto the battlefield
  -- blocking", and this rule says the opposite in as many words.
  --
  -- CR 702.25a's flanking is the observer, and Benalish Cavalry {1}{W} 2/2
  -- carries it and nothing else. The control is Pawl.KeywordTriggerSpec's own:
  -- Icehide Golem, the same 2/2 without the keyword, so the pair differs in
  -- exactly one thing -- and both boards are the DECLARATION-free one, bob
  -- holding no creature to declare, so what fires the trigger can only be the
  -- entry.
  --
  -- Read at the combat damage step, before damage is dealt: CR 509.2a resolves
  -- these triggers in the declare blockers step, so a dead Saproling there is
  -- flanking's and never combat's.
  Spec.it s "CR 509.3d a creature put onto the battlefield blocking it triggers the attacker's becomes-blocked-by ability" $ do
    forest <- S.printingOf s registry "Forest"
    cavalry <- S.printingOf s registry "Benalish Cavalry"
    golem <- S.printingOf s registry "Icehide Golem"
    foliage <- S.printingOf s registry "Flash Foliage"
    case (soloFoliageBoard forest cavalry foliage, soloFoliageBoard forest golem foliage) of
      ((flankerBoard, [flanker]), (plainBoard, [plain])) -> do
        let toDamage victim board =
              S.runToStep
                (Phase.Combat CombatStep.CombatDamage)
                (castFoliage victim)
                (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer board)
            struck = toDamage flanker flankerBoard
            control = toDamage plain plainBoard
        -- GAMEPLAY FIRST, and the two legs read the same way: what the token is
        -- worth on the board. The flanker's Saproling took CR 702.25a's -1/-1,
        -- went to 0/0 and CR 704.5f buried it; the control's is untouched.
        Spec.assertEqWith s "CR 509.3d: flanking fired off the entry, so the 1/1 Saproling is gone before damage" (fmap (`S.powerToughnessOf` struck) (S.tokensOf struck)) []
        Spec.assertEqWith s "control: the same token, blocking the same 2/2 without flanking, is untouched" (fmap (`S.powerToughnessOf` control) (S.tokensOf control)) [Just (1, 1)]
        -- Anti-vacuity: the token did enter blocking on the leg where it is
        -- gone, so its absence is the trigger rather than a spell that fizzled.
        -- CR 509.1h keeps the attacker blocked with its blocker gone.
        Spec.assertBool s (Combat.isBlocked flanker struck) "CR 509.1h: the flanker is a blocked creature, so a token really did block it"
        Spec.assertBool s (Combat.isBlocked plain control) "and so is the control's attacker"
        -- CR 509.4's two roads, told apart on the board that took the second: no
        -- creature was DECLARED here, and the event CR 509.3d matched carries the
        -- flag that keeps CR 509.3b off it.
        -- The same split on the STATE, read on the leg where the token survives
        -- to be read at all: CR 509.1g's blocking creature, and not CR 509.1a's
        -- declared one.
        Spec.assertEqWith s "CR 509.1g: the surviving token is blocking the control's attacker" (Combat.blockersOf plain control) (Set.fromList (S.tokensOf control))
        Spec.assertEqWith s "CR 509.4b: and it is in no declaration, so it is blocking without ever having blocked" (Combat.Type.declaredBlockers (GameState.combat control)) Set.empty
        Spec.assertBool s (not (any (blockerWasDeclared . LoggedEvent.event) (GameState.events struck))) "CR 509.4: nothing was declared as a blocker on this board"
        Spec.assertBool s (any (blockerEnteredBlocking . LoggedEvent.event) (GameState.events struck)) "and the event that fired the trigger is the entry's own"
      _ -> Spec.assertFailure s "fixture should have one attacker per board"
  -- CR 509.4's OTHER door, and the one nothing could reach until Aetherplasm:
  -- the creature put onto the battlefield blocking is a CARD moved out of a
  -- hand, not a token minted. Nothing in rule 509.4 privileges either -- it is
  -- worded "put onto the battlefield" -- and Pawl.Engine.Resolve's MoveToZone
  -- arm now hands its arrival to the same Combat.putOntoBattlefieldBlocking the
  -- Create arm above hands its tokens to.
  --
  -- Aetherplasm {2}{U}{U} 1/1 Illusion -- "Whenever this creature blocks a
  -- creature, you may return this creature to its owner's hand. If you do, you
  -- may put a creature card from your hand onto the battlefield blocking that
  -- creature." CR 509.4's parenthetical case again, and the slot it names is a
  -- TRIGGER'S BINDING ("that creature") where Flash Foliage's is a target.
  --
  -- WHAT DOES NOT DISCRIMINATE HERE, and it is the reverse of the Flash Foliage
  -- cases above: LIFE TOTALS. Aetherplasm's own declaration already made the
  -- Golem a blocked creature (CR 509.1h), and that rule's last sentence keeps it
  -- blocked once Aetherplasm leaves, so CR 510.1c's second sentence gives it no
  -- combat damage to assign whether or not the Evangel arrives blocking. Bob is
  -- at 20 on every leg. The quantity that moves is SURVIVAL, which is why the
  -- first assertion of each leg counts creatures rather than life -- and bob's
  -- 20 is asserted as a labelled control, pinning that the divergence is the
  -- rider rather than a leaked Game.removeFromCombat regression.
  --
  -- Three legs off one board and one answerer, differing only in how the two
  -- printed "may"s are answered.
  Spec.it s "CR 509.4 whole card: Aetherplasm swaps itself out for a creature card from hand, blocking" $ do
    golemP <- S.printingOf s registry "Icehide Golem"
    plasmP <- S.printingOf s registry "Aetherplasm"
    evangelP <- S.printingOf s registry "Cabal Evangel"
    case aetherBoard golemP plasmP evangelP of
      (gs, [golem], [plasm], evangel) -> do
        let golemName = S.nameOf (Printing.card golemP)
            evangelName = S.nameOf (Printing.card evangelP)
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            takeBoth = const OptionalDecision.Exercises
            -- The same answerer twice: once stopped before combat damage, where
            -- the arrival is still alive and the combat record can be read
            -- against it, and once run to the end, where the survivors are.
            -- Spelled out at each use rather than bound once: runToStep takes a
            -- rank-2 answerer, and a let-bound one would be monomorphic.
            blocking = S.runToStep (Phase.Combat CombatStep.CombatDamage) (aetherAnswer plasm golem evangel takeBoth) atBlockers
            atEnd = runToEndOfCombat (aetherAnswer plasm golem evangel takeBoth) atBlockers
        -- GAMEPLAY FIRST: both 2/2s traded, so neither is left. Under a
        -- MoveToZone that ignores the rider the Evangel enters as an ordinary
        -- permanent, the blocked Golem assigns nothing (CR 510.1c) and takes
        -- nothing, and this pair reads (1, 1).
        Spec.assertEqWith s "CR 510.1c: the Golem and the creature put onto the battlefield blocking it traded, so neither survives" (length (battlefieldNamed golemName S.alice atEnd), length (battlefieldNamed evangelName S.bob atEnd)) (0, 0)
        -- Control, on the leg the rider fires: the blocked Golem assigns no
        -- combat damage either way (CR 509.1h, CR 510.1c), so life cannot be the
        -- discriminating quantity and must not be read as one.
        Spec.assertEqWith s "control: CR 509.1h keeps the Golem blocked, so bob takes nothing whatever the rider did" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertEqWith s "control: and nothing reached alice" (S.lifeOf S.alice atEnd) (Just 20)
        -- The rider landed on the attacker the TRIGGER bound, read before damage
        -- removes the participants.
        case battlefieldNamed evangelName S.bob blocking of
          [arrival] -> do
            Spec.assertEqWith s "CR 509.4: the creature card from hand is blocking the Golem" (liveBlockersOf golem blocking) [arrival]
            Spec.assertEqWith s "CR 506.4: and it joined combat under bob" (Map.lookup arrival (Combat.Type.joinedUnder (GameState.combat blocking))) (Just S.bob)
            -- CR 509.4's "never blocked": the DECLARED blocker was Aetherplasm,
            -- so read the arrival specifically rather than the whole set.
            Spec.assertBool s (not (Set.member arrival (Combat.Type.declaredBlockers (GameState.combat blocking)))) "CR 509.4: the arrival was never declared as a blocker"
          other -> Spec.assertFailure s ("expected exactly one arrival on bob's battlefield, got " <> show (length other))
        -- Clause 0 ran, which is what clause 1's "If you do" hangs on.
        Spec.assertBool s (not (S.onBattlefield plasm atEnd)) "CR 506.4: Aetherplasm left the battlefield, so it was removed from combat"
        Spec.assertEqWith s "CR 400.3: and bob's hand holds Aetherplasm alone, the Evangel having left it" (fmap (fmap S.nameOf . (`Game.cardOf` atEnd)) (Game.zoneMembers Zone.Hand S.bob atEnd)) [Just (S.nameOf (Printing.card plasmP))]
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  -- The CR 608.2c fence, differing from the leg above in ONE answer: bob
  -- declines the first "may", so Aetherplasm stays where it is and clause 1's
  -- "If you do" never holds. Nothing is asked about clause 1 at all.
  --
  -- What the leg pins is that the gate is real: an engine that ran clause 1
  -- regardless would put the Evangel onto the battlefield blocking the Golem
  -- too, and the Golem would take 1 from Aetherplasm plus 2 from the Evangel and
  -- die. So the Golem's survival is the discriminating quantity, and the
  -- answerer exercises every clause but the first for exactly that reason.
  --
  -- A trigger that never fired at all would leave the same board, which is what
  -- the leg above rules out: it is the SAME fixture and the same answerer, and
  -- there the trigger moves Aetherplasm and the Evangel both.
  Spec.it s "CR 608.2c declining to return Aetherplasm skips the clause its 'If you do' hangs on" $ do
    golemP <- S.printingOf s registry "Icehide Golem"
    plasmP <- S.printingOf s registry "Aetherplasm"
    evangelP <- S.printingOf s registry "Cabal Evangel"
    case aetherBoard golemP plasmP evangelP of
      (gs, [golem], [plasm], evangel) -> do
        let golemName = S.nameOf (Printing.card golemP)
            evangelName = S.nameOf (Printing.card evangelP)
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            declineFirst cIdx = if cIdx == ClauseIndex.MkClauseIndex 0 then OptionalDecision.Declines else OptionalDecision.Exercises
            atEnd = runToEndOfCombat (aetherAnswer plasm golem evangel declineFirst) atBlockers
        -- GAMEPLAY FIRST: the Golem took Aetherplasm's 1 and lived, and nothing
        -- came out of bob's hand. Ungated, the Evangel would have arrived
        -- blocking and the pair would read (0, 1).
        Spec.assertEqWith s "CR 608.2c: the second clause never ran, so the Golem took only Aetherplasm's 1 and nothing left bob's hand" (length (battlefieldNamed golemName S.alice atEnd), length (battlefieldNamed evangelName S.bob atEnd)) (1, 0)
        Spec.assertBool s (List.elem evangel (Game.zoneMembers Zone.Hand S.bob atEnd)) "the Evangel is still the card bob is holding"
        Spec.assertBool s (not (S.onBattlefield plasm atEnd)) "CR 510.1c: Aetherplasm stayed blocking and took the Golem's 2, so it died"
        Spec.assertEqWith s "control: bob takes nothing, the Golem being blocked throughout" (S.lifeOf S.bob atEnd) (Just 20)
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  -- The rider's own control, differing from the whole-card leg in ONE answer:
  -- bob returns Aetherplasm and then declines to put anything out. Everything
  -- else on the card has run, so what is left out is the arrival and nothing
  -- else -- and the Golem, blocked with no blockers, assigns no combat damage
  -- (CR 509.1h, CR 510.1c) and takes none.
  Spec.it s "CR 509.1h an attacker whose only blocker returned to hand stays blocked and assigns nothing" $ do
    golemP <- S.printingOf s registry "Icehide Golem"
    plasmP <- S.printingOf s registry "Aetherplasm"
    evangelP <- S.printingOf s registry "Cabal Evangel"
    case aetherBoard golemP plasmP evangelP of
      (gs, [golem], [plasm], evangel) -> do
        let golemName = S.nameOf (Printing.card golemP)
            evangelName = S.nameOf (Printing.card evangelP)
            atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs
            declineSecond cIdx = if cIdx == ClauseIndex.MkClauseIndex 0 then OptionalDecision.Exercises else OptionalDecision.Declines
            blocking = S.runToStep (Phase.Combat CombatStep.CombatDamage) (aetherAnswer plasm golem evangel declineSecond) atBlockers
            atEnd = runToEndOfCombat (aetherAnswer plasm golem evangel declineSecond) atBlockers
        Spec.assertEqWith s "CR 510.1c: with nothing put onto the battlefield blocking it the Golem is untouched, and bob's hand is untouched too" (length (battlefieldNamed golemName S.alice atEnd), length (battlefieldNamed evangelName S.bob atEnd)) (1, 0)
        Spec.assertEqWith s "CR 509.1h: the Golem is still a blocked creature with its blocker gone" (liveBlockersOf golem blocking) []
        Spec.assertBool s (Combat.isBlocked golem blocking) "so CR 510.1c gives it no combat damage to assign rather than letting it through"
        Spec.assertEqWith s "control: which is why bob is at 20 on this leg as well as the one the Evangel arrives on" (S.lifeOf S.bob atEnd) (Just 20)
        Spec.assertBool s (List.elem evangel (Game.zoneMembers Zone.Hand S.bob atEnd)) "the declined clause left the Evangel in hand"
        Spec.assertBool s (not (S.onBattlefield plasm atEnd)) "and Aetherplasm did return to hand, so the leg differs from the whole-card one in the second may alone"
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker"
  -- CR 509.4's second sentence -- a creature put onto the battlefield blocking
  -- is "blocking" but "never blocked" -- and CR 509.3b's last sentence, which
  -- says the same thing from the trigger's side: "It won't trigger if the
  -- creature is put onto the battlefield blocking."
  --
  -- Until Aetherplasm this was a REGRESSION FENCE. The only card that could put
  -- a creature onto the battlefield blocking minted a vanilla token, which can
  -- bear no trigger, so deleting matchesTrigger's read of
  -- BecameBlocking.putOntoBattlefield left the whole suite green. A creature
  -- CARD arrives with its own text.
  --
  -- Loyal Sentry {W} 1/1 -- "Whenever this creature blocks a creature, destroy
  -- that creature and this creature." It is already in the pool, it already
  -- reads `thatAttacker` off this very trigger condition, and its effect is
  -- loud: if the exclusion leaked, the Golem would be DESTROYED outright rather
  -- than merely taking a 1/1's damage.
  --
  -- A PAIR differing in exactly one thing -- how the Sentry came to block the
  -- Golem. Same card, same attacker, same seats, same 2/2: put onto the
  -- battlefield blocking on one leg, DECLARED on the other. CR 509.3b's second
  -- sentence ("It triggers if the creature is declared as a blocker") is what
  -- makes the control leg the exclusion's opposite rather than an unrelated
  -- board.
  Spec.it s "CR 509.3b a creature put onto the battlefield blocking never blocked, so its own blocks trigger stays silent" $ do
    golemP <- S.printingOf s registry "Icehide Golem"
    plasmP <- S.printingOf s registry "Aetherplasm"
    sentryP <- S.printingOf s registry "Loyal Sentry"
    case (aetherBoard golemP plasmP sentryP, S.combatBoardOf [golemP] [sentryP]) of
      ((gs, [golem], [plasm], sentry), (declaredGs, [otherGolem], [otherSentry])) -> do
        let golemName = S.nameOf (Printing.card golemP)
            sentryName = S.nameOf (Printing.card sentryP)
            entered =
              runToEndOfCombat
                (aetherAnswer plasm golem sentry (const OptionalDecision.Exercises))
                (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer gs)
            declared =
              runToEndOfCombat
                (declareBlock otherSentry otherGolem)
                (S.runToStep (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer declaredGs)
        -- GAMEPLAY FIRST, and the pair reads the same quantity on both legs: is
        -- the attacker still there. Silent, the Sentry only trades its 1 power
        -- into a 2/2 and dies to the Golem's 2; leaked, the ability destroys the
        -- Golem outright.
        Spec.assertEqWith s "CR 509.3b: the Sentry entered blocking, so it never blocked and the Golem survives its 1 damage" (length (battlefieldNamed golemName S.alice entered)) 1
        Spec.assertEqWith s "control: the SAME Sentry DECLARED as a blocker does trigger, and the Golem is destroyed" (length (battlefieldNamed golemName S.alice declared)) 0
        -- Anti-vacuity: the Sentry really did arrive and really did block, so its
        -- silence is CR 509.3b and not a clause that never ran.
        Spec.assertBool s (Combat.isBlocked golem entered) "CR 509.1h: the Golem was a blocked creature on the entry leg"
        Spec.assertEqWith s "and the Sentry is gone, having taken the Golem's 2 as a blocking creature (CR 510.1c)" (length (battlefieldNamed sentryName S.bob entered)) 0
        Spec.assertBool s (not (S.onBattlefield plasm entered)) "Aetherplasm returned to hand, which is what put the Sentry out"
        -- CR 509.3b again on the control leg, in the direction it DOES fire: the
        -- Sentry destroys itself too, which a mere trade would also do, so the
        -- Golem above is the assertion that separates them.
        Spec.assertEqWith s "control: and the Sentry destroyed itself, as its own text says" (length (battlefieldNamed sentryName S.bob declared)) 0
      _ -> Spec.assertFailure s "fixture should have one attacker and one blocker on each board"

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
        declared = S.runPure declineBlocks beforeDeclaration (Combat.declareBlockers S.manaPerformer)
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
        exerted = S.runPure (exertAnswer OptionalDecision.Exercises) gs (Combat.declareAttackers S.manaPerformer S.alice)
        declined = S.runPure (exertAnswer OptionalDecision.Declines) gs (Combat.declareAttackers S.manaPerformer S.alice)
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
        exerted = S.runPure (exertAnswer OptionalDecision.Exercises) gs (Combat.declareAttackers S.manaPerformer S.alice)
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
-- Curse of the Nightly Hunt's, in attackCostSpec above and in Pawl.CombatSpec;
-- publicEnemySpec below is this axis on the PRINTED carrier.
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
        let after = S.runPure (announcing jaceId) activated (Combat.declareAttackers S.manaPerformer S.alice)
            without = S.runPure (announcing jaceId) control (Combat.declareAttackers S.manaPerformer S.alice)
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

-- CR 508.1d's OBJECT axis on the PRINTED carrier, proved by Public Enemy
-- ("{2}{U} Aura, Enchant creature / All creatures attack enchanted creature's
-- controller each combat if able / When enchanted creature dies, draw a card").
-- Alluring Siren above is the same axis on the resolution-created carrier;
-- Curse of the Nightly Hunt, in attackCostSpec, is the printed carrier with the
-- axis absent, and is the control the last two cases are written against.
--
-- The Aura's object is a RELATION -- Pawl.Types.RequiredDefender's
-- ControllerOfAttached -- rather than a player it names, which is why the two
-- legs of the first case attach the same Aura to two different opponents'
-- creatures and change nothing else.
publicEnemySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
publicEnemySpec s registry = Spec.describe s "PublicEnemy" $ do
  Spec.it s "CR 508.1d no attack is required when the enchanted creature's controller is not being attacked" $ do
    -- THREE seats, because two collapse the object axis onto the one player
    -- alice could attack. bob is the defending player and carol is not, so the
    -- SAME Aura mints a requirement on one leg and none on the other.
    --
    -- Both legs put a creature on both opponents' sides and the Aura on exactly
    -- one of them, so the boards differ in the host alone.
    enemy <- S.printingOf s registry "Public Enemy"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs, hers) = S.threePlayerCombat [piker] [piker] [piker]
        -- threePlayerCombat sits at the beginning of combat with no defending
        -- player, since CR 703.4h is what fills that in; a direct-call test
        -- states both, as combatBoardOf does for two seats.
        defending =
          base
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]}
            }
        enchanting host =
          let (aura, withAura) = S.addCreature enemy S.alice defending
           in S.attach aura host withAura
    case (mine, theirs, hers) of
      ([attacker], [bobs], [carols]) -> do
        let onCarol = enchanting carols
            onBob = enchanting bobs
        -- Gameplay first: whether alice may decline to attack at all.
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [] onCarol) "carol cannot be attacked this combat, so nothing is required"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] onBob)) "bob can, so the same Aura forbids declining"
        -- Neither leg refused the attacker outright, which is the other way a
        -- board answers "declining is legal" (CR 508.1a).
        Spec.assertEqWith s "the attacker is offered on the carol leg" (Combat.legalAttackers S.alice onCarol) [attacker]
        Spec.assertEqWith s "and on the bob leg" (Combat.legalAttackers S.alice onBob) [attacker]
        Spec.assertBool s (Combat.legalAttackDeclaration S.alice [attacker] onCarol) "attacking anyway stays legal where nothing is required"
      _ -> Spec.assertFailure s "fixture should have one creature per seat"
  Spec.it s "CR 508.1d the requirement is obeyed by attacking that player and not by attacking their planeswalker" $ do
    -- CR 508.1b offers alice's attacker two announcements, so the object axis
    -- has something to narrow -- Alluring Siren's board, reached from the
    -- printed carrier instead. Curse of the Nightly Hunt enchanting ALICE is the
    -- control: it requires the same creature to attack and states no object, so
    -- its requirement is obeyed by EITHER announcement, and the illegality below
    -- is the object axis rather than anything else on the board.
    enemy <- S.printingOf s registry "Public Enemy"
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = S.combatBoardOf [piker] [jace, piker]
    case (mine, theirs) of
      ([attacker], [jaceId, host]) -> do
        let loyal = S.addCounter CounterKind.Loyalty 3 jaceId base
            (aura, withAura) = S.addCreature enemy S.alice loyal
            narrowed = S.attach aura host withAura
            control = cursingBoard curse S.alice loyal
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclarationAs S.alice [(attacker, AttackTarget.OfPlaneswalker jaceId)] narrowed))
          "CR 508.1d: announcing Jace does not obey 'attack enchanted creature's controller'"
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(attacker, AttackTarget.OfPlayer S.bob)] narrowed)
          "announcing bob does"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] narrowed)) "and declining obeys nothing"
        -- The control, one printing different and the same two seats: the
        -- subject-only Curse requires the attack and admits either announcement.
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(attacker, AttackTarget.OfPlaneswalker jaceId)] control)
          "under a requirement that names no object, Jace is a legal announcement"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] control)) "while declining is illegal there too"
      _ -> Spec.assertFailure s "fixture should have one attacker and bob's two permanents"
  Spec.it s "CR 508.1d whole cards: a real declare attackers step sends the creature at the enchanted creature's controller" $ do
    -- Through Combat.declareAttackers rather than the legality predicate, with
    -- an interpreter that announces JACE for every attacker: CR 508.1d refuses
    -- that declaration, and what the combat record then says is the assertion.
    enemy <- S.printingOf s registry "Public Enemy"
    curse <- S.printingOf s registry "Curse of the Nightly Hunt"
    jace <- S.printingOf s registry "Jace Beleren"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, theirs) = S.combatBoardOf [piker] [jace, piker]
    case (mine, theirs) of
      ([attacker], [jaceId, host]) -> do
        let loyal = S.addCounter CounterKind.Loyalty 3 jaceId base
            (aura, withAura) = S.addCreature enemy S.alice loyal
            narrowed = S.attach aura host withAura
            control = cursingBoard curse S.alice loyal
            announced gs = Map.lookup attacker (Combat.Type.attackers (GameState.combat (S.runPure (announcing jaceId) gs (Combat.declareAttackers S.manaPerformer S.alice))))
        Spec.assertEqWith s "the attacker is redirected to bob" (announced narrowed) (Just (AttackTarget.OfPlayer S.bob))
        Spec.assertEqWith s "where a requirement naming no object leaves the announcement alone" (announced control) (Just (AttackTarget.OfPlaneswalker jaceId))
      _ -> Spec.assertFailure s "fixture should have one attacker and bob's two permanents"

-- CR 508.1d's OBJECT axis naming a SET of players rather than a relation to one
-- object, proved by Galactus, Devourer of Worlds ("{10} 12/12 / Insatiable
-- Hunger --- Galactus attacks an opponent with the most life among your
-- opponents each combat if able unless you control a creature named Silver
-- Surfer, Galactus's Herald"). Public Enemy above is the same axis spelled as a
-- relation.
--
-- THREE seats, and both opponents defending: CR 802.3's declarableTargets is a
-- concatenation in APNAP order, so the leader on life can be the SECOND
-- announcement rather than the first. That is what makes
-- Combat.attackCeilingGiven's `bestFor` a maximization observable at gameplay
-- level -- "the earliest freely announceable target" and "the earliest of those
-- obeying the most" disagree here, where on every board reachable before this
-- card they agreed.
--
-- The two legs differ in the two life totals alone, and the leg where bob leads
-- is the control: there the requirement names the FIRST announcement, so it
-- cannot tell the two readings apart and every assertion must still hold.
mostLifeRequirementSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mostLifeRequirementSpec s registry = Spec.describe s "MostLifeRequirement" $ do
  Spec.it s "CR 508.1d the requirement is obeyed only by attacking the opponent with the most life" $ do
    galactus <- S.printingOf s registry "Galactus, Devourer of Worlds"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, _, _) = S.threePlayerCombat [galactus] [piker] [piker]
        -- threePlayerCombat sits at the beginning of combat with no defending
        -- player, since CR 703.4h is what fills that in. Both opponents defend,
        -- which is CR 802.2, and bob is named first because Game.apnapOrder is.
        defending gs =
          gs
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob, S.carol]}
            }
        carolAhead = defending (atLife S.bob 17 (atLife S.carol 23 base))
        bobAhead = defending (atLife S.bob 23 (atLife S.carol 17 base))
    case mine of
      [devourer] -> do
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclarationAs S.alice [(devourer, AttackTarget.OfPlayer S.bob)] carolAhead))
          "CR 508.1d: attacking bob obeys nothing while carol has the most life, and bob is the FIRST announcement"
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(devourer, AttackTarget.OfPlayer S.carol)] carolAhead)
          "attacking carol obeys it"
        Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [] carolAhead)) "and declining obeys nothing"
        -- The control: the same board with the two life totals exchanged. The
        -- leader is now the first announcement, so the two readings of the fold
        -- agree and the narrowing is the life comparison rather than seat order.
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(devourer, AttackTarget.OfPlayer S.bob)] bobAhead)
          "with the totals exchanged bob is the legal announcement"
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclarationAs S.alice [(devourer, AttackTarget.OfPlayer S.carol)] bobAhead))
          "and carol is not"
      _ -> Spec.assertFailure s "fixture should have one Galactus"
  Spec.it s "CR 508.1d whole cards: a real declare attackers step sends the creature at the opponent with the most life" $ do
    -- Through Combat.declareAttackers rather than the legality predicate, with
    -- an interpreter that announces BOB for every attacker: CR 508.1d refuses
    -- that declaration on the leg where carol leads, and the combat record is the
    -- assertion. Combat.forcedAttackDeclaration reads `bestFor`'s target
    -- directly, so this is the fold's own output.
    galactus <- S.printingOf s registry "Galactus, Devourer of Worlds"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, mine, _, _) = S.threePlayerCombat [galactus] [piker] [piker]
        defending gs =
          gs
            { GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
              GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob, S.carol]}
            }
        carolAhead = defending (atLife S.bob 17 (atLife S.carol 23 base))
        bobAhead = defending (atLife S.bob 23 (atLife S.carol 17 base))
        announcingBob :: Prompt.Prompt r -> r
        announcingBob p = case p of
          Prompt.ChooseAttackTarget {} -> AttackTarget.OfPlayer S.bob
          _ -> S.aggressiveAnswer p
        announced gs oid = Map.lookup oid (Combat.Type.attackers (GameState.combat (S.runPure announcingBob gs (Combat.declareAttackers S.manaPerformer S.alice))))
    case mine of
      [devourer] -> do
        Spec.assertEqWith s "the announcement against bob is refused and Galactus is redirected to carol" (announced carolAhead devourer) (Just (AttackTarget.OfPlayer S.carol))
        Spec.assertEqWith s "with the totals exchanged the same announcement stands" (announced bobAhead devourer) (Just (AttackTarget.OfPlayer S.bob))
      _ -> Spec.assertFailure s "fixture should have one Galactus"

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
-- The pair of boards differs in ONE thing -- which opponent randomness named --
-- and under CR 802.2 both opponents are defending players, so BOTH are
-- attackable on both boards and the requirement is what separates them:
--
--   * named carol: attacking carol obeys the requirement, attacking bob does
--     not, and CR 508.1d forbids both declining and the bob declaration.
--   * named bob: the same three assertions with the seats swapped.
--
-- That symmetry is what CR 802.2 bought. Before it the bob leg was vacuous --
-- one opponent was attackable at a time, so a requirement naming the other
-- could not be obeyed by any declaration and declining was legal.
--
-- An engine that rolled the head of the offer itself rather than honouring the
-- answer collapses the pair onto the bob leg, and one that never landed the bind
-- makes declining legal on both.
randomOpponentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
randomOpponentSpec s registry = Spec.describe s "RandomOpponent" $ do
  Spec.it s "CR 608.2d the opponent randomness named is the one the requirement makes Ruhan attack" $ do
    ruhan <- S.printingOf s registry "Ruhan of the Fomori"
    let (board, mine, _, _) = S.threePlayerCombat [ruhan] [] []
        atCarol = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (ruhanAnswer S.carol) board
        atBob = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (ruhanAnswer S.bob) board
    case mine of
      [ruhanId] -> do
        -- THE GAMEPLAY ASSERTION. The requirement names carol, so CR 508.1d
        -- refuses both the empty declaration and the one aimed at bob -- who
        -- CR 802.2 makes just as attackable.
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclaration S.alice [] atCarol))
          "CR 508.1d: with carol named, declining disobeys the requirement randomness bound"
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclarationAs S.alice [(ruhanId, AttackTarget.OfPlayer S.bob)] atCarol))
          "and so does attacking bob, whom CR 802.2 leaves attackable"
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(ruhanId, AttackTarget.OfPlayer S.carol)] atCarol)
          "and attacking carol obeys it"
        -- The paired board, one thing different: randomness named bob, so the
        -- three assertions above hold with the seats swapped.
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclaration S.alice [] atBob))
          "CR 508.1d: with bob named, declining disobeys it too"
        Spec.assertBool
          s
          (not (Combat.legalAttackDeclarationAs S.alice [(ruhanId, AttackTarget.OfPlayer S.carol)] atBob))
          "and attacking carol is what is illegal on that board"
        Spec.assertBool
          s
          (Combat.legalAttackDeclarationAs S.alice [(ruhanId, AttackTarget.OfPlayer S.bob)] atBob)
          "while attacking bob obeys it"
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
  Spec.it s "CR 104.3a the offer is the opponents still in the game, and never the controller" $ do
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

-- Pins which opponent randomness names (CR 608.2d). FILTERED out of the offered
-- candidates rather than built, so an answer the engine never offered cannot slip
-- through, and falling back to the head where `who` was not offered.
--
-- CR 507.1's question is not pinned because it is not asked: CR 802.2 takes the
-- turn-based action without a choice, and every opponent defends.
ruhanAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
ruhanAnswer who p = case p of
  Prompt.RandomOpponent offered ->
    Maybe.fromMaybe (NonEmpty.head offered) (List.find (== who) (NonEmpty.toList offered))
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
        ((_, after), asked) = State.runState (Engine.runGame declineThenAttackAnswer gs (Combat.declareAttackers S.manaPerformer S.alice)) 0
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
        let ((_, after), asked) = State.runState (Engine.runGame declineThenBlockAnswer gs (Combat.declareBlockers S.manaPerformer)) 0
        Spec.assertEqWith s "CR 509.1: both creatures block, where the ceiling's declaration sends the Screen alone" (blockersOf attacker after) (Set.fromList theirs)
        Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty gs)) "declining really is illegal, so the first answer really was rewound"
        Spec.assertEqWith s "CR 509.1's preamble asked for a fresh declaration" asked 2
      _ -> Spec.assertFailure s "fixture should have one attacker"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Combat" $ do
  landSubtypeStripSpec s registry
  becomesBlockedSpec s registry
  castingWindowSpec s registry
  putOntoBattlefieldBlockingSpec s registry
  attackCostSpec s registry
  alluringSirenSpec s registry
  publicEnemySpec s registry
  mostLifeRequirementSpec s registry
  conditionalAttackRequirementSpec s registry
  randomOpponentSpec s registry
  declarationRetrySpec s registry
  blockCostSpec s registry
  exertSpec s registry

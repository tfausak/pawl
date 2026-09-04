{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Replacement over life-total replacements (CR 119, CR 120.4c, CR
-- 614.11): Worship, Bloodletter of Aclazotz, Words of Worship, Alms Collector,
-- Boon Reflection, Ashiok. Split out of Pawl.ReplacementSpec, which keeps the
-- machinery.
module Pawl.LifeReplacementSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Extra.Int as Int
import Pawl.PreventionSpec (answersFor, atLife, attackAndBlock, attackNoBlock, bobAttacks, inMainPhase, theAbility, wasAskedToReplace)
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.Zone as Zone

-- CR 614.1a / 120.4c: Worship ({3}{W} Enchantment, "If you control a creature,
-- damage that would reduce your life total to less than 1 reduces it to 1
-- instead" -- name, cost, type line and Oracle text checked against
-- api.scryfall.com 2026-08-28).
--
-- The card that separates CR 120.4b's damage from CR 120.4c's results, and the
-- damage-scoped half of the life-total class -- Bloodletter of Aclazotz below is
-- the half whose clause names no cause. Its own Gatherer rulings state both
-- halves: "It reduces your life total to 1, not the damage to 1", and "Worship
-- does not prevent damage. It causes some damage to be unable to lower your life
-- total. So any damage rendered useless by Worship was still dealt ... Worship
-- does not prevent loss of life, so loss of life bypasses Worship."
-- data/cards/serra-the-benevolent.json's emblem prints the same clause word for
-- word, and this group is the BATTLEFIELD half of the pair: an emblem needs a
-- resolution to mint it, where the enchantment is a permanent a fixture can
-- place. Pawl.ProjectionSpec proves the identical row out of the command zone
-- (CR 113.6p), which is the half this group cannot reach.
--
-- REAL COMBAT with a LIFELINK attacker, because that is the board on which the
-- two readings of the rule differ. An implementation that shrank the DAMAGE
-- instead of the life loss would leave alice at 1 just the same and gain bob 1
-- rather than 3; the lifelink total is what tells them apart, and CR 120.3f is
-- what makes it observable ("in addition to the damage's other results").
--
-- Every number is distinct -- alice at 2 or 5, a 3/4 attacker, a floor of 1, a
-- 4-life drain -- so no two readings of the rule land on the same total.
worshipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
worshipSpec s registry = Spec.describe s "Worship (CR 120.4c)" $ do
  Spec.it s "CR 614.1a the life total stops at 1, and the damage is dealt in full anyway" $ do
    plains <- S.printingOf s registry "Plains"
    worship <- S.printingOf s registry "Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    let base = S.landsInPlay plains 2
        (_, g1) = S.addCreature worship S.alice base
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addCreature celestine S.bob g2
        after = S.runCombat attackNoBlock (bobAttacks (atLife S.alice 2 g3))
    Spec.assertEqWith s "CR 614.1a alice stops at 1 rather than the -1 the damage would give" (S.lifeOf S.alice after) (Just 1)
    Spec.assertEqWith s "CR 120.3f the whole 3 was still dealt, so lifelink gains bob 3 and not 1" (S.lifeOf S.bob after) (Just 23)
    Spec.assertEqWith s "CR 120.4b the damage event itself is undiminished" (fmap DamageEvent.amount (S.damageEventsOf after)) [3]
  -- CR 120.4d's own Worship example, in its second reading: "Worship's effect sees
  -- that the damage event would not reduce the player's life total to less than 1,
  -- so Worship's effect is not applied."
  Spec.it s "CR 120.4d damage that does not reach the floor is left alone" $ do
    plains <- S.printingOf s registry "Plains"
    worship <- S.printingOf s registry "Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    let base = S.landsInPlay plains 2
        (_, g1) = S.addCreature worship S.alice base
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addCreature celestine S.bob g2
        after = S.runCombat attackNoBlock (bobAttacks (atLife S.alice 5 g3))
    Spec.assertEqWith s "the 3 lands whole: 5 - 3 = 2, not clamped to anything" (S.lifeOf S.alice after) (Just 2)
  -- The CONTROL is the same board with alice's creature taken away, so the only
  -- difference is the printed clause.
  Spec.it s "CR 604.2 with no creature the printed clause is false and the whole 3 lands" $ do
    plains <- S.printingOf s registry "Plains"
    worship <- S.printingOf s registry "Worship"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    let base = S.landsInPlay plains 2
        (_, g1) = S.addCreature worship S.alice base
        (_, g2) = S.addCreature celestine S.bob g1
        after = S.runCombat attackNoBlock (bobAttacks (atLife S.alice 2 g2))
    Spec.assertEqWith s "alice controls no creature, so she takes all 3 and ends at -1" (S.lifeOf S.alice after) (Just (-1))
  -- CR 510.2's simultaneity, and CR 120.4d's first Worship example scaled down:
  -- two attackers, each of whom alone would carry alice past the floor. Reading
  -- each proposal against the PRE-BATCH life would cut both to a loss of 1 and
  -- leave her at 0, dead to CR 704.5a.
  Spec.it s "CR 510.2 two simultaneous hits still leave exactly 1" $ do
    plains <- S.printingOf s registry "Plains"
    worship <- S.printingOf s registry "Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    -- A 2/1 lifelink Child of Night rather than a second Celestine: CR 704.5j
    -- would bury one of two legends before either could attack, and the two
    -- amounts differ anyway, which no single number could tell apart.
    child <- S.printingOf s registry "Child of Night"
    let base = S.landsInPlay plains 2
        (_, g1) = S.addCreature worship S.alice base
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addCreature celestine S.bob g2
        (_, g4) = S.addCreature child S.bob g3
        after = S.runCombat attackNoBlock (bobAttacks (atLife S.alice 2 g4))
    Spec.assertEqWith s "alice is at 1, not at the 0 a pre-batch reading would give" (S.lifeOf S.alice after) (Just 1)
    Spec.assertEqWith s "CR 702.15e each lifelink source gained its own amount, 3 and 2" (S.lifeOf S.bob after) (Just 25)
  -- CR 120.4d's SECOND Example, which is the half CR 120.3f's gain makes
  -- reachable without Awe Strike: one damage event whose results are both a loss
  -- and a gain. "That's processed into its results, so the damage event is now
  -- [the defending player loses 5 life, the defending player gains 5 life].
  -- Worship's effect sees that the damage event would not reduce the player's
  -- life total to less than 1, so Worship's effect is not applied."
  --
  -- Alice at 4 takes 5 from an unblocked Jedit Ojanen and gains 3 from her own
  -- lifelink Celestine striking the Goblin Piker she blocked -- one CR 510.2
  -- batch. The event leaves her at 2, so the floor is never breached. Reading
  -- the loss against a board the gain has not reached yet gives 4, which is what
  -- this discriminates; see #2563.
  --
  -- Every number distinct: 4 life, 5 damage, 3 gained, 2 left, a floor of 1.
  Spec.it s "CR 120.4c a simultaneous life gain keeps the same event off the floor" $ do
    plains <- S.printingOf s registry "Plains"
    worship <- S.printingOf s registry "Worship"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = S.landsInPlay plains 2
        (_, g1) = S.addCreature worship S.alice base
        (blocker, g2) = S.addCreature celestine S.alice g1
        (_, g3) = S.addCreature jedit S.bob g2
        (blocked, g4) = S.addCreature piker S.bob g3
        after = S.runCombat (attackAndBlock blocker blocked) (bobAttacks (atLife S.alice 4 g4))
    Spec.assertEqWith s "CR 120.4c alice ends at 4 - 5 + 3 = 2, the floor never applying" (S.lifeOf S.alice after) (Just 2)
    Spec.assertEqWith s "setup: the block happened, so Celestine's 3 killed the 2/1 Piker (CR 704.5g)" (S.creaturesInPlay S.bob after) 1
    Spec.assertEqWith s "setup: alice's lifelink blocker survived the Piker's 2, so Worship's clause stayed true" (S.creaturesInPlay S.alice after) 1
  -- Worship's own ruling: "Worship does not prevent loss of life, so loss of life
  -- bypasses Worship." Zof Consumption ({4}{B}{B} Sorcery, "Each opponent loses 4
  -- life and you gain 4 life") is the road CR 119.3 owns, against the same board
  -- at the same life that survives 3 damage above.
  Spec.it s "CR 119.3 life loss from an effect bypasses the clause" $ do
    swamp <- S.printingOf s registry "Swamp"
    worship <- S.printingOf s registry "Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    discipline <- S.printingOf s registry "Stronghold Discipline"
    let base = S.landsInPlay swamp 4
        (_, g1) = S.addCreature worship S.alice base
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addCreature piker S.alice g2
        (_, g4) = S.addCreature piker S.alice g3
        (held, g5) = S.addHandCard discipline S.alice g4
        ready = inMainPhase S.alice (atLife S.alice 2 g5)
        after = S.runPure S.identityAnswer ready (S.cast S.alice held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "alice loses the whole 3, floor and all: 2 - 3 = -1" (S.lifeOf S.alice after) (Just (-1))
    Spec.assertEqWith s "and bob, controlling no creature, loses nothing" (S.lifeOf S.bob after) (Just 20)

-- Fills every target slot with bob, the opponent whose life total the exchange
-- cases below drive down. FILTERED rather than hand-built, so CR 608.2b's re-read
-- at resolution keeps the recipient the prompt offered.
exchangingWithBob :: Prompt.Prompt r -> r
exchangingWithBob p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring wanted sets
  _ -> S.identityAnswer p
  where
    wanted r = case r of
      Recipient.ToPlayer pid -> pid == S.bob
      _ -> False

-- CR 614.1a with CR 119.4 and CR 119.5: Bloodletter of Aclazotz ({1}{B}{B}{B}
-- Creature -- Vampire Demon, 2/4, "Flying / If an opponent would lose life during
-- your turn, they lose twice that much life instead. (Damage causes loss of
-- life.)" -- name, cost, type line, power, toughness and Oracle text checked
-- against api.scryfall.com 2026-08-28).
--
-- The life-total replacement whose clause is NOT scoped to damage, which is what
-- makes the non-damage roads to a life loss observable at all: CR 119.4's
-- payment, CR 119.5's set-a-total, CR 701.12c's exchange and CR 119.7's
-- redistribution. Every FLOOR-shaped printing is scoped to damage instead: Scryfall
-- o:"life total to less than", 2026-08-28, is eight cards -- Ali from Cairo,
-- Angel of Grace, Angel's Grace, Elderscale Wurm, Fortune Thief, Serra the
-- Benevolent, Sustaining Spirit, Worship -- and all eight print "DAMAGE that
-- would reduce your life total to less than 1". A printing worded "if you would
-- lose life ... instead", with no damage clause, would refute that and belongs
-- here beside this one; Scryfall o:"lose life" o:instead -o:damage and
-- o:"much life instead" -o:gain on the same date name none but this card.
--
-- Its own ruling fixes the half this does NOT touch: "Bloodletter of Aclazotz's
-- last ability doesn't change the amount of damage dealt to opponents", which is
-- CR 120.4c again -- the loss is resized, the damage is not.
--
-- THREE SEATS, because "an opponent" and "you" cannot be told apart on a board
-- where one of the two players holds both roles, and because a downward set, an
-- upward set and a self-set have to be read off one board to prove the row
-- discriminates rather than that three boards differ.
bloodletterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bloodletterSpec s registry = Spec.describe s "Bloodletter of Aclazotz (CR 119.4 / 119.5 / 701.12c)" $ do
  -- CR 119.4's second sentence is the whole of this case: "If a player pays life,
  -- the payment is subtracted from their life total; in other words, the player
  -- loses that much life." A payment is a life loss, so a row watching life loss
  -- reaches it. Greed ({3}{B} Enchantment, "{2}{B}, Pay 2 life: Draw a card") is
  -- the payer, and the COST it charges is unchanged at 2 -- what doubles is the
  -- loss the payment causes.
  Spec.it s "CR 119.4 life paid as a cost is a life loss, so the row resizes it" $ do
    swamp <- S.printingOf s registry "Swamp"
    greed <- S.printingOf s registry "Greed"
    bloodletter <- S.printingOf s registry "Bloodletter of Aclazotz"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature bloodletter S.alice S.threePlayerGame
        (bobsGreed, g2) = S.addCreature greed S.bob g1
        (alicesGreed, g3) = S.addCreature greed S.alice g2
        -- A card to draw for each activation, so no seat is decked out from under
        -- the assertion (CR 104.3c) and the draw is a real one.
        (_, g4) = S.addLibraryCard piker S.bob g3
        (_, g5) = S.addLibraryCard piker S.alice g4
        withMana = S.landsFor swamp S.bob 3 (S.landsFor swamp S.alice 3 g5)
        pay who what = S.runPure S.identityAnswer withMana (Activate.activateAbility who what (theAbility greed))
        bobPaid = pay S.bob bobsGreed
        alicePaid = pay S.alice alicesGreed
    Spec.assertEqWith s "CR 119.4 bob is alice's opponent on alice's turn, so his 2-life payment costs him 4" (S.lifeOf S.bob bobPaid) (Just 16)
    -- CR 109.5: "an opponent" is read against the ROW's controller, and alice is
    -- not her own opponent. The same card, the same payment, the same turn.
    Spec.assertEqWith s "alice paying the same 2 life loses exactly 2" (S.lifeOf S.alice alicePaid) (Just 18)
    Spec.assertEqWith s "and bob's payment left alice alone" (S.lifeOf S.alice bobPaid) (Just 20)
  -- CR 120.4c, the control that matters: Worship's clause names DAMAGE, so it must
  -- not see a payment. The two boards differ in one thing -- what takes alice's
  -- last 2 life -- and Worship's own ruling says which way it falls: "Worship does
  -- not prevent loss of life, so loss of life bypasses Worship."
  Spec.it s "CR 120.4c a damage-scoped row does not see a payment" $ do
    swamp <- S.printingOf s registry "Swamp"
    greed <- S.printingOf s registry "Greed"
    worship <- S.printingOf s registry "Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature worship S.alice S.threePlayerGame
        (_, g2) = S.addCreature piker S.alice g1
        (alicesGreed, g3) = S.addCreature greed S.alice g2
        (_, g4) = S.addLibraryCard piker S.alice g3
        (bobsPiker, g5) = S.addCreature piker S.bob g4
        base = atLife S.alice 2 (S.landsFor swamp S.alice 3 g5)
        paid = S.runPure S.identityAnswer base (Activate.activateAbility S.alice alicesGreed (theAbility greed))
        damaged =
          S.runPure
            S.identityAnswer
            base
            (Damage.applyDamage [DamageEvent.MkDamageEvent bobsPiker (Recipient.ToPlayer S.alice) 3 False False False 0 Nothing DamageKind.Noncombat])
    Spec.assertEqWith s "CR 119.4 the payment bypasses Worship's floor: 2 - 2 = 0" (S.lifeOf S.alice paid) (Just 0)
    Spec.assertEqWith s "CR 120.4c the same board's DAMAGE is still floored at 1" (S.lifeOf S.alice damaged) (Just 1)
  -- CR 120.4c is untouched by this unit, and this is the case that says so: the
  -- damage road proposes its loss ONCE. A second proposal would double it again
  -- and leave bob at 8.
  Spec.it s "CR 120.4c the damage road proposes its loss once, and the row resizes it once" $ do
    bloodletter <- S.printingOf s registry "Bloodletter of Aclazotz"
    piker <- S.printingOf s registry "Goblin Piker"
    let (source, g1) = S.addCreature bloodletter S.alice S.threePlayerGame
        (_, g2) = S.addCreature piker S.bob g1
        after =
          S.runPure
            S.identityAnswer
            g2
            (Damage.applyDamage [DamageEvent.MkDamageEvent source (Recipient.ToPlayer S.bob) 3 False False False 0 Nothing DamageKind.Noncombat])
    Spec.assertEqWith s "3 damage costs bob 6 life, not the 12 a second application would" (S.lifeOf S.bob after) (Just 14)
    Spec.assertEqWith s "CR 120.4b the damage event itself is undiminished" (fmap DamageEvent.amount (S.damageEventsOf after)) [3]
  -- CR 119.5: "If an effect sets a player's life total to a specific number, the
  -- player gains or loses the necessary amount of life to end up with the new
  -- total." Biorhythm ({6}{G}{G} Sorcery, "Each player's life total becomes the
  -- number of creatures they control") sets three seats at once off one pre-effect
  -- board (CR 608.2f), which is what lets one case read all three answers:
  -- an opponent's LOSS is resized, the controller's own loss is not, and a GAIN is
  -- not a loss at all and proposes nothing.
  --
  -- Every number is distinct -- alice 20 -> 1, bob 20 -> -20 rather than 0, carol
  -- 2 -> 3 rather than 4 -- so no two readings of the rule land on the same total.
  Spec.it s "CR 119.5 a total set LOWER is a life loss the row resizes, and one set higher is not" $ do
    forest <- S.printingOf s registry "Forest"
    biorhythm <- S.printingOf s registry "Biorhythm"
    bloodletter <- S.printingOf s registry "Bloodletter of Aclazotz"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature bloodletter S.alice S.threePlayerGame
        (_, g2) = S.addCreature piker S.carol g1
        (_, g3) = S.addCreature piker S.carol g2
        (_, g4) = S.addCreature piker S.carol g3
        (held, g5) = S.addHandCard biorhythm S.alice g4
        ready = inMainPhase S.alice (atLife S.carol 2 (S.landsFor forest S.alice 8 g5))
        after = S.runPure S.identityAnswer ready (S.cast S.alice held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 119.5 bob's total becomes 0, a loss of 20, which the row doubles to 40" (S.lifeOf S.bob after) (Just (-20))
    Spec.assertEqWith s "alice's own loss of 19 down to her one creature is not an opponent's" (S.lifeOf S.alice after) (Just 1)
    Spec.assertEqWith s "carol's three creatures RAISE her, and a gain is no life loss to resize" (S.lifeOf S.carol after) (Just 3)
  -- CR 701.12c: "each player gains or loses the amount of life necessary to equal
  -- the other player's previous life total. Replacement effects may modify these
  -- gains and losses". So an exchange is not a pair of assignments: its lowered
  -- side is a life loss, and this row resizes it -- which leaves the two seats on
  -- DIFFERENT totals, the outcome the rule's own sentence licenses.
  --
  -- Mirror Universe ({6} Artifact, "{T}, Sacrifice this artifact: Exchange life
  -- totals with target opponent. Activate only during your upkeep." -- name, cost,
  -- type line and Oracle text checked against api.scryfall.com 2026-09-02) is the
  -- producer, on alice's own upkeep, which is where the row's clause looks too.
  --
  -- The two boards differ in ONE thing: which side of the exchange is the lower
  -- total, and so which seat loses. alice's own loss is no opponent's and is not
  -- resized, so the pair also rules out a row that doubles whatever it sees.
  --
  -- Every number is distinct -- 5, 22, 13, a difference of 17, a doubled 34 and
  -- the -12 it lands on -- so the unreplaced reading (bob on alice's 5) and this
  -- one cannot coincide.
  Spec.it s "CR 701.12c an exchange's lowered side is a life loss the row resizes" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    bloodletter <- S.printingOf s registry "Bloodletter of Aclazotz"
    let board aliceLife bobLife =
          let (_, g1) = S.addCreature bloodletter S.alice S.threePlayerGame
              (mirrorId, g2) = S.addCreature mirror S.alice g1
              upkeep =
                (atLife S.alice aliceLife (atLife S.bob bobLife (atLife S.carol 13 g2)))
                  { GameState.activePlayer = S.alice,
                    GameState.phase = Phase.Beginning BeginningStep.Upkeep
                  }
           in (mirrorId, upkeep)
        exchange aliceLife bobLife =
          let (mirrorId, gs) = board aliceLife bobLife
           in S.runPure exchangingWithBob gs (Activate.activateAbility S.alice mirrorId (theAbility mirror) Monad.>> Stack.resolveTop)
        bobLower = exchange 5 22
        aliceLower = exchange 22 5
    Spec.assertEqWith s "CR 701.12c bob's loss of 17 is doubled to 34: 22 - 34 = -12, not alice's 5" (S.lifeOf S.bob bobLower) (Just (-12))
    Spec.assertEqWith s "CR 701.12c alice's side is a GAIN and is not resized: she takes bob's 22" (S.lifeOf S.alice bobLower) (Just 22)
    Spec.assertEqWith s "carol, no side of it, keeps her 13" (S.lifeOf S.carol bobLower) (Just 13)
    -- CR 109.5: the row is alice's, so its "your turn" is hers and the opponents
    -- it names are hers -- and she is not her own opponent.
    Spec.assertEqWith s "the same exchange the other way costs alice exactly her 17" (S.lifeOf S.alice aliceLower) (Just 5)
    Spec.assertEqWith s "and bob, gaining, takes alice's 22" (S.lifeOf S.bob aliceLower) (Just 22)
  -- CR 119.7 / 119.8 with CR 119.5: a redistribution hands out totals, and every
  -- seat that lands lower loses "the necessary amount of life" -- the same
  -- proposal a set makes, so the same row reaches it. Reverse the Sands ({6}{W}{W}
  -- Sorcery, "Redistribute any number of players' life totals. (Each of those
  -- players gets one life total back.)" -- name, cost, type line and Oracle text
  -- checked against api.scryfall.com 2026-09-02).
  --
  -- The permutation is PINNED rather than searched, so no answerer can repair a
  -- mutation by finding another legal one. It is a 3-cycle, which is what puts two
  -- losers on one board: bob's, an opponent's, doubled, and alice's own, not.
  --
  -- Every number is distinct -- 20, 30, 12 before; 12, 10, 30 after -- and the
  -- unreplaced reading leaves bob on alice's 20 rather than 10.
  Spec.it s "CR 119.7 a redistribution's lowered total is a life loss the row resizes" $ do
    plains <- S.printingOf s registry "Plains"
    sands <- S.printingOf s registry "Reverse the Sands"
    bloodletter <- S.printingOf s registry "Bloodletter of Aclazotz"
    let (_, g1) = S.addCreature bloodletter S.alice S.threePlayerGame
        (held, g2) = S.addHandCard sands S.alice g1
        ready = inMainPhase S.alice (atLife S.alice 20 (atLife S.bob 30 (atLife S.carol 12 (S.landsFor plains S.alice 8 g2))))
        assigning :: Prompt.Prompt r -> r
        assigning p = case p of
          Prompt.ChooseRedistribution {} -> Map.fromList [(S.alice, S.carol), (S.bob, S.alice), (S.carol, S.bob)]
          _ -> S.identityAnswer p
        after = S.runPure assigning ready (S.cast S.alice held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 119.5 bob's loss of 10 down to alice's 20 is doubled: 30 - 20 = 10" (S.lifeOf S.bob after) (Just 10)
    Spec.assertEqWith s "alice's own loss of 8 down to carol's 12 is not an opponent's" (S.lifeOf S.alice after) (Just 12)
    Spec.assertEqWith s "carol takes bob's 30, and a gain is no life loss to resize" (S.lifeOf S.carol after) (Just 30)

-- CR 614.11 / 121.6: Words of Worship ({2}{W} Enchantment, "{1}: The next time
-- you would draw a card this turn, you gain 5 life instead" -- name, cost, type
-- line and Oracle text checked against api.scryfall.com 2026-08-29).
--
-- The pool's GainLife DrawR producer -- Ring of Ma'rûf is the other, in
-- Pawl.OutsideTheGameSpec -- and the card-draw event class end to end: an
-- activated ability installs a floating row (CR 614.3) with Uses.Once and an
-- end-of-turn duration, and the next draw is replaced rather than rewritten --
-- CR 614.6's "it never happens", so no card leaves the library and CR 121.2's
-- tally does not move.
--
-- Every case is a PAIR of boards differing in one thing: whether the ability was
-- activated. Nothing else about the board changes, so "the draw was replaced" and
-- "she drew normally" are told apart by the life total AND the hand.
--
-- Every number is distinct -- 20 life, a gain of 5, two library cards, one card
-- drawn -- so no two readings land on the same total.
wordsOfWorshipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wordsOfWorshipSpec s registry = Spec.describe s "Words of Worship (CR 614.11)" $ do
  Spec.it s "CR 614.6 the draw never happens: alice gains 5 and her hand and library are untouched" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    let (armed, _) = wordsBoard plains wordsOfWorship piker True
        after = S.runPure S.identityAnswer armed (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 614.1a alice is at 25, so the row applied" (S.lifeOf S.alice after) (Just 25)
    Spec.assertEqWith s "CR 614.6 nothing reached her hand" (S.handSize S.alice after) 0
    Spec.assertEqWith s "CR 614.6 and nothing left her library" (length (Game.zoneMembers Zone.Library S.alice after)) 2
  -- The CONTROL: the same board with the ability never activated, so the only
  -- difference is the floating row.
  Spec.it s "CR 121.1 with no row installed the same board draws normally" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    let (unarmed, _) = wordsBoard plains wordsOfWorship piker False
        after = S.runPure S.identityAnswer unarmed (Event.drawCard S.alice)
    Spec.assertEqWith s "she gained nothing" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "CR 121.1 and drew the card" (S.handSize S.alice after) 1
    Spec.assertEqWith s "which came off her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- CR 614.3 / 614.1a: "the NEXT time", which is Uses.Once on the carrier. The
  -- second draw of the same turn is an ordinary draw.
  Spec.it s "CR 614.3 the row is used up, so the second draw of the turn is ordinary" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    let (armed, _) = wordsBoard plains wordsOfWorship piker True
        after = S.runPure S.identityAnswer armed (Event.drawCard S.alice >> Event.drawCard S.alice)
    Spec.assertEqWith s "CR 614.3 the second draw gained nothing more" (S.lifeOf S.alice after) (Just 25)
    Spec.assertEqWith s "CR 121.1 and put one card in her hand" (S.handSize S.alice after) 1
  -- CR 614.11's first sentence: "these effects are applied even if no cards could
  -- be drawn because there are no cards in the affected player's library". The
  -- funnel therefore proposes the event BEFORE it looks at the library, and a
  -- player whose library is empty gains the life instead of attempting the draw
  -- CR 121.4 and CR 704.5b would kill her for.
  Spec.it s "CR 614.11 an empty library still gets the life, and no failed draw is recorded" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    let armed = emptyLibraryBoard plains wordsOfWorship True
        after = S.runPure S.identityAnswer armed (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 614.11 the row applied off an empty library" (S.lifeOf S.alice after) (Just 25)
    Spec.assertBool s (not (Set.member S.alice (GameState.drewFromEmpty after))) "CR 121.4 no draw was attempted, so nothing is owed"
  -- The CONTROL for the case above, on the same empty library.
  Spec.it s "CR 121.4 with no row the same empty library records the failed draw" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    let unarmed = emptyLibraryBoard plains wordsOfWorship False
        after = S.runPure S.identityAnswer unarmed (Event.drawCard S.alice)
    Spec.assertEqWith s "she gained nothing" (S.lifeOf S.alice after) (Just 20)
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "CR 704.5b and the failed draw is on the books"
  -- CR 109.5's "you": the pattern is ControllerRelation.Yours, so bob's draw is
  -- not alice's, and the row is still there afterwards.
  --
  -- BOB's life is what discriminates, not alice's: the rewrite gains the life to
  -- the player the EVENT named, so a row that wrongly matched his draw would gain
  -- HIM the 5 and leave alice at 20 either way.
  Spec.it s "CR 109.5 the row watches its controller's draws and nobody else's" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    let (armed, _) = wordsBoard plains wordsOfWorship piker True
        stocked = snd (S.addLibraryCard piker S.bob armed)
        after = S.runPure S.identityAnswer stocked (Event.drawCard S.bob)
    Spec.assertEqWith s "CR 109.5 bob's draw is not alice's, so the rewrite never ran" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "CR 121.1 and bob drew his card" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice gained nothing either" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "CR 614.3 the row is unspent" (length (GameState.replacements after)) 1

  -- CR 614.3 / 608.2: the row is an effect of a resolution that has ENDED, so its
  -- CR 109.5 "you" was fixed when the ability resolved. Destroying the
  -- enchantment afterwards does not give the draw back.
  --
  -- What this discriminates is reading the relation off the candidate's baked
  -- controller against re-projecting the source: CR 400.7 makes the destroyed
  -- enchantment a new object in a graveyard, Projection.controllerOf answers
  -- Nothing for the id the row still carries, and a live reading silently stops
  -- matching (#2662).
  --
  -- LIFE first, and it cannot pass for the wrong reason: the rewrite gains to the
  -- player the event named, who is alice on either reading, so 25 is reachable
  -- only if the row applied to her draw at all.
  Spec.it s "CR 614.3 the row outlives its source leaving the battlefield" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    piker <- S.printingOf s registry "Goblin Piker"
    let (armed, enchantment) = wordsBoard plains wordsOfWorship piker True
        gone = S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [enchantment])
        after = S.runPure S.identityAnswer gone (Event.drawCard S.alice)
    Spec.assertBool s (not (Set.member enchantment (GameState.battlefield gone))) "setup: the enchantment really left the battlefield"
    Spec.assertEqWith s "CR 614.3 the row still applied, so alice is at 25" (S.lifeOf S.alice after) (Just 25)
    Spec.assertEqWith s "CR 614.6 and the draw still never happened" (S.handSize S.alice after) 0
  -- The same field one reading over: a CONTROL CHANGE. Confiscate ({4}{U}{U}
  -- Aura, "Enchant permanent / You control enchanted permanent") hands bob the
  -- enchantment, and the row stays alice's -- a live reading would answer bob and
  -- stop matching her draw.
  Spec.it s "CR 109.5 the row stays with the player who activated it, not the enchantment" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    confiscate <- S.printingOf s registry "Confiscate"
    piker <- S.printingOf s registry "Goblin Piker"
    let (armed, enchantment) = wordsBoard plains wordsOfWorship piker True
        (aura, g1) = S.addCreature confiscate S.bob armed
        stolen = S.attachTo aura (Recipient.ToObject enchantment) g1
        after = S.runPure S.identityAnswer stolen (Event.drawCard S.alice)
    Spec.assertEqWith s "setup: bob really controls the enchantment now" (Projection.controllerOf enchantment stolen) (Just S.bob)
    Spec.assertEqWith s "CR 109.5 the row is still alice's, so she is at 25" (S.lifeOf S.alice after) (Just 25)
    Spec.assertEqWith s "CR 614.6 and her draw still never happened" (S.handSize S.alice after) 0

-- alice with a Plains, a Words of Worship and two library cards, with the
-- ability activated and resolved when `arm` is True and untouched when it is
-- False. One builder for both legs, so the two boards differ in that alone.
wordsBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (GameState.GameState, ObjectId.ObjectId)
wordsBoard plains wordsOfWorship stock arm =
  let base = S.landsInPlay plains 1
      (enchantment, g1) = S.addCreature wordsOfWorship S.alice base
      g2 = snd (S.addLibraryCard stock S.alice (snd (S.addLibraryCard stock S.alice g1)))
      final =
        if arm
          then S.runPure S.identityAnswer g2 (Activate.activateAbility S.alice enchantment (theAbility wordsOfWorship) >> Stack.resolveTop)
          else g2
   in (final, enchantment)

-- wordsBoard with no library at all, which is CR 614.11's own board.
emptyLibraryBoard :: Printing.Printing -> Printing.Printing -> Bool -> GameState.GameState
emptyLibraryBoard plains wordsOfWorship arm =
  let base = S.landsInPlay plains 1
      (enchantment, g1) = S.addCreature wordsOfWorship S.alice base
   in if arm
        then S.runPure S.identityAnswer g1 (Activate.activateAbility S.alice enchantment (theAbility wordsOfWorship) >> Stack.resolveTop)
        else g1

-- CR 121.2a / 616.1g: Alms Collector ({3}{W} Creature -- Cat Cleric, 3/4, "Flash /
-- If an opponent would draw two or more cards, instead you and that player each
-- draw a card." -- name, cost, type line, power, toughness and Oracle text checked
-- against api.scryfall.com 2026-09-02).
--
-- The pool's one DrawCountR producer, and the INSTRUCTION event class end to end.
-- Words of Worship above watches one of CR 121.2's individual draws; this row
-- watches the instruction that names how many of them there are, which CR 616.1g
-- settles first.
--
-- Every board is three-seated, so "an opponent" (CR 102.2) is not the seat that
-- cast the spell: carol casts Ancestral Recall at bob while ALICE holds the
-- collector, which a rewrite reading the resolution's controller rather than the
-- row's would get wrong here and right on a two-player board by accident.
--
-- Every number is distinct -- an instruction naming three cards, four cards in
-- bob's library, three in alice's, two in carol's, one card drawn each -- so no two
-- readings land on the same count.
almsCollectorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
almsCollectorSpec s registry = Spec.describe s "Alms Collector (CR 121.2a)" $ do
  Spec.it s "CR 121.2a an opponent's instruction to draw three becomes one card each" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    collector <- S.printingOf s registry "Alms Collector"
    recall <- S.printingOf s registry "Ancestral Recall"
    let (gs, held) = collectorBoard island piker collector recall True
        after = S.runPure (atPlayerAnswer S.bob) gs (S.cast S.carol held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 121.2a bob's instruction to draw three left him one card" (S.handSize S.bob after) 1
    Spec.assertEqWith s "CR 614.1a and alice, whose collector applied, drew the other" (S.handSize S.alice after) 1
    Spec.assertEqWith s "carol, who cast it, drew none of them" (S.handSize S.carol after) 0
    Spec.assertEqWith s "CR 121.2 one card left bob's library of four" (length (Game.zoneMembers Zone.Library S.bob after)) 3
  -- The CONTROL: the same board with no collector on it, so the only difference is
  -- the row.
  Spec.it s "CR 121.2 with no collector the same instruction draws all three" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    collector <- S.printingOf s registry "Alms Collector"
    recall <- S.printingOf s registry "Ancestral Recall"
    let (gs, held) = collectorBoard island piker collector recall False
        after = S.runPure (atPlayerAnswer S.bob) gs (S.cast S.carol held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "bob drew all three" (S.handSize S.bob after) 3
    Spec.assertEqWith s "and alice drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "three cards left bob's library of four" (length (Game.zoneMembers Zone.Library S.bob after)) 1
  -- CR 102.2 / 109.5: "an opponent", read against the row's own controller, and
  -- alice is not her own opponent. The same spell and the same count, the other
  -- target.
  Spec.it s "CR 102.2 the collector's controller draws her own three cards in full" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    collector <- S.printingOf s registry "Alms Collector"
    recall <- S.printingOf s registry "Ancestral Recall"
    let (gs, held) = collectorBoard island piker collector recall True
        after = S.runPure (atPlayerAnswer S.alice) gs (S.cast S.carol held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 102.2 alice's own instruction is not an opponent's, so she drew three" (S.handSize S.alice after) 3
    Spec.assertEqWith s "and bob drew nothing" (S.handSize S.bob after) 0
    Spec.assertEqWith s "her library of three is empty" (length (Game.zoneMembers Zone.Library S.alice after)) 0
  -- CR 614.1a: "two or more". An instruction naming ONE card does not meet the
  -- row's condition, so it is left alone -- Greed ({3}{B} Enchantment, "{2}{B}, Pay
  -- 2 life: Draw a card"), the one-card instruction the Bloodletter group above
  -- uses, put under bob's control so its drawer is alice's opponent.
  Spec.it s "CR 614.1a an opponent's one-card instruction is under the threshold" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    collector <- S.printingOf s registry "Alms Collector"
    greed <- S.printingOf s registry "Greed"
    let (_, g1) = S.addCreature collector S.alice S.threePlayerGame
        (bobsGreed, g2) = S.addCreature greed S.bob g1
        g3 = snd (S.addLibraryCard piker S.alice (snd (S.addLibraryCard piker S.bob g2)))
        ready = inMainPhase S.alice (S.landsFor swamp S.bob 3 g3)
        after = S.runPure S.identityAnswer ready (Activate.activateAbility S.bob bobsGreed (theAbility greed) Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 614.1a bob drew his one card" (S.handSize S.bob after) 1
    Spec.assertEqWith s "and alice, the instruction being under the threshold, drew nothing" (S.handSize S.alice after) 0
  -- CR 616.1g: "one replacement effect may apply to an event, and another may apply
  -- to an event contained within the first". The instruction is the outer event and
  -- each draw it leaves is an inner one, so Words of Worship still meets alice's
  -- half of the collector's rewrite.
  --
  -- CR 121.2c puts alice's draw first, she being the active player, which is what
  -- lets it be the draw the once-only row (CR 614.3) meets.
  Spec.it s "CR 616.1g a per-draw row still applies to a draw the instruction was replaced with" $ do
    island <- S.printingOf s registry "Island"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    collector <- S.printingOf s registry "Alms Collector"
    recall <- S.printingOf s registry "Ancestral Recall"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    let (gs, held) = collectorBoard island piker collector recall True
        (enchantment, g1) = S.addCreature wordsOfWorship S.alice gs
        armed = S.runPure S.identityAnswer (S.landsFor plains S.alice 1 g1) (Activate.activateAbility S.alice enchantment (theAbility wordsOfWorship) Monad.>> Stack.resolveTop)
        after = S.runPure (atPlayerAnswer S.bob) armed (S.cast S.carol held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 614.6 alice's half of the rewrite was itself replaced, so she gained 5" (S.lifeOf S.alice after) (Just 25)
    Spec.assertEqWith s "and drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "CR 121.2a bob still drew the one card the replacement left him" (S.handSize S.bob after) 1
  -- CR 616.1 through Replacement.readsApplier: two collectors under DIFFERENT
  -- controllers are not value-equal, because which one applies decides who gets the
  -- extra card. So the affected player -- bob, whom the instruction names -- is
  -- asked, and the answer is pinned by source id rather than by candidate order.
  --
  -- bob casts it at himself, so that alice and carol both hold a matching row and
  -- the pair differs in nothing but its controller.
  Spec.it s "CR 616.1 two collectors under different controllers are told apart, and the drawer chooses" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    collector <- S.printingOf s registry "Alms Collector"
    recall <- S.printingOf s registry "Ancestral Recall"
    let (alices, g1) = S.addCreature collector S.alice S.threePlayerGame
        (carols, g2) = S.addCreature collector S.carol g1
        g3 = stockLibraries piker g2
        (held, g4) = S.addHandCard recall S.bob g3
        ready = inMainPhase S.alice (S.landsFor island S.bob 1 g4)
        cast = S.cast S.bob held Monad.>> Stack.resolveTop
        toAlice = S.runPure (collectorRaceAnswer alices) ready cast
        toCarol = S.runPure (collectorRaceAnswer carols) ready cast
        asked = answersFor (collectorRaceAnswer alices) ready cast
    Spec.assertEqWith s "CR 616.1 taking alice's collector hands alice the card" (S.handSize S.alice toAlice) 1
    Spec.assertEqWith s "and carol none of it" (S.handSize S.carol toAlice) 0
    Spec.assertEqWith s "CR 616.1 taking carol's hands carol the card instead" (S.handSize S.carol toCarol) 1
    Spec.assertEqWith s "and alice none of it" (S.handSize S.alice toCarol) 0
    Spec.assertEqWith s "CR 121.2a either way bob's instruction of three left him one card" (S.handSize S.bob toAlice) 1
    Spec.assertBool s (wasAskedToReplace asked) "a ChooseReplacement was raised"

-- Aim a spell's player slot at one seat -- Ancestral Recall's "target player".
atPlayerAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
atPlayerAnswer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer pid))) sets
  _ -> S.identityAnswer p

-- atPlayerAnswer aimed at bob, plus a CR 616.1 race answered by the candidate whose
-- SOURCE is `preferred` -- by id, so the assertion does not depend on the engine's
-- canonical candidate order. raceAnswer above is the same shape over a creature
-- target rather than a player one.
collectorRaceAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> r
collectorRaceAnswer preferred p = case p of
  Prompt.ChooseReplacement _ _ entries -> maybe 0 Int.toNaturalSaturating (List.findIndex ((== preferred) . ReplacementEntry.source) entries)
  _ -> atPlayerAnswer S.bob p

-- Four cards for bob, three for alice, two for carol: distinct depths, so a hand
-- size and a library size cannot agree by coincidence, and no seat is decked out
-- from under an assertion (CR 104.3c).
stockLibraries :: Printing.Printing -> GameState.GameState -> GameState.GameState
stockLibraries piker gs =
  let stock pid n g = List.foldl' (\g' _ -> snd (S.addLibraryCard piker pid g')) g [1 .. (n :: Int)]
   in stock S.bob 4 (stock S.alice 3 (stock S.carol 2 gs))

-- alice holds the collector when `collecting`, carol holds Ancestral Recall and the
-- Island to cast it, and the libraries are stocked by stockLibraries.
collectorBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> (GameState.GameState, ObjectId.ObjectId)
collectorBoard island piker collector recall collecting =
  let g0 = S.threePlayerGame
      g1 = if collecting then snd (S.addCreature collector S.alice g0) else g0
      (held, g2) = S.addHandCard recall S.carol (stockLibraries piker g1)
   in (inMainPhase S.alice (S.landsFor island S.carol 1 g2), held)

-- CR 614.6 with CR 119.4: Ashiok, Wicked Manipulator ({3}{B}{B} Legendary
-- Planeswalker -- Ashiok, loyalty 5, "If you would pay life while your library
-- has at least that many cards in it, exile that many cards from the top of your
-- library instead." -- name, cost, type line, loyalty and Oracle text checked
-- against api.scryfall.com 2026-08-28).
--
-- The one life-total row that CANCELS. Worship's floor and Bloodletter's doubling
-- above both resize the loss and leave a life loss standing; this removes the
-- event and runs a different action, which is what CR 614.6 describes and what no
-- other printing in data/cards/ asks of this event class.
--
-- Its own rulings fix all three readings this group asserts, and are quoted as
-- printed, misnamed card and all: "Ashiok, Wicked
-- Nightmare's first ability isn't optional. You can't choose to pay life instead
-- of exiling cards from the top of your library ... and you can't split the
-- payment between life and cards"; "If you would pay life while you control
-- Ashiok and your library does not have at least that many cards in it, you'll
-- just pay life as normal"; "Ashiok's first ability doesn't allow you to attempt
-- to pay an amount of life greater than your current life total."
--
-- Not implemented: the three loyalty abilities, none of whose effects pawl has --
-- a look-at-two-and-split, a token minted with an exile-conditioned trigger, and
-- an exile-from-the-top counted by mana value in exile (#2551). pawl's card is
-- STRICTER than printed: it can do nothing its printing cannot.
--
-- Greed ({3}{B} Enchantment, "{B}, Pay 2 life: Draw a card" -- checked the same
-- day) is the payer, ReplacementSpec's Bloodletter group's choice for the same
-- reason: it charges CR 119.4's payment as a COST, which is the road this row
-- watches, and the mana half of that cost is one Swamp.
-- CR 614.1a / 119.10: Boon Reflection ({4}{W} Enchantment, "If you would gain
-- life, you gain twice that much life instead." -- name, cost, type line and
-- Oracle text checked against api.scryfall.com 2026-09-04).
--
-- The life-total class's GAIN half, and the mirror of the Bloodletter group
-- above: that row watches CR 119.3's downward direction and this one the upward,
-- which CR 119.10 reads as "if a source would cause you to gain life". The
-- producer was picked over its two siblings for having nothing else printed on
-- it -- Rhox Faithmender carries lifelink and Alhammarret's Archive a second
-- replacement, and each would put a second row on the board under test.
--
-- Its own rulings fix the two readings this group asserts and the CR does not
-- spell out: "if you have 3 life and an effect says that your life total
-- 'becomes 10,' your life total will actually become 17", which is CR 119.5's
-- gain proposed like any other, and "if you control two Boon Reflections, you'll
-- gain four times the original amount of life", which is CR 616.2 re-collecting
-- against the rewritten event rather than CR 614.5 spending both rows on one.
--
-- THREE SEATS wherever the clause's CR 109.5 "you" is the question, because a
-- two-player board cannot tell "the row's controller" from "the player the event
-- names" when one seat holds both roles.
boonReflectionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
boonReflectionSpec s registry = Spec.describe s "Boon Reflection (CR 119.10 / 614.1a)" $ do
  -- CR 119.3's plain road: Pawl.Engine.Resolve's Effect.GainLife arm. ONE board,
  -- two casts of the SAME spell, so the only thing that differs between the two
  -- answers is which seat controls the row. Blossoming Calm ({W} Instant,
  -- "Choose one -- * You gain 2 life. * ..." in pawl's transcription: hexproof
  -- until your next turn and 2 life) is an instant, so bob may cast his on
  -- alice's main phase.
  --
  -- Every number distinct -- a gain of 2, a doubled 4, and totals of 24, 22 and
  -- 20 -- so no two readings of the rule land on the same total.
  Spec.it s "CR 119.3 an effect's life gain is doubled for the row's controller and nobody else" $ do
    plains <- S.printingOf s registry "Plains"
    calm <- S.printingOf s registry "Blossoming Calm"
    boon <- S.printingOf s registry "Boon Reflection"
    let (_, g1) = S.addCreature boon S.alice S.threePlayerGame
        (alicesCalm, g2) = S.addHandCard calm S.alice g1
        (bobsCalm, g3) = S.addHandCard calm S.bob g2
        ready = S.landsFor plains S.bob 1 (S.landsFor plains S.alice 1 (inMainPhase S.alice g3))
        after =
          S.runPure
            S.identityAnswer
            ready
            ( S.cast S.alice alicesCalm
                Monad.>> Stack.resolveTop
                Monad.>> S.cast S.bob bobsCalm
                Monad.>> Stack.resolveTop
            )
    Spec.assertEqWith s "CR 614.1a alice's printed gain of 2 becomes 4" (S.lifeOf S.alice after) (Just 24)
    Spec.assertEqWith s "CR 109.5 bob is not the row's you, so his same 2 stays 2" (S.lifeOf S.bob after) (Just 22)
    Spec.assertEqWith s "and carol, who gained nothing, is untouched" (S.lifeOf S.carol after) (Just 20)
  -- CR 616.2 rather than CR 614.5: the rewritten event is re-collected, so the
  -- SECOND row applies to the doubled gain. The card's own ruling states the
  -- answer -- "if you control two Boon Reflections, you'll gain four times the
  -- original amount" -- and the board is the case above with one more row, so the
  -- two differ in exactly one thing.
  Spec.it s "CR 616.2 two rows compound rather than one spending the event" $ do
    plains <- S.printingOf s registry "Plains"
    calm <- S.printingOf s registry "Blossoming Calm"
    boon <- S.printingOf s registry "Boon Reflection"
    let (_, g1) = S.addCreature boon S.alice S.threePlayerGame
        (_, g2) = S.addCreature boon S.alice g1
        (alicesCalm, g3) = S.addHandCard calm S.alice g2
        ready = S.landsFor plains S.alice 1 (inMainPhase S.alice g3)
        after = S.runPure S.identityAnswer ready (S.cast S.alice alicesCalm Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "2 doubled twice is 8, not the 4 one row alone gives" (S.lifeOf S.alice after) (Just 28)
  -- CR 120.3f: lifelink's gain is a life gain event like any other, so the row
  -- reaches it -- which is the half of this unit that no Effect.GainLife road can
  -- prove, Pawl.Engine.Damage having its own write.
  --
  -- Sanguine Bond ({3}{B}{B} Enchantment, "Whenever you gain life, target
  -- opponent loses that much life") is what makes the RECORDED amount observable
  -- at gameplay level: a gain written as 6 and recorded as 3 would leave bob's
  -- own total wrong while alice's looked right.
  --
  -- REAL COMBAT with Celestine, the Living Saint (3/4 lifelink) attacking, the
  -- Worship group's fixture reused. TWO SEATS here rather than three, because
  -- Sanguine Bond's "target opponent" would otherwise be a choice this case does
  -- not mean to make; the CR 109.5 discrimination is the case above's job.
  --
  -- Every number distinct -- 3 dealt, 6 gained, 6 drained, bob at 11 and alice at
  -- 26 -- and the unreplaced reading (bob at 14, alice at 23) shares none of them.
  Spec.it s "CR 120.3f lifelink's gain goes through the same funnel" $ do
    plains <- S.printingOf s registry "Plains"
    boon <- S.printingOf s registry "Boon Reflection"
    bond <- S.printingOf s registry "Sanguine Bond"
    celestine <- S.printingOf s registry "Celestine, the Living Saint"
    let armed extra =
          let (_, g1) = S.addCreature bond S.bob (S.landsInPlay plains 2)
              (_, g2) = S.addCreature celestine S.bob g1
           in S.runCombat attackNoBlock (bobAttacks (extra g2))
        doubled = armed (snd . S.addCreature boon S.bob)
        control = armed id
    Spec.assertEqWith s "CR 614.1a bob's lifelink 3 becomes 6" (S.lifeOf S.bob doubled) (Just 26)
    Spec.assertEqWith s "and the Bond drains the SETTLED 6, so alice takes 3 + 6" (S.lifeOf S.alice doubled) (Just 11)
    Spec.assertEqWith s "the same board without the row gains bob the printed 3" (S.lifeOf S.bob control) (Just 23)
    Spec.assertEqWith s "and drains alice 3, so she takes 3 + 3" (S.lifeOf S.alice control) (Just 14)
  -- CR 614.11's substituted gain, the fourth road into the funnel: Words of
  -- Worship's "the next time you would draw a card this turn, you gain 5 life
  -- instead" gains life through a REPLACEMENT rather than through an effect, and
  -- rule 119.10 knows no difference -- a source caused alice to gain life.
  --
  -- The board is the Words of Worship group's armed one with a row added, so the
  -- two differ in exactly one thing, and the draw is still cancelled either way.
  Spec.it s "CR 614.11 the life a draw replacement substitutes is a gain the row resizes" $ do
    plains <- S.printingOf s registry "Plains"
    wordsOfWorship <- S.printingOf s registry "Words of Worship"
    boon <- S.printingOf s registry "Boon Reflection"
    piker <- S.printingOf s registry "Goblin Piker"
    let (armed, _) = wordsBoard plains wordsOfWorship piker True
        after = S.runPure S.identityAnswer (snd (S.addCreature boon S.alice armed)) (Event.drawCard S.alice)
    Spec.assertEqWith s "CR 614.1a the substituted 5 becomes 10, so alice is at 30" (S.lifeOf S.alice after) (Just 30)
    Spec.assertEqWith s "CR 614.6 and the draw still never happened" (S.handSize S.alice after) 0
  -- CR 119.5's upward direction, through Pawl.Engine.Resolve's changeLifeByDelta:
  -- "if an effect sets a player's life total to a specific number, the player
  -- gains or loses the necessary amount of life". The card's own ruling is the
  -- exact claim -- "your life total will actually become 17" from 3 on a set to
  -- 10 -- so the resulting total OVERSHOOTS the number the effect named.
  --
  -- Biorhythm ({6}{G}{G} Sorcery, "Each player's life total becomes the number of
  -- creatures they control") is the Bloodletter group's producer one direction
  -- over: one instruction, three seats, three answers off one pre-effect board
  -- (CR 608.2f). carol holds the row and is the only seat whose total goes UP.
  --
  -- Every number distinct -- alice to 1, bob to 2, carol from 2 by a gain of 1
  -- doubled to 2 -- so the unreplaced reading of carol (3) collides with nothing.
  Spec.it s "CR 119.5 a total set HIGHER is a life gain the row resizes" $ do
    forest <- S.printingOf s registry "Forest"
    biorhythm <- S.printingOf s registry "Biorhythm"
    boon <- S.printingOf s registry "Boon Reflection"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature boon S.carol S.threePlayerGame
        (_, g2) = S.addCreature piker S.alice g1
        (_, g3) = S.addCreature piker S.bob g2
        (_, g4) = S.addCreature piker S.bob g3
        (_, g5) = S.addCreature piker S.carol g4
        (_, g6) = S.addCreature piker S.carol g5
        (_, g7) = S.addCreature piker S.carol g6
        (held, g8) = S.addHandCard biorhythm S.alice g7
        ready = inMainPhase S.alice (atLife S.carol 2 (S.landsFor forest S.alice 8 g8))
        after = S.runPure S.identityAnswer ready (S.cast S.alice held Monad.>> Stack.resolveTop)
    Spec.assertEqWith s "CR 119.5 carol gains the 1 that would reach 3, doubled, so she ends at 4" (S.lifeOf S.carol after) (Just 4)
    Spec.assertEqWith s "alice's total falls to her one creature, and a loss is no gain to resize" (S.lifeOf S.alice after) (Just 1)
    Spec.assertEqWith s "and bob's falls to his two, the row being carol's alone" (S.lifeOf S.bob after) (Just 2)

ashiokSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ashiokSpec s registry = Spec.describe s "Ashiok, Wicked Manipulator (CR 119.4 / 614.6)" $ do
  -- Every number distinct: three cards in the library, two paid for, one left,
  -- and a life total that does not move at all. The SURVIVOR is named rather than
  -- counted, which is what makes "from the TOP" an assertion rather than a
  -- coincidence -- Pawl.Support.addLibraryCard prepends, so `bottom` goes in first
  -- and is the only card two exiles off the top can leave behind. The exiled cards
  -- are counted instead of named, because CR 400.7 gives each a new id as it
  -- arrives and the library ids they had are gone.
  Spec.it s "CR 614.6 a payment is replaced: the cards are exiled and no life is lost" $ do
    swamp <- S.printingOf s registry "Swamp"
    greed <- S.printingOf s registry "Greed"
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature ashiok S.alice S.threePlayerGame
        (alicesGreed, g2) = S.addCreature greed S.alice g1
        (bottom, g3) = S.addLibraryCard piker S.alice g2
        (_, g4) = S.addLibraryCard piker S.alice g3
        (_, g5) = S.addLibraryCard piker S.alice g4
        ready = S.landsFor swamp S.alice 1 g5
        after = S.runPure S.identityAnswer ready (Activate.activateAbility S.alice alicesGreed (theAbility greed))
    Spec.assertEqWith s "CR 614.6 the loss never happens, so alice is still at 20" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and the 2 life she would have paid is 2 cards exiled instead" (length (Game.zoneMembers Zone.Exile S.alice after)) 2
    Spec.assertEqWith s "leaving the ONE card that was under them, by identity" (Game.zoneMembers Zone.Library S.alice after) [bottom]
  -- CR 119.4's applicability clause read against the EVENT's amount rather than
  -- against a printed constant: the same board, the same payment, one card fewer
  -- in the library. Its ruling: "you'll just pay life as normal."
  Spec.it s "CR 119.4 a library shorter than the payment leaves the payment alone" $ do
    swamp <- S.printingOf s registry "Swamp"
    greed <- S.printingOf s registry "Greed"
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature ashiok S.alice S.threePlayerGame
        (alicesGreed, g2) = S.addCreature greed S.alice g1
        (_, g3) = S.addLibraryCard piker S.alice g2
        ready = S.landsFor swamp S.alice 1 g3
        after = S.runPure S.identityAnswer ready (Activate.activateAbility S.alice alicesGreed (theAbility greed))
    Spec.assertEqWith s "CR 119.4 one card cannot cover a payment of 2, so alice pays: 20 - 2 = 18" (S.lifeOf S.alice after) (Just 18)
    Spec.assertEqWith s "and nothing is exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0
    Spec.assertEqWith s "her one card is still in her library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- CR 120.4c, the cause control: LifeLossPattern's whichCause names ByPayment, so
  -- a loss that is not a payment must not reach this row at all. The library is
  -- stocked to 3 and the damage is 3, so the clause the case above turns on is
  -- SATISFIED here and the cause is the only thing left to stop the row. Worship's
  -- ruling states the same split from the other side: "loss of life bypasses
  -- Worship."
  Spec.it s "CR 120.4c damage is not a payment, so the row does not see it" $ do
    ashiok <- S.printingOf s registry "Ashiok, Wicked Manipulator"
    piker <- S.printingOf s registry "Goblin Piker"
    let (_, g1) = S.addCreature ashiok S.alice S.threePlayerGame
        (bobsPiker, g2) = S.addCreature piker S.bob g1
        stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) g2 [1 .. (3 :: Int)]
        after =
          S.runPure
            S.identityAnswer
            stocked
            (Damage.applyDamage [DamageEvent.MkDamageEvent bobsPiker (Recipient.ToPlayer S.alice) 3 False False False 0 Nothing DamageKind.Noncombat])
    Spec.assertEqWith s "CR 120.4c alice loses the damage as life: 20 - 3 = 17" (S.lifeOf S.alice after) (Just 17)
    Spec.assertEqWith s "and her library, long enough to have covered it, is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 3
    Spec.assertEqWith s "with nothing exiled" (length (Game.zoneMembers Zone.Exile S.alice after)) 0

-- Cast Divine Deflection for `x`, aiming CR 615.5's rider at a player. The target
-- is FILTERED out of the offered set rather than built, so an aim the card's pool
-- excludes leaves the slot empty instead of quietly becoming a legal one.
castDeflection :: Natural.Natural -> PlayerId.PlayerId -> Prompt.Prompt r -> r
castDeflection x pid p = case p of
  Prompt.ChooseX {} -> x
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer pid) . snd) sets
  _ -> S.identityAnswer p

-- Attack with everything, block `attacker` with `blocker`, and spend a contested
-- prevention shield on the batch's hits in `wanted` order -- by RECIPIENT rather
-- than by position, so the assertions do not depend on the order the batch was
-- gathered in.
deflectionCombat :: ObjectId.ObjectId -> ObjectId.ObjectId -> [Recipient.Recipient] -> Prompt.Prompt r -> r
deflectionCombat blocker attacker wanted p = case p of
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers {} -> Map.singleton blocker (Set.singleton attacker)
  Prompt.OrderDamage _ _ events ->
    let rank e = Maybe.fromMaybe (length wanted) (List.elemIndex (DamageEvent.target e) wanted)
     in fmap fst (List.sortOn (rank . snd) (zip [0 ..] events))
  _ -> S.identityAnswer p

-- Cast Molten Disaster UNKICKED for `x`, spending a contested shield on the
-- batch's hits in `wanted` order. The kicker answer is PINNED rather than
-- deferred: kicking it turns on a static ability that grants split second (CR
-- 702.61a), and the case below is about the damage sentence alone.
--
-- The order is stated by RECIPIENT, deflectionCombat's shape, so the assertions
-- do not depend on the order the instruction's own sweep gathered the batch in.
castDisaster :: Natural.Natural -> [Recipient.Recipient] -> Prompt.Prompt r -> r
castDisaster x wanted p = case p of
  Prompt.ChooseKicker {} -> KickerDecision.MkKickerDecision 0
  Prompt.ChooseX {} -> x
  Prompt.OrderDamage _ _ events ->
    let rank e = Maybe.fromMaybe (length wanted) (List.elemIndex (DamageEvent.target e) wanted)
     in fmap fst (List.sortOn (rank . snd) (zip [0 ..] events))
  _ -> S.identityAnswer p

-- Aim CR 115.4's "any target" at `victim` and spend a contested prevention
-- shield on the batch's hits in `wanted` order. The target is FILTERED out of
-- the offered set rather than built, castDeflection's shape, and the order is
-- stated by RECIPIENT, castDisaster's.
aimCreatureAndOrder :: ObjectId.ObjectId -> [Recipient.Recipient] -> Prompt.Prompt r -> r
aimCreatureAndOrder victim wanted p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature victim) . snd) sets
  Prompt.OrderDamage _ _ events ->
    let rank e = Maybe.fromMaybe (length wanted) (List.elemIndex (DamageEvent.target e) wanted)
     in fmap fst (List.sortOn (rank . snd) (zip [0 ..] events))
  _ -> S.identityAnswer p

-- Win Winter Sky's CR 705.2 call, then spend a contested shield in `wanted`
-- order. The flip is pinned rather than deferred: the losing branch draws cards
-- instead of dealing damage, and the case below is about the damage sentence.
winTheFlipAndOrder :: [Recipient.Recipient] -> Prompt.Prompt r -> r
winTheFlipAndOrder wanted p = case p of
  Prompt.FlipCoin -> CoinFace.Heads
  Prompt.CallCoin {} -> CoinFace.Heads
  Prompt.OrderDamage _ _ events ->
    let rank e = Maybe.fromMaybe (length wanted) (List.elemIndex (DamageEvent.target e) wanted)
     in fmap fst (List.sortOn (rank . snd) (zip [0 ..] events))
  _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Replacement" $ do
  worshipSpec s registry
  wordsOfWorshipSpec s registry
  almsCollectorSpec s registry
  bloodletterSpec s registry
  ashiokSpec s registry
  boonReflectionSpec s registry

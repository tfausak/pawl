{-# LANGUAGE GADTs #-}

-- Covers: CR 701.65 AIRBEND -- Pawl.Engine.Airbend, Effect.Airbend's arm in
-- Pawl.Engine.Resolve.Effect (and the Binding.airbentObjects stamp it reads
-- back), and the alternative cost Pawl.Types.ExilePlayPermission now carries into
-- Pawl.Engine.Cost.candidateCostsGiven's exile arm.
--
-- Airbending Lesson ({2}{W} Instant -- Lesson, "Airbend target nonland
-- permanent. Draw a card.") is the fixture: the airbend is its whole first
-- clause and states nothing the rulebook does not, so every assertion below is
-- about rule 701.65a.
--
-- THE BOARD SHAPE that makes the cases discriminating: BOB owns and controls TWO
-- Hill Giants, identical in every respect, and alice's spell names ONE of them.
-- So the exile is rule 701.65a and not a fact about Hill Giants, and the
-- permission is the exiled card's OWNER's and not the airbending spell's
-- controller's -- alice casts and bob gets the offer.
--
-- Hill Giant is a {3}{R} vanilla, and bob's two Mountains are what separate rule
-- 701.65a's {2} from every other reading: at that amount the alternative cost is
-- payable and the printed one is not, so the third Hill Giant in bob's HAND --
-- same card, same lands, same turn -- is the paired negative that differs in one
-- thing. The one-Mountain board below is the other half, and separates {2} from
-- CR 118.9's {0}.
module Pawl.AirbendSpec where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- Aim a target slot at this permanent, PINNED rather than searched: an answerer
-- that took whatever was legal would find the other Hill Giant after a mutation
-- and keep the case green.
--
-- FILTERED out of the offered set rather than built from the id: CR 115.1's pool
-- of permanents offers its own Recipient, and a hand-built one of the same
-- permanent need not be what CR 608.2b re-reads at resolution.
aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedAt victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter ((== Just victim) . Recipient.objectOf) offered) sets
  _ -> S.castAnswer p

-- alice: six Plains and Airbending Lesson in hand, plus a library card so the
-- spell's own draw does not deck her (CR 104.3c). Six and not three: casting the
-- Lesson taps three, and the rest are what let the "alice may not cast it" case
-- fail for the permission rather than for the mana.
--
-- bob: `mountains` Mountains, two Hill Giants on the battlefield and a third in
-- hand.
--
-- Returns (victim, twin, the spare in bob's hand, the Lesson, state).
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
board plains mountain giant lesson mountains =
  let (victim, g1) = S.addPermanent giant S.bob (S.landsFor mountain S.bob mountains (S.landsInPlay plains 6))
      (twin, g2) = S.addPermanent giant S.bob g1
      (spare, g3) = S.addHandCard giant S.bob g2
      (spell, g4) = S.addHandCard lesson S.alice g3
      (_, g5) = S.addLibraryCard plains S.alice g4
   in (victim, twin, spare, spell, g5)

-- alice casts the Lesson at the victim and it resolves.
airbent :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
airbent victim spell gs =
  let cast = S.runPure (aimedAt victim) gs (S.cast S.alice spell)
   in S.runPure (aimedAt victim) cast Stack.resolveTop

-- Hand this player the turn, so CR 302.1's timing lets them cast a creature. STATED rather than run, Pawl.EarthbendSpec's combat posture: nothing
-- here is about the turn-based actions in between.
--
-- LOAD-BEARING and not tidiness: Pawl.Engine.Setup's board opens in the untap
-- step, where no creature is castable by anybody, and a "this player may not
-- cast it" case asked there passes for the timing rather than for the
-- permission.
mainPhaseOf :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
mainPhaseOf pid gs = gs {GameState.activePlayer = pid, GameState.phase = Phase.PrecombatMain}

-- The one card in bob's exile, which is the incarnation rule 701.65a's second
-- sentence is about (CR 400.7).
exiledCard :: GameState.GameState -> Maybe ObjectId.ObjectId
exiledCard gs = case Game.zoneMembers Zone.Exile S.bob gs of
  [only] -> Just only
  _ -> Nothing

-- How many of this player's lands of this printing are still untapped, which is
-- the reading that says what a cast actually paid.
untappedNamedFor :: PlayerId.PlayerId -> Printing.Printing -> GameState.GameState -> Int
untappedNamedFor pid printing gs =
  let untapped oid =
        fmap Object.tapped (Game.lookupObject oid gs) == Just TapState.Untapped
          && fmap S.nameOf (Game.cardOf oid gs) == Just (S.printingName printing)
   in length (filter untapped (Game.zoneMembers Zone.Battlefield pid gs))

untappedNamed :: Printing.Printing -> GameState.GameState -> Int
untappedNamed = untappedNamedFor S.bob

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Airbend" $ do
  Spec.it s "CR 701.65a the airbent permanent is exiled, and the one beside it is not" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    giant <- S.printingOf s registry "Hill Giant"
    lesson <- S.printingOf s registry "Airbending Lesson"
    let (victim, twin, _, spell, before) = board plains mountain giant lesson 2
        after = airbent victim spell before
    Spec.assertBool s (notElem victim (Game.zoneMembers Zone.Battlefield S.bob after)) "CR 701.65a the airbent permanent left the battlefield"
    Spec.assertEqWith s "CR 400.7 and a card of it is in exile" (length (Game.zoneMembers Zone.Exile S.bob after)) 1
    -- The twin on the SAME board is untouched, so the exile is not a fact about
    -- Hill Giants, and it is still there before the airbend too.
    Spec.assertBool s (List.elem twin (Game.zoneMembers Zone.Battlefield S.bob after)) "the Hill Giant beside it stayed"
    Spec.assertBool s (List.elem victim (Game.zoneMembers Zone.Battlefield S.bob before)) "and the victim was on the battlefield before the spell"
  Spec.it s "CR 701.65a its owner casts the exiled card for {2} rather than its mana cost" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    giant <- S.printingOf s registry "Hill Giant"
    lesson <- S.printingOf s registry "Airbending Lesson"
    let (victim, _, spare, spell, before) = board plains mountain giant lesson 2
        after = mainPhaseOf S.bob (airbent victim spell before)
        recast oid = S.runPure S.castAnswer after (S.cast S.bob oid *> Stack.resolveTop)
    case exiledCard after of
      Nothing -> Spec.assertBool s False "the airbend put exactly one card into bob's exile"
      Just exiled -> do
        -- THE gameplay reading, and first: bob pays {2} out of two Mountains and
        -- the Hill Giant is back on the battlefield, making two of them.
        Spec.assertEqWith s "CR 701.65a the owner's cast of the exiled card resolves" (S.countOnBattlefieldByName (S.printingName giant) S.bob (recast exiled)) 2
        -- The amount, which the resolve above does not pin on its own: both
        -- Mountains are tapped, so the cast paid {2} and not CR 118.9's {0}.
        Spec.assertEqWith s "and it cost {2}, so no Mountain is left untapped" (untappedNamed mountain (recast exiled)) 0
        -- The paired negative, differing in ONE thing: the same card, the same
        -- two Mountains, the same turn -- but in bob's hand, where its printed
        -- {3}{R} is what he owes.
        Spec.assertBool s (not (S.castable S.bob spare after)) "the same card in hand, at the same two lands, is unaffordable"
  -- CR 701.65a says "its OWNER", where every other permission in the tree names
  -- the granting resolution's controller. Its OWN case, so a mutation that hands
  -- the permission to the airbender reddens this assertion rather than the
  -- resolve above it.
  Spec.it s "CR 701.65a the airbending spell's controller is not the one offered the cast" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    giant <- S.printingOf s registry "Hill Giant"
    lesson <- S.printingOf s registry "Airbending Lesson"
    let (victim, _, _, spell, before) = board plains mountain giant lesson 2
        -- alice's own turn, alice's own main phase: the spell taps three of her
        -- six Plains, so three are left and the {2} would be payable if she had
        -- it. The case fails for the permission or for nothing.
        after = mainPhaseOf S.alice (airbent victim spell before)
    Spec.assertEqWith s "alice has mana left over" (untappedNamedFor S.alice plains after) 3
    case exiledCard after of
      Nothing -> Spec.assertBool s False "the airbend put exactly one card into bob's exile"
      Just exiled -> Spec.assertBool s (not (S.castable S.alice exiled after)) "CR 701.65a the airbender is not offered the cast"
  -- The other half of the amount: one Mountain is not two, so the alternative
  -- cost is a cost and not a waiver.
  Spec.it s "CR 701.65a the {2} is required, not waived" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    giant <- S.printingOf s registry "Hill Giant"
    lesson <- S.printingOf s registry "Airbending Lesson"
    let (victim, _, _, spell, before) = board plains mountain giant lesson 1
        after = mainPhaseOf S.bob (airbent victim spell before)
    case exiledCard after of
      Nothing -> Spec.assertBool s False "the airbend put exactly one card into bob's exile"
      Just exiled -> Spec.assertBool s (not (S.castable S.bob exiled after)) "CR 701.65a one Mountain does not pay {2}"

-- Covers: Pawl.Engine.PlayerDesignation (CR 702.131 ascend and CR 702.195
-- storied), the settle pass Pawl.Engine.Engine runs it from,
-- Pawl.Types.Player's designations field, Pawl.Types.PlayerDesignation and its
-- Pawl.Types.PlayerDesignationTally payload, and Pawl.Engine.Quantity's
-- HasPlayerDesignation arm.
--
-- Gameplay-level throughout: every case reads the mark through a CARD whose
-- printed "as long as you have" clause is gated on it -- Skymarcher Aspirant's
-- flying, Ori, Keeper of Songs' +1/+0 and vigilance, and Secrets of the Golden
-- City's third card -- and the field itself is read only after that, as the
-- assertion that says WHICH of the two marks moved.
--
-- Each negative board is its positive board with exactly one permanent swapped,
-- so what it proves is the threshold and not some other thing the board lacked.
module Pawl.PlayerDesignationSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerDesignation as PlayerDesignation
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Regenerability as Regenerability

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "PlayerDesignation" $ do
  ascendSpec s registry
  storiedSpec s registry

ascendSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ascendSpec s registry = Spec.describe s "Ascend" $ do
  -- CR 702.131b, ascend's STATIC half: the keyword does nothing on its own, and
  -- the continuous check is what turns controlling ten permanents into having the
  -- city's blessing.
  --
  -- Ten exactly: the Aspirant and nine Swamps. The flying assertion comes first,
  -- so what a broken check reddens is the behaviour and not the field that
  -- proxies it.
  Spec.it s "CR 702.131b ten permanents grant the city's blessing and Skymarcher Aspirant flies" $ do
    aspirant <- S.printingOf s registry "Skymarcher Aspirant"
    swamp <- S.printingOf s registry "Swamp"
    let (aspirantId, board) = S.addPermanent aspirant S.alice (S.landsInPlay swamp 9)
    Spec.assertBool s (not (flies aspirantId board)) "before the check the Aspirant does not fly"
    let after = settle board
    Spec.assertBool s (flies aspirantId after) "CR 702.131b the Aspirant flies"
    Spec.assertEqWith s "alice has the city's blessing" (marksOf S.alice after) (Just (Set.singleton PlayerDesignation.CitysBlessing))
    -- Bob controls no permanent with ascend, so the rule passes him by. The
    -- falsifier for an implementation that blessed the table.
    Spec.assertEqWith s "bob, who controls none, has nothing" (marksOf S.bob after) (Just Set.empty)
  -- The positive board with one Swamp taken away: nine permanents, so CR
  -- 702.131b's "ten or more" is not met and nothing is granted.
  Spec.it s "CR 702.131b nine permanents grant nothing" $ do
    aspirant <- S.printingOf s registry "Skymarcher Aspirant"
    swamp <- S.printingOf s registry "Swamp"
    let (aspirantId, board) = S.addPermanent aspirant S.alice (S.landsInPlay swamp 8)
        after = settle board
    Spec.assertBool s (not (flies aspirantId after)) "the Aspirant does not fly"
    Spec.assertEqWith s "and alice has nothing" (marksOf S.alice after) (Just Set.empty)
  -- CR 702.131a/b's "for the rest of the game": the mark does not track the count
  -- that granted it. Alice reaches ten, then a Swamp is destroyed and the board
  -- settles again at nine -- the case above is what says nine alone would never
  -- have granted it.
  Spec.it s "CR 702.131b the blessing survives falling back below ten" $ do
    aspirant <- S.printingOf s registry "Skymarcher Aspirant"
    swamp <- S.printingOf s registry "Swamp"
    let (aspirantId, board) = S.addPermanent aspirant S.alice (S.landsInPlay swamp 9)
        blessed = settle board
        victims = take 1 (filter (/= aspirantId) (Set.toAscList (GameState.battlefield blessed)))
        shrunk = settle (S.runPure S.identityAnswer blessed (Event.destroy Regenerability.Regenerable victims))
    Spec.assertEqWith s "alice now controls nine permanents" (Set.size (GameState.battlefield shrunk)) 9
    Spec.assertBool s (flies aspirantId shrunk) "the Aspirant still flies"
    Spec.assertEqWith s "alice still has the city's blessing" (marksOf S.alice shrunk) (Just (Set.singleton PlayerDesignation.CitysBlessing))

  -- CR 702.131a, ascend's SPELL half, which rule 702.131b's cases above do not
  -- reach: nothing on this board carries ascend as a permanent, so the continuous
  -- check grants nothing and only the resolving sorcery can.
  --
  -- The THIRD CARD is the gameplay-level assertion, and it is what proves the
  -- ORDER as well as the grant: CR 608.2c follows a spell's instructions in
  -- printed order, ascend being printed above the rest, so "if you have the
  -- city's blessing, draw three cards instead" reads a mark this same resolution
  -- granted. A grant made after the clauses, or not at all, draws two.
  Spec.it s "CR 702.131a Secrets of the Golden City blesses its controller and then draws three" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    secrets <- S.printingOf s registry "Secrets of the Golden City"
    let (gs, spellId) = S.handOne secrets (stocked piker (S.landsInPlay island 10))
        after = resolveOne gs spellId
    Spec.assertEqWith s "CR 702.131a alice drew three cards, not two" (S.handSize S.alice after) 3
    Spec.assertEqWith s "and holds the city's blessing once the sorcery is gone" (marksOf S.alice (settle after)) (Just (Set.singleton PlayerDesignation.CitysBlessing))
    Spec.assertEqWith s "bob, who cast nothing, has nothing" (marksOf S.bob after) (Just Set.empty)
  -- The board above with one Island taken away: nine permanents, so CR 702.131a's
  -- "ten or more" is not met, the mark is not granted, and the card's own
  -- unblessed clause draws two. Three Islands still pay {1}{U}{U}, so the
  -- negative cannot pass for want of mana.
  Spec.it s "CR 702.131a nine permanents leave Secrets of the Golden City drawing two" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    secrets <- S.printingOf s registry "Secrets of the Golden City"
    let (gs, spellId) = S.handOne secrets (stocked piker (S.landsInPlay island 9))
        after = resolveOne gs spellId
    Spec.assertEqWith s "alice drew two cards" (S.handSize S.alice after) 2
    Spec.assertEqWith s "and has nothing" (marksOf S.alice (settle after)) (Just Set.empty)

storiedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
storiedSpec s registry = Spec.describe s "Storied" $ do
  -- CR 702.195a: "three or more permanents that are artifacts, Sagas, and/or
  -- legendary". One permanent per disjunct and no permanent answering two of them
  -- -- Ori is legendary and neither of the others, Ashnod's Altar is an artifact
  -- and neither of the others, History of Benalia is a Saga and neither of the
  -- others -- so a check that dropped any one arm of the disjunction falls short
  -- of three.
  Spec.it s "CR 702.195a an artifact, a Saga and a legend grant an enduring story" $ do
    ori <- S.printingOf s registry "Ori, Keeper of Songs"
    altar <- S.printingOf s registry "Ashnod's Altar"
    benalia <- S.printingOf s registry "History of Benalia"
    let (oriId, board) = S.addPermanent ori S.alice (Setup.emptyGame S.bothPlayers)
        (_, withAltar) = S.addPermanent altar S.alice board
        (_, withSaga) = S.addPermanent benalia S.alice withAltar
    Spec.assertEqWith s "before the check Ori is a plain 3/3" (Projection.powerOf oriId withSaga) (Just 3)
    let after = settle withSaga
    Spec.assertEqWith s "CR 702.195a Ori is 4/3" (Projection.powerOf oriId after) (Just 4)
    Spec.assertBool s (hasKeyword Keyword.Vigilance oriId after) "and has vigilance"
    Spec.assertEqWith s "alice has an enduring story" (marksOf S.alice after) (Just (Set.singleton PlayerDesignation.EnduringStory))
  -- The positive board with the Saga swapped for a Swamp, which is none of the
  -- three: two qualifying permanents, so CR 702.195a's "three or more" is not met.
  Spec.it s "CR 702.195a two qualifying permanents grant nothing" $ do
    ori <- S.printingOf s registry "Ori, Keeper of Songs"
    altar <- S.printingOf s registry "Ashnod's Altar"
    swamp <- S.printingOf s registry "Swamp"
    let (oriId, board) = S.addPermanent ori S.alice (Setup.emptyGame S.bothPlayers)
        (_, withAltar) = S.addPermanent altar S.alice board
        (_, withSwamp) = S.addPermanent swamp S.alice withAltar
        after = settle withSwamp
    Spec.assertEqWith s "Ori is still a 3/3" (Projection.powerOf oriId after) (Just 3)
    Spec.assertBool s (not (hasKeyword Keyword.Vigilance oriId after)) "and has no vigilance"
    Spec.assertEqWith s "and alice has nothing" (marksOf S.alice after) (Just Set.empty)

-- CR 117.5's settle, which is where CR 702.131b's and CR 702.195a's checks run --
-- neither is a state-based action, so Pawl.Support's settleSba would not reach
-- them.
settle :: GameState.GameState -> GameState.GameState
settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority

marksOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe (Set.Set PlayerDesignation.PlayerDesignation)
marksOf pid gs = fmap Player.designations (Map.lookup pid (GameState.players gs))

flies :: ObjectId.ObjectId -> GameState.GameState -> Bool
flies = hasKeyword Keyword.Flying

hasKeyword :: Keyword.Keyword -> ObjectId.ObjectId -> GameState.GameState -> Bool
hasKeyword keyword oid gs = Map.member keyword (Projection.keywordsOf oid gs)

-- Four cards in alice's library, which is one more than the widest draw here --
-- CR 104.3c would otherwise decide the game before an assertion runs.
stocked :: Printing.Printing -> GameState.GameState -> GameState.GameState
stocked printing base = List.foldl' (\gs _ -> snd (S.addLibraryCard printing S.alice gs)) base [1 :: Int .. 4]

-- Cast the one card in alice's hand and resolve it. No mana is asked about
-- beyond what the Islands pay, and CR 608.2b has nothing to re-validate: the
-- sorcery targets nothing.
resolveOne :: GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveOne gs spellId =
  let cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
   in snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)

{-# LANGUAGE GADTs #-}

-- Covers: CR 701.48 LEARN -- Pawl.Engine.Learn and Effect.Learn's arm in
-- Pawl.Engine.Resolve.Effect.
--
-- Cram Session ({1}{B/G} Sorcery, "You gain 4 life. Learn.") is the fixture: the
-- keyword action is its whole second clause and states nothing the rulebook does
-- not, so every assertion below is about rule 701.48a. The gain of 4 is what
-- says the spell resolved at all, so a branch that did nothing is told apart
-- from a spell that never ran.
--
-- THE BOARD SHAPE that makes the cases discriminating. One board, three answers:
-- alice holds a Hill Giant to discard, has a Plains to draw, and owns TWO cards
-- outside the game -- Airbending Lesson (an Instant -- Lesson) and Sign in Blood
-- (a sorcery, no Lesson). So the third sentence's "Lesson card" is a real gate,
-- and the three cases differ in the ANSWER alone.
module Pawl.LearnSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LearnMode as LearnMode
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

-- alice: two Swamps, Cram Session and a Hill Giant in hand, a Plains in her
-- library, and Airbending Lesson plus Sign in Blood outside the game.
--
-- The pool is written onto the player directly, Pawl.OutsideTheGameSpec's
-- posture: CR 103.2a's road from a Deck is that spec's subject, and this one is
-- about what rule 701.48a does with a pool that exists.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
board swamp cram giant plains lesson sorcery =
  let (_, g1) = S.addHandCard giant S.alice (S.landsInPlay swamp 2)
      (spell, g2) = S.addHandCard cram S.alice g1
      (_, g3) = S.addLibraryCard plains S.alice g2
      (lessonId, g4) = Game.intern lesson g3
      (sorceryId, g5) = Game.intern sorcery g4
      stock p = p {Player.outsideTheGame = Map.fromList [(lessonId, 1), (sorceryId, 1)]}
   in (spell, g5 {GameState.players = Map.adjust stock S.alice (GameState.players g5)})

-- The rule 701.48a branch this run takes, pinned by index rather than searched:
-- an answerer that took whatever was legal would repair itself after a mutation.
learning :: Maybe LearnMode.LearnMode -> Prompt.Prompt r -> r
learning mode p = case p of
  Prompt.ChooseLearn {} -> mode
  _ -> S.castAnswer p

-- alice casts Cram Session and it resolves, under that branch.
learned :: Maybe LearnMode.LearnMode -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
learned mode spell gs =
  let cast = S.runPure (learning mode) gs (S.cast S.alice spell)
   in S.runPure (learning mode) cast Stack.resolveTop

-- The printings of the cards in a zone, so a case can say WHICH card is there.
printingsIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Printing.Printing]
printingsIn zone pid gs = Maybe.mapMaybe (\oid -> Game.printingOfObject oid gs) (Game.zoneMembers zone pid gs)

-- What is left in a player's pool, by printing.
poolOf :: PlayerId.PlayerId -> GameState.GameState -> Map.Map PrintingId.PrintingId Natural.Natural
poolOf pid gs = maybe Map.empty Player.outsideTheGame (Map.lookup pid (GameState.players gs))

lifeOf :: PlayerId.PlayerId -> GameState.GameState -> Integer
lifeOf pid gs = maybe 0 Player.life (Map.lookup pid (GameState.players gs))

-- What rule 701.48a put into her graveyard, which is her graveyard WITHOUT Cram
-- Session: CR 608.2m puts the resolved sorcery there itself, and that is a fact
-- about the fixture rather than about learning.
discardedBy :: Printing.Printing -> GameState.GameState -> [Printing.Printing]
discardedBy cram gs = List.sort (filter (/= cram) (printingsIn Zone.Graveyard S.alice gs))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Learn" $ do
  Spec.it s "CR 701.48a the first branch discards a card and then draws one" $ do
    swamp <- S.printingOf s registry "Swamp"
    cram <- S.printingOf s registry "Cram Session"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    lesson <- S.printingOf s registry "Airbending Lesson"
    sorcery <- S.printingOf s registry "Sign in Blood"
    let (spell, before) = board swamp cram giant plains lesson sorcery
        after = learned (Just LearnMode.DiscardAndDraw) spell before
    -- THE gameplay reading, and first: the Hill Giant is in the graveyard and
    -- the library's Plains is in her hand, which is rule 701.48a's first two
    -- sentences and nothing else.
    Spec.assertEqWith s "CR 701.48a the discarded card is in her graveyard" (discardedBy cram after) [giant]
    Spec.assertEqWith s "CR 701.48a and the drawn card is in her hand" (printingsIn Zone.Hand S.alice after) [plains]
    -- The spell did resolve, so an empty branch and an unrun spell differ.
    Spec.assertEqWith s "and the spell's own clause gained her 4" (lifeOf S.alice after) (lifeOf S.alice before + 4)
    -- CR 701.48a's third sentence is conditioned on NOT discarding, so the pool
    -- is untouched.
    Spec.assertEqWith s "CR 701.48a nothing came in from outside the game" (poolOf S.alice after) (poolOf S.alice before)
  Spec.it s "CR 701.48a the second branch takes the Lesson from outside the game, and leaves the non-Lesson there" $ do
    swamp <- S.printingOf s registry "Swamp"
    cram <- S.printingOf s registry "Cram Session"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    lesson <- S.printingOf s registry "Airbending Lesson"
    sorcery <- S.printingOf s registry "Sign in Blood"
    let (spell, before) = board swamp cram giant plains lesson sorcery
        after = learned (Just LearnMode.TakeLesson) spell before
    -- THE gameplay reading, and first: the Lesson joined the Hill Giant she
    -- already held, and the non-Lesson in the same pool did not.
    Spec.assertEqWith s "CR 701.48a the Lesson is in her hand" (List.sort (printingsIn Zone.Hand S.alice after)) (List.sort [giant, lesson])
    Spec.assertEqWith s "CR 400.11b and the pool is one card lighter" (Map.size (poolOf S.alice after)) 1
    Spec.assertBool s (notElem sorcery (printingsIn Zone.Hand S.alice after)) "CR 701.48a the non-Lesson beside it stayed outside the game"
    -- Nothing was discarded: the two branches are exclusive.
    Spec.assertEqWith s "CR 701.48a and her graveyard is empty" (discardedBy cram after) []
    Spec.assertEqWith s "and the spell's own clause gained her 4" (lifeOf S.alice after) (lifeOf S.alice before + 4)
  -- Rule 701.48a is two "you may"s, so declining both is an outcome. The SAME
  -- board, differing in the answer alone.
  Spec.it s "CR 701.48a a learner may decline both branches" $ do
    swamp <- S.printingOf s registry "Swamp"
    cram <- S.printingOf s registry "Cram Session"
    giant <- S.printingOf s registry "Hill Giant"
    plains <- S.printingOf s registry "Plains"
    lesson <- S.printingOf s registry "Airbending Lesson"
    sorcery <- S.printingOf s registry "Sign in Blood"
    let (spell, before) = board swamp cram giant plains lesson sorcery
        after = learned Nothing spell before
    Spec.assertEqWith s "CR 701.48a her hand is as it was" (printingsIn Zone.Hand S.alice after) [giant]
    Spec.assertEqWith s "CR 701.48a nothing was discarded" (discardedBy cram after) []
    Spec.assertEqWith s "CR 701.48a and the pool is untouched" (poolOf S.alice after) (poolOf S.alice before)
    Spec.assertEqWith s "and the spell still resolved" (lifeOf S.alice after) (lifeOf S.alice before + 4)

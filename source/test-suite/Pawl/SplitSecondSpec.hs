{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 702.61 split second -- Pawl.Types.Keyword's SplitSecond arm, the
-- reader Pawl.Engine.SplitSecond.inForce, and the three gates that ask it
-- (Pawl.Engine.Cast.castable, Pawl.Engine.Cast.castableWhenOffered and
-- Pawl.Engine.Activate.activatableGiven).
--
-- Gameplay-level throughout, off Pawl.Engine.Action.legalActions: what the rule
-- takes away is the MENU, so that is what every case reads.
--
-- Sudden Shock ({1}{R} Instant, "Split second. Sudden Shock deals 2 damage to any
-- target.") is the fixture, and Lightning Bolt is its CONTROL. Every negative
-- below is a pair of boards built by the same function from the same hands, the
-- same eight Mountains a seat and the same one spell on the stack -- so the only
-- thing between the two answers is the keyword on the spell that is already
-- there, and no case can pass for want of mana, for timing, or because the
-- action was never offered at all.
--
-- Two seats are enough and three would prove nothing more: CR 702.61a names no
-- player, so the cases that matter are the split-second spell's own controller
-- and somebody else, which is exactly alice and bob.
--
-- Monastery Swiftspear brings CR 702.61b's second sentence in -- prowess is a
-- trigger that watches a noncreature cast, so casting Sudden Shock arms it and
-- the restriction has to let it onto the stack. Circling Vultures brings the
-- special-action half, and bob's Mountains the mana-ability half. Prodigal
-- Sorcerer is the non-mana activated ability CR 702.61a does stop.
--
-- Molten Disaster ({X}{R}{R} Sorcery, "Kicker {R}. If this spell was kicked, it
-- has split second. Molten Disaster deals X damage to each creature without
-- flying and each player.") is the GRANTED half, and it is the only fixture
-- that can be: Sudden Shock prints the keyword, so no board built on it can
-- tell a printed reading of the stack from a projected one. Its pair of boards
-- differ in the CR 702.33a kicker answer and in nothing else.
module Pawl.SplitSecondSpec where

import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "SplitSecond" $ do
  castSpec s registry
  activateSpec s registry
  stillAllowedSpec s registry
  durationSpec s registry
  grantedSpec s registry

-- alice: eight Mountains, the `subject` spell and two Lightning Bolts in hand.
-- bob: eight Mountains, one Lightning Bolt in hand, a Prodigal Sorcerer out.
-- alice's own precombat main phase, empty stack.
--
-- The subject is Sudden Shock for the printed cases and Molten Disaster for the
-- granted ones; nothing else about the board changes between them.
--
-- Eight Mountains a seat is four times any cost here, so no case below can turn
-- on affordability -- and both boards the cases compare are built from THIS one,
-- so they cannot differ in it either.
data Board = MkBoard
  { subject :: ObjectId.ObjectId,
    aliceBolt :: ObjectId.ObjectId,
    aliceSpare :: ObjectId.ObjectId,
    bobBolt :: ObjectId.ObjectId,
    state :: GameState.GameState
  }

board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Board
board mountain suddenShock lightningBolt prodigal =
  let addMany printing n gs = foldr (\_ g -> snd (S.addCreature printing S.bob g)) gs [1 .. n :: Int]
      base = addMany mountain (8 :: Int) (S.landsInPlay mountain 8)
      (a, gs1) = S.addHandCard suddenShock S.alice base
      (b, gs2) = S.addHandCard lightningBolt S.alice gs1
      (c, gs3) = S.addHandCard lightningBolt S.alice gs2
      (d, gs4) = S.addHandCard lightningBolt S.bob gs3
      (_, gs5) = S.addCreature prodigal S.bob gs4
   in MkBoard
        { subject = a,
          aliceBolt = b,
          aliceSpare = c,
          bobBolt = d,
          state =
            gs5
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
        }

-- alice casts one of her cards and the triggers settle onto the stack. The one
-- difference between every pair of boards below is WHICH card this is given.
after :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
after oid gs = S.runPure S.identityAnswer gs (S.cast S.alice oid >> Engine.settleForPriority)

isCast :: A.Action -> Bool
isCast action = case action of
  A.Cast {} -> True
  A.Pass -> False
  A.Play {} -> False
  A.Activate _ _ -> False
  A.TurnFaceUp _ -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot _ -> False
  A.Ignore _ -> False
  A.ActivateManaAbility _ -> False

isActivate :: A.Action -> Bool
isActivate action = case action of
  A.Activate _ _ -> True
  A.Cast {} -> False
  A.Pass -> False
  A.Play {} -> False
  A.TurnFaceUp _ -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot _ -> False
  A.Ignore _ -> False
  A.ActivateManaAbility _ -> False

isManaAbility :: A.Action -> Bool
isManaAbility action = case action of
  A.ActivateManaAbility _ -> True
  A.Activate _ _ -> False
  A.Cast {} -> False
  A.Pass -> False
  A.Play {} -> False
  A.TurnFaceUp _ -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot _ -> False
  A.Ignore _ -> False

castOf :: ObjectId.ObjectId -> Printing.Printing -> A.Action
castOf oid printing = A.Cast oid (S.printingName printing) Facing.FaceUp

castSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castSpec s registry =
  Spec.describe s "casting" $ do
    -- The pair that carries the whole rule. Both boards hold one instant on the
    -- stack, cast the same way out of the same hand with the same mana left; the
    -- Bolt board still offers bob his own Bolt and the Sudden Shock board offers
    -- him nothing.
    Spec.it s "CR 702.61a a spell with split second stops an opponent's cast" $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain suddenShock lightningBolt prodigal
          bolted = after (aliceBolt b) (state b)
          shocked = after (subject b) (state b)
      Spec.assertBool s (elem (castOf (bobBolt b) lightningBolt) (Action.legalActions S.bob bolted)) "control: an ordinary instant on the stack leaves the cast offered"
      Spec.assertBool s (notElem (castOf (bobBolt b) lightningBolt) (Action.legalActions S.bob shocked)) "split second takes it away"
      Spec.assertEqWith s "and takes away every other cast with it" (filter isCast (Action.legalActions S.bob shocked)) []

    -- CR 702.61a says "players", with no possessive: the controller of the
    -- split-second spell is as stopped as anybody. Same pair of boards, asked of
    -- alice about the Bolt she still holds.
    Spec.it s "CR 702.61a it stops its own controller too" $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain suddenShock lightningBolt prodigal
          bolted = after (aliceBolt b) (state b)
          shocked = after (subject b) (state b)
      Spec.assertBool s (elem (castOf (aliceSpare b) lightningBolt) (Action.legalActions S.alice bolted)) "control: her spare Bolt is offered"
      Spec.assertEqWith s "and split second takes every cast of hers away" (filter isCast (Action.legalActions S.alice shocked)) []

    -- CR 608.2g's offered cast is a cast too, which is the other gate
    -- (Cast.castableWhenOffered). Seven Forests pay Panglacial Wurm's {5}{G}{G},
    -- and the spell is PLACED on the stack rather than cast, so the two boards
    -- differ in the placed card and in nothing else at all.
    Spec.it s "CR 702.61a it also stops a Panglacial Wurm cast from the library" $ do
      forest <- S.printingOf s registry "Forest"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      panglacialWurm <- S.printingOf s registry "Panglacial Wurm"
      let (_, base) = S.addLibraryCard panglacialWurm S.alice (S.landsInPlay forest 7)
          (_, bolted) = S.spellOnStack lightningBolt S.alice base
          (_, shocked) = S.spellOnStack suddenShock S.alice base
      Spec.assertEqWith s "control: with an ordinary spell on the stack the Wurm is offered" (length (Cast.castableWhileSearching S.alice bolted)) 1
      Spec.assertEqWith s "split second takes the offer away" (Cast.castableWhileSearching S.alice shocked) []

activateSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
activateSpec s registry =
  Spec.describe s "activating" $ do
    -- CR 702.61a's second limb. Prodigal Sorcerer's "{T}: deal 1 damage to any
    -- target" is not a mana ability (CR 605.1a), so it goes.
    Spec.it s "CR 702.61a it stops an ability that isn't a mana ability" $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain suddenShock lightningBolt prodigal
          bolted = after (aliceBolt b) (state b)
          shocked = after (subject b) (state b)
          activationsOf gs = filter isActivate (Action.legalActions S.bob gs)
      Spec.assertEqWith s "control: the Sorcerer's ability is offered" (length (activationsOf bolted)) 1
      Spec.assertEqWith s "split second takes it away" (activationsOf shocked) []

stillAllowedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stillAllowedSpec s registry =
  Spec.describe s "what CR 702.61b leaves alone" $ do
    -- "Players may activate mana abilities": the count is unchanged, not merely
    -- nonzero, so a gate that reached CR 605.3a's window would be caught.
    Spec.it s "CR 702.61b mana abilities are still offered" $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain suddenShock lightningBolt prodigal
          bolted = after (aliceBolt b) (state b)
          shocked = after (subject b) (state b)
          manaOf gs = length (filter isManaAbility (Action.legalActions S.bob gs))
      Spec.assertEqWith s "control: one per Mountain" (manaOf bolted) 8
      Spec.assertEqWith s "split second leaves all eight" (manaOf shocked) 8

    -- "and take special actions". Circling Vultures' "You may discard this card
    -- any time you could cast an instant" is CR 116.2e's, which never uses the
    -- stack.
    Spec.it s "CR 702.61b special actions are still offered" $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      vultures <- S.printingOf s registry "Circling Vultures"
      let b = board mountain suddenShock lightningBolt prodigal
          (v, withVultures) = S.addHandCard vultures S.bob (state b)
          shocked = after (subject b) withVultures
      Spec.assertBool s (elem (A.DiscardFromHand v) (Action.legalActions S.bob shocked)) "the discard is still on the menu"

    -- "Triggered abilities trigger and are put on the stack as normal."
    -- Monastery Swiftspear's prowess watches a noncreature spell being cast, so
    -- Sudden Shock arms it on the way down and the ability has to land on top.
    Spec.it s "CR 702.61b a triggered ability still reaches the stack" $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      swiftspear <- S.printingOf s registry "Monastery Swiftspear"
      let b = board mountain suddenShock lightningBolt prodigal
          (_, withSwiftspear) = S.addCreature swiftspear S.alice (state b)
          bare = after (subject b) (state b)
          armed = after (subject b) withSwiftspear
      Spec.assertEqWith s "without the Swiftspear the spell is alone on the stack" (length (GameState.stack bare)) 1
      Spec.assertEqWith s "with it, prowess is on the stack above the spell" (length (GameState.stack armed)) 2
      Spec.assertBool s (fmap (\oid -> Game.isAbility oid armed) (take 1 (GameState.stack armed)) == [True]) "and the ability is the top object"

durationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
durationSpec s registry =
  -- CR 702.61a's "as long as this spell is on the stack". Nothing is stored, so
  -- resolving the spell lifts the restriction with nothing to unwind.
  Spec.describe s "duration"
    . Spec.it s "CR 702.61a the restriction ends when the spell leaves the stack"
    $ do
      mountain <- S.printingOf s registry "Mountain"
      suddenShock <- S.printingOf s registry "Sudden Shock"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain suddenShock lightningBolt prodigal
          shocked = after (subject b) (state b)
          resolved = S.runPure S.identityAnswer shocked Stack.resolveTop
      Spec.assertEqWith s "the stack is empty again" (GameState.stack resolved) []
      Spec.assertEqWith s "with the Sudden Shock card in a graveyard -- CR 400.7 gives it a new id, so it is named rather than compared" (fmap (\oid -> S.soleFaceName oid resolved) (Game.zoneMembers Zone.Graveyard S.alice resolved)) [S.printingName suddenShock]
      Spec.assertBool s (elem (castOf (bobBolt b) lightningBolt) (Action.legalActions S.bob resolved)) "and bob may cast again"

-- Answers CR 702.33a's kicker question with `decision` and CR 601.2b's X with 1,
-- deferring everywhere else. Pinned answers rather than searched ones, so a
-- mutation cannot be repaired by an interpreter that finds another legal reply.
disaster :: KickerDecision.KickerDecision -> Prompt.Prompt r -> r
disaster decision p = case p of
  Prompt.ChooseKicker {} -> decision
  Prompt.ChooseX {} -> 1
  _ -> S.identityAnswer p

grantedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
grantedSpec s registry =
  -- CR 702.61a read off the CR 613 projection rather than the printed face.
  -- Molten Disaster prints no keyword; a static ability of its own grants it one
  -- while it is on the stack (CR 113.6), gated on CR 702.33d's kicked
  -- designation.
  Spec.describe s "granted by a continuous effect" $ do
    -- THE PROVING CASE. One board, one hand, one spell on the stack, the same
    -- X=1; the kicker answer is the only difference, and it is what the whole
    -- pair turns on. A printed read of the stack cannot tell the two apart, so
    -- this case fails on origin/main.
    Spec.it s "CR 702.61a a KICKED Molten Disaster stops an opponent's cast, and an unkicked one does not" $ do
      mountain <- S.printingOf s registry "Mountain"
      moltenDisaster <- S.printingOf s registry "Molten Disaster"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain moltenDisaster lightningBolt prodigal
          declined = castDisaster KickerDecision.Declines b
          kicked = castDisaster KickerDecision.Kicks b
      Spec.assertEqWith s "control: the unkicked spell is on the stack" (length (GameState.stack declined)) 1
      Spec.assertEqWith s "and so is the kicked one, so neither board is empty-handed" (length (GameState.stack kicked)) 1
      Spec.assertBool s (elem (castOf (bobBolt b) lightningBolt) (Action.legalActions S.bob declined)) "control: unkicked, bob's Bolt is offered"
      Spec.assertEqWith s "kicked, split second takes every cast of his away" (filter isCast (Action.legalActions S.bob kicked)) []
    -- CR 702.61a's second limb, off the same pair. Prodigal Sorcerer's ability is
    -- not a mana ability (CR 605.1a).
    Spec.it s "CR 702.61a a kicked Molten Disaster stops an ability that isn't a mana ability" $ do
      mountain <- S.printingOf s registry "Mountain"
      moltenDisaster <- S.printingOf s registry "Molten Disaster"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      prodigal <- S.printingOf s registry "Prodigal Sorcerer"
      let b = board mountain moltenDisaster lightningBolt prodigal
          declined = castDisaster KickerDecision.Declines b
          kicked = castDisaster KickerDecision.Kicks b
          activationsOf gs = filter isActivate (Action.legalActions S.bob gs)
      Spec.assertEqWith s "control: unkicked, the Sorcerer's ability is offered" (length (activationsOf declined)) 1
      Spec.assertEqWith s "kicked, it is gone" (activationsOf kicked) []
      -- CR 702.61b, asked at gameplay level off the MENU rather than through
      -- Activate.activatable, which answers False for a mana ability on every
      -- board (CR 605.3b).
      Spec.assertEqWith s "and CR 702.61b leaves bob all eight Mountains" (length (filter isManaAbility (Action.legalActions S.bob kicked))) 8
    -- The card as printed, so the grant above is not the only thing the file
    -- says. X=1 into Goblin Piker (2/1, no flying) and Bird Maiden (1/2, which
    -- prints flying), plus both players -- the caster included, which is what
    -- separates "each player" from "each opponent".
    Spec.it s "Molten Disaster deals X to each creature without flying and each player" $ do
      mountain <- S.printingOf s registry "Mountain"
      moltenDisaster <- S.printingOf s registry "Molten Disaster"
      piker <- S.printingOf s registry "Goblin Piker"
      birdMaiden <- S.printingOf s registry "Bird Maiden"
      let (pikerId, g1) = S.addCreature piker S.bob (S.landsInPlay mountain 8)
          (maidenId, g2) = S.addCreature birdMaiden S.bob g1
          (gs, spellId) = S.handOne moltenDisaster g2
          answer = disaster KickerDecision.Declines
          cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
          resolved = snd (Engine.runGamePure answer cast Stack.resolveTop)
      Spec.assertEqWith s "1 marked on the Goblin Piker, which has no flying" (S.damageOf pikerId resolved) (Just 1)
      Spec.assertEqWith s "and nothing on the Bird Maiden, which does" (S.damageOf maidenId resolved) (Just 0)
      Spec.assertEqWith s "each player means the caster too" (S.lifeOf S.alice resolved) (Just 19)
      Spec.assertEqWith s "and the opponent" (S.lifeOf S.bob resolved) (Just 19)

-- alice casts the `subject` spell with the given kicker answer, and the triggers
-- settle. `after`'s body but for the answerer.
castDisaster :: KickerDecision.KickerDecision -> Board -> GameState.GameState
castDisaster decision b = S.runPure (disaster decision) (state b) (S.cast S.alice (subject b) >> Engine.settleForPriority)

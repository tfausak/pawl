{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Resolve over the effects that move an object between zones or
-- change a life total: library position, drawing, losing life, the life-total
-- exchanges and the zone exchanges. The machinery is Pawl.ResolveSpec.
module Pawl.ZoneChangeSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve.Effect as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GraveyardArrangement as GraveyardArrangement
import qualified Pawl.Types.GraveyardOrder as GraveyardOrder
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- The names of the cards in one player's copy of a zone, in that zone's order.
-- Named rather than compared by id because CR 400.7 mints a new object on every
-- move, so an id taken before a zone change never matches the one after it.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [Maybe CardName.CardName]
namesIn zone pid gs = fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Answers ChooseTargets by pointing every slot at bob (the opponent), otherwise
-- behaves like identityAnswer. Used to aim a player-targeting spell at bob.
atBobAnswer :: Prompt.Prompt r -> r
atBobAnswer p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.bob))) sets
  _ -> S.identityAnswer p

-- atBobAnswer with Library of Leng's CR 614.1a "may" answered as given.
lengAnswer :: OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
lengAnswer decision p = case p of
  Prompt.ChooseRedirect {} -> decision
  _ -> atBobAnswer p

-- The Wheel of Sun and Moon board with Library of Leng under BOB in the Wheel's
-- place: alice casts Psychic Miasma at bob, who holds one swamp over a stocked
-- library, and bob answers Leng's "may" as given. Returns the resolved state and
-- the three printings the assertions name.
lengBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> OptionalDecision.OptionalDecision -> m (GameState.GameState, Printing.Printing, Printing.Printing, Printing.Printing)
lengBoard s registry decision = do
  swamp <- S.printingOf s registry "Swamp"
  piker <- S.printingOf s registry "Goblin Piker"
  miasma <- S.printingOf s registry "Psychic Miasma"
  leng <- S.printingOf s registry "Library of Leng"
  let base = S.landsInPlay swamp 3
      (_, withLeng) = S.addPermanent leng S.bob base
      (_, stocked) = S.addLibraryCard piker S.bob withLeng
      withHand = handCards swamp S.bob 1 stocked
      (gs, spellId) = S.handOne miasma withHand
      cast = snd (Engine.runGamePure (lengAnswer decision) gs (S.cast S.alice spellId))
      after = snd (Engine.runGamePure (lengAnswer decision) cast Stack.resolveTop)
  pure (after, swamp, piker, miasma)

-- CR 401.4's owner, answering Prompt.ArrangeLibraryArrivals: bob reverses the
-- order he is offered when `reversing`, and keeps it otherwise; any other seat
-- asked keeps it, so asking the wrong player leaves the move order.
arrangedBy :: Bool -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [Natural]
arrangedBy reversing pid oids
  | reversing && pid == S.bob = reverse (zipWith const [0 ..] oids)
  | otherwise = zipWith const [0 ..] oids

-- Library of Leng under bob, who holds a Forest and an Island over a library
-- of one Goblin Piker; alice casts Mind Rot at him and he puts both discards on
-- top. Two cards are his whole hand, so CR 609.3 takes both unasked, in hand
-- order: handCards adds at the front, so the Island goes first and the Forest
-- lands on it. Returns the resolved state.
lengMindRotBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
lengMindRotBoard s registry reversing = do
  swamp <- S.printingOf s registry "Swamp"
  forest <- S.printingOf s registry "Forest"
  island <- S.printingOf s registry "Island"
  piker <- S.printingOf s registry "Goblin Piker"
  mindRot <- S.printingOf s registry "Mind Rot"
  leng <- S.printingOf s registry "Library of Leng"
  let base = S.landsInPlay swamp 3
      (_, withLeng) = S.addPermanent leng S.bob base
      (_, stocked) = S.addLibraryCard piker S.bob withLeng
      withHand = handCards island S.bob 1 (handCards forest S.bob 1 stocked)
      (gs, spellId) = S.handOne mindRot withHand
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeLibraryArrivals _ pid _ oids -> arrangedBy reversing pid oids
        _ -> lengAnswer OptionalDecision.Exercises p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Wheel of Sun and Moon, alice's, enchanting bob, who controls a Goblin Piker
-- and an Ogre Sentry over a library of one Typhoid Rats; alice casts Day of
-- Judgment, and both creatures go to the bottom of bob's library. Returns the
-- resolved state.
wheelJudgmentBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
wheelJudgmentBoard s registry reversing = do
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  rats <- S.printingOf s registry "Typhoid Rats"
  wheel <- S.printingOf s registry "Wheel of Sun and Moon"
  judgment <- S.printingOf s registry "Day of Judgment"
  let base = S.landsInPlay plains 4
      (wheelId, withWheel) = S.addPermanent wheel S.alice base
      enchanting = S.attachTo wheelId (Recipient.ToPlayer S.bob) withWheel
      (_, withPiker) = S.addPermanent piker S.bob enchanting
      (_, withSentry) = S.addPermanent sentry S.bob withPiker
      (_, stocked) = S.addLibraryCard rats S.bob withSentry
      (gs, spellId) = S.handOne judgment stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeLibraryArrivals _ pid _ oids -> arrangedBy reversing pid oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Wheel of Sun and Moon, alice's, enchanting bob, over a library of Plains,
-- Island, Swamp, Mountain and Forest from the top, then a Goblin Piker; alice
-- casts Tome Scour at bob, and the five milled cards go to the bottom of that
-- same library one by one. Returns the resolved state.
wheelScourBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
wheelScourBoard s registry reversing = do
  island <- S.printingOf s registry "Island"
  wheel <- S.printingOf s registry "Wheel of Sun and Moon"
  scour <- S.printingOf s registry "Tome Scour"
  pile <- traverse (S.printingOf s registry) ["Goblin Piker", "Forest", "Mountain", "Swamp", "Island", "Plains"]
  let base = S.landsInPlay island 1
      (wheelId, withWheel) = S.addPermanent wheel S.alice base
      enchanting = S.attachTo wheelId (Recipient.ToPlayer S.bob) withWheel
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) enchanting pile
      (gs, spellId) = S.handOne scour stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeLibraryArrivals _ pid _ oids -> arrangedBy reversing pid oids
        _ -> atBobAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Wheel of Sun and Moon enchanting alice, over her library of Plains, Island,
-- Goblin Piker and Swamp from the top; she casts Curate and surveils both top
-- cards into her graveyard, Plains first, so both go to the bottom; then she
-- draws the Piker, and Curate itself follows them under (CR 608.2n). She
-- reverses the arrangement she is offered when
-- `reversing`. Returns the resolved state.
wheelCurateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m GameState.GameState
wheelCurateBoard s registry reversing = do
  island <- S.printingOf s registry "Island"
  wheel <- S.printingOf s registry "Wheel of Sun and Moon"
  curate <- S.printingOf s registry "Curate"
  pile <- traverse (S.printingOf s registry) ["Swamp", "Goblin Piker", "Island", "Plains"]
  let base = S.landsInPlay island 2
      (wheelId, withWheel) = S.addPermanent wheel S.alice base
      enchanting = S.attachTo wheelId (Recipient.ToPlayer S.alice) withWheel
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) enchanting pile
      (gs, spellId) = S.handOne curate stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseSurveil _ _ looked -> (looked, [])
        Prompt.ArrangeLibraryArrivals _ pid _ oids
          | reversing && pid == S.alice -> reverse (zipWith const [0 ..] oids)
          | otherwise -> zipWith const [0 ..] oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- CR 404.3's owner, answering Prompt.ArrangeGraveyardArrivals: bob gives
-- `answer` the batch he is offered; any other seat asked takes any order, so
-- asking the wrong player leaves the move order.
graveyardBy :: ([ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement) -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement
graveyardBy answer pid oids
  | pid == S.bob = answer oids
  | otherwise = GraveyardArrangement.AnyOrder

-- Reverse the offered batch.
reversedBatch :: [ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement
reversedBatch oids = GraveyardArrangement.InOrder (reverse (zipWith const [0 ..] oids))

-- bob controls Volrath's Shapeshifter over a library of Ogre Sentry, Plains,
-- Island, Swamp and Mountain from the top, then a Goblin Piker, with his
-- graveyard order set to `order`, or left at the game's default; alice casts Tome Scour at him, so the Sentry
-- is milled first and the Mountain last, and bob gives `answer` any
-- arrangement he is offered. Returns the resolved state and the Shapeshifter.
shapeshifterScourBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Maybe GraveyardOrder.GraveyardOrder -> ([ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement) -> m (GameState.GameState, ObjectId.ObjectId)
shapeshifterScourBoard s registry order arrange = do
  island <- S.printingOf s registry "Island"
  shapeshifter <- S.printingOf s registry "Volrath's Shapeshifter"
  scour <- S.printingOf s registry "Tome Scour"
  pile <- traverse (S.printingOf s registry) ["Goblin Piker", "Mountain", "Swamp", "Island", "Plains", "Ogre Sentry"]
  let base = maybe id (Game.setGraveyardOrder S.bob) order (S.landsInPlay island 1)
      (shifterId, withShifter) = S.addPermanent shapeshifter S.bob base
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.bob g)) withShifter pile
      (gs, spellId) = S.handOne scour stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeGraveyardArrivals _ pid oids -> graveyardBy arrange pid oids
        _ -> atBobAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop), shifterId)

-- bob controls a Goblin Piker and an Ogre Sentry, and arranges what he is
-- offered with `answer`, his graveyard order Matters; alice casts Day of
-- Judgment. Returns the resolved state.
judgmentBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> ([ObjectId.ObjectId] -> GraveyardArrangement.GraveyardArrangement) -> m GameState.GameState
judgmentBoard s registry arrange = do
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  sentry <- S.printingOf s registry "Ogre Sentry"
  judgment <- S.printingOf s registry "Day of Judgment"
  let base = Game.setGraveyardOrder S.bob GraveyardOrder.Matters (S.landsInPlay plains 4)
      (_, withPiker) = S.addPermanent piker S.bob base
      (_, withSentry) = S.addPermanent sentry S.bob withPiker
      (gs, spellId) = S.handOne judgment withSentry
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ArrangeGraveyardArrivals _ pid oids -> graveyardBy arrange pid oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- alice, her graveyard order Matters, casts Curate over her library of Plains,
-- Island and Goblin Piker from the top and surveils both top cards into her
-- graveyard, Plains first. Any graveyard arrangement she is offered she
-- reverses. Returns the resolved state.
curateBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m GameState.GameState
curateBoard s registry = do
  island <- S.printingOf s registry "Island"
  curate <- S.printingOf s registry "Curate"
  pile <- traverse (S.printingOf s registry) ["Goblin Piker", "Island", "Plains"]
  let base = Game.setGraveyardOrder S.alice GraveyardOrder.Matters (S.landsInPlay island 2)
      stocked = List.foldl' (\g printing -> snd (S.addLibraryCard printing S.alice g)) base pile
      (gs, spellId) = S.handOne curate stocked
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseSurveil _ _ looked -> (looked, [])
        Prompt.ArrangeGraveyardArrivals _ _ oids -> reversedBatch oids
        _ -> S.identityAnswer p
      cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
  pure (snd (Engine.runGamePure answer cast Stack.resolveTop))

-- Three seats: alice controls Pulmonic Sliver; carol owns a Lymph Sliver bob
-- controls, over a library of one Goblin Piker each for bob and carol. The
-- Lymph Sliver is destroyed; bob answers the redirect as given and any other
-- seat asked answers the opposite. Returns the state and the two printings the
-- assertions name.
pulmonicBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> OptionalDecision.OptionalDecision -> m (GameState.GameState, Printing.Printing, Printing.Printing)
pulmonicBoard s registry decision = do
  pulmonic <- S.printingOf s registry "Pulmonic Sliver"
  lymph <- S.printingOf s registry "Lymph Sliver"
  piker <- S.printingOf s registry "Goblin Piker"
  let (_, withPulmonic) = S.addPermanent pulmonic S.alice S.threePlayerGame
      (lymphId, withLymph) = S.addPermanent lymph S.carol withPulmonic
      stolen = S.giveControl lymphId S.bob withLymph
      (_, g1) = S.addLibraryCard piker S.bob stolen
      (_, g2) = S.addLibraryCard piker S.carol g1
      opposite = if decision == OptionalDecision.Exercises then OptionalDecision.Declines else OptionalDecision.Exercises
      answer :: Prompt.Prompt r -> r
      answer p = case p of
        Prompt.ChooseRedirect _ pid _ _ -> if pid == S.bob then decision else opposite
        _ -> S.identityAnswer p
      after = S.runPure answer g2 (Event.destroy Regenerability.Regenerable [lymphId])
  pure (after, lymph, piker)

-- atBobAnswer with CR 701.9a's choice pinned by INDEX rather than left to the
-- identity answerer, so a discard of two takes two distinct cards.
discardAtBob :: Prompt.Prompt r -> r
discardAtBob p = case p of
  Prompt.ChooseDiscard _ _ ids n -> take (Natural.toIntSaturating n) ids
  _ -> atBobAnswer p

-- Add k cards of a printing to pid's hand (each a fresh Hand-zone object).
handCards :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
handCards printing pid k gs =
  let addOne g =
        let (printingId, gP) = Game.intern printing g
            (oid, g1) = Game.freshObjectId gP
            obj =
              (Game.cardObject oid pid printingId Zone.Hand (Timestamp.MkTimestamp 0))
                { Object.sickness = Sickness.Settled pid
                }
         in g1
              { GameState.objects = Map.insert oid obj (GameState.objects g1),
                GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand g1)
              }
   in List.foldl' (\g _ -> addOne g) gs [1 .. k]

-- Put k cards of a printing into pid's library, each on top of the last, for a
-- draw to find.
stockLibrary :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
stockLibrary printing pid k gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. k]

-- alice's upkeep begins, settled to the point where any trigger it woke is on
-- the stack (CR 603.3b) waiting to resolve.
settleAtAlicesUpkeep :: GameState.GameState -> GameState.GameState
settleAtAlicesUpkeep gs =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
   in snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)

-- Who drew, in the order they drew, read off the turn-scoped event log. CR
-- 121.1 makes a draw one library-to-hand move, and a library and a hand each
-- belong to one player, so the moved card's owner is the drawer. Any OTHER route
-- from library to hand would count here too; no fixture below has one.
drawersOf :: GameState.GameState -> [PlayerId.PlayerId]
drawersOf gs =
  let drawer zc =
        if ZoneChange.from zc == Zone.Library && ZoneChange.to zc == Zone.Hand
          then fmap Object.owner (Game.lookupObject (ZoneChange.object zc) gs)
          else Nothing
   in Maybe.mapMaybe drawer (S.zoneChangesOf gs)

-- Shahrazad and Sindbad on alice's battlefield, untapped and settled so its {T}
-- is payable (CR 302.6), over a library whose TOP card is `top` and a hand
-- holding `handLand` and `handSpell` -- two cards named neither `top` nor each
-- other, so any of the three is identifiable in the result by name alone.
--
-- `filler` sits under the drawn card twice over, which is what keeps CR 104.3c
-- out of the fixture: alice still has a library after the draw.
sindbadBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, GameState.GameState)
sindbadBoard sindbad top filler handLand handSpell =
  let (sindbadId, onBoard) = S.addPermanent sindbad S.alice (Setup.emptyGame S.bothPlayers)
      -- S.addLibraryCard puts each card ON TOP of the last, so `top` goes in
      -- after the filler.
      stocked = snd (S.addLibraryCard top S.alice (stockLibrary filler S.alice 2 onBoard))
   in (sindbadId, snd (S.addHandCard handSpell S.alice (snd (S.addHandCard handLand S.alice stocked))))

-- Activate the permanent's one activated ability for alice and resolve it. LOUD
-- rather than quiet when the card offers a different number of them: a helper
-- that silently did nothing is how this test would stay green while activating
-- no ability at all.
activateSole :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
activateSole oid gs = case Activatable.abilitiesFor oid gs of
  [ability] ->
    let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
     in S.runPure S.identityAnswer activated Stack.resolveTop
  other -> error ("Pawl.ZoneChangeSpec: expected exactly one activated ability, got " <> show (length other))

zoneChangeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
zoneChangeSpec s registry = Spec.describe s "ZoneChange" $ do
  -- CR 121.2c: "If more than one player is instructed to draw cards, the
  -- active player performs all of their draws first, then each other player
  -- in turn order does the same." The seat order the players map answers in
  -- is not that order, so this needs an active player who is not the first
  -- seat: alice casts an INSTANT on BOB's turn, which makes seat order
  -- [alice, bob, carol] and turn order [bob, carol, alice] disagree.
  --
  -- The draws are read back off the turn-scoped event log -- the same log a
  -- trigger scans (CR 603.2) -- because that is where the order of the
  -- individual draws is observable; the hand sizes alone are order-blind.
  Spec.it s "CR 121.2c Vision Skeins draws for the active player first, then in turn order" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let -- S.landsInPlay builds its own two-seat game, so the {1}{U} goes on
        -- a three-seat board one Island at a time.
        withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        -- handOne hands alice the turn along with the card, so bob takes the
        -- turn back. Cast.castSpell gates neither timing nor priority, but
        -- the fixture is a legal board regardless: Vision Skeins is an
        -- INSTANT, which alice may cast on bob's turn.
        gs = gs0 {GameState.activePlayer = S.bob}
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith
      s
      "bob (active) draws both of his, then carol, then the caster"
      (drawersOf after)
      [S.bob, S.bob, S.carol, S.carol, S.alice, S.alice]
    Spec.assertEqWith s "and everyone holds two" (fmap (\pid -> S.handSize pid after) [S.alice, S.bob, S.carol]) [2, 2, 2]
  -- The card that proves Effect.Draw's `Relative Opponent` arm (#276), and
  -- the one shape no "you draw" card can stand in for: Master of the Feast's
  -- trigger is a DRAWBACK, drawing for everyone except the player who
  -- controls it (CR 109.5 makes "your upkeep" that controller's).
  Spec.it s "CR 121.1 Master of the Feast's upkeep trigger draws for the opponent, not its controller" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addPermanent masterOfTheFeast S.alice (Setup.emptyGame S.bothPlayers)
        withLibs = stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board)
        onStack = settleAtAlicesUpkeep withLibs
        after = snd (Engine.runGamePure S.identityAnswer onStack Stack.resolveTop)
    Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
    Spec.assertEqWith s "bob drew" (S.handSize S.bob after) 1
    Spec.assertEqWith s "alice, who controls it, did not" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and alice's library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 1
  -- The discriminator, and it needs a THIRD seat: at two players an
  -- `Opponent` arm that reached only ONE opponent is indistinguishable from
  -- one that reaches them all. CR 806.1: in a Free-for-All the players
  -- compete as individuals, so every other player is an opponent -- CR 102.3's
  -- teammates are the one exception, and this board is played between no teams
  -- -- and both of them draw.
  Spec.it s "CR 806.1 at three seats each opponent draws off Master of the Feast, and only opponents" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    masterOfTheFeast <- S.printingOf s registry "Master of the Feast"
    let (_, board) = S.addPermanent masterOfTheFeast S.alice S.threePlayerGame
        withLibs = stockLibrary piker S.carol 1 (stockLibrary piker S.bob 1 (stockLibrary piker S.alice 1 board))
        after = snd (Engine.runGamePure S.identityAnswer (settleAtAlicesUpkeep withLibs) Stack.resolveTop)
    -- A drawer whose library was empty would draw no card and so record no
    -- zone change; this is what keeps the list below honest about that.
    Spec.assertEqWith s "no draw outran a library" (GameState.drewFromEmpty after) Set.empty
    Spec.assertEqWith s "both opponents drew, and the controller did not" (drawersOf after) [S.bob, S.carol]
  -- CR 102.1: "A player is one of the people in the game", so once CR 800.4a
  -- takes carol out, `Relative AnyPlayer` stops naming her (#279). It needs three
  -- seats twice over: CR 800.4 says only a multiplayer game -- CR 800.1's,
  -- one that BEGAN with more than two players -- continues after a
  -- departure, and a two-seat game would already have ended under CR 104.2a
  -- with nothing left to resolve.
  --
  -- drewFromEmpty is what makes this observable rather than merely tidy.
  -- CR 800.4a took carol's library out of the game with her, so a draw aimed
  -- at her finds it empty and Event.drawCard writes her seat into that set --
  -- engine state recorded for someone who is not in the game.
  Spec.it s "CR 800.4a Vision Skeins does not draw for a player who has left the game" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    visionSkeins <- S.printingOf s registry "Vision Skeins"
    let withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
        withLibs = stockLibrary piker S.carol 2 (stockLibrary piker S.bob 2 (stockLibrary piker S.alice 2 withMana))
        (gs0, spellId) = S.handOne visionSkeins withLibs
        gs = S.departs Departure.Type.Conceded S.carol gs0
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the two players still in the game drew, in APNAP order" (drawersOf after) [S.alice, S.alice, S.bob, S.bob]
    Spec.assertEqWith s "and nothing was drawn against carol's departed library" (GameState.drewFromEmpty after) Set.empty
  -- The card that proves Effect.Draw's SLOT (#1899): Shahrazad and Sindbad's
  -- "{T}: Draw a card and reveal it. If it isn't a land card, discard it."
  -- (Unknown Event; Oracle text and type line checked against
  -- api.scryfall.com). Every draw above answers "who" and "how many"; this one
  -- is the first that has to answer WHICH CARD, because the reveal and the
  -- discard both say "it" -- CR 121.1 puts the card in the hand, and the two
  -- later instructions name that card there.
  --
  -- The pair below differ in EXACTLY ONE THING, the top card of the library, so
  -- what the second board shows is the condition reading the bound card rather
  -- than a board assembled to pass. A binding that named the wrong card would
  -- reverse both: the nonland board would bury a card the draw never touched,
  -- and the land board -- whose hand holds a nonland the whole time -- would
  -- discard THAT instead of leaving the graveyard empty. A binding that named
  -- nothing empties the first board's graveyard.
  Spec.it s "CR 121.1 / 701.20a Shahrazad and Sindbad discards the nonland card it just drew, and only that card" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    piker <- S.printingOf s registry "Goblin Piker"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    let (sindbadId, board) = sindbadBoard sindbad piker plains island divination
        after = activateSole sindbadId board
    Spec.assertEqWith
      s
      "the Goblin Piker it drew is what went to the graveyard"
      (namesIn Zone.Graveyard S.alice after)
      [Just (S.printingName piker)]
    Spec.assertEqWith
      s
      "and the two cards the hand already held, one of them a nonland, are untouched"
      (List.sort (namesIn Zone.Hand S.alice after))
      (List.sort [Just (S.printingName island), Just (S.printingName divination)])
    Spec.assertEqWith
      s
      "CR 701.20a it was shown to the table as it was drawn"
      (S.revealsOf after)
      [(S.alice, Set.singleton (S.printingName piker))]
  Spec.it s "CR 121.1 the same ability keeps a LAND it drew, so the clause condition reads the bound card" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    mountain <- S.printingOf s registry "Mountain"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    divination <- S.printingOf s registry "Divination"
    let (sindbadId, board) = sindbadBoard sindbad mountain plains island divination
        after = activateSole sindbadId board
    Spec.assertEqWith
      s
      "the Mountain it drew joined the hand and stayed there"
      (List.sort (namesIn Zone.Hand S.alice after))
      (List.sort [Just (S.printingName mountain), Just (S.printingName island), Just (S.printingName divination)])
    Spec.assertEqWith s "and nothing was discarded" (namesIn Zone.Graveyard S.alice after) []
    Spec.assertEqWith
      s
      "CR 701.20a the reveal happens either way"
      (S.revealsOf after)
      [(S.alice, Set.singleton (S.printingName mountain))]
  -- CR 121.6c: the card Plagiarize has alice draw in place of bob's draw is
  -- drawn as a result of the replacement, so bob's "reveal it" does not reach it.
  -- The pair differ in EXACTLY ONE THING, whether alice cast Plagiarize at bob
  -- before he activated; bob's library tops with a Goblin Piker and alice's with
  -- Divination, so a reveal of either is identifiable by name.
  Spec.it s "CR 121.6c a draw Plagiarize redirects binds nothing for Shahrazad and Sindbad's reveal" $ do
    sindbad <- S.printingOf s registry "Shahrazad and Sindbad"
    plagiarize <- S.printingOf s registry "Plagiarize"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    let (sindbadId, withSindbad) = S.addPermanent sindbad S.bob (Setup.emptyGame S.bothPlayers)
        stocked = stockLibrary divination S.alice 2 (snd (S.addLibraryCard piker S.bob (stockLibrary plains S.bob 2 withSindbad)))
        withMana = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) stocked [1 .. (4 :: Int)]
        (board, plagiarizeId) = S.handOne plagiarize withMana
        activateForBob gs = case Activatable.abilitiesFor sindbadId gs of
          [ability] ->
            let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.bob sindbadId ability)
             in S.runPure S.identityAnswer activated Stack.resolveTop
          other -> error ("Pawl.ZoneChangeSpec: expected exactly one activated ability, got " <> show (length other))
        plagiarized =
          let cast = S.runPure atBobAnswer board (S.cast S.alice plagiarizeId)
           in S.runPure S.identityAnswer cast Stack.resolveTop
        after = activateForBob plagiarized
        control = activateForBob board
    Spec.assertEqWith s "nothing is revealed: alice's Divination was drawn as a result of the replacement" (S.revealsOf after) []
    Spec.assertEqWith
      s
      "and on the board without Plagiarize, bob reveals the Goblin Piker he drew"
      (S.revealsOf control)
      [(S.bob, Set.singleton (S.printingName piker))]
    Spec.assertEqWith s "alice drew the Divination in bob's place" (namesIn Zone.Hand S.alice after) [Just (S.printingName divination)]
    Spec.assertEqWith s "and bob drew nothing" (namesIn Zone.Hand S.bob after) []
  -- CR 701.9a's per-turn TALLY, the same log read as a number rather than as the
  -- set one resolution moved: Dream Salvage's "draw cards equal to the number of
  -- cards target opponent discarded this turn". The declared target is read ONLY
  -- through that number -- the draw itself is CR 109.5's caster -- which is the
  -- shape the D4 dataflow lint could not see until Resolve.Slots.quantitySlots
  -- folded QuantitySlot.nestedRefs; Pawl.AbilitySlotLintSpec's "the lint itself
  -- catches a slot read only from inside a number" is the rejecting half.
  --
  -- TWO boards differing in exactly one thing, the size of bob's hand, so the
  -- draw cannot be a literal that happens to agree: Mind Rot names two cards and
  -- CR 609.3 makes a one-card hand give what it has, so the tallies are two and
  -- one. Alice's library is stocked past the larger draw, and bob's hand is
  -- filled with a printing alice's library does not hold, so a drawn card cannot
  -- be mistaken for a discarded one.
  Spec.it s "CR 701.9a Dream Salvage draws as many cards as the target discarded this turn" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mindRot <- S.printingOf s registry "Mind Rot"
    salvage <- S.printingOf s registry "Dream Salvage"
    let drawnAfterDiscarding held =
          let base = stockLibrary piker S.alice 3 (S.landsInPlay swamp 4)
              withHand = handCards swamp S.bob held base
              (gs, rotId) = S.handOne mindRot withHand
              (staged, salvageId) = S.handOne salvage gs
              rotCast = snd (Engine.runGamePure discardAtBob staged (S.cast S.alice rotId))
              rotted = snd (Engine.runGamePure discardAtBob rotCast Stack.resolveTop)
              salvageCast = snd (Engine.runGamePure discardAtBob rotted (S.cast S.alice salvageId))
           in snd (Engine.runGamePure discardAtBob salvageCast Stack.resolveTop)
        two = drawnAfterDiscarding 3
        one = drawnAfterDiscarding 1
    Spec.assertEqWith s "two discarded, two drawn" (namesIn Zone.Hand S.alice two) [Just (S.printingName piker), Just (S.printingName piker)]
    Spec.assertEqWith s "and bob really discarded two" (namesIn Zone.Graveyard S.bob two) [Just (S.printingName swamp), Just (S.printingName swamp)]
    Spec.assertEqWith s "one discarded, one drawn" (namesIn Zone.Hand S.alice one) [Just (S.printingName piker)]
    Spec.assertEqWith s "and bob really discarded one" (namesIn Zone.Graveyard S.bob one) [Just (S.printingName swamp)]
  -- The same rider through a discard hidden WITHOUT a reveal: CR 701.9c
  -- undefines the card's characteristics, so CR 400.7j's find has nothing to
  -- ask and the rider must not fire. Library of Leng, {1} Artifact: "If an
  -- effect causes you to discard a card, discard it, but you may put it on top
  -- of your library instead of into your graveyard." Its ruling: the card "is
  -- not revealed unless the spell or ability requiring the discard specifically
  -- says it is". Bob controls it and takes the option.
  --
  -- The pair differs in bob's answer alone: declining sends the swamp to his
  -- graveyard, where the rider finds it and the spell returns. Making
  -- findableAfterMove's hidden-zone arm answer True returns the spell in the
  -- first case too, and its first assertion is what reddens.
  Spec.it s "CR 701.9c a land discarded into a library unrevealed does not return Psychic Miasma" $ do
    (after, swamp, piker, miasma) <- lengBoard s registry OptionalDecision.Exercises
    Spec.assertEqWith s "psychic miasma stayed in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName miasma)]
    Spec.assertEqWith s "and did not return to alice's hand" (namesIn Zone.Hand S.alice after) []
    -- The guard against a pass that never hid the card: CR 401.2's top, and no
    -- CR 701.20a reveal on the way.
    Spec.assertEqWith s "bob's discarded swamp is on top of his library" (namesIn Zone.Library S.bob after) [Just (S.printingName swamp), Just (S.printingName piker)]
    Spec.assertEqWith s "and nobody revealed it" (S.revealsOf after) []
    Spec.assertEqWith s "bob's graveyard is empty" (namesIn Zone.Graveyard S.bob after) []
  -- CR 401.4: "if an effect puts two or more cards in a specific position in a
  -- library at the same time, the owner of those cards may arrange them in any
  -- order" -- Library of Leng's own ruling says the same. Mind Rot's two
  -- discards are one event (CR 608.2f), each redirected onto the top of bob's
  -- library, so bob is asked their order. The pair differs in bob's answer
  -- alone: kept, the Forest moved last is on top; reversed, the Island is, and
  -- it is what bob draws next.
  Spec.it s "CR 401.4 bob orders the two Mind Rot discards Library of Leng puts on top of his library" $ do
    reversed <- lengMindRotBoard s registry True
    kept <- lengMindRotBoard s registry False
    let name = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "reversed: the Island is on top, over the Forest" (namesIn Zone.Library S.bob reversed) [name "Island", name "Forest", name "Goblin Piker"]
    Spec.assertEqWith s "kept: the Forest is on top, over the Island" (namesIn Zone.Library S.bob kept) [name "Forest", name "Island", name "Goblin Piker"]
    Spec.assertEqWith s "and his graveyard is empty" (namesIn Zone.Graveyard S.bob reversed) []
  -- The same rule through the destroy funnel: Wheel of Sun and Moon sends Day of
  -- Judgment's two victims to the bottom of bob's library together, and bob
  -- orders them from the bottom up.
  Spec.it s "CR 401.4 bob orders the two creatures Wheel of Sun and Moon puts under his library" $ do
    reversed <- wheelJudgmentBoard s registry True
    kept <- wheelJudgmentBoard s registry False
    let name = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "reversed: the Goblin Piker is on the bottom" (namesIn Zone.Library S.bob reversed) [name "Typhoid Rats", name "Ogre Sentry", name "Goblin Piker"]
    Spec.assertEqWith s "kept: the Ogre Sentry is on the bottom" (namesIn Zone.Library S.bob kept) [name "Typhoid Rats", name "Goblin Piker", name "Ogre Sentry"]
  -- And through the mill funnel, which moves its cards one at a time outside
  -- any event bracket: Tome Scour's five cards reach the bottom together all
  -- the same (CR 701.17a, one instruction), so bob orders them.
  Spec.it s "CR 401.4 bob orders the five cards Wheel of Sun and Moon puts under his library from Tome Scour" $ do
    reversed <- wheelScourBoard s registry True
    kept <- wheelScourBoard s registry False
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "reversed: the Plains, milled first, is on the bottom" (namesIn Zone.Library S.bob reversed) (names ["Goblin Piker", "Forest", "Mountain", "Swamp", "Island", "Plains"])
    Spec.assertEqWith s "kept: the Forest, milled last, is on the bottom" (namesIn Zone.Library S.bob kept) (names ["Goblin Piker", "Plains", "Island", "Swamp", "Mountain", "Forest"])
  -- And through a surveil's graveyard half (CR 701.25a), the same one-by-one
  -- road.
  Spec.it s "CR 401.4 alice orders the two cards she surveils under her library with Wheel of Sun and Moon" $ do
    reversed <- wheelCurateBoard s registry True
    kept <- wheelCurateBoard s registry False
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "reversed: the Plains, put first, is under the Island" (namesIn Zone.Library S.alice reversed) (names ["Swamp", "Island", "Plains", "Curate"])
    Spec.assertEqWith s "kept: the Island, put last, is under the Plains" (namesIn Zone.Library S.alice kept) (names ["Swamp", "Plains", "Island", "Curate"])
    Spec.assertEqWith s "and she drew the Goblin Piker" (namesIn Zone.Hand S.alice reversed) (names ["Goblin Piker"])
  -- CR 404.3: "if an effect or rule puts two or more cards into the same
  -- graveyard at the same time, the owner of those cards may arrange them in
  -- any order", and CR 404.1 makes the top observable: Volrath's Shapeshifter
  -- has the full text of the top card of bob's graveyard. Tome Scour mills
  -- five at once (CR 701.17a), so bob, whose graveyard order Matters, is asked
  -- -- he and not alice, who cast it.
  Spec.it s "CR 404.3 bob puts the milled Ogre Sentry on top of his graveyard, and Volrath's Shapeshifter becomes it" $ do
    (reversed, shifterId) <- shapeshifterScourBoard s registry (Just GraveyardOrder.Matters) reversedBatch
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Shapeshifter is a 3/3 Ogre Sentry" (Projection.namesOf shifterId reversed, S.powerToughnessOf shifterId reversed) (Set.singleton (CardName.MkCardName (Text.pack "Ogre Sentry")), Just (3, 3))
    Spec.assertEqWith s "the Ogre Sentry is on top" (namesIn Zone.Graveyard S.bob reversed) (names ["Mountain", "Swamp", "Island", "Plains", "Ogre Sentry"])
  -- The same board with bob's standing setting left at a new game's default:
  -- the order does not matter to him, so he is not asked and the cards keep the order they
  -- moved in, his reversing answer never given.
  Spec.it s "CR 404.3 bob, to whom graveyard order does not matter, is not asked" $ do
    (unasked, shifterId) <- shapeshifterScourBoard s registry Nothing reversedBatch
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Mountain, milled last, is on top, and the Shapeshifter is its printed 0/1" (Projection.namesOf shifterId unasked, S.powerToughnessOf shifterId unasked) (Set.singleton (CardName.MkCardName (Text.pack "Volrath's Shapeshifter")), Just (0, 1))
    Spec.assertEqWith s "in the order they moved" (namesIn Zone.Graveyard S.bob unasked) (names ["Ogre Sentry", "Plains", "Island", "Swamp", "Mountain"])
  -- Asked, bob may answer that this batch's order does not matter to him; it
  -- then keeps the order it moved in.
  Spec.it s "CR 404.3 answering any order keeps the order the cards moved in" $ do
    (anyOrder, shifterId) <- shapeshifterScourBoard s registry (Just GraveyardOrder.Matters) (const GraveyardArrangement.AnyOrder)
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Mountain is on top" (namesIn Zone.Graveyard S.bob anyOrder) (names ["Ogre Sentry", "Plains", "Island", "Swamp", "Mountain"])
    Spec.assertEqWith s "and the Shapeshifter is its printed 0/1" (S.powerToughnessOf shifterId anyOrder) (Just (0, 1))
  -- The same rule through the destroy funnel: Day of Judgment's two victims
  -- reach bob's graveyard at once, and he orders them.
  Spec.it s "CR 404.3 bob orders the two creatures Day of Judgment puts into his graveyard" $ do
    reversed <- judgmentBoard s registry reversedBatch
    kept <- judgmentBoard s registry (GraveyardArrangement.InOrder . zipWith const [0 ..])
    Spec.assertEqWith s "reversed is kept, the other way up" (namesIn Zone.Graveyard S.bob reversed) (reverse (namesIn Zone.Graveyard S.bob kept))
    Spec.assertEqWith s "two cards" (length (namesIn Zone.Graveyard S.bob kept)) 2
  -- A surveil's answer already orders the cards it puts into the graveyard
  -- (Prompt.ChooseSurveil), so alice is not asked again: her reversing
  -- answerer leaves the Plains, put first, under the Island, and Curate on top
  -- (CR 608.2n).
  Spec.it s "CR 404.3 a surveil's graveyard cards are not arranged a second time" $ do
    after <- curateBoard s registry
    let names = fmap (Just . CardName.MkCardName . Text.pack) :: [String] -> [Maybe CardName.CardName]
    Spec.assertEqWith s "the Plains, then the Island, then Curate" (namesIn Zone.Graveyard S.alice after) (names ["Plains", "Island", "Curate"])
  -- CR 118.12: Tweeze's "You may discard a card. If you do, draw a card" makes
  -- the discard a cost paid on resolution, and Library of Leng's ruling: "you
  -- can't use the Library of Leng ability ... when you discard a card as a cost,
  -- because costs aren't effects". Alice controls the Library and takes every
  -- "may" and every redirect she is offered; were the redirect offered, the
  -- Forest would go on top of her library and be the card she draws.
  Spec.it s "CR 118.12 Library of Leng does not reach Tweeze's discard, a cost" $ do
    mountain <- S.printingOf s registry "Mountain"
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    tweeze <- S.printingOf s registry "Tweeze"
    leng <- S.printingOf s registry "Library of Leng"
    let (_, withLeng) = S.addPermanent leng S.alice (S.landsInPlay mountain 3)
        (_, stocked) = S.addLibraryCard piker S.alice withLeng
        (withSpell, spellId) = S.handOne tweeze stocked
        (_, gs) = S.addHandCard forest S.alice withSpell
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseOptional {} -> OptionalDecision.Exercises
          _ -> lengAnswer OptionalDecision.Exercises p
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
        name = Just . CardName.MkCardName . Text.pack
    Spec.assertEqWith s "the Forest is in alice's graveyard beside Tweeze" (List.sort (namesIn Zone.Graveyard S.alice after)) (List.sort [name "Forest", name "Tweeze"])
    Spec.assertEqWith s "and she drew the Goblin Piker, not the Forest" (namesIn Zone.Hand S.alice after) [name "Goblin Piker"]
    Spec.assertEqWith s "bob took the 3 damage" (S.lifeOf S.bob after) (Just 17)
  -- The row's other gate, its ruling's "you can't use the Library of Leng
  -- ability ... when you discard a card as a cost, because costs aren't
  -- effects": a DiscardCause.Ordinary discard is never offered the row, so an
  -- answerer that would take it is never asked and the card reaches the
  -- graveyard.
  Spec.it s "CR 701.9a Library of Leng does not reach a discard no effect caused" $ do
    swamp <- S.printingOf s registry "Swamp"
    leng <- S.printingOf s registry "Library of Leng"
    let (_, withLeng) = S.addPermanent leng S.bob (S.landsInPlay swamp 3)
        (swampId, gs) = S.addHandCard swamp S.bob withLeng
        after = S.runPure (lengAnswer OptionalDecision.Exercises) gs (Event.discard DiscardCause.Ordinary S.bob swampId)
    Spec.assertEqWith s "bob's swamp is in his graveyard" (namesIn Zone.Graveyard S.bob after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "and not in his library" (namesIn Zone.Library S.bob after) []
  -- Pulmonic Sliver's GRANTED "may" row (CR 614.1a), on a stolen Sliver: alice
  -- controls Pulmonic, carol owns Lymph Sliver, bob controls it. CR 109.5 makes
  -- bob, the controller of the Sliver the ability is on, the one asked; CR 400.3
  -- sends the card to carol's library. The pair differs in the answerer's flip
  -- alone: bob's answer is as given and every other seat answers the opposite,
  -- so asking alice or carol lands the card in the wrong zone in both cases.
  Spec.it s "CR 109.5 / 400.3 Pulmonic Sliver's granted redirect asks the Sliver's controller and tops its owner's library" $ do
    (after, lymph, piker) <- pulmonicBoard s registry OptionalDecision.Exercises
    Spec.assertEqWith s "lymph sliver is on top of carol's library" (namesIn Zone.Library S.carol after) [Just (S.printingName lymph), Just (S.printingName piker)]
    Spec.assertEqWith s "and bob's library is untouched" (namesIn Zone.Library S.bob after) [Just (S.printingName piker)]
  Spec.it s "CR 614.1a Pulmonic Sliver's granted redirect declined leaves the Sliver in its owner's graveyard" $ do
    (after, lymph, piker) <- pulmonicBoard s registry OptionalDecision.Declines
    Spec.assertEqWith s "lymph sliver is in carol's graveyard" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName lymph)]
    Spec.assertEqWith s "and carol's library is untouched" (namesIn Zone.Library S.carol after) [Just (S.printingName piker)]
  -- The Wheel's own relation, CR 303.4b's enchanted player
  -- (ControllerRelation.EnchantedPlayers, judged by Replacement.relationHolds),
  -- as the whole card: cast at bob, then one card of each seat's headed for its
  -- owner's graveyard. THREE seats, because two cannot part the relation from
  -- its siblings: alice controls the Wheel, so Related You would take her card, and
  -- carol is her opponent, so Opponents would take carol's. Bob's library is
  -- stocked so the bottom is a position; the reveal (CR 701.20a) is asserted off
  -- the log, and is bob's, who holds the card.
  Spec.it s "CR 303.4b / 614.1a Wheel of Sun and Moon reroutes only the enchanted player's cards" $ do
    forest <- S.printingOf s registry "Forest"
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    wheel <- S.printingOf s registry "Wheel of Sun and Moon"
    let lands = S.landsFor forest S.alice 2 S.threePlayerGame
        (staged, spellId) = S.handOne wheel lands
        cast = snd (Engine.runGamePure atBobAnswer staged (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        (_, stocked) = S.addLibraryCard piker S.bob resolved
        (bobs, g1) = S.addHandCard swamp S.bob stocked
        (alices, g2) = S.addHandCard swamp S.alice g1
        (carols, g3) = S.addHandCard swamp S.carol g2
        after = S.runPure S.identityAnswer g3 (mapM_ (\oid -> Event.changeZone oid Zone.Graveyard) [bobs, alices, carols])
        enchanting = fmap Object.attachedTo (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after)))
    Spec.assertEqWith s "bob's card went to the bottom of bob's library" (namesIn Zone.Library S.bob after) [Just (S.printingName piker), Just (S.printingName swamp)]
    Spec.assertEqWith s "and not to bob's graveyard" (namesIn Zone.Graveyard S.bob after) []
    Spec.assertEqWith s "alice's own card reached her graveyard -- she is not the enchanted player" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "carol's card reached her graveyard -- an opponent is not the enchanted player" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName swamp)]
    Spec.assertEqWith s "bob revealed the card on its way" (S.revealsOf after) [(S.bob, Set.singleton (S.printingName swamp))]
    Spec.assertEqWith s "the Wheel is attached to bob" enchanting [Just (Recipient.ToPlayer S.bob)]
  -- CR 701.9's OTHER arity: Tinybones Joins Up's "any number of target players
  -- each discard a card", where the slot names three seats rather than Mind
  -- Rot's one. CR 101.4 is the ordering rule -- its own worked example is a
  -- table-wide edict -- so every seat is asked before any card moves, in turn
  -- order from the active player.
  --
  -- THREE seats and each hand a distinct printing, so both readings of "each"
  -- are separated: a fold over the controller's hand would empty alice's and
  -- leave the other two, and legalOne's Nothing would leave all three at two.
  Spec.it s "CR 701.9 / 101.4 Tinybones Joins Up has every targeted player discard, in APNAP order" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    sentry <- S.printingOf s registry "Ogre Sentry"
    rats <- S.printingOf s registry "Typhoid Rats"
    tinybones <- S.printingOf s registry "Tinybones Joins Up"
    let base = S.landsFor swamp S.alice 1 S.threePlayerGame
        (withSpell, spellId) = S.handOne tinybones base
        -- The hands are stocked AFTER S.handOne, which sets alice's hand rather
        -- than adding to it.
        gs = handCards rats S.carol 2 (handCards sentry S.bob 2 (handCards piker S.alice 2 withSpell))
        -- CR 601.2c: "any number" announces zero unless the answer says
        -- otherwise, and a zero-target announcement is satisfied by BOTH
        -- readings. So announce every recipient the board offers, and take the
        -- whole offered set rather than building recipients by hand.
        --
        -- The state is the seats ChooseDiscard was raised for, in the order they
        -- were asked, which is what makes CR 101.4's ordering observable.
        answer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
        answer p = case p of
          Prompt.AnnounceTargets _ _ _ slots -> pure (fmap (\(_, candidates) -> Natural.length candidates) slots)
          Prompt.ChooseTargets _ _ _ sets -> pure (fmap snd sets)
          Prompt.ChooseDiscard _ victim held _ -> do
            State.modify' (<> [victim])
            pure (take 1 held)
          _ -> pure (S.identityAnswer p)
        (after, asked) =
          flip State.runState [] $ do
            (_, castGs) <- Engine.runGame answer gs (S.cast S.alice spellId)
            -- The enchantment resolves, then settling puts its CR 603.3d enters
            -- trigger on the stack with its targets announced.
            (_, entered) <- Engine.runGame answer castGs (Stack.resolveTop >> Engine.settleForPriority)
            (_, resolved) <- Engine.runGame answer entered Stack.resolveTop
            pure resolved
    Spec.assertEqWith s "alice discarded one of her two" (S.handSize S.alice after) 1
    Spec.assertEqWith s "bob discarded one of his two" (S.handSize S.bob after) 1
    Spec.assertEqWith s "carol discarded one of her two" (S.handSize S.carol after) 1
    -- WHOSE card left whose hand: a fold reading the controller's hand three
    -- times would put three pikers in one graveyard.
    Spec.assertEqWith s "alice's graveyard holds her own piker" (namesIn Zone.Graveyard S.alice after) [Just (S.printingName piker)]
    Spec.assertEqWith s "bob's graveyard holds his own sentry" (namesIn Zone.Graveyard S.bob after) [Just (S.printingName sentry)]
    Spec.assertEqWith s "carol's graveyard holds her own rats" (namesIn Zone.Graveyard S.carol after) [Just (S.printingName rats)]
    Spec.assertEqWith s "CR 101.4: asked in turn order from the active player" asked [S.alice, S.bob, S.carol]

-- CR 401.7's "Nth from the top": Temporal Cleansing leaves the depth or the
-- bottom to the OWNER, Oust states the depth, and Unexpectedly Absent reads it
-- off X. Every board aims at a creature bob OWNS and alice CONTROLS, so the
-- owner and the controller are different seats.
libraryDepthSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
libraryDepthSpec s registry = Spec.describe s "LibraryDepth" $ do
  let -- bob's Goblin Piker under alice's control, bob's library seeded with
      -- the given names (the LAST is the top), and the spell in alice's hand
      -- over three Islands and three Plains.
      depthBoard spellName libraryNames = do
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        spell <- S.printingOf s registry spellName
        seeded <- traverse (S.printingOf s registry) libraryNames
        let (pikerId, g1) = S.addPermanent piker S.bob (S.landsFor plains S.alice 3 (S.landsInPlay island 3))
            g2 = S.giveControl pikerId S.alice g1
            (libraryIds, g3) = List.foldl' (\(ids, g) p -> let (oid, g') = S.addLibraryCard p S.bob g in (oid : ids, g')) ([], g2) seeded
            (gs, spellId) = S.handOne spell g3
        pure (gs, spellId, pikerId, libraryIds)
      -- Cast and resolve, recording every ChooseLibraryEnd as (who, how many
      -- above the upper option) and answering it with `end`.
      castAt :: ObjectId.ObjectId -> Natural -> LibraryPosition.LibraryPosition -> GameState.GameState -> ObjectId.ObjectId -> (GameState.GameState, [(PlayerId.PlayerId, Natural)])
      castAt pikerId x end gs spellId =
        let answerer :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Natural)] r
            answerer p = case p of
              -- Filtered from the offer: a Permanents pool offers no ToCreature.
              Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter ((== Just pikerId) . Recipient.objectOf) . snd) sets)
              Prompt.ChooseX {} -> pure x
              Prompt.ChooseLibraryEnd _ pid _ above -> do
                State.modify (<> [(pid, above)])
                pure end
              _ -> pure (S.identityAnswer p)
         in State.runState
              ( fmap snd . Engine.runGame answerer gs $ do
                  S.cast S.alice spellId
                  Stack.resolveTop
              )
              []
      named = Just . CardName.MkCardName . Text.pack
  Spec.it s "CR 401.7 Temporal Cleansing: the OWNER picks second from the top, and it lands under the top card" $ do
    (gs, spellId, pikerId, _) <- depthBoard "Temporal Cleansing" ["Lightning Bolt", "Unsummon", "Griptide"]
    let (after, asked) = castAt pikerId 0 LibraryPosition.Top gs spellId
    Spec.assertEqWith s "bob's library, top first" (namesIn Zone.Library S.bob after) (fmap named ["Griptide", "Goblin Piker", "Unsummon", "Lightning Bolt"])
    Spec.assertEqWith s "the owner was asked, offered one card above the upper option" asked [(S.bob, 1)]

-- Every question Aetherspouts raises: which object each owner was asked to place
-- (CR 401.2), and which batch each owner was asked to arrange (CR 401.4).
type SpoutsLog = ([(PlayerId.PlayerId, ObjectId.ObjectId)], [(PlayerId.PlayerId, LibraryPosition.LibraryPosition, [ObjectId.ObjectId])])

-- alice is mid-combat attacking with `mine` creatures she owns and `stolen`
-- creatures BOB owns under her control, holds an Aetherspouts and the five
-- Islands that pay for it, and both libraries are two cards deep so an arrival
-- at either end is distinguishable from one at the other.
--
-- The stolen creature is what makes a ONE-COMBAT board hold two owners at all:
-- CR 508.1a says "the active player chooses which creatures THAT THEY CONTROL
-- ... will attack", so every attacker in one combat shares a controller and only
-- separating owner from controller can put two owners' cards in the batch.
-- S.giveControl also settles it under alice, which is that rule's second
-- sentence ("controlled by the active player continuously since the turn
-- began").
spoutsBoard :: Printing.Printing -> Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId])
spoutsBoard island spouts mine stolen =
  let addAll pid ps gs = List.foldl' (\(ids, g) p -> let (oid, g1) = S.addPermanent p pid g in (ids <> [oid], g1)) ([], gs) ps
      (gs0, ours, _) = S.combatBoardOf mine []
      (theirs, gs1) = addAll S.bob stolen gs0
      gs2 = List.foldl' (\g oid -> S.giveControl oid S.alice g) gs1 theirs
      withLands = List.foldl' (\g _ -> snd (S.addPermanent island S.alice g)) gs2 [1 :: Int .. 5]
      stocked = List.foldl' (\g pid -> snd (S.addLibraryCard island pid (snd (S.addLibraryCard island pid g)))) withLands [S.alice, S.bob]
      (withCard, spell) = S.handOne spouts stocked
   in ( -- handOne parks its state in a precombat main phase; this board is
        -- mid-combat, the way Pawl.MassEffectSpec's trumpetBoard restores it.
        withCard
          { GameState.phase = GameState.phase gs0,
            GameState.priority = GameState.priority gs0
          },
        spell,
        ours,
        theirs
      )

-- Declare alice's attack, then cast and resolve the Aetherspouts under an
-- answerer that records every question it is asked.
--
-- `end` picks each owner's answer BY WHO IS ASKED, which is the whole point of
-- the two-owner board: an implementation that raised the prompt with the
-- resolving CONTROLLER would hand both cards the same end.
castSpouts :: (PlayerId.PlayerId -> LibraryPosition.LibraryPosition) -> [Natural] -> GameState.GameState -> ObjectId.ObjectId -> (GameState.GameState, SpoutsLog)
castSpouts end arrangement board spell =
  let attacking = S.runPure S.aggressiveAnswer board (Combat.declareAttackers S.manaPerformer S.alice)
      answerer :: Prompt.Prompt r -> State.State SpoutsLog r
      answerer p = case p of
        Prompt.ChooseLibraryEnd _ pid oid _ -> do
          State.modify (\(ends, arrs) -> (ends <> [(pid, oid)], arrs))
          pure (end pid)
        Prompt.ArrangeLibraryArrivals _ pid position oids -> do
          State.modify (\(ends, arrs) -> (ends, arrs <> [(pid, position, oids)]))
          pure arrangement
        _ -> pure (S.identityAnswer p)
   in State.runState
        ( fmap snd . Engine.runGame answerer attacking $ do
            S.cast S.alice spell
            Stack.resolveTop
        )
        ([], [])

-- Aetherspouts ({3}{U}{U} instant, "For each attacking creature, its owner puts
-- it on their choice of the top or bottom of their library"): the pool's
-- producer for a library end the OWNER picks (CR 401.2, #1035), and a producer
-- for CR 401.4's arrangement of two or more cards reaching one end at once
-- (#990) -- Surging Dementia's ripple states its end and still hands over the
-- arrangement, so it reaches that half too. WotC's own 2014-07-18 ruling on the
-- card states both halves.
--
-- Nothing here bears on the order SoulfireEruption's group asks about: CR 608.2f's
-- secondary sentence is guarded by "if
-- the action can't be processed simultaneously", and CR 401.4 gives a library
-- destination its own rule with its own decider -- so a correct Aetherspouts
-- SCREENS the sweep order off rather than exposing it.
aetherspoutsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherspoutsSpec s registry = Spec.describe s "Aetherspouts" $ do
  -- The claim: the end each creature goes to is decided by that creature's
  -- OWNER, not by the spell's controller and not by the engine. alice controls
  -- both attackers; bob owns one of them, and only he can send it to the top.
  Spec.it s "CR 401.2 each attacking creature's OWNER picks the end, not the resolving controller" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    giant <- S.printingOf s registry "Hill Giant"
    spouts <- S.printingOf s registry "Aetherspouts"
    let (board, spell, ours, theirs) = spoutsBoard island spouts [piker] [giant]
        (after, (ends, arrangements)) = castSpouts (\pid -> if pid == S.bob then LibraryPosition.Top else LibraryPosition.Bottom) [] board spell
        -- By NAME, through `namesIn`: CR 400.7 mints a fresh id at the
        -- destination, so a library arrival is never the id it had on the
        -- battlefield. That is also why the two attackers are two different
        -- printings -- two Pikers would be indistinguishable at either end.
        bobs = namesIn Zone.Library S.bob after
        alices = namesIn Zone.Library S.alice after
    -- Without this the sweep could have found nothing and every assertion below
    -- would pass vacuously.
    Spec.assertEqWith s "both attackers left the battlefield" (filter (`S.onBattlefield` after) (ours <> theirs)) []
    Spec.assertEqWith s "each creature's own owner was asked, once, in the sweep's APNAP order" ends (fmap ((,) S.alice) ours <> fmap ((,) S.bob) theirs)
    -- One card per (owner, end) group, so CR 401.4 has nothing to arrange. The
    -- negative half of the elision pair; the positive is the next test.
    Spec.assertEqWith s "and nobody was asked to arrange a batch of one" arrangements []
    Spec.assertEqWith s "each library grew by exactly its owner's card" (length bobs, length alices) (3, 3)
    -- The discriminating half. An implementation that ignored the answers would
    -- fall back on LibraryPosition.defaultValue and put everything on the
    -- BOTTOM, so it is bob's Giant at the TOP that catches it -- a
    -- both-cards-to-the-bottom board would pass for the wrong reason.
    Spec.assertEqWith
      s
      "bob answered Top so his Giant heads his library; alice answered Bottom so her Piker is last in hers"
      (take 1 bobs <> drop 2 alices)
      [Just (S.printingName giant), Just (S.printingName piker)]

drawCardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
drawCardSpec s registry = Spec.describe s "DrawCard" $ do
  Spec.it s "CR 121.2 drawCard moves the top library card to hand" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (_, withCard) = S.addLibraryCard piker S.alice base
        after = S.runPure S.identityAnswer withCard (Event.drawCard S.alice)
    Spec.assertEqWith s "one card in hand" (S.handSize S.alice after) 1
    Spec.assertEqWith s "library empty" (Game.zoneMembers Zone.Library S.alice after) []
  Spec.it s "CR 121.3 drawing from an empty library records the failed draw" $ do
    let after = S.runPure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Event.drawCard S.alice)
    Spec.assertBool s (Set.member S.alice (GameState.drewFromEmpty after)) "drewFromEmpty marked"

loseLifeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
loseLifeSpec s registry = Spec.describe s "LoseLife" $ do
  -- Both cases are Sign in Blood, the card that proves the opcode (#273): its
  -- two clauses share one target slot, so the player who draws is the player
  -- who loses life, and neither is aimed at the caster.
  -- The last assertion is the falsifier for a life loss spelled as damage.
  -- CR 119.2 makes damage a CAUSE of life loss, not a synonym for it, so
  -- this records no damage event for CR 614/615's replacement and
  -- prevention, infect's CR 120.3b diversion or CR 704.5h's deathtouch scan
  -- to read.
  Spec.it s "CR 119.3 Sign in Blood makes the player it targets draw two and lose two life" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    signInBlood <- S.printingOf s registry "Sign in Blood"
    let base = S.landsInPlay swamp 2
        withLib = stockLibrary piker S.bob 3 base
        (gs, spellId) = S.handOne signInBlood withLib
        cast = snd (Engine.runGamePure atBobAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure atBobAnswer cast Stack.resolveTop)
        isDamage ev = case ev of
          GameEvent.DamageDealt _ -> True
          _ -> False
    Spec.assertEqWith s "bob drew two" (S.handSize S.bob after) 2
    Spec.assertEqWith s "and lost two life" (S.lifeOf S.bob after) (fmap (subtract 2) (S.lifeOf S.bob gs))
    Spec.assertEqWith s "alice, who cast it, lost none" (S.lifeOf S.alice after) (S.lifeOf S.alice gs)
    Spec.assertBool s (not (any isDamage (S.eventsOf after))) "no damage was dealt (CR 119.2)"

-- A per-player amount that is a number of the RECIPIENT'S OWN, on three opcodes
-- and through the two spellings of that reading:
--
--   * Stronghold Discipline, {2}{B}{B} Sorcery: "Each player loses 1 life for
--     each creature they control." Effect.LoseLife over a count filtered by
--     Filter.ControlledByRecipient -- CR 110.2's CONTROL, read over the shared
--     battlefield (CR 400.1), which no per-seat scope can express (see #161).
--   * Nature's Resurgence, {2}{G}{G} Sorcery: "Each player draws a card for each
--     creature card in their graveyard." Effect.Draw over a count whose SCOPE is
--     the recipient's own graveyard -- PlayerRef.Candidate, substituted by
--     Quantity.forCandidate, which is the half a nested Count used to be left out
--     of.
--   * Acidic Soil, {2}{R} Sorcery: "Acidic Soil deals damage to each player equal
--     to the number of lands they control." Effect.DealDamage over Stronghold
--     Discipline's spelling, and the case for CR 608.2f: the amount is read once
--     per recipient and the damage is still dealt as ONE batch.
--
-- Three seats taking three DIFFERENT amounts in each case, because a board where
-- two of them take the same number cannot tell a per-recipient reading from one
-- evaluation shared by the table. alice, the CONTROLLER, is one of the three:
-- "each player" reaches her, and handing everyone the controller's number is
-- exactly the error being excluded.
--
-- All three cards are mandatory and targetless, so no prompt is raised during
-- any of the resolutions and no answerer can repair a mutated reading.
perRecipientAmountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
perRecipientAmountSpec s registry =
  let castAndResolve spellId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast Stack.resolveTop
      addPikers piker pid n gs = List.foldl' (\board _ -> snd (S.addPermanent piker pid board)) gs [1 .. n :: Int]
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
      -- alice controls 3 Pikers, bob 2 and carol 1, on life totals 20, 17 and 13
      -- -- distinct counts against distinct totals, so no seat's answer is any
      -- other seat's and none of them is a total either. The Swamps are alice's
      -- {2}{B}{B}; a land is not a creature, so they do not enter the count.
      disciplineBoard = do
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        discipline <- S.printingOf s registry "Stronghold Discipline"
        let withLands = S.landsFor swamp S.alice 4 S.threePlayerGame
            withCreatures = addPikers piker S.bob 2 (addPikers piker S.alice 3 withLands)
            (carolPiker, withCarol) = S.addPermanent piker S.carol withCreatures
            lifed = withCarol {GameState.players = at S.alice 20 (at S.bob 17 (at S.carol 13 (GameState.players withCarol)))}
            (gs, spellId) = S.handOne discipline lifed
        pure (carolPiker, spellId, gs)
      -- alice controls 3 Mountains, bob 2 and carol 1, on life totals 20, 17 and
      -- 13. alice's three are also the {2}{R}, and they are TAPPED by the time
      -- the spell resolves -- "lands they control" counts a tapped land, so her
      -- own answer is still 3.
      acidicBoard = do
        mountain <- S.printingOf s registry "Mountain"
        soil <- S.printingOf s registry "Acidic Soil"
        let withAlice = S.landsFor mountain S.alice 3 S.threePlayerGame
            withBob = S.landsFor mountain S.bob 2 withAlice
            (carolMountain, withCarol) = S.addPermanent mountain S.carol withBob
            lifed = withCarol {GameState.players = at S.alice 20 (at S.bob 17 (at S.carol 13 (GameState.players withCarol)))}
            (gs, spellId) = S.handOne soil lifed
        pure (carolMountain, spellId, gs)
      damages gs = fmap (\ev -> (DamageEvent.target ev, DamageEvent.amount ev)) (S.damageEventsOf gs)
   in Spec.describe s "PerRecipientAmount" $ do
        Spec.it s "CR 119.3 Stronghold Discipline charges each player for their OWN creatures" $ do
          (_carolPiker, spellId, gs) <- disciplineBoard
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "alice, controlling 3, went 20 -> 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, controlling 2, went 17 -> 15 -- not alice's 3" (S.lifeOf S.bob after) (Just 15)
          Spec.assertEqWith s "carol, controlling 1, went 13 -> 12" (S.lifeOf S.carol after) (Just 12)
          Spec.assertEqWith s "three losses, each the payer's own count" (lifeLosses after) [(S.alice, 3), (S.bob, 2), (S.carol, 1)]
        -- The control twin, differing in ONE thing: carol's Piker is under bob's
        -- control. She still OWNS it, so an amount read off the owner-sliced
        -- battlefield would leave both their answers where they were; CR 110.2's
        -- control is what moves the 1 from carol to bob.
        Spec.it s "CR 110.2 the control: a creature carol owns but bob controls is charged to BOB" $ do
          (carolPiker, spellId, gs0) <- disciplineBoard
          let gs = S.giveControl carolPiker S.bob gs0
              after = castAndResolve spellId gs
          Spec.assertEqWith s "alice is unmoved at 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, now controlling 3, went 17 -> 14" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "carol, controlling nothing, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "and CR 119.9 leaves her out of the log entirely" (lifeLosses after) [(S.alice, 3), (S.bob, 3)]
          Spec.assertEqWith s "the Piker is still carol's card" (fmap Object.owner (Game.lookupObject carolPiker after)) (Just S.carol)
        Spec.it s "CR 120.3a Acidic Soil deals each player their OWN land count" $ do
          (_carolMountain, spellId, gs) <- acidicBoard
          let after = castAndResolve spellId gs
          Spec.assertEqWith s "alice, controlling 3, went 20 -> 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, controlling 2, went 17 -> 15 -- not alice's 3" (S.lifeOf S.bob after) (Just 15)
          Spec.assertEqWith s "carol, controlling 1, went 13 -> 12" (S.lifeOf S.carol after) (Just 12)
          -- Three events, and CR 608.2f's simultaneity is what the ONE
          -- applyDamage call keeps: the amounts differ per seat while the batch
          -- does not become three batches.
          Spec.assertEqWith s "one batch of three, each the recipient's own count" (damages after) [(Recipient.ToPlayer S.alice, 3), (Recipient.ToPlayer S.bob, 2), (Recipient.ToPlayer S.carol, 1)]
        -- The control twin, differing in ONE thing: carol's Mountain is under
        -- bob's control. She still OWNS it, so an amount read off an
        -- owner-sliced battlefield would leave both their answers where they
        -- were; CR 110.2's control is what moves the 1 from carol to bob.
        Spec.it s "CR 110.2 the control: a land carol owns but bob controls is charged to BOB" $ do
          (carolMountain, spellId, gs0) <- acidicBoard
          let gs = S.giveControl carolMountain S.bob gs0
              after = castAndResolve spellId gs
          Spec.assertEqWith s "alice is unmoved at 17" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "bob, now controlling 3, went 17 -> 14" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "carol, controlling nothing, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "and CR 120.8 drops her recipient without dropping the batch" (damages after) [(Recipient.ToPlayer S.alice, 3), (Recipient.ToPlayer S.bob, 3)]
          Spec.assertEqWith s "the Mountain is still carol's card" (fmap Object.owner (Game.lookupObject carolMountain after)) (Just S.carol)

-- Mirror Universe (Legends) on alice's battlefield, in her own upkeep, with the
-- three seats at three DIFFERENT life totals: "{T}, Sacrifice this artifact:
-- Exchange life totals with target opponent. Activate only during your upkeep."
--
-- Three seats because a two-player board cannot tell the exchange's TARGET from
-- "the other player". carol is a second legal target the interpreter can pick,
-- and the totals are distinct so that no pair of them coincides.
--
-- The schedule loses its head for augurBoard's reason (ActivateSpec): emptyGame's
-- `remaining` still begins with the upkeep step, so a runStep-driven test would
-- otherwise advance out of the step the card names.
mirrorBoard :: Printing.Printing -> Integer -> Integer -> Integer -> (ObjectId.ObjectId, GameState.GameState)
mirrorBoard mirror aliceLife bobLife carolLife =
  let (mirrorId, gs1) = S.addPermanent mirror S.alice S.threePlayerGame
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in ( mirrorId,
        gs1
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs1),
            GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players gs1)))
          }
      )

-- Soul Conduit (Eldritch Moon) on alice's battlefield over six untapped Islands,
-- with the three seats at three DIFFERENT life totals: "{6}, {T}: Two target
-- players exchange life totals."
--
-- The same three seats and the same schedule surgery as mirrorBoard, and for the
-- same reasons -- but here the point of the third seat is that the two sides of
-- the exchange can BOTH be players other than the controller, which two seats
-- cannot express.
soulConduitBoard :: Printing.Printing -> Printing.Printing -> Integer -> Integer -> Integer -> (ObjectId.ObjectId, GameState.GameState)
soulConduitBoard conduit island aliceLife bobLife carolLife =
  let withLands = List.foldl' (\gs _ -> snd (S.addPermanent island S.alice gs)) S.threePlayerGame [1 .. 6 :: Int]
      (conduitId, gs1) = S.addPermanent conduit S.alice withLands
      at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
   in ( conduitId,
        gs1
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.Beginning BeginningStep.Upkeep,
            GameState.priority = Just S.alice,
            GameState.remaining = Seq.drop 1 (GameState.remaining gs1),
            GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players gs1)))
          }
      )

-- Takes the first activation offered, taps whatever the payment asks for, and
-- fills the target slot with `sides` -- S.preferring rather than a fixed set, so
-- the announced count (CR 601.2c) is what decides how many are named.
conduitAnswer :: [PlayerId.PlayerId] -> Prompt.Prompt r -> r
conduitAnswer sides p =
  let wanted r = case r of
        Recipient.ToPlayer pid -> elem pid sides
        _ -> False
   in case p of
        Prompt.ChooseAction _ _ options -> case filter isActivation options of
          a : _ -> a
          [] -> A.Pass
        Prompt.ChooseManaSource _ _ candidates -> Just (NonEmpty.head candidates)
        Prompt.ChooseTargets _ _ _ sets -> S.preferring wanted sets
        _ -> S.identityAnswer p

-- Takes the first activation offered and aims every target slot at `who`.
exchangeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
exchangeAnswer who p = case p of
  Prompt.ChooseAction _ _ options -> case filter isActivation options of
    a : _ -> a
    [] -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
  _ -> S.identityAnswer p

isActivation :: A.Action -> Bool
isActivation a = case a of
  A.Activate {} -> True
  A.Pass -> False
  A.Play {} -> False
  A.Cast {} -> False
  A.TurnFaceUp {} -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot {} -> False
  A.Foretell _ -> False
  A.Suspend _ -> False
  A.PutCompanionIntoHand -> False
  A.RollPlanarDie -> False
  A.ActivateManaAbility _ -> False
  A.Ignore _ _ -> False
  A.EndEffect _ -> False

-- The life events the whole step logged, by player and amount. CR 701.12c makes
-- the exchange a GAIN and a LOSS rather than two assignments, so this is what a
-- "whenever you gain life" trigger would have to read.
lifeGains :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeGains gs = Maybe.mapMaybe (\ev -> case ev of GameEvent.LifeGained (LifeChange.MkLifeChange pid n) -> Just (pid, n); _ -> Nothing) (S.eventsOf gs)

lifeLosses :: GameState.GameState -> [(PlayerId.PlayerId, Natural)]
lifeLosses gs = Maybe.mapMaybe (\ev -> case ev of GameEvent.LifeLost (LifeChange.MkLifeChange pid n) -> Just (pid, n); _ -> Nothing) (S.eventsOf gs)

exchangeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exchangeLifeTotalsSpec s registry = Spec.describe s "ExchangeLifeTotals" $ do
  -- The gameplay-level proof (design.md section 4), driven through
  -- Engine.runStep and the priority loop: alice at 4 and bob at 27 swap, and
  -- each reaches the other's PREVIOUS total -- an implementation that wrote one
  -- side before reading the other would leave both on one number.
  Spec.it s "CR 701.12c whole card: Mirror Universe swaps its controller's total with the target's" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (mirrorId, board) = mirrorBoard mirror 4 27 13
        after = S.runPure (exchangeAnswer S.bob) board Engine.runStep
    Spec.assertEqWith s "alice took bob's 27" (S.lifeOf S.alice after) (Just 27)
    Spec.assertEqWith s "bob took alice's 4" (S.lifeOf S.bob after) (Just 4)
    Spec.assertEqWith s "carol, untargeted, is untouched" (S.lifeOf S.carol after) (Just 13)
    Spec.assertBool s (not (Set.member mirrorId (GameState.battlefield after))) "the Universe paid itself"
    -- CR 701.12c's "gains or loses the amount of life necessary", as events:
    -- 27 - 4 either way, and nothing else moved a life total this step.
    Spec.assertEqWith s "alice gained 23" (lifeGains after) [(S.alice, 23)]
    Spec.assertEqWith s "bob lost 23" (lifeLosses after) [(S.bob, 23)]
    -- The printed "target opponent" is the card's own filter, read from the
    -- perspective of the player activating it (CR 109.5), so alice is never a
    -- candidate. Paired with the outcome above rather than asserted alone, since
    -- a candidate list nothing consumed proves nothing.
    let candidates = case Activatable.abilitiesFor mirrorId board of
          [ability] -> case Seq.lookup 0 (Modal.modes (ActivatedAbility.modal ability)) of
            Just mode -> Map.elems (Target.legalSets (Just S.alice) False Map.empty mirrorId (Mode.targetSlots mode) board)
            Nothing -> []
          _ -> []
    Spec.assertEqWith s "both opponents are candidates, alice is not" candidates [Set.fromList [Recipient.ToPlayer S.bob, Recipient.ToPlayer S.carol]]

  -- CR 119.9: equal totals are an exchange that moves nobody, and a gain of 0 is
  -- no life gain event at all -- so "whenever you gain life" must not fire on it.
  Spec.it s "CR 119.9 an exchange between equal totals logs no life event" $ do
    mirror <- S.printingOf s registry "Mirror Universe"
    let (mirrorId, board) = mirrorBoard mirror 15 15 13
        after = S.runPure (exchangeAnswer S.bob) board Engine.runStep
    Spec.assertEqWith s "alice is still at 15" (S.lifeOf S.alice after) (Just 15)
    Spec.assertEqWith s "bob is still at 15" (S.lifeOf S.bob after) (Just 15)
    Spec.assertBool s (not (Set.member mirrorId (GameState.battlefield after))) "and the ability was activated: its sacrifice was paid"
    Spec.assertEqWith s "no gain" (lifeGains after) []
    Spec.assertEqWith s "no loss" (lifeLosses after) []

  -- CR 701.12c's other shape: BOTH sides come out of one instance of the word
  -- "target" (CR 601.2c), and neither of them need be the controller. bob at 27
  -- and carol at 13 swap while alice, who activated it, keeps her 4 -- so the
  -- reading in which the controller is always one side gets a different answer
  -- for every seat. The four numbers (4, 13, 27 and the 14 that moves) are
  -- distinct, so no pair of readings coincides.
  Spec.it s "CR 701.12c Soul Conduit exchanges the totals of two players, neither of them its controller" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (conduitId, board) = soulConduitBoard conduit island 4 27 13
        after = S.runPure (conduitAnswer [S.bob, S.carol]) board Engine.runStep
    Spec.assertEqWith s "bob took carol's 13" (S.lifeOf S.bob after) (Just 13)
    Spec.assertEqWith s "carol took bob's 27" (S.lifeOf S.carol after) (Just 27)
    Spec.assertEqWith s "alice, who activated it, is untouched" (S.lifeOf S.alice after) (Just 4)
    Spec.assertEqWith s "carol gained 14" (lifeGains after) [(S.carol, 14)]
    Spec.assertEqWith s "bob lost 14" (lifeLosses after) [(S.bob, 14)]
    -- The ability was really activated, so an exchange that did nothing cannot
    -- pass the assertions above by leaving the board alone.
    Spec.assertEqWith s "and the Conduit paid its own {T}" (fmap Object.tapped (Game.lookupObject conduitId after)) (Just TapState.Tapped)

  -- CR 701.12a: "if the entire exchange can't be completed, no part of the
  -- exchange occurs." One of the two targets leaves the game after the ability is
  -- on the stack, so CR 608.2b drops her (a departed player is no longer in CR
  -- 115's pool) and the ability resolves with one side and no exchange -- rather
  -- than falling back on the controller, which is the reading this discriminates.
  Spec.it s "CR 701.12a an exchange left with one side does nothing at all" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    let (conduitId, board) = soulConduitBoard conduit island 4 27 13
        answer :: Prompt.Prompt r -> r
        answer = conduitAnswer [S.bob, S.carol]
    case Activatable.abilitiesFor conduitId board of
      [ability] -> do
        let activated = S.runPure answer board (Activate.activateAbility S.alice conduitId ability)
            gone = S.runPure answer activated (Departure.leaveGame Departure.Type.Conceded S.carol)
            after = S.runPure answer gone Stack.resolveTop
        Spec.assertEqWith s "bob, the surviving target, keeps his 27" (S.lifeOf S.bob after) (Just 27)
        Spec.assertEqWith s "and alice, who is no side of it, keeps her 4" (S.lifeOf S.alice after) (Just 4)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
      other -> Spec.assertFailure s ("expected exactly one activated ability on the Conduit, got " <> show (length other))

  -- CR 701.12c's deferral to CR 119.7: bob is at 27 and carol at 13, so the
  -- exchange would RAISE carol -- and carol can't gain life, because alice
  -- controls a Giant Cindermaw ("Players can't gain life"). CR 119.7 says the
  -- exchange won't happen, and the WHOLE exchange: bob's loss does not happen
  -- either, which is the reading a per-side gate would get wrong.
  Spec.it s "CR 119.7 an exchange that would raise a player who can't gain life doesn't happen at all" $ do
    conduit <- S.printingOf s registry "Soul Conduit"
    island <- S.printingOf s registry "Island"
    cindermaw <- S.printingOf s registry "Giant Cindermaw"
    let (conduitId, plain) = soulConduitBoard conduit island 4 27 13
        board = snd (S.addPermanent cindermaw S.alice plain)
        after = S.runPure (conduitAnswer [S.bob, S.carol]) board Engine.runStep
    Spec.assertEqWith s "carol, whom it would have raised, keeps her 13" (S.lifeOf S.carol after) (Just 13)
    Spec.assertEqWith s "and bob, whom it would have lowered, keeps his 27" (S.lifeOf S.bob after) (Just 27)
    Spec.assertEqWith s "no gain" (lifeGains after) []
    Spec.assertEqWith s "no loss" (lifeLosses after) []
    -- The ability was really activated, so the assertions above cannot pass
    -- because nothing happened at all.
    Spec.assertEqWith s "and the Conduit paid its own {T}" (fmap Object.tapped (Game.lookupObject conduitId after)) (Just TapState.Tapped)

-- CR 701.12g. The life-for-power and life-for-toughness exchanges are the
-- scenarios under data/scenarios/zone-change/cr-701-12g-*.json and
-- cr-119-7-*.json.
exchangeValuesSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
exchangeValuesSpec s registry = Spec.describe s "ExchangeValues" $ do
  -- Serene Master, a 0/2, blocks alice's Hill Giant, a 3/3, and its trigger
  -- targets the Giant (CR 509.1g's IsBlockedBySource): the powers swap until
  -- end of combat (CR 511.2), so the Master deals 3 and takes 0.
  Spec.it s "CR 701.12g Serene Master swaps powers with the creature it blocks until end of combat" $ do
    master <- S.printingOf s registry "Serene Master"
    giant <- S.printingOf s registry "Hill Giant"
    let (board, giants, masters) = S.combatBoardOf [giant] [master]
        -- Aims AWAY from the Giant whenever anything else is offered, so a filter
        -- letting the Master itself in is caught aiming there.
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> S.preferring (\r -> notElem r (fmap Recipient.ToCreature giants)) sets
          _ -> S.aggressiveAnswer p
        after = S.runCombat answer board
    case (giants, masters) of
      ([giantId], [masterId]) -> do
        Spec.assertBool s (not (Set.member giantId (GameState.battlefield after))) "the Giant died to the Master's borrowed 3 power"
        Spec.assertEqWith s "CR 511.2 the Master survived the Giant's borrowed 0 power and is a 0/2 again after combat" (S.powerToughnessOf masterId after) (Just (0, 2))
      _ -> Spec.assertFailure s "expected one creature a side"

-- CR 119.5: "If an effect sets a player's life total to a specific number, the
-- player gains or loses the necessary amount of life to end up with the new
-- total." So a set is NOT a third kind of life event: it is a gain or a loss,
-- whichever the arithmetic makes it, and everything that watches gaining or
-- losing life sees it. The whole group exists to hold that reading in place.
--
-- Two cards prove it, and it takes two because neither reaches both directions:
--
--   * Magister Sphinx, {4}{W}{U}{B} Artifact Creature -- Sphinx 5/5 with flying,
--     "When this creature enters, target player's life total becomes 10." The
--     literal, so the SAME card is a gain at one seat and a loss at another --
--     and the first two cases below are one board differing in nothing but which
--     seat the trigger names.
--   * Arbiter of Knollridge, {6}{W} Creature -- Giant Wizard 5/5 with vigilance,
--     "When this creature enters, each player's life total becomes the highest
--     life total among all players." The fold, and the several-recipients shape a
--     targeted card cannot reach.
--
-- Three seats at 4, 27 and 13 -- distinct, and one above 10 and two below, so the
-- Sphinx cases tell a gain from a loss. 27 is not the sum (44), the count (3) or
-- the least (4), so Arbiter's one number falsifies every other fold. Only the
-- CR 119.9 case changes a starting total, moving carol to 10 so that the seat the
-- trigger names is already there.
--
-- The watchers are what make the claim about EVENTS rather than about totals.
-- Ajani's Pridemate ("whenever you gain life, put a +1/+1 counter on this
-- creature") is on the board for the gain side and Mindcrank ("whenever an
-- opponent loses life, that player mills that many cards") for the loss side, so
-- every case asserts which of the two fired -- and a set that wrote Player.life
-- directly would leave both silent while every life total still came out right.
setLifeTotalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
setLifeTotalSpec s registry =
  let -- Cast, then settle-and-resolve until the stack runs dry: the spell, then
      -- CR 603.6a's entry trigger, then whatever the life change itself
      -- triggered. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears GameState.events out from under lifeGains and lifeLosses. Six
      -- passes is more than the deepest case needs, and settling or resolving an
      -- empty stack is a no-op.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- Pinned, not searched: the trigger's target slot takes `who` and nothing
      -- else, so a mutation cannot be repaired by an answerer that goes looking
      -- for a legal option. Three players and a count of one, so there is a real
      -- choice to pin.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- The three seats, alice holding the mana and both watchers, and every
      -- library stocked so Mindcrank has something to mill and CR 104.3c never
      -- fires. `lands` is the mana base the spell needs; everything else is
      -- shared by all four cases.
      setBoard lands pridemate mindcrank filler spell aliceLife bobLife carolLife =
        let withLands = List.foldl' (\board (printing, n) -> S.landsFor printing S.alice n board) S.threePlayerGame lands
            (aliceMate, withAliceMate) = S.addPermanent pridemate S.alice withLands
            (bobMate, withBobMate) = S.addPermanent pridemate S.bob withAliceMate
            (_, withCrank) = S.addPermanent mindcrank S.alice withBobMate
            stocked = List.foldl' (\board pid -> stockLibrary filler pid 30 board) withCrank [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice aliceLife (at S.bob bobLife (at S.carol carolLife (GameState.players stocked)))}
            (gs, spellId) = S.handOne spell lifed
         in (aliceMate, bobMate, spellId, gs)
      addPikers piker pid n gs = List.foldl' (\board _ -> snd (S.addPermanent piker pid board)) gs [1 .. n :: Int]
      -- Biorhythm's board, where what differs between the seats is their CREATURE
      -- COUNT rather than their life total. setBoard leaves alice a Pridemate and
      -- a Mindcrank -- an ARTIFACT, so not one of her creatures -- and bob a
      -- Pridemate, so the Pikers below take alice to 4 creatures and bob to 3.
      -- `carolPikers` is the one knob the pair of cases turns.
      --
      -- Life totals 2, 3 and 9 against counts 4, 3 and 0: every seat's answer is
      -- distinct, no seat's answer is any other seat's, and only bob's happens to
      -- equal his own starting total -- which is what the CR 119.9 half of the
      -- pair needs. bob's count and bob's life coinciding is why the other two
      -- seats are there: alice's 2 against 4 and carol's 9 against 0 tell "counts
      -- creatures" from "reads a life total".
      biorhythmBoard carolPikers = do
        forest <- S.printingOf s registry "Forest"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        biorhythm <- S.printingOf s registry "Biorhythm"
        let (aliceMate, bobMate, spellId, gs) = setBoard [(forest, 8)] pridemate mindcrank piker biorhythm 2 3 9
        pure (aliceMate, bobMate, spellId, addPikers piker S.carol carolPikers (addPikers piker S.bob 2 (addPikers piker S.alice 3 gs)))
      sphinxBoard aliceLife bobLife carolLife = do
        plains <- S.printingOf s registry "Plains"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sphinx <- S.printingOf s registry "Magister Sphinx"
        pure (setBoard [(plains, 5), (island, 1), (swamp, 1)] pridemate mindcrank piker sphinx aliceLife bobLife carolLife)
   in Spec.describe s "SetLifeTotal" $ do
        -- The gain direction. alice is BELOW 10, so reaching it is a gain of 6 --
        -- and her Pridemate sees it, which is the whole CR 119.5 claim.
        Spec.it s "CR 119.5 Magister Sphinx sets a total UPWARD, and that is a life gain" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.alice) spellId gs
          Spec.assertEqWith s "alice reached 10" (S.lifeOf S.alice after) (Just 10)
          Spec.assertEqWith s "bob, untargeted, keeps his 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly 6" (lifeGains after) [(S.alice, 6)]
          Spec.assertEqWith s "and as no loss at all" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw the gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: it was not his life" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank stayed silent: nobody lost life" (graveyardSize S.bob after) 0
        -- The control twin, differing in ONE thing: the seat the trigger names.
        -- bob is ABOVE 10, so the identical card is a LOSS of 17 -- Mindcrank
        -- fires and the Pridemate does not, which is the pair that tells a set
        -- from a gain.
        Spec.it s "CR 119.5 the control: the same card set DOWNWARD is a life loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 13
          let after = castAndTrigger (aimedAt S.bob) spellId gs
          Spec.assertEqWith s "bob came down to 10" (S.lifeOf S.bob after) (Just 10)
          Spec.assertEqWith s "alice, untargeted, keeps her 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a loss of exactly 17" (lifeLosses after) [(S.bob, 17)]
          Spec.assertEqWith s "and as no gain at all" (lifeGains after) []
          Spec.assertEqWith s "Mindcrank milled bob for exactly 17" (graveyardSize S.bob after) 17
          Spec.assertEqWith s "alice's Pridemate stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "and so did bob's: losing life is not gaining it" (countersOn bobMate after) (Just 0)
        -- CR 119.9's own last sentence, on the set: carol is ALREADY at 10, so
        -- the necessary amount is 0, no life event occurs, and neither watcher
        -- may fire. The Sphinx really entered, so a spell that did nothing cannot
        -- pass this by leaving the board alone.
        Spec.it s "CR 119.9 setting a total to the number it already holds is neither a gain nor a loss" $ do
          (aliceMate, bobMate, spellId, gs) <- sphinxBoard 4 27 10
          let after = castAndTrigger (aimedAt S.carol) spellId gs
          Spec.assertEqWith s "carol is still at 10" (S.lifeOf S.carol after) (Just 10)
          Spec.assertEqWith s "no gain" (lifeGains after) []
          Spec.assertEqWith s "no loss" (lifeLosses after) []
          Spec.assertEqWith s "no Pridemate counter anywhere" (fmap (\oid -> countersOn oid after) [aliceMate, bobMate]) [Just 0, Just 0]
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
          Spec.assertEqWith s "and the Sphinx really entered, so a spell that never resolved cannot pass this" (Set.size (GameState.battlefield after)) (Set.size (GameState.battlefield gs) + 1)
        -- Arbiter of Knollridge: SEVERAL recipients from one instruction, and a
        -- folded number rather than a literal. 27 is the highest, and it is not
        -- the sum (44), the count (3), the least (4) or any seat's own total, so
        -- one set of three assertions falsifies every other reading.
        --
        -- bob is the seat that is ALREADY highest, and his own Pridemate is the
        -- point of the case: he ends on the number he started on, so CR 119.9
        -- says no life gain event happened to him even though the effect named
        -- him. That is the assertion a raw "write the total to every player"
        -- implementation fails.
        Spec.it s "CR 119.5 Arbiter of Knollridge raises every seat to the HIGHEST total, gaining only where the total moves" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          mindcrank <- S.printingOf s registry "Mindcrank"
          piker <- S.printingOf s registry "Goblin Piker"
          arbiter <- S.printingOf s registry "Arbiter of Knollridge"
          let (aliceMate, bobMate, spellId, gs) = setBoard [(plains, 7)] pridemate mindcrank piker arbiter 4 27 13
              after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice rose from 4 to 27" (S.lifeOf S.alice after) (Just 27)
          Spec.assertEqWith s "bob, already highest, stayed at 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol rose from 13 to 27" (S.lifeOf S.carol after) (Just 27)
          Spec.assertEqWith s "exactly the two seats that moved gained, by exactly their deltas" (lifeGains after) [(S.alice, 23), (S.carol, 14)]
          Spec.assertEqWith s "nobody lost life" (lifeLosses after) []
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: a total set to itself is no gain (CR 119.9)" (countersOn bobMate after) (Just 0)
          Spec.assertEqWith s "and Mindcrank milled nobody" (graveyardSize S.carol after) 0
        -- Biorhythm, {6}{G}{G} Sorcery: "Each player's life total becomes the
        -- number of creatures they control." The number is EACH RECIPIENT'S OWN,
        -- which neither producer above can tell from a single evaluation: the
        -- Sphinx names one seat and Arbiter names one number for the whole table.
        --
        -- Three seats, three different counts -- alice 4, bob 3, carol 0 -- so no
        -- seat's answer can stand in for another's, and a reading that evaluated
        -- once from the CONTROLLER's perspective would hand bob and carol alice's
        -- 4. Each seat carries one half of the rule besides:
        --
        --   * alice gains (2 -> 4), and her Pridemate sees it;
        --   * bob's count is his current total, so CR 119.9 leaves him with no
        --     life event at all and his Pridemate silent;
        --   * carol controls nothing, so her total becomes 0 and CR 104.3b takes
        --     her out of the game.
        Spec.it s "CR 119.5 Biorhythm sets EACH seat to its OWN creature count" $ do
          (aliceMate, bobMate, spellId, gs) <- biorhythmBoard 0
          let after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice, controlling 4 creatures, rose from 2 to 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob, controlling 3, is at 3 -- not alice's 4" (S.lifeOf S.bob after) (Just 3)
          Spec.assertEqWith s "carol, controlling none, fell from 9 to 0" (S.lifeOf S.carol after) (Just 0)
          Spec.assertEqWith s "only alice gained, and by her own delta" (lifeGains after) [(S.alice, 2)]
          Spec.assertEqWith s "only carol lost, and by hers" (lifeLosses after) [(S.carol, 9)]
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's did not: his total was already his count (CR 119.9)" (countersOn bobMate after) (Just 0)
          Spec.assertBool s (notElem S.carol (Game.stillPlaying after)) "CR 104.3b took carol, at 0 life, out of the game"
          Spec.assertEqWith s "and left the other two in it" (filter (`elem` Game.stillPlaying after) [S.alice, S.bob]) [S.alice, S.bob]
        -- The control twin, differing in ONE thing: carol controls a single Piker.
        -- Her answer moves 0 -> 1 while alice's and bob's do not move at all,
        -- which is the pair that shows the count is read per seat; and a total of
        -- 1 is a total CR 104.3b has no quarrel with, so the state-based action
        -- above fired on carol's number rather than on her being named.
        Spec.it s "CR 104.3b the control: one creature is one life, and carol stays in the game" $ do
          (aliceMate, bobMate, spellId, gs) <- biorhythmBoard 1
          let after = castAndTrigger S.identityAnswer spellId gs
          Spec.assertEqWith s "alice is unmoved at 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob is unmoved at 3" (S.lifeOf S.bob after) (Just 3)
          Spec.assertEqWith s "carol, controlling one creature, fell from 9 to 1" (S.lifeOf S.carol after) (Just 1)
          Spec.assertEqWith s "alice still gained 2" (lifeGains after) [(S.alice, 2)]
          Spec.assertEqWith s "carol lost 8 rather than 9" (lifeLosses after) [(S.carol, 8)]
          Spec.assertBool s (elem S.carol (Game.stillPlaying after)) "and stayed in the game"
          Spec.assertEqWith s "Mindcrank milled carol for exactly her loss" (graveyardSize S.carol after) 8
          Spec.assertEqWith s "alice's Pridemate saw her gain" (countersOn aliceMate after) (Just 1)
          Spec.assertEqWith s "bob's stayed silent" (countersOn bobMate after) (Just 0)

-- Beacon of Immortality ({5}{W} Instant -- "Double target player's life total.
-- Shuffle Beacon of Immortality into its owner's library.", Oracle text verified
-- against Scryfall 2026-09-19).
--
-- CR 701.10d states doubling a life total as an arithmetic rather than a game
-- action: the player "gains or loses an amount of life such that their new life
-- total is twice its current value". So it needs no opcode of its own, CR
-- 701.10b's reason in Pawl.PowerToughnessSpec's Unleash Fury group. It is CR
-- 119.5's set -- setLifeTotalSpec above -- over Arithmetic.Times of 2 and the
-- TARGET's own Quantity.LifeTotal, and the gain or loss falls out of that set.
--
-- Three seats at 4, 27 and 13: distinct, and no seat's double (8, 54, 26) is any
-- seat's total or any other seat's double. So the totals falsify a reading that
-- doubles the CONTROLLER's total, one that doubles a literal, and one that
-- doubles every seat -- and the two cases are ONE board differing in nothing but
-- the seat the target names.
--
-- bob's Ajani's Pridemate ("whenever you gain life, put a +1\/+1 counter on this
-- creature") is what makes the claim about the life EVENT rule 701.10d names
-- rather than about the total: a doubling written as a raw Player.life write
-- leaves it silent while 54 still appears.
--
-- Only the UPWARD direction is reachable, and that is a fact about the rules
-- rather than a gap: doubling lowers a total only from one below 0, and CR
-- 104.3b takes a player at 0 or less out of the game the next time a player
-- would receive priority. A total of exactly 0 doubles to itself, which CR 119.9
-- makes no life gain event, and setLifeTotalSpec's CR 119.9 case holds that
-- reading already.
--
-- Written against S.runPure rather than the Board harness, and that is the
-- shuffle clause's doing: the harness has no vocabulary for Prompt.Shuffle,
-- which randomness rather than any player answers (Pawl.Scenario.Prompt.deciderOf), so a card
-- that shuffles cannot be scripted through it.
doubleLifeTotalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
doubleLifeTotalSpec s registry =
  let -- setLifeTotalSpec's driver: cast, then settle-and-resolve until the stack
      -- runs dry, so the spell and the life gain's trigger both resolve.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step g = S.runPure answer (S.runPure answer g Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\g _ -> step g) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- PINNED to a seat rather than searched for, so a mutation cannot be
      -- repaired by an answerer that goes hunting for a legal target; three seats
      -- against a count of one leaves a real choice to pin.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer who) sets
        _ -> S.identityAnswer p
      -- alice holds six Plains for {5}{W} and nothing in her library, so the
      -- shuffled Beacon is the whole of it. Nothing here draws a card, so an
      -- empty library never reaches CR 104.3c.
      beaconBoard = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        beacon <- S.printingOf s registry "Beacon of Immortality"
        let withLands = S.landsFor plains S.alice 6 S.threePlayerGame
            (mate, withMate) = S.addPermanent pridemate S.bob withLands
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = withMate {GameState.players = at S.alice 4 (at S.bob 27 (at S.carol 13 (GameState.players withMate)))}
            (gs, spellId) = S.handOne beacon lifed
        pure (mate, spellId, gs)
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
   in Spec.describe s "Beacon of Immortality" $ do
        Spec.it s "CR 701.10d doubling a life total leaves the target at twice its OWN total, as a life gain" $ do
          (mate, spellId, gs) <- beaconBoard
          let after = castAndTrigger (aimedAt S.bob) spellId gs
          Spec.assertEqWith s "bob, the target, went from 27 to 54" (S.lifeOf S.bob after) (Just 54)
          Spec.assertEqWith s "alice, who cast it, keeps her 4: the total doubled is the TARGET's" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "carol, untargeted, keeps her 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly bob's own 27" (lifeGains after) [(S.bob, 27)]
          Spec.assertEqWith s "and as no loss at all" (lifeLosses after) []
          Spec.assertEqWith s "bob's Pridemate saw the gain, so CR 119.5's gain is what happened" (countersOn mate after) (Just 1)
          Spec.assertEqWith s "and the Beacon shuffled ITSELF into its owner's library" (namesIn Zone.Library S.alice after) [Just (CardName.MkCardName (Text.pack "Beacon of Immortality"))]
          Spec.assertEqWith s "rather than resolving into her graveyard" (namesIn Zone.Graveyard S.alice after) []
        -- The control twin, differing in ONE thing: the seat the target names.
        Spec.it s "CR 701.10d the control: the same card aimed at another seat doubles THAT seat's total" $ do
          (mate, spellId, gs) <- beaconBoard
          let after = castAndTrigger (aimedAt S.alice) spellId gs
          Spec.assertEqWith s "alice, now the target, went from 4 to 8" (S.lifeOf S.alice after) (Just 8)
          Spec.assertEqWith s "bob is unmoved at 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol is unmoved at 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "logged as a gain of exactly alice's own 4" (lifeGains after) [(S.alice, 4)]
          Spec.assertEqWith s "and bob's Pridemate stayed silent: it was not his life" (countersOn mate after) (Just 0)

-- Reverse the Sands, {6}{W}{W} Sorcery: "Redistribute any number of players'
-- life totals. (Each of those players gets one life total back.)" CR 119.7 and
-- CR 119.8 name the action; every seat's new total is CR 119.5's gain or loss,
-- which setLifeTotalSpec above holds in place for one recipient and this group
-- holds for a whole permutation.
--
-- The choice is the resolving controller's (CR 608.2c-d, and the card's own
-- ruling "you choose which player gets which life total when the spell
-- resolves"), so the engine may never pick it. Three seats at 27, 4 and 13 --
-- distinct, so no two assignments produce the same board -- and the two positive
-- cases are ONE board answered two ways, differing in nothing but the
-- permutation. They disagree at every seat, which is what no fixed permutation
-- the engine could have chosen for itself can do.
--
-- Neither permutation is the identity and neither is a rotation: both are
-- transpositions, so each has a seat that is chosen and mapped to ITSELF. That
-- seat is the one that tells "kept its own total" from "was never chosen" --
-- both leave the total alone, and only the watchers agree with CR 119.9 that
-- neither is a life event.
--
-- The watchers are what make the claim about EVENTS rather than totals. A
-- Pridemate under each seat is the gain side ("whenever you gain life") and
-- bob's Mindcrank is the loss side ("whenever an opponent loses life, that
-- player mills that many cards"), so each case pins which of the two fired at
-- which seat. A redistribution written as three raw Player.life writes would
-- leave all four silent while every total still came out right.
redistributeLifeTotalsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
redistributeLifeTotalsSpec s registry =
  let -- setLifeTotalSpec's driver: cast, then settle-and-resolve until the stack
      -- runs dry, so the spell and everything its life changes triggered all
      -- resolve. Deliberately NOT Engine.priorityLoop, which advances the turn
      -- and clears GameState.events out from under lifeGains and lifeLosses.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step board = S.runPure answer (S.runPure answer board Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\board _ -> step board) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- PINNED, not searched: the answer is exactly these pairs whatever the
      -- prompt offers, so a mutation to the engine's own handling cannot be
      -- repaired by an answerer that goes hunting for a legal permutation.
      assigning :: [(PlayerId.PlayerId, PlayerId.PlayerId)] -> Prompt.Prompt r -> r
      assigning pairs p = case p of
        Prompt.ChooseRedistribution {} -> Map.fromList pairs
        _ -> S.identityAnswer p
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      graveyardSize pid gs = Seq.length (Map.findWithDefault Seq.empty pid (GameState.graveyard gs))
      -- alice holds the mana and casts; every seat has a Pridemate, bob has the
      -- Mindcrank, and every library is stocked deep enough that a 23-card mill
      -- never reaches CR 104.3c.
      sandsBoard = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        mindcrank <- S.printingOf s registry "Mindcrank"
        piker <- S.printingOf s registry "Goblin Piker"
        sands <- S.printingOf s registry "Reverse the Sands"
        let withLands = S.landsFor plains S.alice 8 S.threePlayerGame
            (aliceMate, g1) = S.addPermanent pridemate S.alice withLands
            (bobMate, g2) = S.addPermanent pridemate S.bob g1
            (carolMate, g3) = S.addPermanent pridemate S.carol g2
            (_, g4) = S.addPermanent mindcrank S.bob g3
            stocked = List.foldl' (\board pid -> stockLibrary piker pid 40 board) g4 [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players stocked)))}
            (gs, spellId) = S.handOne sands lifed
        pure (aliceMate, bobMate, carolMate, spellId, gs)
      -- The board every refused answer is checked against: nothing moved, and
      -- nothing was logged. Shared so the three #222 cases differ in exactly one
      -- thing -- the answer.
      assertUntouched after = do
        Spec.assertEqWith s "alice keeps her 27" (S.lifeOf S.alice after) (Just 27)
        Spec.assertEqWith s "bob keeps his 4" (S.lifeOf S.bob after) (Just 4)
        Spec.assertEqWith s "carol keeps her 13" (S.lifeOf S.carol after) (Just 13)
        Spec.assertEqWith s "no gain" (lifeGains after) []
        Spec.assertEqWith s "no loss" (lifeLosses after) []
        -- Load-bearing: the spent sorcery and nothing else is in alice's
        -- graveyard, so the spell really RESOLVED and no Mindcrank mill followed.
        -- Without it every case below would pass just as well on a spell that
        -- fizzled.
        Spec.assertEqWith s "the spell resolved, and milled nobody doing it" (graveyardSize S.alice after) 1
   in Spec.describe s "RedistributeLifeTotals" $ do
        -- alice hands her 27 to carol and takes carol's 13; bob is CHOSEN and
        -- mapped to himself. One seat gains, one loses, one is chosen and stays
        -- put -- and 13, 4 and 27 are three different numbers, so this one set of
        -- assertions falsifies every other permutation of the three.
        Spec.it s "Reverse the Sands whole card: the controller's permutation is what happens, seat by seat" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.carol), (S.bob, S.bob), (S.carol, S.alice)]) spellId gs
          Spec.assertEqWith s "alice took carol's 13" (S.lifeOf S.alice after) (Just 13)
          Spec.assertEqWith s "bob, mapped to himself, is still on 4" (S.lifeOf S.bob after) (Just 4)
          Spec.assertEqWith s "carol took alice's 27" (S.lifeOf S.carol after) (Just 27)
          -- CR 119.5, per seat: the necessary amount, and its own sign.
          Spec.assertEqWith s "carol gained exactly the difference" (lifeGains after) [(S.carol, 14)]
          Spec.assertEqWith s "alice lost exactly the difference" (lifeLosses after) [(S.alice, 14)]
          Spec.assertEqWith s "carol's Pridemate saw her gain" (countersOn carolMate after) (Just 1)
          Spec.assertEqWith s "alice's did not: losing life is not gaining it" (countersOn aliceMate after) (Just 0)
          -- CR 119.9 on the fixed point: bob was chosen, so a redistribution
          -- that handed out totals blindly would still have "given" him one.
          -- Taking his own is a delta of 0 and therefore no life event at all.
          Spec.assertEqWith s "bob's Pridemate stayed silent: his own total back is no gain" (countersOn bobMate after) (Just 0)
          -- 14 milled, plus the spent sorcery itself (CR 608.2n), which is alice's
          -- card and lands in her graveyard -- so this also witnesses that the
          -- spell really resolved rather than fizzling quietly.
          Spec.assertEqWith s "bob's Mindcrank milled alice for exactly what she lost" (graveyardSize S.alice after) 15
          Spec.assertEqWith s "and milled carol for nothing: she gained" (graveyardSize S.carol after) 0
        -- The control twin: the SAME board, differing in nothing but the
        -- permutation the controller names. Every seat lands somewhere else than
        -- it did above, so no permutation the engine picked for itself can
        -- satisfy both cases -- which is the whole second-invariant claim.
        Spec.it s "the same board answered differently redistributes differently at every seat" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.bob), (S.bob, S.alice), (S.carol, S.carol)]) spellId gs
          Spec.assertEqWith s "alice took bob's 4" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob took alice's 27" (S.lifeOf S.bob after) (Just 27)
          Spec.assertEqWith s "carol, mapped to herself, is still on 13" (S.lifeOf S.carol after) (Just 13)
          Spec.assertEqWith s "bob gained exactly the difference" (lifeGains after) [(S.bob, 23)]
          Spec.assertEqWith s "alice lost exactly the difference" (lifeLosses after) [(S.alice, 23)]
          Spec.assertEqWith s "bob's Pridemate saw his gain" (countersOn bobMate after) (Just 1)
          Spec.assertEqWith s "alice's stayed silent" (countersOn aliceMate after) (Just 0)
          Spec.assertEqWith s "carol's stayed silent: her own total back is no gain" (countersOn carolMate after) (Just 0)
          -- 23 milled plus the spent sorcery, as in the case above.
          Spec.assertEqWith s "bob's Mindcrank milled alice for exactly what she lost" (graveyardSize S.alice after) 24
        -- A ROTATION, and the reason the two transpositions above are not enough
        -- on their own: a transposition is its own inverse, so reading the answer
        -- backwards -- giving each named player's total AWAY instead of handing it
        -- TO them -- lands on the very same board. This assignment's inverse is
        -- the other rotation, which lands on a different total at all three
        -- seats, so this is the case that pins the direction of the map.
        Spec.it s "a rotation moves every seat, and in the direction the answer names" $ do
          (aliceMate, bobMate, carolMate, spellId, gs) <- sandsBoard
          let after = castAndTrigger (assigning [(S.alice, S.bob), (S.bob, S.carol), (S.carol, S.alice)]) spellId gs
          Spec.assertEqWith s "alice took bob's 4, not carol's 13" (S.lifeOf S.alice after) (Just 4)
          Spec.assertEqWith s "bob took carol's 13, not alice's 27" (S.lifeOf S.bob after) (Just 13)
          Spec.assertEqWith s "carol took alice's 27, not bob's 4" (S.lifeOf S.carol after) (Just 27)
          Spec.assertEqWith s "the two seats that rose gained their own differences" (lifeGains after) [(S.bob, 9), (S.carol, 14)]
          Spec.assertEqWith s "and the one that fell lost hers" (lifeLosses after) [(S.alice, 23)]
          Spec.assertEqWith s "bob's Pridemate saw his gain" (countersOn bobMate after) (Just 1)
          Spec.assertEqWith s "carol's saw hers" (countersOn carolMate after) (Just 1)
          Spec.assertEqWith s "alice's stayed silent" (countersOn aliceMate after) (Just 0)
          -- The rotation is also what proves the totals are read from ONE
          -- snapshot: an implementation that set each seat in turn against the
          -- live board would hand bob the 4 alice had just taken.
          Spec.assertEqWith s "23 milled plus the spent sorcery" (graveyardSize S.alice after) 24
        -- The ruling's option (a), "leave the life totals as they are": "any
        -- number of players" includes none, so the empty answer is legal and
        -- quiet rather than refused.
        Spec.it s "redistributing among nobody is a legal answer and moves nothing" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning []) spellId gs)
        -- #222, an outsider: dave is not in this game. This answer IS a
        -- permutation -- its keys and its values are the same two seats -- so
        -- only the candidate check refuses it, which is what makes this case
        -- discriminating rather than a second copy of the one above.
        Spec.it s "#222 an answer naming a player who is not in the game is refused" $ do
          (_, _, _, spellId, gs) <- sandsBoard
          assertUntouched (castAndTrigger (assigning [(S.alice, S.dave), (S.dave, S.alice)]) spellId gs)
        -- CR 102.1: the offer is the players IN the game, not the keys of
        -- GameState.players, which keep a departed seat's row. Driven through
        -- Resolve.applyEffect rather than a cast, the narrowest path that raises
        -- the prompt at all.
        Spec.it s "CR 102.1 every player in the game is offered, beside the total they hold, and a departed seat is not" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, g0) = S.addPermanent piker S.alice S.threePlayerGame
              at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
              gs = g0 {GameState.players = at S.alice 27 (at S.bob 4 (at S.carol 13 (GameState.players g0)))}
              recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Integer)] r
              recording p = case p of
                Prompt.ChooseRedistribution _ _ offered -> do
                  State.put offered
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              offerOf g = List.sort (State.execState (Engine.runGame recording g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) [])
              gone = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.carol)
          Spec.assertEqWith s "all three seats, each beside its own total" (offerOf gs) [(S.alice, 27), (S.bob, 4), (S.carol, 13)]
          Spec.assertEqWith s "carol conceded, so she is no longer a candidate" (offerOf gone) [(S.alice, 27), (S.bob, 4)]
        -- Where the rules leave nothing to ask, do not ask. One candidate admits
        -- only the identity and no candidate not even that, so both are the same
        -- assignment however they are answered; two candidates is a real choice.
        Spec.it s "one remaining player leaves only the identity, so no prompt is raised" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          let (src, gs) = S.addPermanent piker S.alice S.threePlayerGame
              countingAnswer :: Prompt.Prompt r -> State.State Int r
              countingAnswer p = case p of
                Prompt.ChooseRedistribution {} -> do
                  State.modify (+ 1)
                  pure (S.identityAnswer p)
                _ -> pure (S.identityAnswer p)
              asks g = State.execState (Engine.runGame countingAnswer g (Resolve.applyEffect src src S.alice Map.empty Map.empty Effect.RedistributeLifeTotals)) 0
              leaves pid g = S.runPure S.identityAnswer g (Departure.leaveGame Departure.Type.Conceded pid)
              two = leaves S.carol gs
              one = leaves S.bob two
          Spec.assertEqWith s "three seats: a real decision" (asks gs) 1
          Spec.assertEqWith s "two seats: still a real decision, the swap being legal" (asks two) 1
          Spec.assertEqWith s "one seat: only the identity, so nothing to ask" (asks one) 0

-- One with the Machine, the card that proves Aggregation.Greatest (#254):
-- "Draw cards equal to the greatest mana value among artifacts you control."
-- Nothing but the fold is new -- the effect is the existing Draw, the scope and
-- the filter were both already expressible, and the per-member quantity is the
-- existing Quantity.ManaValue (CR 202.3), the same read Karn, Legacy Reforged
-- wants.
--
-- Alice's board is Bonesplitter ({1}), Serum Powder ({3}) and Mindslaver ({6}),
-- chosen so that greatest (6), count (3), sum (10) and least (1) are four
-- DIFFERENT numbers: one hand-size assertion falsifies every other fold.
greatestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greatestSpec s registry = Spec.describe s "Greatest" $ do
  -- CR 202.3b, second sentence: "If a permanent or spell is a copy of the back
  -- face of a nonmodal double-faced object (even if the card representing that
  -- copy is itself a double-faced card), the mana value of the copy is 0."
  --
  -- The NUMBER is the whole of what this adds to
  -- cr-707-2-a-clone-copying-darksteel-myr-counts-as-mana-value.json.
  -- CR 707.2 already makes the Clone read the copied object's mana value rather
  -- than its own printed {3}{U}, and CR 712.8e already makes the copied object
  -- -- a transformed Thraben Gargoyle // Stonewing Antagonizer -- read its FRONT
  -- face's {1}. Without this rule the copy inherits that 1; with it the source
  -- and the copy report DIFFERENT mana values off the same copiable snapshot,
  -- and CR 202.3b is the only thing separating them.
  --
  -- The Gargoyle is BOB's, for the Darksteel Myr case's reason: it leaves the
  -- Clone alone among "artifacts YOU control", so the maximum folds over one
  -- member and the hand size is that member's mana value and nothing else. Both
  -- faces are Artifact Creature, so the copy is in the fold whichever face was
  -- copied and the card type is not what changes.
  --
  -- 0 is a dangerous number to assert: an empty fold, a copy that never
  -- happened and a Clone left as its printed 0/0 self all draw nothing too. So
  -- the copy is IDENTIFIED before its mana value is read -- its name, card types
  -- and 4/2 body say it really is Stonewing Antagonizer under alice's control --
  -- and bob's source is asserted at 1 in the same breath, which is what shows
  -- the 0 is the copy's own answer rather than a mana value reader that broke.
  Spec.it s "CR 202.3b a Clone copying a TRANSFORMED Stonewing Antagonizer has mana value 0, not the front face's 1" $ do
    island <- S.printingOf s registry "Island"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    case cloneOfGargoyle True island gargoyle clone piker oneWithTheMachine of
      (_, [], _) -> Spec.assertFailure s "no copy on alice's battlefield"
      (source, copy : _, after) -> do
        Spec.assertEqWith s "the copy is Stonewing Antagonizer" (Projection.namesOf copy after) (Set.singleton antagonizerName)
        Spec.assertEqWith s "an artifact creature alice controls" (Projection.cardTypesOf copy after, Projection.controllerOf copy after) (Set.fromList [CardType.Artifact, CardType.Creature], Just S.alice)
        Spec.assertEqWith s "with the back face's 4/2 body" (S.powerToughnessOf copy after) (Just (4, 2))
        Spec.assertEqWith s "CR 712.8e: bob's transformed permanent still reads its front face's 1" (Filter.manaValue (Projection.viewOfObject source after)) (Just 1)
        Spec.assertEqWith s "CR 202.3b: the copy of that back face reads 0" (Filter.manaValue (Projection.viewOfObject copy after)) (Just 0)
        Spec.assertEqWith s "so alice drew nothing" (S.handSize S.alice after) 0
        Spec.assertEqWith s "and her library is untouched" (length (Game.zoneMembers Zone.Library S.alice after)) 10
  -- The control, and the reason the case above is not passed by an engine that
  -- answers 0 for every copy: the SAME fixture with the Gargoyle left front-face
  -- up. CR 202.3b's second sentence is about a copy of the BACK face, so a copy
  -- of the front face keeps CR 707.2's ordinary answer -- the copied object's
  -- {1} -- and alice draws one card rather than none.
  Spec.it s "CR 707.2 a Clone copying the UNTRANSFORMED Thraben Gargoyle keeps that face's mana value" $ do
    island <- S.printingOf s registry "Island"
    gargoyle <- S.printingOf s registry "Thraben Gargoyle"
    clone <- S.printingOf s registry "Clone"
    piker <- S.printingOf s registry "Goblin Piker"
    oneWithTheMachine <- S.printingOf s registry "One with the Machine"
    case cloneOfGargoyle False island gargoyle clone piker oneWithTheMachine of
      (_, [], _) -> Spec.assertFailure s "no copy on alice's battlefield"
      (source, copy : _, after) -> do
        Spec.assertEqWith s "the copy is Thraben Gargoyle" (Projection.namesOf copy after) (Set.singleton gargoyleName)
        Spec.assertEqWith s "an artifact creature alice controls" (Projection.cardTypesOf copy after, Projection.controllerOf copy after) (Set.fromList [CardType.Artifact, CardType.Creature], Just S.alice)
        Spec.assertEqWith s "with the front face's 2/2 body" (S.powerToughnessOf copy after) (Just (2, 2))
        Spec.assertEqWith s "bob's permanent reads 1" (Filter.manaValue (Projection.viewOfObject source after)) (Just 1)
        Spec.assertEqWith s "and so does the copy of it" (Filter.manaValue (Projection.viewOfObject copy after)) (Just 1)
        Spec.assertEqWith s "so alice drew one" (S.handSize S.alice after) 1

-- The two names Thraben Gargoyle // Stonewing Antagonizer prints, for the CR
-- 202.3b pair above.
gargoyleName, antagonizerName :: CardName.CardName
gargoyleName = CardName.MkCardName (Text.pack "Thraben Gargoyle")
antagonizerName = CardName.MkCardName (Text.pack "Stonewing Antagonizer")

-- The board the two CR 202.3b cases share, differing only in `turnOver`: bob's
-- Thraben Gargoyle, turned over or not; alice's Clone copying it; then alice
-- casting One with the Machine and its draw resolving. Answers with bob's
-- permanent, the creatures alice controls, and the state after the draw.
--
-- The printings come in the order a case fetches them: Island, Thraben
-- Gargoyle, Clone, Goblin Piker, One with the Machine.
cloneOfGargoyle ::
  Bool ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
cloneOfGargoyle turnOver island gargoyle clone piker oneWithTheMachine =
  let base = S.landsInPlay island 4
      (source, withGargoyle) = S.addPermanent gargoyle S.bob base
      turned = if turnOver then transformEveryCreature withGargoyle else withGargoyle
      (_, staged) = S.spellOnStack clone S.alice turned
      -- CR 614.12a: the copy choice happens inside the Clone's own entry, and
      -- bob's Gargoyle is the only creature on the battlefield to offer.
      entered = snd (Engine.runGamePure copyTheOnlyTarget staged (Stack.resolveTop >> Engine.settleForPriority))
      -- CR 400.7 minted a new id when the Clone left the stack, so the copy is
      -- found by what it IS: the only creature alice controls, her other four
      -- permanents being Islands.
      copies = filter (\oid -> Projection.isCreatureOf oid entered) (Game.zoneMembers Zone.Battlefield S.alice entered)
      -- CR 104.3c: ten cards is far more than the one this draws at most, so
      -- alice cannot deck herself before the assertion.
      withLib = stockLibrary piker S.alice 10 entered
      (gs, spellId) = S.handOne oneWithTheMachine withLib
      cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
      after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
   in (source, copies, after)

-- "Transform each creature" applied straight, which is Moonmist's shape (CR
-- 701.27a) with a wider filter. Stonewing Antagonizer prints no way back and the
-- Gargoyle here is BOB's, so paying its own {6} would need a board of bob's
-- lands that says nothing about CR 202.3b.
transformEveryCreature :: GameState.GameState -> GameState.GameState
transformEveryCreature gs =
  S.runPure
    S.identityAnswer
    gs
    (Resolve.applyEffect S.noSource S.noSource S.alice Map.empty Map.empty (Effect.Transform (ObjectRef.EachMatching (Filter.Type.HasCardType CardType.Creature))))

-- Answers the CR 614.12a copy choice with the first legal target and delegates
-- everything else, for a fixture where exactly one creature is legal.
copyTheOnlyTarget :: Prompt.Prompt r -> r
copyTheOnlyTarget p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> Maybe.listToMaybe legal
  _ -> S.identityAnswer p

-- Randomness under CR 400.7's zone change: Elkin Lair {3}{R} World
-- Enchantment (Homelands; Oracle text checked against api.scryfall.com,
-- 2026-09-18) -- "At the beginning of each player's upkeep, that player exiles a
-- card at random from their hand. The player may play that card this turn. At
-- the beginning of the next end step, if the player hasn't played the card, they
-- put it into their graveyard."
--
-- The pool's only card moving a card at random OUT of a hand (Scryfall
-- o:/(exiles?|puts?|returns?|shuffles?) .{0,20}cards? at random from (your|their|his or her|that player.s|target player.s|each player.s) hand/,
-- 2026-09-11, one hit), so Effect.MoveToZone's gather asking randomCardsInHand
-- is what this card is here to exercise.
--
-- TWO SEATS, and ALICE controls the enchantment while BOB takes the upkeep:
-- "that player" and "the resolving controller" are the same seat on a one-seat
-- board, and alice holds a card of her own so a gather reading the controller's
-- hand exiles the wrong one rather than nothing.
--
-- THREE distinct printings in bob's hand, and the PAIR of legs is the proof: "at
-- random" is not a property of one outcome, since the engine does not roll, so
-- the two legs differ in the index the interpreter answered with and in nothing
-- else. Exiling the head of the hand unasked passes the second and fails the
-- first.
elkinLairSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
elkinLairSpec s registry =
  let -- S.addHandCard puts its card at the FRONT of the hand, so the list is
      -- stocked in reverse and index 0 is the first card named.
      board lair mine theirs =
        let (_, withLair) = S.addPermanent lair S.alice (Setup.emptyGame S.bothPlayers)
            withAlices = List.foldl' (\g q -> snd (S.addHandCard q S.alice g)) withLair (reverse mine)
         in List.foldl' (\g q -> snd (S.addHandCard q S.bob g)) withAlices (reverse theirs)
      -- BOB's upkeep, stamped and recorded -- the half TurnScope.EachTurn buys,
      -- since under ControllersTurn the trigger would not fire here at all.
      runBobsUpkeep :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      runBobsUpkeep answer gs = S.runPure answer (bobsStepBegins (Phase.Beginning BeginningStep.Upkeep) answer gs) Engine.priorityLoop
      -- A step of bob's begun and its triggers put on the stack, none resolved.
      bobsStepBegins :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      bobsStepBegins step answer gs =
        let began =
              Event.recordEvent
                (GameEvent.StepBegan (StepBegan.MkStepBegan step S.bob))
                (gs {GameState.phase = step, GameState.activePlayer = S.bob})
         in S.runPure answer began Engine.settleForPriority
      endStep = Phase.Ending EndingStep.EndStep
      -- Pinned by INDEX into the offer rather than read off the prompt's fields:
      -- an answerer that hunted for "a legal card" would go on answering legally
      -- after a mutation broke which card the engine honours.
      rolling :: Int -> Prompt.Prompt r -> r
      rolling i p = case p of
        Prompt.RandomObject offered -> case List.drop (min i (length (NonEmpty.toList offered) - 1)) (NonEmpty.toList offered) of
          h : _ -> h
          [] -> NonEmpty.head offered
        _ -> S.identityAnswer p
      named n = Just (CardName.MkCardName (Text.pack n))
      bobsCards = ["Goblin Piker", "Bog Wraith", "Bird Maiden"]
   in Spec.describe s "ElkinLair" $ do
        -- The proving case.
        Spec.it s "CR 400.7 the card exiled is the one randomness named, not the first in hand" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          bolt <- S.printingOf s registry "Lightning Bolt"
          ps <- traverse (S.printingOf s registry) bobsCards
          let after = runBobsUpkeep (rolling 2) (board lair [bolt] ps)
          Spec.assertEqWith s "the LAST card of bob's hand is in exile" (namesIn Zone.Exile S.bob after) [named "Bird Maiden"]
          Spec.assertEqWith s "and the two randomness passed over stay in hand" (namesIn Zone.Hand S.bob after) [named "Goblin Piker", named "Bog Wraith"]
          Spec.assertEqWith s "CR 603.2's \"that player\" is bob: alice's own card is untouched" (namesIn Zone.Hand S.alice after) [named "Lightning Bolt"]
          Spec.assertEqWith s "and the trigger resolved" (length (GameState.stack after)) 0
        -- The other half of the pair: the same board, the same everything, one
        -- different answer.
        Spec.it s "CR 400.7 the same board with a different roll exiles a different card" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          bolt <- S.printingOf s registry "Lightning Bolt"
          ps <- traverse (S.printingOf s registry) bobsCards
          let after = runBobsUpkeep (rolling 0) (board lair [bolt] ps)
          Spec.assertEqWith s "the FIRST card this time" (namesIn Zone.Exile S.bob after) [named "Goblin Piker"]
          Spec.assertEqWith s "and the other two stay in hand" (namesIn Zone.Hand S.bob after) [named "Bog Wraith", named "Bird Maiden"]
        -- The THIRD clause: "at the beginning of the next end step, if the player
        -- hasn't played the card, they put it into their graveyard". Three boards
        -- differing only in what bob did with the card between: nothing, cast it
        -- (CR 601.2a), or played it as a land (CR 305.1). Two Mountains each, so
        -- the unplayed Goblin Piker was affordable and stayed unplayed.
        Spec.it s "CR 603.4 an unplayed card goes to the graveyard at the next end step" $ do
          lair <- S.printingOf s registry "Elkin Lair"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let withMana g = snd (S.addPermanent mountain S.bob (snd (S.addPermanent mountain S.bob g)))
              upkept = runBobsUpkeep (rolling 0) (withMana (board lair [] [piker]))
              after = S.runPure S.identityAnswer (bobsStepBegins endStep S.identityAnswer upkept) Engine.priorityLoop
          Spec.assertEqWith s "the Goblin Piker bob never played is in his graveyard" (namesIn Zone.Graveyard S.bob after) [named "Goblin Piker"]
          Spec.assertEqWith s "and no longer in exile" (namesIn Zone.Exile S.bob upkept, namesIn Zone.Exile S.bob after) ([named "Goblin Piker"], [])

-- Elkin Lair's third clause with "you" as the player and "cast" as the verb
-- (Quantity.PlayedBy). Oracle text checked against api.scryfall.com,
-- 2026-09-26:
--
-- Psychic Theft {1}{U} Sorcery -- "Target player reveals their hand. You choose
-- an instant or sorcery card from it and exile that card. You may cast that card
-- for as long as it remains exiled. At the beginning of the next end step, if
-- you haven't cast the card, return it to its owner's hand."
--
-- Planeswalker's Mischief {2}{U} Enchantment -- "{3}{U}: Target opponent
-- reveals a card at random from their hand. If it's an instant or sorcery card,
-- exile it. You may cast it without paying its mana cost for as long as it
-- remains exiled. At the beginning of the next end step, if you haven't cast it,
-- return it to its owner's hand. Activate only as a sorcery."
--
-- Bob's Goblin Piker is there so Psychic Theft's choice has a card to pass over.
castTheCardSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
castTheCardSpec s registry =
  let -- Alice's end step begun, its triggers put on the stack, none resolved.
      theftBoardWith theft alicesLands bolt piker =
        let lands = List.foldl' (\g p -> snd (S.addPermanent p S.alice g)) (Setup.emptyGame S.bothPlayers) alicesLands
            withBobs = List.foldl' (\g q -> snd (S.addHandCard q S.bob g)) lands [piker, bolt]
            (theftId, gs) = S.addHandCard theft S.alice withBobs
            cast = S.runPure atBobAnswer gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice} (S.cast S.alice theftId)
         in S.runPure atBobAnswer cast Stack.resolveTop
   in Spec.describe s "CastTheCard" $ do
        -- CR 724.1e: Time Stop skips alice's end step, so the delayed trigger
        -- waits for bob's. "Haven't cast" has no "this turn", so the play alice
        -- made on the turn before still answers it.
        Spec.it s "CR 724.1e Psychic Theft's card cast before a Time Stop triggers nothing at the next turn's end step" $ do
          ps <- traverse (S.printingOf s registry) ["Psychic Theft", "Island", "Mountain", "Lightning Bolt", "Goblin Piker", "Time Stop"]
          case ps of
            [theft, island, mountain, bolt, piker, timeStop] -> do
              let exiled = theftBoardWith theft (replicate 5 island <> replicate 4 mountain) bolt piker
              case Game.zoneMembers Zone.Exile S.bob exiled of
                [boltId] -> do
                  let resolved = S.runPure atBobAnswer (S.runPure atBobAnswer exiled (S.cast S.alice boltId)) Stack.resolveTop
                      (stopId, holding) = S.addHandCard timeStop S.alice resolved
                      stopped = S.runPure atBobAnswer holding (S.cast S.alice stopId)
                      bobsTurn = fst (TurnSpec.runTurn atBobAnswer stopped)
                      step = Phase.Ending EndingStep.EndStep
                      ending = S.runPure atBobAnswer (Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan step S.bob)) bobsTurn {GameState.phase = step, GameState.activePlayer = S.bob}) Engine.settleForPriority
                  Spec.assertEqWith s "nothing triggered at bob's end step" (length (GameState.stack ending)) 0
                  Spec.assertEqWith s "the trigger was still waiting when bob's turn began" (length (GameState.delayedTriggers bobsTurn), GameState.activePlayer bobsTurn) (1, S.bob)
                _ -> Spec.assertFailure s "Psychic Theft should exile exactly one card"
            _ -> Spec.assertFailure s "six printings"

-- "Until the beginning of your next upkeep" (Duration.UntilYourNextUpkeep).
-- Oracle text checked against api.scryfall.com, 2026-09-26:
--
-- Grinning Totem {4} Artifact -- "{2}, {T}, Sacrifice this artifact: Search
-- target opponent's library for a card and exile it. Then that player
-- shuffles. Until the beginning of your next upkeep, you may play that card. At
-- the beginning of your next upkeep, if you haven't played it, put it into its
-- owner's graveyard."
--
-- Elkin Bottle {3} Artifact -- "{3}, {T}: Exile the top card of your library.
-- Until the beginning of your next upkeep, you may play that card."
--
-- The answerer casts the exiled Lightning Bolt the moment alice is offered it,
-- so what the permission allows is read off bob's life.
nextUpkeepSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
nextUpkeepSpec s registry =
  let -- alice in her main phase with priority, the rest of her turn scheduled.
      mainPhase gs = gs {GameState.phase = Phase.PrecombatMain, GameState.remaining = S.phasesAfter Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
      stock printing pid g = List.foldl' (\h _ -> snd (S.addLibraryCard printing pid h)) g [1 :: Int .. 4]
      -- Whole steps, stopping BEFORE the first state `stop` holds of.
      runUntil :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState -> Bool) -> GameState.GameState -> GameState.GameState
      runUntil answer stop =
        let go n g =
              if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || stop g
                then g
                else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
         in go 64
      -- The rest of alice's turn and all of bob's, passing throughout.
      untilAlicesNextTurn g0 = runUntil S.identityAnswer (\g -> GameState.activePlayer g == S.alice && GameState.turnNumber g > GameState.turnNumber g0) g0
      -- Then alice's turn up to and including the step `target`.
      through :: (forall r. Prompt.Prompt r -> r) -> Phase.Phase -> GameState.GameState -> GameState.GameState
      through answer target g = snd (Engine.runGamePure answer (runUntil answer (\h -> GameState.phase h == target) g) Engine.runStep)
      -- Casts `boltId` at bob whenever alice is offered it, and passes otherwise.
      castingBolt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      castingBolt boltId p = case p of
        Prompt.ChooseAction _ _ actions -> Maybe.fromMaybe A.Pass (List.find (\a -> case a of A.Cast oid _ _ -> oid == boltId; _ -> False) actions)
        _ -> atBobAnswer p
      activated source board = case Activatable.abilitiesFor source board of
        [ability] -> Just (S.runPure atBobAnswer (S.runPure atBobAnswer board (Activate.activateAbility S.alice source ability)) Stack.resolveTop)
        _ -> Nothing
   in Spec.describe s "UntilYourNextUpkeep" $ do
        -- CR 500.11: Eon Hub skips every upkeep, so the duration outlasts the
        -- start of alice's next turn and she may still cast the card then.
        Spec.it s "CR 611.2a Elkin Bottle's card stays castable into alice's next turn when Eon Hub skips her upkeep" $ do
          ps <- traverse (S.printingOf s registry) ["Elkin Bottle", "Eon Hub", "Mountain", "Island", "Lightning Bolt"]
          case ps of
            [bottle, hub, mountain, island, bolt] -> do
              let (bottleId, withBottle) = S.addPermanent bottle S.alice (stock island S.bob (stock island S.alice (Setup.emptyGame S.bothPlayers)))
                  withHub = snd (S.addPermanent hub S.bob withBottle)
                  withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) withHub [1 :: Int .. 3]
                  (_, board) = S.addLibraryCard bolt S.alice withLands
              case activated bottleId (mainPhase board) of
                Just exiled -> case Game.zoneMembers Zone.Exile S.alice exiled of
                  [boltId] -> do
                    let drawn = through (castingBolt boltId) (Phase.Beginning BeginningStep.DrawStep) (untilAlicesNextTurn exiled)
                    Spec.assertEqWith s "alice cast the Lightning Bolt on her next turn" (S.lifeOf S.bob drawn) (Just 17)
                  _ -> Spec.assertFailure s "Elkin Bottle should exile exactly one card"
                Nothing -> Spec.assertFailure s "one activated ability"
            _ -> Spec.assertFailure s "five printings"

-- CR 701.9b's two exceptions to the discarding player's own choice, both over
-- Discard.These. bob holds four distinct cards, so every assertion reads
-- identity, and the expectations are read off his hand's own order rather than
-- assumed. Each answerer pins its pick to the LAST card offered -- the one
-- answer a fallback to the head of the offer, or CR 701.9b's default choice
-- answered with the first cards, would not produce.
--
-- Hymn to Tourach {B}{B} Sorcery -- "Target player discards two cards at
-- random." Duress {B} Sorcery -- "Target opponent reveals their hand. You choose
-- a noncreature, nonland card from it. That player discards that card." (both
-- checked against api.scryfall.com, 2026-09-11).
discardExceptionsSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
discardExceptionsSpec s registry = Spec.describe s "CR 701.9b discard exceptions" $ do
  Spec.it s "CR 701.9b Duress discards the card its caster chose from the revealed hand" $ do
    duress <- S.printingOf s registry "Duress"
    swamp <- S.printingOf s registry "Swamp"
    cards <- traverse (S.printingOf s registry) ["Goblin Piker", "Lightning Bolt", "Forest", "Murder"]
    let (withSpell, spell) = S.handOne duress (S.landsInPlay swamp 1)
        ready = List.foldl' (\g p -> snd (S.addHandCard p S.bob g)) withSpell cards
        hand = Game.zoneMembers Zone.Hand S.bob ready
        nameOf oid = fmap S.nameOf (Game.cardOf oid ready)
        spells = [oid | oid <- hand, nameOf oid `elem` fmap (Just . CardName.MkCardName . Text.pack) ["Lightning Bolt", "Murder"]]
        -- Records who was asked over which cards, and answers the last one.
        answer :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, [ObjectId.ObjectId])] r
        answer p = case p of
          Prompt.ChooseCardFromAmong _ asked _ offered -> do
            State.modify' (<> [(asked, NonEmpty.toList offered)])
            pure (NonEmpty.last offered)
          _ -> pure (atBobAnswer p)
        (after, asks) = State.runState (fmap snd (Engine.runGame answer ready (S.cast S.alice spell *> Stack.resolveTop))) []
    Spec.assertEqWith s "the card alice chose is the one bob discarded" (namesIn Zone.Graveyard S.bob after) (fmap nameOf (take 1 (reverse spells)))
    Spec.assertEqWith s "CR 608.2d alice was asked, over the noncreature, nonland cards alone" asks [(S.alice, spells)]
    Spec.assertEqWith
      s
      "CR 701.20a bob revealed his whole hand"
      (List.sort (S.revealsOf after))
      (List.sort (fmap (\p -> (S.bob, Set.singleton (S.printingName p))) cards))

-- CR 701.9a's "discarded this way" over Discard.These, the random discard's
-- look-back. Aether Rift {1}{R}{G} Enchantment -- "At the beginning of your
-- upkeep, discard a card at random. If you discard a creature card this way,
-- return it from your graveyard to the battlefield unless any player pays 5
-- life." Cragganwick Cremator {2}{R}{R} Creature -- Giant Shaman 5/4 -- "When
-- this creature enters, discard a card at random. If you discard a creature card
-- this way, this creature deals damage equal to that card's power to target
-- player or planeswalker." (both checked against api.scryfall.com, 2026-09-28).
--
-- THREE seats, so "any player" is not "an opponent". alice holds exactly one
-- card, so randomness has one answer and the card discarded is the one the
-- board names. A Hill Giant already in her graveyard is a creature card "it"
-- must not reach, so a return over the whole graveyard is caught.
discardedThisWaySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
discardedThisWaySpec s registry = Spec.describe s "CR 701.9a discarded this way" $ do
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- Aether Rift on alice's battlefield, `held` her one card, a Hill Giant in
      -- her graveyard, `redirect` (if any) on bob's battlefield, and alice's
      -- upkeep begun with the trigger on the stack.
      riftBoard redirect held = do
        rift <- S.printingOf s registry "Aether Rift"
        giant <- S.printingOf s registry "Hill Giant"
        heldCard <- S.printingOf s registry held
        redirecting <- traverse (S.printingOf s registry) redirect
        let (_, withRift) = S.addPermanent rift S.alice S.threePlayerGame
            withRedirect = maybe withRift (\p -> snd (S.addPermanent p S.bob withRift)) redirecting
            (_, withGiant) = S.addGraveyardCard giant S.alice withRedirect
            (_, withHand) = S.addHandCard heldCard S.alice withGiant
            begun = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep S.alice)) (withHand {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            onStack = S.runPure S.identityAnswer begun Engine.settleForPriority
        pure onStack
      -- The one seat that pays 5 life, if any.
      paysFor :: Maybe PlayerId.PlayerId -> Prompt.Prompt r -> r
      paysFor who p = case p of
        Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
          | Just d == who && Just player == who -> PaymentDecision.Pays
        _ -> S.identityAnswer p
      payResponses = filter (\r -> case r of Response.ChoseToPay _ -> True; _ -> False)
      named = Just . CardName.MkCardName . Text.pack
      battlefieldNames gs = List.sort (fmap (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Set.toList (GameState.battlefield gs)))
  Spec.it s "CR 701.9a Aether Rift returns the creature card it discarded when nobody pays" $ do
    onStack <- riftBoard Nothing "Goblin Piker"
    Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
    let ((_, after), transcript) = Replay.record (paysFor Nothing) onStack Stack.resolveTop
    Spec.assertEqWith s "the discarded Piker is on the battlefield beside the Rift" (battlefieldNames after) [named "Aether Rift", named "Goblin Piker"]
    Spec.assertEqWith s "and the Hill Giant already there stays in the graveyard" (namesIn Zone.Graveyard S.alice after) [named "Hill Giant"]
    Spec.assertEqWith s "alice's hand is empty" (S.handSize S.alice after) 0
    -- Every player is offered, in APNAP order.
    Spec.assertEqWith s "CR 118.12a: all three were offered and declined" (payResponses transcript) (replicate 3 (Response.ChoseToPay PaymentDecision.Declines))
    Spec.assertEqWith s "and nobody lost life" (fmap (`S.lifeOf` after) [S.alice, S.bob, S.carol]) (replicate 3 (Just 20))
  -- The same board, differing only in carol's answer.
  Spec.it s "CR 118.12a carol pays 5 life, so the discarded creature stays in the graveyard" $ do
    onStack <- riftBoard Nothing "Goblin Piker"
    let ((_, after), transcript) = Replay.record (paysFor (Just S.carol)) onStack Stack.resolveTop
    Spec.assertEqWith s "the Piker stays in alice's graveyard" (List.sort (namesIn Zone.Graveyard S.alice after)) [named "Goblin Piker", named "Hill Giant"]
    Spec.assertEqWith s "and only the Rift is on the battlefield" (battlefieldNames after) [named "Aether Rift"]
    Spec.assertEqWith s "carol paid 5 life" (S.lifeOf S.carol after) (Just 15)
    Spec.assertEqWith s "CR 101.4: alice and bob declined before carol paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines, Response.ChoseToPay PaymentDecision.Declines, Response.ChoseToPay PaymentDecision.Pays]
  -- The same board with a noncreature card held: the "if" is false, so nobody
  -- is even offered the payment.
  Spec.it s "CR 701.9a a noncreature card discarded by Aether Rift stays put and nobody is asked to pay" $ do
    onStack <- riftBoard Nothing "Lightning Bolt"
    let ((_, after), transcript) = Replay.record (paysFor Nothing) onStack Stack.resolveTop
    Spec.assertEqWith s "the Bolt is in alice's graveyard" (List.sort (namesIn Zone.Graveyard S.alice after)) [named "Hill Giant", named "Lightning Bolt"]
    Spec.assertEqWith s "and only the Rift is on the battlefield" (battlefieldNames after) [named "Aether Rift"]
    Spec.assertEqWith s "nobody was offered the payment" (payResponses transcript) []
  -- CR 701.9c through a CR 614 redirect: Rest in Peace exiles the discarded
  -- Piker. It was still discarded, so the "if" holds and the payment is still
  -- offered, but "return it from your graveyard" finds nothing there -- and must
  -- not take the Hill Giant that is.
  Spec.it s "CR 701.9c a creature card Aether Rift discarded into exile is not returned" $ do
    onStack <- riftBoard (Just "Rest in Peace") "Goblin Piker"
    let ((_, after), transcript) = Replay.record (paysFor Nothing) onStack Stack.resolveTop
    Spec.assertEqWith s "the Piker is in exile" (namesIn Zone.Exile S.alice after) [named "Goblin Piker"]
    Spec.assertEqWith s "and nothing of alice's reached the battlefield" (battlefieldNames after) [named "Aether Rift", named "Rest in Peace"]
    Spec.assertEqWith s "the Hill Giant stays in the graveyard" (namesIn Zone.Graveyard S.alice after) [named "Hill Giant"]
    Spec.assertEqWith s "the discard still counted: all three were offered" (payResponses transcript) (replicate 3 (Response.ChoseToPay PaymentDecision.Declines))
  -- Cragganwick Cremator: two boards differing in the one card alice holds once
  -- the Cremator is cast. The Piker's power, 2, is not the Cremator's own 5.
  let cremate held = do
        mountain <- S.printingOf s registry "Mountain"
        cremator <- S.printingOf s registry "Cragganwick Cremator"
        heldCard <- S.printingOf s registry held
        let lands = S.landsFor mountain S.alice 4 S.threePlayerGame
            (withCremator, crematorId) = S.handOne cremator lands
            (_, ready) = S.addHandCard heldCard S.alice withCremator
            cast = S.runPure atBobAnswer ready (S.cast S.alice crematorId)
            resolved = S.runPure atBobAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
        pure (resolved, S.runPure atBobAnswer resolved Stack.resolveTop)
  Spec.it s "CR 701.9a Cragganwick Cremator deals the discarded creature card's power" $ do
    (onStack, after) <- cremate "Goblin Piker"
    Spec.assertBool s (not (null (GameState.stack onStack))) "the enters trigger really reached the stack"
    Spec.assertEqWith s "bob took the Piker's 2" (S.lifeOf S.bob after) (Just 18)
    Spec.assertEqWith s "the Piker is in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [named "Goblin Piker"]
  Spec.it s "CR 701.9a Cragganwick Cremator deals nothing for a noncreature card" $ do
    (onStack, after) <- cremate "Lightning Bolt"
    Spec.assertBool s (not (null (GameState.stack onStack))) "the enters trigger really reached the stack"
    Spec.assertEqWith s "bob took nothing" (S.lifeOf S.bob after) (Just 20)
    Spec.assertEqWith s "the Bolt is in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [named "Lightning Bolt"]

-- CR 107.1c / 701.9b: "discard any number of" matching cards -- the discarding
-- player picks the subset, none included, out of the matching cards in their own
-- hand (Pawl.Types.Discard's AnyNumber arm), and what moved is bound for the
-- rest of the resolution to count.
--
-- A test-local answerer rather than the Board harness, whose vocabulary has no
-- resolution-time subset choice: each leg pins the subset by position in the
-- offer, so a fallback that happened to agree cannot answer for the engine.
anyNumberDiscardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
anyNumberDiscardSpec s registry =
  let -- Cast the card and run the priority loop out under one answerer, keeping
      -- every response so "never asked" and "asked, answered nothing" differ.
      runCast :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, [Response.Response])
      runCast answer (withSpell, spell) =
        let ((_, afterCast), castResponses) = Replay.record answer withSpell (S.cast S.alice spell)
            ((_, after), loopResponses) = Replay.record answer afterCast Engine.priorityLoop
         in (after, castResponses <> loopResponses)
      -- Accepts every printed "may" and discards the offered cards `pick` keeps.
      discarding :: ([ObjectId.ObjectId] -> [ObjectId.ObjectId]) -> (forall q. Prompt.Prompt q -> q) -> Prompt.Prompt r -> r
      discarding pick fallback p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseAnyNumberToDiscard _ _ _ offered _ -> Set.fromList (pick offered)
        _ -> fallback p
      -- The rest of alice's hand, added AFTER S.handOne, which replaces it.
      withHand fill (gs, spell) = (fill gs, spell)
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      onBattlefield name gs = List.find (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just name) (Set.toList (GameState.battlefield gs))
      -- Aims the reflexive trigger at the one creature named, FILTERING the offer.
      atWall :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      atWall wallId p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (\r -> Recipient.objectOf r == Just wallId) offered) sets
        _ -> S.identityAnswer p
      askedForTargets :: [Response.Response] -> Bool
      askedForTargets = any (\r -> case r of Response.ChoseTargets _ -> True; _ -> False)
      sortedNames zone pid gs = List.sort (namesIn zone pid gs)
   in Spec.describe s "CR 107.1c discard any number of cards" $ do
        -- Borborygmos and Fblthp {2}{G}{U}{R}: "whenever Borborygmos and Fblthp
        -- enters or attacks, draw a card, then you may discard any number of land
        -- cards. When you discard one or more cards this way, Borborygmos and
        -- Fblthp deals twice that much damage to target creature."
        --
        -- alice holds three Mountains and a Goblin Piker and draws a Piker, so the
        -- offer is the three lands alone; bob's Wall of Stone is a 0/8, alive
        -- under every amount below, so its marked damage IS the reading. Three
        -- legs over one board, differing only in the subset: all three lands, one,
        -- none. Twice three and twice one separate the count from a literal and
        -- from the Times; none is CR 603.12's "when you discard one or more"
        -- staying unarmed.
        let borborygmosBoard = do
              borborygmos <- S.printingOf s registry "Borborygmos and Fblthp"
              forest <- S.printingOf s registry "Forest"
              island <- S.printingOf s registry "Island"
              mountain <- S.printingOf s registry "Mountain"
              swamp <- S.printingOf s registry "Swamp"
              piker <- S.printingOf s registry "Goblin Piker"
              wall <- S.printingOf s registry "Wall of Stone"
              let lands = List.foldl' (\g land -> snd (S.addPermanent land S.alice g)) (Setup.emptyGame S.bothPlayers) [forest, island, mountain, swamp, swamp]
                  (wallId, withWall) = S.addPermanent wall S.bob lands
                  staged = withHand (handCards piker S.alice 1 . handCards mountain S.alice 3) (S.handOne borborygmos (stockLibrary piker S.alice 3 withWall))
              pure (wallId, staged, S.printingName mountain, S.printingName piker)
        Spec.it s "CR 603.12 Borborygmos and Fblthp deals twice the lands discarded to the target" $ do
          (wallId, staged, mountain, piker) <- borborygmosBoard
          let (after, _) = runCast (discarding id (atWall wallId)) staged
          Spec.assertEqWith s "the wall took twice three" (fmap Object.damage (Game.lookupObject wallId after)) (Just 6)
          Spec.assertEqWith s "every land in hand was discarded" (sortedNames Zone.Graveyard S.alice after) (replicate 3 (Just mountain))
          Spec.assertEqWith s "and the pikers were never offered" (sortedNames Zone.Hand S.alice after) (replicate 2 (Just piker))
        Spec.it s "CR 107.1c Borborygmos and Fblthp discards only the chosen subset" $ do
          (wallId, staged, mountain, piker) <- borborygmosBoard
          let (after, _) = runCast (discarding (take 1) (atWall wallId)) staged
          Spec.assertEqWith s "the wall took twice one" (fmap Object.damage (Game.lookupObject wallId after)) (Just 2)
          Spec.assertEqWith s "one land discarded" (sortedNames Zone.Graveyard S.alice after) [Just mountain]
          Spec.assertEqWith s "the other two kept" (sortedNames Zone.Hand S.alice after) [Just piker, Just piker, Just mountain, Just mountain]
        Spec.it s "CR 603.12 Borborygmos and Fblthp discarding none arms no reflexive trigger" $ do
          (wallId, staged, _, _) <- borborygmosBoard
          let (after, responses) = runCast (discarding (const []) (atWall wallId)) staged
          Spec.assertEqWith s "the wall took nothing" (fmap Object.damage (Game.lookupObject wallId after)) (Just 0)
          Spec.assertBool s (not (askedForTargets responses)) "and no reflexive trigger asked for a target"
          Spec.assertEqWith s "nothing discarded" (namesIn Zone.Graveyard S.alice after) []
          Spec.assertBool s (any (\r -> case r of Response.ChoseAnyNumberToDiscard _ -> True; _ -> False) responses) "though the choice was put to alice"
        -- Nantuko Cultivator {3}{G}: "when this creature enters, you may discard
        -- any number of land cards. Put that many +1/+1 counters on this creature
        -- and draw that many cards." Two of three lands, so "that many" is
        -- neither the hand nor the offer.
        Spec.it s "CR 107.1c Nantuko Cultivator counts and draws the lands discarded" $ do
          cultivator <- S.printingOf s registry "Nantuko Cultivator"
          forest <- S.printingOf s registry "Forest"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let lands = List.foldl' (\g land -> snd (S.addPermanent land S.alice g)) (Setup.emptyGame S.bothPlayers) [forest, swamp, swamp, swamp]
              staged = withHand (handCards mountain S.alice 3) (S.handOne cultivator (stockLibrary piker S.alice 3 lands))
              (after, _) = runCast (discarding (take 2) S.identityAnswer) staged
              cultivatorId = onBattlefield (S.printingName cultivator) after
          Spec.assertEqWith s "two counters" (cultivatorId >>= \oid -> countersOn oid after) (Just 2)
          Spec.assertEqWith s "two cards drawn beside the kept land" (sortedNames Zone.Hand S.alice after) [Just (S.printingName piker), Just (S.printingName piker), Just (S.printingName mountain)]
          Spec.assertEqWith s "two lands discarded" (namesIn Zone.Graveyard S.alice after) (replicate 2 (Just (S.printingName mountain)))
        -- Mind Maggots {3}{B}: "when this creature enters, discard any number of
        -- creature cards. For each card discarded this way, put two +1/+1
        -- counters on this creature." Every offered card taken, so the land in
        -- hand staying put is the filter's doing.
        Spec.it s "CR 107.1c Mind Maggots offers only creature cards and puts two counters per discard" $ do
          maggots <- S.printingOf s registry "Mind Maggots"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          let staged = withHand (handCards mountain S.alice 1 . handCards piker S.alice 2) (S.handOne maggots (S.landsInPlay swamp 4))
              (after, _) = runCast (discarding id S.identityAnswer) staged
              maggotsId = onBattlefield (S.printingName maggots) after
          Spec.assertEqWith s "four counters" (maggotsId >>= \oid -> countersOn oid after) (Just 4)
          Spec.assertEqWith s "both creature cards discarded" (namesIn Zone.Graveyard S.alice after) (replicate 2 (Just (S.printingName piker)))
          Spec.assertEqWith s "the land kept" (namesIn Zone.Hand S.alice after) [Just (S.printingName mountain)]
        -- Flux {2}{U}: "each player discards any number of cards, then draws that
        -- many cards. Draw a card." Three seats discarding one, three and none, so
        -- "that many" is each drawer's own: the union of four read by every seat
        -- would give alice five, bob four and carol four.
        Spec.it s "CR 107.1c Flux draws each player the number THEY discarded" $ do
          flux <- S.printingOf s registry "Flux"
          island <- S.printingOf s registry "Island"
          piker <- S.printingOf s registry "Goblin Piker"
          sentry <- S.printingOf s registry "Ogre Sentry"
          rats <- S.printingOf s registry "Typhoid Rats"
          let stocked = List.foldl' (\g pid -> stockLibrary piker pid 6 g) (S.landsFor island S.alice 3 S.threePlayerGame) [S.alice, S.bob, S.carol]
              staged = withHand (handCards rats S.carol 2 . handCards sentry S.bob 4 . handCards piker S.alice 2) (S.handOne flux stocked)
              perSeat :: Prompt.Prompt r -> r
              perSeat p = case p of
                Prompt.ChooseAnyNumberToDiscard _ victim _ offered _
                  | victim == S.alice -> Set.fromList (take 1 offered)
                  | victim == S.bob -> Set.fromList (take 3 offered)
                  | otherwise -> Set.empty
                _ -> S.identityAnswer p
              (after, _) = runCast perSeat staged
              drew pid = 6 - length (Game.zoneMembers Zone.Library pid after)
          Spec.assertEqWith s "alice drew her one and Flux's one" (drew S.alice) 2
          Spec.assertEqWith s "bob drew his three" (drew S.bob) 3
          Spec.assertEqWith s "carol drew none" (drew S.carol) 0
          Spec.assertEqWith s "bob discarded three sentries" (namesIn Zone.Graveyard S.bob after) (replicate 3 (Just (S.printingName sentry)))
          Spec.assertEqWith s "carol kept both rats" (namesIn Zone.Hand S.carol after) (replicate 2 (Just (S.printingName rats)))
        -- Steal the Show {2}{R}: "choose one or both -- target player discards any
        -- number of cards, then draws that many cards; Steal the Show deals damage
        -- equal to the number of instant and sorcery cards in your graveyard to
        -- target creature or planeswalker." Both modes: bob discards two of three,
        -- and alice's graveyard holds three instants and sorceries beside a land,
        -- so the 0/8 wall's damage is neither the discard count nor the graveyard.
        Spec.it s "CR 700.2 Steal the Show's discard draws that many and its damage counts instants and sorceries" $ do
          steal <- S.printingOf s registry "Steal the Show"
          mountain <- S.printingOf s registry "Mountain"
          bolt <- S.printingOf s registry "Lightning Bolt"
          divination <- S.printingOf s registry "Divination"
          sentry <- S.printingOf s registry "Ogre Sentry"
          rats <- S.printingOf s registry "Typhoid Rats"
          wall <- S.printingOf s registry "Wall of Stone"
          let (wallId, withWall) = S.addPermanent wall S.bob (S.landsInPlay mountain 3)
              graveyard = List.foldl' (\g p -> snd (S.addGraveyardCard p S.alice g)) withWall [bolt, bolt, divination, mountain]
              staged = withHand (handCards sentry S.bob 3) (S.handOne steal (stockLibrary rats S.bob 4 graveyard))
              bothAtBob :: Prompt.Prompt r -> r
              bothAtBob p = case p of
                Prompt.ChooseModes _ _ _ offered _ -> Seq.fromList (Set.toList offered)
                Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (\r -> Recipient.playerOf r == Just S.bob || Recipient.objectOf r == Just wallId) offered) sets
                Prompt.ChooseAnyNumberToDiscard _ _ _ offered _ -> Set.fromList (take 2 offered)
                _ -> S.identityAnswer p
              (after, _) = runCast bothAtBob staged
          Spec.assertEqWith s "the wall took three" (fmap Object.damage (Game.lookupObject wallId after)) (Just 3)
          Spec.assertEqWith s "bob drew two" (length (Game.zoneMembers Zone.Library S.bob after)) 2
          Spec.assertEqWith s "bob holds his kept sentry and two drawn rats" (sortedNames Zone.Hand S.bob after) (List.sort [Just (S.printingName sentry), Just (S.printingName rats), Just (S.printingName rats)])

-- Random discards that need more than the discard itself, and a repeat loop.
-- Checked against api.scryfall.com, 2026-09-28:
--
-- Rowdy Crew {2}{R}{R} Creature -- Human Pirate 3/3, trample -- "When this
-- creature enters, draw three cards, then discard two cards at random. If two
-- cards that share a card type are discarded this way, put two +1/+1 counters on
-- this creature."
--
-- Rites of Initiation {R} Instant -- "Discard any number of cards at random.
-- Creatures you control get +1/+0 until end of turn for each card discarded this
-- way."
--
-- Kindle the Carnage {1}{R}{R} Sorcery -- "Discard a card at random. If you do,
-- Kindle the Carnage deals damage equal to that card's mana value to each
-- creature. You may repeat this process any number of times."
randomDiscardProcessSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
randomDiscardProcessSpec s registry = Spec.describe s "CR 701.9b random discards that share, announce or repeat" $ do
  -- Rites of Initiation off one Mountain. alice holds three other cards and
  -- controls a Goblin Piker (2/1); bob controls a Llanowar Elves (1/1), which
  -- "creatures you control" must not reach.
  let rites number = do
        mountain <- S.printingOf s registry "Mountain"
        ritesCard <- S.printingOf s registry "Rites of Initiation"
        piker <- S.printingOf s registry "Goblin Piker"
        elves <- S.printingOf s registry "Llanowar Elves"
        held <- traverse (S.printingOf s registry) ["Hill Giant", "Lightning Bolt", "Mountain"]
        let lands = S.landsFor mountain S.alice 1 S.threePlayerGame
            (pikerId, withPiker) = S.addPermanent piker S.alice lands
            (elvesId, withElves) = S.addPermanent elves S.bob withPiker
            (withRites, ritesId) = S.handOne ritesCard withElves
            stocked = List.foldl' (\gs p -> snd (S.addHandCard p S.alice gs)) withRites held
            named_ :: Natural
            named_ = number
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseNumber {} -> named_
              _ -> S.identityAnswer p
            cast = S.runPure answer stocked (S.cast S.alice ritesId)
            ((_, after), transcript) = Replay.record answer cast Stack.resolveTop
            numbers = filter (\r -> case r of Response.ChoseNumber _ -> True; _ -> False) transcript
        pure (fmap fst (S.powerToughnessOf pikerId after), fmap fst (S.powerToughnessOf elvesId after), S.handSize S.alice after, numbers)
  Spec.it s "CR 107.1c Rites of Initiation discards the two cards alice names and pumps her creature by two" $ do
    (pikerPower, elvesPower, handLeft, numbers) <- rites 2
    Spec.assertEqWith s "the Piker gets +2/+0" pikerPower (Just 4)
    Spec.assertEqWith s "one card left in alice's hand" handLeft 1
    Spec.assertEqWith s "bob's Elves are untouched" elvesPower (Just 1)
    Spec.assertEqWith s "CR 608.2d alice named the number" numbers [Response.ChoseNumber 2]
  -- Five named, three held: CR 609.3 discards the three, and the bonus counts
  -- the cards discarded rather than the number named.
  Spec.it s "CR 609.3 Rites of Initiation counts the cards discarded, not the number named" $ do
    (pikerPower, _, handLeft, _) <- rites 5
    Spec.assertEqWith s "the Piker gets +3/+0" pikerPower (Just 5)
    Spec.assertEqWith s "alice's hand is empty" handLeft 0
  Spec.it s "CR 107.1c Rites of Initiation with zero named discards nothing" $ do
    (pikerPower, _, handLeft, _) <- rites 0
    Spec.assertEqWith s "the Piker is unchanged" pikerPower (Just 2)
    Spec.assertEqWith s "alice keeps all three cards" handLeft 3
  -- Kindle the Carnage off three Mountains. alice holds a Goblin Piker (mana
  -- value 2) and a Hill Giant (4), and randomness takes the first card it is
  -- offered, the Piker; bob controls a Platinum Emperion (8/8), which survives all of it.
  -- `runs` is how many times alice runs the process before declining.
  let kindle runs = do
        mountain <- S.printingOf s registry "Mountain"
        kindleCard <- S.printingOf s registry "Kindle the Carnage"
        emperion <- S.printingOf s registry "Platinum Emperion"
        held <- traverse (S.printingOf s registry) ["Hill Giant", "Goblin Piker"]
        let lands = S.landsFor mountain S.alice 3 S.threePlayerGame
            (emperionId, withEmperion) = S.addPermanent emperion S.bob lands
            (withKindle, kindleId) = S.handOne kindleCard withEmperion
            stocked = List.foldl' (\gs p -> snd (S.addHandCard p S.alice gs)) withKindle held
            -- Pinned by the run count the prompt carries, so each ask is told
            -- apart.
            limit :: Natural
            limit = runs
            answer :: Prompt.Prompt r -> r
            answer p = case p of
              Prompt.ChooseRepeat _ _ _ done -> if done < limit then OptionalDecision.Exercises else OptionalDecision.Declines
              _ -> S.identityAnswer p
            cast = S.runPure answer stocked (S.cast S.alice kindleId)
            ((_, after), transcript) = Replay.record answer cast Stack.resolveTop
            repeats = filter (\r -> case r of Response.ChoseRepeat _ -> True; _ -> False) transcript
        pure (S.damageOf emperionId after, S.handSize S.alice after, length repeats)
  Spec.it s "CR 608.2d Kindle the Carnage run once deals the one discarded card's mana value" $ do
    (damage, handLeft, asked) <- kindle 1
    Spec.assertEqWith s "the Emperion took the Piker's 2" damage (Just 2)
    Spec.assertEqWith s "the Hill Giant stays in alice's hand" handLeft 1
    Spec.assertEqWith s "alice was asked once" asked 1
  Spec.it s "CR 608.2d Kindle the Carnage repeated deals each discarded card's mana value" $ do
    (damage, handLeft, asked) <- kindle 2
    Spec.assertEqWith s "the Emperion took 2 then 4" damage (Just 6)
    Spec.assertEqWith s "alice's hand is empty" handLeft 0
    Spec.assertEqWith s "alice was asked after each run" asked 2
  -- A third run finds an empty hand: it discards nothing, so it deals nothing,
  -- rather than the Hill Giant's 4 again.
  Spec.it s "CR 608.2d a Kindle the Carnage run over an empty hand deals nothing" $ do
    (damage, handLeft, asked) <- kindle 3
    Spec.assertEqWith s "the Emperion took only 2 then 4" damage (Just 6)
    Spec.assertEqWith s "alice's hand is empty" handLeft 0
    Spec.assertEqWith s "alice was asked after each of three runs" asked 3

-- Ad Nauseam {3}{B}{B} Instant -- "Reveal the top card of your library and put
-- that card into your hand. You lose life equal to its mana value. You may
-- repeat this process any number of times."
--
-- Trade Secrets {1}{U}{U} Sorcery -- "Target opponent draws two cards, then you
-- draw up to four cards. That opponent may repeat this process as many times as
-- they choose."
repeatProcessSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
repeatProcessSpec s registry = Spec.describe s "CR 608.2d processes a player may repeat" $ do
  -- Trade Secrets off three Islands, aimed at carol rather than bob, so "that
  -- opponent" is not the one APNAP order reaches first. Whoever is asked to
  -- repeat answers by seat: carol takes a second run and declines a third, and
  -- anyone else declines outright. alice names six each time, past the four the
  -- card allows.
  Spec.it s "CR 608.2d Trade Secrets' target opponent chooses to repeat, and alice draws at most four" $ do
    island <- S.printingOf s registry "Island"
    spell <- S.printingOf s registry "Trade Secrets"
    filler <- S.printingOf s registry "Mountain"
    let lands = S.landsFor island S.alice 3 S.threePlayerGame
        (withSpell, spellId) = S.handOne spell lands
        stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard filler pid g)) gs [1 :: Int .. 12]
        stocked = stock S.carol (stock S.alice withSpell)
        answer :: Prompt.Prompt r -> r
        answer p = case p of
          Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer S.carol) . snd) sets
          Prompt.ChooseRepeat _ pid _ done -> if pid == S.carol && done < 2 then OptionalDecision.Exercises else OptionalDecision.Declines
          Prompt.ChooseNumber {} -> 6
          _ -> S.identityAnswer p
        cast = S.runPure answer stocked (S.cast S.alice spellId)
        ((_, after), transcript) = Replay.record answer cast Stack.resolveTop
        numbers = filter (\r -> case r of Response.ChoseNumber _ -> True; _ -> False) transcript
    Spec.assertEqWith s "carol drew two cards in each of two runs" (S.handSize S.carol after) 4
    Spec.assertEqWith s "alice drew four, not six, in each of two runs" (S.handSize S.alice after) 8
    Spec.assertEqWith s "bob drew nothing" (S.handSize S.bob after) 0
    Spec.assertEqWith s "CR 608.2d alice named each run's number" numbers [Response.ChoseNumber 6, Response.ChoseNumber 6]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  zoneChangeSpec s registry
  discardExceptionsSpec s registry
  discardedThisWaySpec s registry
  randomDiscardProcessSpec s registry
  repeatProcessSpec s registry
  anyNumberDiscardSpec s registry
  elkinLairSpec s registry
  castTheCardSpec s registry
  nextUpkeepSpec s registry
  libraryDepthSpec s registry
  aetherspoutsSpec s registry
  drawCardSpec s registry
  loseLifeSpec s registry
  perRecipientAmountSpec s registry
  exchangeLifeTotalsSpec s registry
  exchangeValuesSpec s registry
  setLifeTotalSpec s registry
  doubleLifeTotalSpec s registry
  redistributeLifeTotalsSpec s registry
  greatestSpec s registry

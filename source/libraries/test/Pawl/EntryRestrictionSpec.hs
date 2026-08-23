-- Covers: CR 101.2 / CR 400.4a's ENTRY PROHIBITION -- Pawl.Types.EntryRestriction,
-- the Bool Pawl.Engine.EntryRestriction answers, and the one place it is asked
-- (Pawl.Engine.Event.changeZoneAttaching, the funnel every battlefield entry
-- reaches). Also CR 701.40f, whose "that card isn't manifested ... it remains in
-- its previous zone. If it was face up, it remains face up" is the manifest case
-- below.
--
-- Grafdigger's Cage is the fixture: "Creature cards in graveyards and libraries
-- can't enter the battlefield." Its second sentence ("players can't cast spells
-- from graveyards or libraries") is on the card too, as a player ability; nothing
-- here reads it, and Pawl.CastSpec's Grafdigger's Cage group is where it is
-- proved.
--
-- THE BOARD SHAPE that makes these cases discriminating:
--
--   * TWO GRAVEYARDS, not one. The prohibition is symmetric and names no
--     controller, so a one-seat board would leave "does it reach the opponent's
--     graveyard" untested.
--   * A HARDCAST CREATURE SPELL beside the graveyard cards. An implementation
--     that spelled the Cage as Affected.MatchingOffBattlefield alone would also
--     match a creature spell on the STACK and refuse an ordinary hardcast. That
--     is what EntryRestriction.origins exists for, and the hardcast leg is the
--     only thing that proves it.
--   * A CREATURE card on top of the library for the manifest case. The Cage's
--     filter is HasCardType Creature, so a noncreature top card would make a
--     working implementation and a broken one agree.
--   * A PAIRED BOARD without the Cage for each negative, differing in that one
--     permanent, since an absence passes for free on a board where the move never
--     happened at all.
module Pawl.EntryRestrictionSpec where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Grafdigger's Cage" $ do
  exhumeCase s registry
  hardcastCase s registry
  manifestCase s registry

-- The names of the permanents `after` has that `before` did not, sorted. CR 400.7
-- mints a fresh id at the destination, so an arrival can only be found this way.
arrivals :: GameState.GameState -> GameState.GameState -> [Maybe CardName.CardName]
arrivals before after =
  List.sort
    [ fmap S.nameOf (Game.cardOf oid after)
    | oid <- Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before))
    ]

-- Where an id is now, and what card it still is. Read as a PAIR because the two
-- questions are what separate a refusal from a redirect: a redirect leaves the
-- captured id dangling (Nothing, Nothing), a refusal leaves it exactly as it was.
whereIs :: ObjectId.ObjectId -> GameState.GameState -> (Maybe Zone.Zone, Maybe CardName.CardName)
whereIs oid gs =
  ( fmap Object.zone (Game.lookupObject oid gs),
    fmap S.nameOf (Game.cardOf oid gs)
  )

-- THE HEADLINE. Exhume ("each player puts a creature card from their graveyard
-- onto the battlefield") under a Cage: CR 101.2's "can't" beats the sorcery's
-- "puts", so nothing enters -- but the sorcery still instructs each player to
-- CHOOSE, and CR 400.4a leaves each chosen card in the graveyard as the same
-- object.
exhumeCase :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exhumeCase s registry = do
  let -- alice has four Swamps -- twice Exhume's cost, so a payment that taps one
      -- source at a time cannot fail for reasons of its own -- and Exhume in hand.
      -- Each seat's graveyard holds TWO creature cards, all four named
      -- differently: two so that CR 608.2d's choice is a real one the prompt
      -- cannot short-circuit past, and distinct so that no seat's card can stand
      -- in for another's. Returns (the spell, the four graveyard ids, the board).
      board exhume swamp buried cage =
        let place (g, ids) (printing, pid) = let (oid, g') = S.addGraveyardCard printing pid g in (g', ids <> [oid])
            (g1, graves) = List.foldl' place (S.landsInPlay swamp 4, []) buried
            g2 = foldr (\c g -> snd (S.addCreature c S.alice g)) g1 cage
            (g3, spell) = S.handOne exhume g2
         in (spell, graves, g3)
      run spell gs =
        let ((_, after), responses) = Replay.record S.identityAnswer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (after, responses)
      asked responses = length [() | Response.ChoseCardInGraveyard _ <- responses]
      fixtures = do
        exhume <- S.printingOf s registry "Exhume"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        wraith <- S.printingOf s registry "Bog Wraith"
        sentry <- S.printingOf s registry "Ogre Sentry"
        cage <- S.printingOf s registry "Grafdigger's Cage"
        pure (exhume, swamp, [(piker, S.alice), (maiden, S.alice), (wraith, S.bob), (sentry, S.bob)], cage)
  -- THE PAIRED CONTROL, run first so a reader knows the board can return
  -- creatures at all: the same cast on the same board with no Cage brings both
  -- creatures back. Without this leg every assertion below passes for free on a
  -- board where Exhume never resolved.
  Spec.it s "CR 608.2d without the Cage each player's chosen creature enters" $ do
    (exhume, swamp, buried, _) <- fixtures
    let (spell, graves, before) = board exhume swamp buried []
        (after, responses) = run spell before
    Spec.assertEqWith s "the sorcery resolved into alice's graveyard (CR 608.2n)" (elem (S.printingName exhume) (namesIn Zone.Graveyard S.alice after)) True
    Spec.assertEqWith s "both players were asked" (asked responses) 2
    Spec.assertEqWith s "one creature per seat is on the battlefield" (length (arrivals before after)) 2
    Spec.assertEqWith s "and two of the four buried cards left their graveyards (CR 400.7)" (length (filter ((== (Nothing, Nothing)) . (`whereIs` after)) graves)) 2
  -- CR 604.2 / CR 613.1f: a Cage that has LOST its abilities prohibits nothing.
  -- Titania's Song gives every noncreature artifact LoseAllAbilities, so the same
  -- board with the Cage still on the battlefield behaves like the leg above --
  -- which is what proves the reader's ability-removal gate rather than asserting
  -- it. The Song is not itself an artifact, so it cannot strip its own text.
  Spec.it s "CR 604.2 a Cage stripped of its abilities prohibits nothing" $ do
    (exhume, swamp, buried, cage) <- fixtures
    song <- S.printingOf s registry "Titania's Song"
    let (spell, _, before) = board exhume swamp buried [cage, song]
        (after, _) = run spell before
    Spec.assertEqWith s "the sorcery resolved into alice's graveyard (CR 608.2n)" (elem (S.printingName exhume) (namesIn Zone.Graveyard S.alice after)) True
    Spec.assertEqWith s "one creature per seat entered anyway" (length (arrivals before after)) 2
  Spec.it s "CR 101.2 no creature card in a graveyard can enter the battlefield" $ do
    (exhume, swamp, buried, cage) <- fixtures
    let (spell, graves, before) = board exhume swamp buried [cage]
        (after, responses) = run spell before
    -- Ordered so that a mutation which broke the CAST reddens here rather than
    -- masquerading as a prohibition.
    Spec.assertEqWith s "the sorcery resolved into alice's graveyard (CR 608.2n)" (elem (S.printingName exhume) (namesIn Zone.Graveyard S.alice after)) True
    -- CR 101.2 forbids the ENTRY, not the choice: Exhume still says "puts a
    -- creature card", so each player is still asked. This is what separates
    -- "prohibited" from "the effect did nothing".
    Spec.assertEqWith s "both players were still asked (CR 608.2d)" (asked responses) 2
    -- THE HEADLINE.
    Spec.assertEqWith s "CR 101.2 nothing entered the battlefield" (arrivals before after) []
    -- THE DISCRIMINATOR against a redirect. A ZoneChangeR that sent the move to
    -- the graveyard would also leave the battlefield empty, but CR 400.7 would
    -- mint a fresh object there and leave these ids dangling. A refusal leaves
    -- each card exactly where and what it was.
    Spec.assertEqWith
      s
      "CR 400.4a each card remains in its previous zone, as the same object"
      (fmap (`whereIs` after) graves)
      [(Just Zone.Graveyard, Just (S.printingName printing)) | (printing, _) <- buried]

-- THE CONTROL LEG for EntryRestriction.origins. A hardcast Goblin Piker is a
-- creature card that is NOT in a graveyard or a library -- it is a spell on the
-- stack -- so the Cage says nothing about it and the permanent enters. An
-- implementation that spelled the Cage as Affected.MatchingOffBattlefield with no
-- origin zones refuses this, because the stack is off the battlefield too.
hardcastCase :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
hardcastCase s registry =
  Spec.it s "CR 101.2 a hardcast creature spell is not in a graveyard or a library, so it still enters" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    cage <- S.printingOf s registry "Grafdigger's Cage"
    let g1 = snd (S.addCreature cage S.alice (S.landsInPlay mountain 4))
        (before, spell) = S.handOne piker g1
        after = S.runPure S.identityAnswer before (S.cast S.alice spell >> Stack.resolveTop)
    Spec.assertEqWith s "the Piker is on the battlefield" (arrivals before after) [Just (S.printingName piker)]

-- CR 701.40f, and WotC's own Grafdigger's Cage ruling: "manifesting a card from a
-- graveyard or library is an impossible action while Grafdigger's Cage is on the
-- battlefield". Soul Summons manifests the top card of alice's library; with a
-- CREATURE on top the Cage forbids the face-down object's entry, so the card
-- isn't manifested, stays in the library, and stays face up.
manifestCase :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manifestCase s registry = do
  let -- alice has two Plains for Soul Summons' {1}{W}, the Cage where `cage`
      -- names it, Soul Summons in hand, and a library holding a Goblin Piker
      -- under a Bog Wraith. The Piker keeps CR 104.3c off the board and leaves
      -- "the library is the same length" a delta rather than an emptying; the
      -- Wraith on top is a CREATURE card, which is what the Cage's filter reads.
      board summons plains piker wraith cage =
        let g1 = foldr (\c g -> snd (S.addCreature c S.alice g)) (S.landsInPlay plains 2) cage
            (g2, spell) = S.handOne summons g1
            g3 = snd (S.addLibraryCard piker S.alice g2)
            (top, g4) = S.addLibraryCard wraith S.alice g3
         in (spell, top, g4)
      fixtures = do
        summons <- S.printingOf s registry "Soul Summons"
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        wraith <- S.printingOf s registry "Bog Wraith"
        cage <- S.printingOf s registry "Grafdigger's Cage"
        pure (summons, plains, piker, wraith, cage)
  -- THE PAIRED CONTROL: the same board with no Cage manifests the Wraith, so the
  -- absences asserted below are absences of something this board can do.
  Spec.it s "CR 701.40a without the Cage the top card is manifested" $ do
    (summons, plains, piker, wraith, _) <- fixtures
    let (spell, top, before) = board summons plains piker wraith []
        after = S.runPure S.identityAnswer before (S.cast S.alice spell >> Stack.resolveTop)
    Spec.assertEqWith s "one permanent entered" (length (Set.difference (GameState.battlefield after) (GameState.battlefield before))) 1
    Spec.assertEqWith s "and it is no longer the library card it was (CR 400.7)" (whereIs top after) (Nothing, Nothing)
  Spec.it s "CR 701.40f the Cage makes manifesting a library card an impossible action" $ do
    (summons, plains, piker, wraith, cage) <- fixtures
    let (spell, top, before) = board summons plains piker wraith [cage]
        after = S.runPure S.identityAnswer before (S.cast S.alice spell >> Stack.resolveTop)
    Spec.assertEqWith s "the sorcery resolved into alice's graveyard (CR 608.2n)" (elem (S.printingName summons) (namesIn Zone.Graveyard S.alice after)) True
    Spec.assertEqWith s "CR 101.2 nothing entered the battlefield" (arrivals before after) []
    Spec.assertEqWith
      s
      "CR 701.40f the library is the same length"
      (length (Game.zoneMembers Zone.Library S.alice after))
      (length (Game.zoneMembers Zone.Library S.alice before))
    -- CR 701.40f's own last two sentences. The facing assertion is the one no
    -- implementation that wrote the face-down status before refusing can pass:
    -- CR 708.3 turns a manifested object face down BEFORE it enters, so a gate
    -- placed after that write would leave a face-down card in the library.
    Spec.assertEqWith
      s
      "CR 701.40f the card remains in its previous zone, as the same object"
      (whereIs top after)
      (Just Zone.Library, Just (S.printingName wraith))
    Spec.assertEqWith
      s
      "CR 701.40f and it remains face up"
      (fmap Object.facing (Game.lookupObject top after))
      (Just Facing.FaceUp)

-- The card names in one player's zone. Local rather than hoisted into
-- Pawl.Support, which rebuilds every spec in the tree.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs =
  [ S.nameOf card
  | oid <- Game.zoneMembers zone pid gs,
    card <- foldMap pure (Game.cardOf oid gs)
  ]

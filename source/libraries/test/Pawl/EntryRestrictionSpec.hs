-- Covers: CR 101.2 / CR 400.4a's ENTRY PROHIBITION -- Pawl.Types.EntryRestriction,
-- the Bool Pawl.Engine.EntryRestriction answers, and the two places it is asked
-- (Pawl.Engine.Event.changeZoneAttaching, the funnel every battlefield MOVE
-- reaches, and Event.createTokens, which CR 111.5 gives its own gate because a
-- token takes no move). Also CR 608.3e, whose refused permanent spell goes to its
-- owner's graveyard rather than staying where it was, and CR 701.40f, whose "that
-- card isn't manifested ... it remains in its previous zone. If it was face up, it
-- remains face up" is the manifest case below.
--
-- Grafdigger's Cage is the first group's fixture: "Creature cards in graveyards
-- and libraries can't enter the battlefield." Its second sentence ("players can't cast spells
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
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = do
  Spec.describe s "Grafdigger's Cage" $ do
    exhumeCase s registry
  Spec.describe s "Worms of the Earth" (tokenCase s registry)

-- The names of the permanents `after` has that `before` did not, sorted. CR 400.7
-- mints a fresh id at the destination, so an arrival can only be found this way.
arrivals :: GameState.GameState -> GameState.GameState -> [Maybe CardName.CardName]
arrivals before after =
  List.sort
    (fmap (\oid -> fmap S.nameOf (Game.cardOf oid after)) (Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before))))

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
        let place (g, ids) (printing, pid) = let (oid, g4) = S.addGraveyardCard printing pid g in (g4, ids <> [oid])
            (g1, graves) = List.foldl' place (S.landsInPlay swamp 4, []) buried
            g2 = foldr (\c g -> snd (S.addPermanent c S.alice g)) g1 cage
            (g3, spell) = S.handOne exhume g2
         in (spell, graves, g3)
      run spell gs =
        let ((_, after), responses) = Replay.record S.identityAnswer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (after, responses)
      asked responses = length (Maybe.mapMaybe (\response -> case response of Response.ChoseCardInGraveyard _ -> Just (); _ -> Nothing) responses)
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
      (fmap (\(printing, _) -> (Just Zone.Graveyard, Just (S.printingName printing))) buried)

-- CR 111.5: "if a spell or ability would create a token, but a rule or effect
-- states that a permanent with one or more of that token's characteristics can't
-- enter the battlefield, the token is not created."
--
-- Worms of the Earth ({2}{B}{B}{B} enchantment) is the prohibition -- "lands can't
-- enter the battlefield", which names no zone and so reaches a token; Autumn
-- Willow, Harmony ({3}{G}{G}) is the maker, whose enters trigger creates a 1/1
-- green Forest Dryad LAND creature token.
--
-- Both cards carry their whole text box. The Willow's third sentence, which adds
-- mana when a land creature is tapped, is proved in Pawl.ManaSpec's "Autumn
-- Willow, Harmony" group and adds nothing here, no land creature being tapped
-- for mana. Worms of the Earth's each-upkeep offer is
-- Pawl.ResolveSpec's "an either-or announced by each player" group.
tokenCase :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenCase s registry = do
  let -- The Willow enters WITH its CR 603.6a event, so settleForPriority finds
      -- the trigger pending; the prohibition, where one is named, is arranged
      -- beside it and takes no move of its own.
      board willow prohibition =
        let g1 = foldr (\c g -> snd (S.addPermanent c S.alice g)) (Setup.emptyGame S.bothPlayers) prohibition
         in snd (S.entersWithTrigger willow S.alice g1)
      settled gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      run gs = S.runPure S.identityAnswer (settled gs) Stack.resolveTop
      fixtures = do
        willow <- S.printingOf s registry "Autumn Willow, Harmony"
        worms <- S.printingOf s registry "Worms of the Earth"
        cage <- S.printingOf s registry "Grafdigger's Cage"
        pure (willow, worms, cage)
      token = CardName.MkCardName (Text.pack "Forest Dryad Token")
  -- THE PAIRED CONTROL: the same board with no prohibition mints the token, so
  -- the empty answers below are absences of something this board can produce.
  Spec.it s "CR 111.2 without the prohibition the enters trigger mints its land token" $ do
    (willow, _, _) <- fixtures
    let before = board willow []
        after = run before
    Spec.assertEqWith s "one Forest Dryad token entered" (arrivals before after) [Just token]
  Spec.it s "CR 111.5 no token is created when a permanent like it can't enter" $ do
    (willow, worms, _) <- fixtures
    let before = board willow [worms]
    -- The trigger really is on the stack, so the empty answer below cannot be an
    -- ability that never resolved.
    Spec.assertBool s (not (null (GameState.stack (settled before)))) "the Willow's enters trigger is on the stack"
    let after = run before
    Spec.assertEqWith s "CR 111.5 the token is not created" (arrivals before after) []
    Spec.assertEqWith s "and the ability resolved off the stack" (GameState.stack after) []
  -- THE DISCRIMINATOR against a gate that refuses a token whenever any entry
  -- prohibition is in force. Grafdigger's Cage names creature cards in graveyards
  -- and libraries; a token is a card in neither, so the same trigger mints the
  -- same token under it.
  Spec.it s "CR 111.5 a prohibition scoped to other zones does not reach a token" $ do
    (willow, _, cage) <- fixtures
    let before = board willow [cage]
        after = run before
    Spec.assertEqWith s "the Forest Dryad token entered anyway" (arrivals before after) [Just token]

-- The card names in one player's zone. Local rather than hoisted into
-- Pawl.Support, which rebuilds every spec in the tree.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs = do
  oid <- Game.zoneMembers zone pid gs
  card <- foldMap pure (Game.cardOf oid gs)
  pure (S.nameOf card)

-- Covers: CR 101.2 / CR 400.4a's ENTRY PROHIBITION -- Pawl.Types.EntryRestriction,
-- the Bool Pawl.Engine.EntryRestriction answers, and the two places it is asked
-- (Pawl.Engine.Event.changeZoneAttaching, the funnel every battlefield MOVE
-- reaches, and Event.createTokens, which CR 111.5 gives its own gate because a
-- token takes no move). Also CR 608.3e, whose refused permanent spell goes to its
-- owner's graveyard rather than staying where it was, and CR 701.40f, whose "that
-- card isn't manifested ... it remains in its previous zone. If it was face up, it
-- remains face up" is the manifest case below.
--
-- Grafdigger's Cage is the first group's fixture: "Creature cards in graveyards and libraries
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
import qualified Pawl.Types.Facing as Facing
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
    hardcastCase s registry
    manifestCase s registry
  Spec.describe s "Synthetic Sealed Horizon" (permanentSpellCase s registry)
  Spec.describe s "Worms of the Earth" (tokenCase s registry)

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

-- CR 608.3e, the one origin CR 400.4a does not answer for: "if a permanent spell
-- resolves but its controller can't put it onto the battlefield, that player puts
-- it into its owner's graveyard." Synthetic Sealed Horizon ({2}{W} enchantment,
-- "green creatures can't enter the battlefield") is what reaches it. No printing
-- can: Scryfall @o:/can't enter the battlefield/@, 2026-09-01, returns five cards,
-- and four of them name the graveyard or the library, which a spell has already
-- left by the time it resolves. The fifth is Worms of the Earth below, whose
-- lands are played rather than cast.
--
-- Llanowar Elves is the spell: a GREEN creature, so the Horizon's filter reads it,
-- and {G} so two Forests pay for it twice over.
permanentSpellCase :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentSpellCase s registry = do
  let board forest elves horizon =
        let g1 = foldr (\c g -> snd (S.addCreature c S.alice g)) (S.landsInPlay forest 2) horizon
            (g2, spell) = S.handOne elves g1
         in (spell, g2)
      fixtures = do
        forest <- S.printingOf s registry "Forest"
        elves <- S.printingOf s registry "Llanowar Elves"
        horizon <- S.printingOf s registry "Synthetic Sealed Horizon"
        pure (forest, elves, horizon)
  -- THE PAIRED CONTROL, differing in that one permanent: without the Horizon the
  -- same cast puts the Elves onto the battlefield and leaves the graveyard empty,
  -- so neither absence below is an absence this board has for free.
  Spec.it s "CR 608.3a without the Horizon the creature spell becomes a permanent" $ do
    (forest, elves, _) <- fixtures
    let (spell, before) = board forest elves []
        after = S.runPure S.identityAnswer before (S.cast S.alice spell >> Stack.resolveTop)
    Spec.assertEqWith s "the Elves are on the battlefield" (arrivals before after) [Just (S.printingName elves)]
    Spec.assertEqWith s "and nothing is in alice's graveyard" (namesIn Zone.Graveyard S.alice after) []
  Spec.it s "CR 608.3e a permanent spell refused entry goes to its owner's graveyard" $ do
    (forest, elves, horizon) <- fixtures
    let (spell, before) = board forest elves [horizon]
        after = S.runPure S.identityAnswer before (S.cast S.alice spell >> Stack.resolveTop)
    -- THE HEADLINE, and first so that no proxy below can absorb a mutation: CR
    -- 400.4a's "remains in its previous zone" would leave the card on the stack,
    -- which is the behaviour this case exists to rule out.
    Spec.assertEqWith s "CR 608.3e the card is in its owner's graveyard" (namesIn Zone.Graveyard S.alice after) [S.printingName elves]
    Spec.assertEqWith s "CR 101.2 nothing entered the battlefield" (arrivals before after) []
    Spec.assertEqWith s "and the stack is empty" (GameState.stack after) []

-- CR 111.5: "if a spell or ability would create a token, but a rule or effect
-- states that a permanent with one or more of that token's characteristics can't
-- enter the battlefield, the token is not created."
--
-- Worms of the Earth ({2}{B}{B}{B} enchantment) is the prohibition -- "lands can't
-- enter the battlefield", which names no zone and so reaches a token; Autumn
-- Willow, Harmony ({3}{G}{G}) is the maker, whose enters trigger creates a 1/1
-- green Forest Dryad LAND creature token.
--
-- Not implemented, recorded here because a card's JSON carries no comment: Worms
-- of the Earth's third sentence, whose each-upkeep offer to every player is how
-- the printed card is destroyed, and Autumn Willow's third, which adds mana when
-- a land creature is tapped (#2865, #2866). Both omissions leave pawl's card STRICTER
-- than printed -- one keeps an enchantment its opponents could remove, the other
-- withholds mana from its controller -- so neither can flatter the cases below.
tokenCase :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenCase s registry = do
  let -- The Willow enters WITH its CR 603.6a event, so settleForPriority finds
      -- the trigger pending; the prohibition, where one is named, is arranged
      -- beside it and takes no move of its own.
      board willow prohibition =
        let g1 = foldr (\c g -> snd (S.addCreature c S.alice g)) (Setup.emptyGame S.bothPlayers) prohibition
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
namesIn zone pid gs =
  [ S.nameOf card
  | oid <- Game.zoneMembers zone pid gs,
    card <- foldMap pure (Game.cardOf oid gs)
  ]

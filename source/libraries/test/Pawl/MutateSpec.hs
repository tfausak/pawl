{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 702.140's mutate and CR 730's merged permanents: Pawl.Types.Keyword's
-- Mutate arm, the target slot and cost Pawl.Engine.Keyword mints from it,
-- Pawl.Engine.Cast's CR 601.2b stamp and CR 601.2c target, Pawl.Engine.Stack's
-- CR 702.140b/702.140c fork, Pawl.Engine.Event.merge, Pawl.Types.Source's
-- OfMerge arm and the projection read Pawl.Engine.Projection.View's
-- withMergedAbilities adds for CR 702.140e. CR 730.2d's token components come
-- through Pawl.Types.MergeComponent, whose two arms are what a merged
-- permanent's token-ness and CR 730.3's departure each read.
--
-- Cubwarden is the producer: {3}{W} 3\/5 Creature -- Cat, "Mutate {2}{W}{W}",
-- lifelink, "Whenever this creature mutates, create two 1\/1 white Cat creature
-- tokens with lifelink". One existing keyword makes the topmost component's
-- ability observable, and the trigger makes rule 702.140d's own event
-- observable.
--
-- Falcon Abomination is the creature it merges with: {2}{U} 2\/2 Creature --
-- Bird Zombie, flying, "When this creature enters, create a 2\/2 black Zombie
-- creature token with decayed". Non-Human (CR 702.140a), a DIFFERENT name, box
-- and subtypes from Cubwarden (CR 730.2a), one keyword of its own (CR 702.140e)
-- and an enters-the-battlefield trigger whose token is what CR 730.2b's "isn't
-- considered to have just entered the battlefield" is read off. The two token
-- kinds are distinct, so one count can never stand in for the other.
module Pawl.MutateSpec where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.MeldSource as MeldSource
import qualified Pawl.Types.MergeComponent as MergeComponent
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MutateSide as MutateSide
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Mutate" $ do
  -- THE case CR 730.2b exists for. A merge implemented as an entry passes every
  -- other assertion here -- the characteristics, the added abilities, the
  -- departure -- and differs in exactly two readings: the under-component's
  -- enters-the-battlefield trigger, and whether the permanent may attack the
  -- turn it merged. Both are asserted, and the trigger one first, since haste
  -- would rescue the attack half on its own.
  Spec.it s "CR 730.2b/730.2c mutating over a creature is not an entry: no enters trigger, and the permanent may still attack" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, board, spellId) = mutateBoard plains falcon cubwarden
        after = merging MutateSide.Over host board spellId
    Spec.assertEqWith
      s
      "CR 730.2b the under component's enters-the-battlefield trigger did not fire"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Zombie Token")) S.alice after)
      0
    Spec.assertBool
      s
      (Combat.canAttack S.alice host after)
      "CR 730.2c the merged permanent kept the summoning sickness it did not have, so it may attack"
    Spec.assertEqWith
      s
      "CR 702.140d the mutate trigger fired, and made two Cats"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Cat Token")) S.alice after)
      2
    -- The paired board, differing in ONE thing: the creature merged with was
    -- summoning sick. CR 730.2c carries that across the merge too, so the
    -- attack assertion above is about what the permanent brought rather than
    -- about anything the merge granted.
    let sick = merging MutateSide.Over host (sickened host board) spellId
    Spec.assertBool
      s
      (not (Combat.canAttack S.alice host sick))
      "CR 730.2c and a permanent that WAS summoning sick still is after merging"
    -- The proxies, after the behaviours: the merge really happened, and the
    -- creature really was settled on the board the attack assertion read.
    Spec.assertEqWith s "the two cards represent one permanent" (componentNames host after) [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Falcon Abomination")]
    Spec.assertEqWith s "and nothing is left on the stack" (length (GameState.stack after)) 0
    Spec.assertEqWith s "setup: the creature merged with was settled" (fmap Object.sickness (Game.lookupObject host board)) (Just (Sickness.Settled S.alice))
    Spec.assertEqWith s "setup: no Cat token was on the battlefield before the merge" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Cat Token")) S.alice board) 0
  -- CR 730.2a's first sentence and CR 702.140e's second, on the same permanent:
  -- every characteristic but the abilities is the TOPMOST component's, and the
  -- abilities are every component's.
  Spec.it s "CR 730.2a/702.140e mutating over: the topmost component's name, types and box, plus the abilities from under it" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, board, spellId) = mutateBoard plains falcon cubwarden
        after = merging MutateSide.Over host board spellId
    Spec.assertEqWith s "CR 730.2a the merged permanent is named Cubwarden" (fmap S.nameOf (Game.cardOf host after)) (Just (CardName.MkCardName (Text.pack "Cubwarden")))
    Spec.assertEqWith s "CR 730.2a with Cubwarden's subtypes" (Projection.subtypesOf host after) (Set.singleton Subtype.Cat)
    Spec.assertEqWith s "CR 730.2a and Cubwarden's power and toughness" (S.powerToughnessOf host after) (Just (3, 5))
    Spec.assertBool s (Projection.hasKeyword Keyword.Flying host after) "CR 702.140e and it has flying, which only the component under it prints"
    Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink host after) "and lifelink, which the topmost one prints"
    -- The proxy behind the flying assertion, after it: the creature had no
    -- flying of Cubwarden's own to inherit, so the keyword came from under.
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Lifelink host board)) "setup: it had no lifelink before the merge"
  -- The same spell put on the OTHER side, which is what makes the case above a
  -- fact about CR 702.140c's choice rather than about the card. Every
  -- characteristic swaps and the ability union does not -- and rule 702.140e's
  -- union is what lets Cubwarden's own trigger fire from underneath at all.
  Spec.it s "CR 702.140c/730.2a mutating under: the other component's characteristics, and the trigger still fires from below" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, board, spellId) = mutateBoard plains falcon cubwarden
        after = merging MutateSide.Under host board spellId
    Spec.assertEqWith s "CR 730.2a the merged permanent is named Falcon Abomination" (fmap S.nameOf (Game.cardOf host after)) (Just (CardName.MkCardName (Text.pack "Falcon Abomination")))
    Spec.assertEqWith s "CR 730.2a with the Bird Zombie subtypes" (Projection.subtypesOf host after) (Set.fromList [Subtype.Bird, Subtype.Zombie])
    Spec.assertEqWith s "CR 730.2a and its 2\\/2 box" (S.powerToughnessOf host after) (Just (2, 2))
    Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink host after) "CR 702.140e and it has lifelink, which only the component under it prints"
    Spec.assertEqWith
      s
      "CR 702.140d/702.140e the mutate trigger fired from underneath, and made two Cats"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Cat Token")) S.alice after)
      2
    Spec.assertEqWith
      s
      "CR 730.2b and the under component's enters trigger still did not fire"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Zombie Token")) S.alice after)
      0
    -- The proxy, after them: the component order is the reverse of the Over
    -- case's, which is the whole of what the two boards differ by.
    Spec.assertEqWith s "the components are the other way up" (componentNames host after) [CardName.MkCardName (Text.pack "Falcon Abomination"), CardName.MkCardName (Text.pack "Cubwarden")]
  -- CR 702.140b, the other half of rule 702.140c's fork: the target became
  -- illegal between the announcement and the resolution, so the spell ceases to
  -- be a mutating creature spell and resolves as the creature spell it also is.
  --
  -- The target is made a HUMAN rather than removed from the battlefield, which
  -- is what makes the rule observable: a target that is GONE stops the merge on
  -- its own (Event.merge has nothing to look up), so both readings agree there.
  -- A live creature the mutate slot no longer admits is the board that tells
  -- rule 702.140b's clear from its absence.
  Spec.it s "CR 702.140b a mutating creature spell whose target became illegal resolves as an ordinary creature spell" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, board, spellId) = mutateBoard plains falcon cubwarden
        cast = S.runPure (mutatingAt MutateSide.Over host) board (S.cast S.alice spellId)
        turned = S.withEffect host (Modification.AddSubtype Subtype.Human) cast
        after = S.runPure (mutatingAt MutateSide.Over host) turned (Monad.replicateM_ 6 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)
    Spec.assertEqWith
      s
      "CR 702.140b nothing merged with the creature that became a Human"
      (componentNames host after)
      []
    Spec.assertEqWith
      s
      "CR 702.140b and nothing mutated, so no Cat token was made"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Cat Token")) S.alice after)
      0
    Spec.assertEqWith
      s
      "CR 702.140b while the spell itself resolved, entering the battlefield on its own"
      (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Cubwarden")) S.alice after)
      1
    -- The proxy, after them: the target really is a Human on the board that
    -- resolved, which is the whole of rule 702.140b's condition.
    Spec.assertBool s (Set.member Subtype.Human (Projection.subtypesOf host turned)) "setup: the target was a Human when the spell began resolving"
  -- CR 730.2a's timestamp sentence, which is the one board it is observable on:
  -- the merge and the copy effect already on the target share layer 1a (CR
  -- 613.2a) and CR 613.7 orders them by timestamp, so the merge -- timestamped
  -- at the merge, always later -- wins. A merge that let the stamped copy
  -- snapshot stand answers every OTHER case in this group correctly and differs
  -- here and only here; see #3371.
  --
  -- The Clone is a copy of Falcon Abomination, so the fold is also read over
  -- RECORDS rather than over printed faces: flying is in the copy's rules text
  -- and nowhere on the Clone card, and Clone's own 0\/0 box and copy ability are
  -- what a printed-face fold would have contributed instead.
  Spec.it s "CR 730.2a/613.2a the merge outranks a copy effect already on the target" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    clone <- S.printingOf s registry "Clone"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (original, withFalcon) = S.addPermanent falcon S.alice (Setup.emptyGame S.bothPlayers)
        (_, staged) = S.spellOnStack clone S.alice withFalcon
        copied = S.runPure (copying original) staged (Stack.resolveTop >> Engine.settleForPriority)
    case cloneOn copied of
      Nothing -> Spec.assertFailure s "the Clone should have entered as a copy of Falcon Abomination"
      Just host -> do
        let (board, spellId) = S.handOne cubwarden (S.landsFor plains S.alice 4 copied)
            after = merging MutateSide.Over host board spellId
        -- The PROJECTION's name, not Game.cardOf's: a copy effect rewrites no
        -- Source, so the card behind this object is the Clone before the merge
        -- and Cubwarden after it whichever layer-1a effect won. CR 709.4a's
        -- projected set is the read that tells them apart.
        Spec.assertEqWith s "CR 730.2a the merged permanent is named Cubwarden, not the copied Falcon Abomination" (Projection.namesOf host after) (Set.singleton (CardName.MkCardName (Text.pack "Cubwarden")))
        Spec.assertEqWith
          s
          "CR 702.140d and the mutate trigger the copy snapshot was hiding fired, making two Cats"
          (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Cat Token")) S.alice after)
          2
        Spec.assertEqWith s "CR 730.2a with Cubwarden's box rather than Clone's 0\\/0" (S.powerToughnessOf host after) (Just (3, 5))
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying host after) "CR 702.140e and flying, which only the COPY under it has -- the Clone card prints none"
        Spec.assertBool s (Projection.hasKeyword Keyword.Lifelink host after) "and lifelink from the topmost component"
        -- The proxies, after the behaviours: the target really was a copy of
        -- Falcon Abomination when the spell merged with it, and it really is the
        -- Clone rather than the original.
        Spec.assertEqWith s "setup: the target projected the name Falcon Abomination before the merge" (Projection.namesOf host board) (Set.singleton (CardName.MkCardName (Text.pack "Falcon Abomination")))
        Spec.assertBool s (host /= original) "setup: and it is the Clone, not the creature it copied"
        Spec.assertEqWith s "setup: the copied original is untouched" (Projection.namesOf original after) (Set.singleton (CardName.MkCardName (Text.pack "Falcon Abomination")))
  -- CR 702.140e read by the gatherer rather than by the projection: a static
  -- ability under the topmost component has to reach Projection.permanentParts,
  -- which walks Projection.View.staticAbilitiesOf and not the seed record. Lord
  -- of Atlantis is the producer -- "other Merfolk get +1\/+1 and have
  -- islandwalk" -- and Merfolk Seer, which prints neither, is what reads it
  -- back; see #3371.
  Spec.it s "CR 702.140e a static ability under the topmost component still applies" $ do
    plains <- S.printingOf s registry "Plains"
    lord <- S.printingOf s registry "Lord of Atlantis"
    seer <- S.printingOf s registry "Merfolk Seer"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, withLord) = S.addPermanent lord S.alice (Setup.emptyGame S.bothPlayers)
        (other, base) = S.addPermanent seer S.alice withLord
        (board, spellId) = S.handOne cubwarden (S.landsFor plains S.alice 4 base)
        after = merging MutateSide.Over host board spellId
    Spec.assertEqWith s "CR 702.140e the other Merfolk is still 3/3" (S.powerToughnessOf other after) (Just (3, 3))
    Spec.assertBool s (Projection.hasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Island)) other after) "CR 702.140e and still has islandwalk"
    -- The BEFORE half of the pair, after the behaviour: the buff was there to
    -- lose, so the two assertions above are about the merge keeping it rather
    -- than about a board that never had it.
    Spec.assertEqWith s "setup: the other Merfolk was 3/3 before the merge" (S.powerToughnessOf other board) (Just (3, 3))
    Spec.assertEqWith s "setup: and the merged permanent is Cubwarden, so the ability is not its own printed one" (fmap S.nameOf (Game.cardOf host after)) (Just (CardName.MkCardName (Text.pack "Cubwarden")))
    Spec.assertBool s (not (Projection.hasKeyword (Keyword.Landwalk (Filter.Type.HasSubtype Subtype.Island)) host after)) "setup: and the merged permanent is no Merfolk, so it does not buff itself"
  -- CR 702.140e again, through the OTHER reader family the seed record does not
  -- feed: Projection.replacementsAffecting's copiable short-circuit. Corpsejack
  -- Menace is the producer -- "if one or more +1/+1 counters would be put on a
  -- creature you control, twice that many are put instead"; see #3371.
  Spec.it s "CR 702.140e a replacement effect under the topmost component still applies" $ do
    plains <- S.printingOf s registry "Plains"
    menace <- S.printingOf s registry "Corpsejack Menace"
    seer <- S.printingOf s registry "Merfolk Seer"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, withMenace) = S.addPermanent menace S.alice (Setup.emptyGame S.bothPlayers)
        (other, base) = S.addPermanent seer S.alice withMenace
        (board, spellId) = S.handOne cubwarden (S.landsFor plains S.alice 4 base)
        after = merging MutateSide.Over host board spellId
        counted gs = S.runPure S.identityAnswer gs (Monad.void (Event.putCounters (CounterCause.ByEffect S.alice) other CounterKind.PlusOnePlusOne 1))
        countersOn gs = Map.lookup CounterKind.PlusOnePlusOne (maybe Map.empty Object.counters (Game.lookupObject other gs))
    Spec.assertEqWith s "CR 702.140e one +1/+1 counter is still doubled after the merge" (countersOn (counted after)) (Just 2)
    -- The BEFORE half of the pair, after the behaviour.
    Spec.assertEqWith s "setup: it was doubled before the merge too" (countersOn (counted board)) (Just 2)
    Spec.assertEqWith s "setup: and the merged permanent is Cubwarden, which prints no replacement effect" (fmap S.nameOf (Game.cardOf host after)) (Just (CardName.MkCardName (Text.pack "Cubwarden")))
  -- CR 702.140c's choice is only put to a player where it decides something. A
  -- MELDED target is one the merge refuses (#874) -- CR 712.8g gives such a
  -- permanent only its combined back face, which is no component of it, so there
  -- is no component list to extend -- and the side would be answered and then
  -- thrown away, an elided rule showing up as a real decision. A pair of boards
  -- differing in exactly one thing: whether the creature the spell targets is
  -- represented by one card or by a meld; see #3371.
  --
  -- The melded permanent is HAND-BUILT, over Falcon Abomination's own printing so
  -- that CR 702.140a still admits it as a target: driving the pool's one meld
  -- pair belongs to Pawl.MeldSpec, and the question here is only which Source
  -- Event.mergeable refuses.
  Spec.it s "CR 702.140c a merge the engine will refuse asks for no side" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let base = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
        (cardTarget, withCard) = S.addPermanent falcon S.alice base
        (meldTarget, withMeldCard) = S.addPermanent falcon S.alice base
        (falconPid, interned) = Game.intern falcon withMeldCard
        (cubPid, bothInterned) = Game.intern cubwarden interned
        melded =
          Source.OfMeld
            MeldSource.MkMeldSource
              { MeldSource.result = falconPid,
                MeldSource.components = falconPid NonEmpty.:| [cubPid]
              }
        withMeld =
          bothInterned
            { GameState.objects = Map.adjust (\obj -> obj {Object.source = melded}) meldTarget (GameState.objects bothInterned)
            }
        counting :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State Int r
        counting host p = case p of
          Prompt.ChooseMutateSide {} -> do
            State.modify' (+ 1)
            pure MutateSide.Over
          _ -> pure (mutatingAt MutateSide.Over host p)
        asks host gs =
          let (board, spellId) = S.handOne cubwarden gs
              cast = S.runPure (mutatingAt MutateSide.Over host) board (S.cast S.alice spellId)
           in State.execState (Engine.runGame (counting host) cast (Monad.replicateM_ 6 (Engine.settleForPriority >> Stack.resolveTop))) 0
    Spec.assertEqWith s "CR 702.140c a melded target the merge refuses is asked no side" (asks meldTarget withMeld) 0
    Spec.assertEqWith s "while a one-card target is asked exactly one" (asks cardTarget withCard) 1
    -- The proxies, after the pair: the melded permanent really was a legal target
    -- of the spell, so the 0 above is the refusal and not an unfillable slot.
    Spec.assertBool s (Projection.isCreatureOf meldTarget withMeld) "setup: the melded permanent is a creature"
    Spec.assertEqWith s "setup: which alice owns" (fmap Object.owner (Game.lookupObject meldTarget withMeld)) (Just S.alice)
    Spec.assertBool s (not (Set.member Subtype.Human (Projection.subtypesOf meldTarget withMeld))) "setup: and is no Human"
  -- CR 730.2d: "if a merged permanent contains a token, the resulting permanent
  -- is a token only if the topmost component is a token". A pair of boards
  -- differing in exactly one thing -- which side of the token Cubwarden goes on
  -- -- so an implementation reading ANY component, or reading none, answers both
  -- alike and neither pair member is about the merge refusing.
  --
  -- The token is one a CARD made: Cubwarden's own CR 702.140d trigger creates
  -- two 1/1 white Cat tokens with lifelink, and a Cat is no Human, so rule
  -- 702.140a admits one as the second Cubwarden's mutate target.
  --
  -- Ashaya, Soul of the Wild ({3}{G}{G} Creature -- Elemental) is what READS the
  -- answer at gameplay level: "each nontoken creature you control is a Forest
  -- land in addition to its other types", a CR 613.1d type-changing effect that
  -- passes a token by.
  Spec.it s "CR 730.2d a merged permanent is a token only if its topmost component is" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    ashaya <- S.printingOf s registry "Ashaya, Soul of the Wild"
    let base = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent falcon S.alice base
        (_, withAshaya) = S.addPermanent ashaya S.alice withHost
        (firstBoard, firstSpell) = S.handOne cubwarden withAshaya
        catBoard = merging MutateSide.Over host firstBoard firstSpell
        cat = catId catBoard
        (secondBoard, secondSpell) = S.handOne cubwarden catBoard
        onto side = merging side cat secondBoard secondSpell
        isForest gs = Set.member Subtype.Forest (Projection.subtypesOf cat gs)
    Spec.assertBool s (isForest (onto MutateSide.Over)) "CR 730.2d a card over a token is topmost, so the merged permanent is no token and Ashaya makes it a Forest"
    Spec.assertBool s (not (isForest (onto MutateSide.Under))) "CR 730.2d while under it the token is topmost, so the merged permanent is a token and Ashaya passes it by"
    -- The proxies, after the behaviours: the same two cards represent the
    -- permanent either way, so the pair differs in the ORDER alone.
    Spec.assertEqWith s "the token and the card represent one permanent, the card on top" (componentNames cat (onto MutateSide.Over)) [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Cat Token")]
    Spec.assertEqWith s "and the other way under" (componentNames cat (onto MutateSide.Under)) [CardName.MkCardName (Text.pack "Cat Token"), CardName.MkCardName (Text.pack "Cubwarden")]
    Spec.assertBool s (not (isForest catBoard)) "setup: the Cat token was a token before anything merged with it, so Ashaya left it alone"
  -- CR 730.3 over a component list that holds a token: "each of the individual
  -- components are put into the appropriate zone", and CR 111.7 is what the
  -- appropriate zone comes to for a token -- it ceases to exist. Cubwarden's own
  -- Cat token is the component, and the card beside it is the control that says
  -- the graveyard was reachable at all.
  Spec.it s "CR 730.3/111.7 a merged permanent's token component ceases to exist while its card is put into the graveyard" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let base = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
        (host, withHost) = S.addPermanent falcon S.alice base
        (firstBoard, firstSpell) = S.handOne cubwarden withHost
        catBoard = merging MutateSide.Over host firstBoard firstSpell
        cat = catId catBoard
        (secondBoard, secondSpell) = S.handOne cubwarden catBoard
        merged = merging MutateSide.Over cat secondBoard secondSpell
        dead = S.runPure S.identityAnswer merged (Event.destroy Regenerability.Regenerable [cat] >> Engine.settleForPriority)
    Spec.assertBool s (notElem (CardName.MkCardName (Text.pack "Cat Token")) (graveyardNames dead)) "CR 111.7 the token component does not stay in the graveyard"
    Spec.assertBool s (elem (CardName.MkCardName (Text.pack "Cubwarden")) (graveyardNames dead)) "CR 730.3 while the card component is put there"
    Spec.assertEqWith s "CR 730.3 and the merged permanent itself is gone" (Game.lookupObject cat dead) Nothing
  -- CR 730.3, which is CR 712.21 restated for a merged permanent and which
  -- Pawl.Engine.Game.componentsOf answers for both. One permanent leaves and two
  -- cards arrive.
  Spec.it s "CR 730.3 a merged permanent dies as one permanent and arrives as two cards" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    let (host, board, spellId) = mutateBoard plains falcon cubwarden
        merged = merging MutateSide.Over host board spellId
        dead = S.runPure S.identityAnswer merged (Event.destroy Regenerability.Regenerable [host])
    Spec.assertEqWith
      s
      "CR 730.3 both cards are put into alice's graveyard"
      (List.sort (graveyardNames dead))
      (List.sort [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Falcon Abomination")])
    Spec.assertEqWith s "CR 730.3 and the merged permanent itself is gone" (Game.lookupObject host dead) Nothing
    Spec.assertEqWith s "setup: alice's graveyard was empty before it died" (graveyardNames merged) []
  -- CR 702.140a's two restrictions, as three casts off ONE board that differ in
  -- the creature the mutate slot is aimed at and in nothing else -- same mana,
  -- same timing, same stock -- so a negative cannot pass for want of a payment.
  -- The legal aim is the control that makes the other two mean something.
  Spec.it s "CR 702.140a a Human, and a creature another player owns, are not legal mutate targets" $ do
    plains <- S.printingOf s registry "Plains"
    falcon <- S.printingOf s registry "Falcon Abomination"
    cubwarden <- S.printingOf s registry "Cubwarden"
    evangel <- S.printingOf s registry "Cabal Evangel"
    piker <- S.printingOf s registry "Goblin Piker"
    let (host, base, spellId) = mutateBoard plains falcon cubwarden
        (humanId, withHuman) = S.addPermanent evangel S.alice base
        (theirsId, board) = S.addPermanent piker S.bob withHuman
        aimedAt victim = merging MutateSide.Over victim board spellId
    Spec.assertEqWith
      s
      "CR 702.140a a Human alice owns is not a legal target, so nothing merged with it"
      (componentNames humanId (aimedAt humanId))
      []
    Spec.assertEqWith
      s
      "CR 702.140a nor is a non-Human creature bob owns"
      (componentNames theirsId (aimedAt theirsId))
      []
    -- The control, on the SAME board and off the same four Plains: the one
    -- creature rule 702.140a admits does merge, so the two negatives above are
    -- about the filter rather than about the cast.
    Spec.assertEqWith
      s
      "CR 702.140a while the non-Human creature alice owns is, and merges"
      (componentNames host (aimedAt host))
      [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Falcon Abomination")]
    -- The proxies, after them: both rejected creatures really were creatures on
    -- the battlefield, so neither negative is about an empty board.
    Spec.assertBool s (humanId /= host && theirsId /= host) "setup: three distinct creatures were on the battlefield"
    Spec.assertEqWith s "setup: the rejected one alice owns is a Human" (Projection.subtypesOf humanId board) (Set.fromList [Subtype.Cleric, Subtype.Human])
    Spec.assertEqWith s "setup: and the other is a creature bob owns" (fmap Object.owner (Game.lookupObject theirsId board)) (Just S.bob)
    Spec.assertEqWith s "setup: which is a creature all the same" (Projection.subtypesOf theirsId board) (Set.fromList [Subtype.Goblin, Subtype.Warrior])
  -- CR 702.140e's second sentence read over the ability families CR 613.11
  -- applies OUTSIDE the layer system -- the twelve Pawl.Types.RuleAbilities
  -- carries. Every case below is a pair of boards differing in the UNDER
  -- component alone, so a refusal is the under component's printed sentence
  -- talking and not the merge breaking the declaration.
  --
  -- Silent Arbiter ({4} Artifact Creature -- Construct 1/5, "No more than one
  -- creature can attack each combat"): a Construct and no Human, so rule
  -- 702.140a admits it as a mutate target.
  Spec.it s "CR 702.140e a Silent Arbiter under a Cubwarden still holds alice to one attacker" $ do
    plains <- S.printingOf s registry "Plains"
    cubwarden <- S.printingOf s registry "Cubwarden"
    falcon <- S.printingOf s registry "Falcon Abomination"
    arbiter <- S.printingOf s registry "Silent Arbiter"
    piker <- S.printingOf s registry "Goblin Piker"
    let (host, board, spellId, one, two) = withTwoPikers piker (mutateBoard plains arbiter cubwarden)
        after = intoCombat (merging MutateSide.Over host board spellId)
        (cHost, cBoard, cSpell, cOne, cTwo) = withTwoPikers piker (mutateBoard plains falcon cubwarden)
        control = intoCombat (merging MutateSide.Over cHost cBoard cSpell)
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [one, two] after)) "CR 702.140e the merged permanent keeps the Arbiter's bound, so the two Pikers cannot attack together"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [one] after) "CR 508.1c the bound is a ceiling: either Piker alone still attacks"
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [two] after) "and so does the other"
    -- The paired board, differing in the under component alone: Falcon
    -- Abomination prints no combat restriction, so the same two Pikers attack.
    Spec.assertBool s (Combat.legalAttackDeclaration S.alice [cOne, cTwo] control) "CR 730.2a with a Falcon Abomination under it instead, the two attack together"
    -- The proxies, after the behaviours.
    Spec.assertEqWith s "the two cards represent one permanent" (componentNames host after) [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Silent Arbiter")]
    Spec.assertBool s (not (Combat.legalAttackDeclaration S.alice [one, two] (intoCombat board))) "setup: the unmerged Arbiter bound them already"
  -- Dormant Gomazoa ({1}{U}{U} Creature -- Jellyfish 0/4, "This creature doesn't
  -- untap during your untap step"), through CR 502.3's turn-based action itself.
  Spec.it s "CR 702.140e a Dormant Gomazoa under a Cubwarden still does not untap" $ do
    plains <- S.printingOf s registry "Plains"
    cubwarden <- S.printingOf s registry "Cubwarden"
    falcon <- S.printingOf s registry "Falcon Abomination"
    gomazoa <- S.printingOf s registry "Dormant Gomazoa"
    let untapping (h, b, sp) =
          let merged = S.tapObject h (merging MutateSide.Over h b sp)
           in (h, S.runPure S.identityAnswer merged (Engine.untapAll S.alice))
        (host, after) = untapping (mutateBoard plains gomazoa cubwarden)
        (cHost, control) = untapping (mutateBoard plains falcon cubwarden)
    Spec.assertEqWith s "CR 702.140e/502.3 the merged permanent keeps the Gomazoa's prohibition and stays tapped" (fmap Object.tapped (Game.lookupObject host after)) (Just TapState.Tapped)
    -- The paired board, differing in the under component alone.
    Spec.assertEqWith s "CR 502.3 with a Falcon Abomination under it instead, the same permanent untaps" (fmap Object.tapped (Game.lookupObject cHost control)) (Just TapState.Untapped)
    Spec.assertEqWith s "the two cards represent one permanent" (componentNames host after) [CardName.MkCardName (Text.pack "Cubwarden"), CardName.MkCardName (Text.pack "Dormant Gomazoa")]
  -- Prized Unicorn ({2}{G} Creature -- Unicorn 2/2, "All creatures able to block
  -- this creature do so"), through CR 509.1c's own gate on bob's declaration.
  Spec.it s "CR 702.140e a Prized Unicorn under a Cubwarden still makes bob block it" $ do
    plains <- S.printingOf s registry "Plains"
    cubwarden <- S.printingOf s registry "Cubwarden"
    unicorn <- S.printingOf s registry "Prized Unicorn"
    piker <- S.printingOf s registry "Goblin Piker"
    let attacking under =
          let (h, b, sp) = mutateBoard plains under cubwarden
              (blocker, withBlocker) = S.addPermanent piker S.bob b
              merged = intoCombat (merging MutateSide.Over h withBlocker sp)
           in (h, blocker, S.runPure S.aggressiveAnswer merged (Combat.declareAttackers S.manaPerformer S.alice))
        (host, blockerId, after) = attacking unicorn
        -- The paired board's under component is a Goblin Piker rather than the
        -- Falcon Abomination the cases above use: the Falcon prints flying, and
        -- a blocker that CANNOT block would make an empty declaration legal for
        -- the wrong reason.
        (cHost, _, control) = attacking piker
    Spec.assertBool s (not (Combat.legalBlockDeclaration S.bob Map.empty after)) "CR 702.140e the merged permanent keeps the Unicorn's requirement, so bob may not decline to block"
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob (Map.singleton blockerId (Set.singleton host)) after) "while blocking it is legal"
    -- The paired board, differing in the under component alone.
    Spec.assertBool s (Combat.legalBlockDeclaration S.bob Map.empty control) "CR 509.1c with a Goblin Piker under it instead, bob may decline"
    -- The proxies, after the behaviours: both merged permanents really attacked,
    -- so neither block question was asked of an empty combat.
    Spec.assertEqWith s "setup: the merged permanent attacked" (S.attackerDeclarationsOf after) [host]
    Spec.assertEqWith s "setup: and so did the control's" (S.attackerDeclarationsOf control) [cHost]
  -- Exalted Dragon ({5}{W} Creature -- Dragon 5/5, "This creature can't attack
  -- unless you sacrifice a land"), through CR 508.1h's payment during the
  -- declaration itself.
  Spec.it s "CR 702.140e an Exalted Dragon under a Cubwarden still charges a land to attack" $ do
    plains <- S.printingOf s registry "Plains"
    cubwarden <- S.printingOf s registry "Cubwarden"
    falcon <- S.printingOf s registry "Falcon Abomination"
    dragon <- S.printingOf s registry "Exalted Dragon"
    let attacking under =
          let (h, b, sp) = mutateBoard plains under cubwarden
              merged = intoCombat (merging MutateSide.Over h b sp)
           in (h, S.runPure S.aggressiveAnswer merged (Combat.declareAttackers S.manaPerformer S.alice))
        (host, after) = attacking dragon
        (cHost, control) = attacking falcon
    Spec.assertEqWith s "CR 702.140e/508.1h the merged permanent keeps the Dragon's toll: one of the four Plains was sacrificed" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Plains")) S.alice after) 3
    Spec.assertEqWith s "and it attacked all the same" (S.attackerDeclarationsOf after) [host]
    -- The paired board, differing in the under component alone.
    Spec.assertEqWith s "CR 508.1 with a Falcon Abomination under it instead, the four Plains all survive" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Plains")) S.alice control) 4
    Spec.assertEqWith s "and that one attacked too" (S.attackerDeclarationsOf control) [cHost]
  -- CR 603.7 read over a merged permanent: a delayed ability's DECLARATION is
  -- card data rather than a characteristic, so no projection carries it and the
  -- lookup walks the components' cards instead (Game.facesOfWithLastKnown).
  --
  -- Ivory Gargoyle ({4}{W} Creature -- Gargoyle 2/2, "When this creature dies,
  -- return it to the battlefield ... at the beginning of the next end step and
  -- you skip your next draw step"): a Gargoyle and no Human, and its dies trigger
  -- arms a delayed ability declared on ITS face and not on Cubwarden's.
  --
  -- The case stops at the ARM rather than at the return, where
  -- Pawl.LeavesTriggerSpec's ivoryGargoyleSpec stops and for that group's reason:
  -- CR 603.7e leaves the delayed ability the dead battlefield id, so its payload
  -- finds nothing to move once the end step arrives (#3173). What this proves is
  -- that the name resolved to text at all.
  Spec.it s "CR 603.7 an Ivory Gargoyle under a Cubwarden still arms its delayed ability" $ do
    plains <- S.printingOf s registry "Plains"
    cubwarden <- S.printingOf s registry "Cubwarden"
    falcon <- S.printingOf s registry "Falcon Abomination"
    gargoyle <- S.printingOf s registry "Ivory Gargoyle"
    let dying under =
          let (h, b, sp) = mutateBoard plains under cubwarden
              merged = merging MutateSide.Over h b sp
              killed = S.runPure S.identityAnswer merged (Event.destroy Regenerability.Regenerable [h])
              settled = S.runPure S.identityAnswer killed Engine.settleForPriority
           in (h, settled, S.runPure S.identityAnswer settled Stack.resolveTop)
        (host, placed, armed) = dying gargoyle
        (_, cPlaced, control) = dying falcon
    Spec.assertEqWith s "CR 603.7 resolving the under component's dies trigger armed its delayed ability" (Seq.length (GameState.delayedTriggers armed)) 1
    -- The paired board, differing in the under component alone: Falcon
    -- Abomination declares no delayed ability and has no dies trigger to arm one.
    Spec.assertEqWith s "CR 730.2a with a Falcon Abomination under it instead, nothing is armed" (Seq.length (GameState.delayedTriggers control)) 0
    -- The proxies, after the behaviour: the merged permanent really died, and its
    -- dies trigger really was the thing that resolved.
    Spec.assertEqWith s "setup: the merged permanent died" (Game.lookupObject host placed) Nothing
    Spec.assertEqWith s "setup: and its dies trigger was on the stack" (length (GameState.stack placed)) 1
    Spec.assertEqWith s "setup: while the control board put nothing there" (length (GameState.stack cPlaced)) 0

-- The merged board moved to CR 508.1's declaration, on S.combatBoardOf's terms:
-- alice active, bob the defending player (CR 506.2), and the beginning of combat
-- step already past. The merge itself has to happen in a main phase, so the two
-- cannot come from one fixture.
intoCombat :: GameState.GameState -> GameState.GameState
intoCombat gs =
  gs
    { GameState.activePlayer = S.alice,
      GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
      GameState.combat = Combat.emptyCombat {Combat.Type.defenders = [S.bob]},
      GameState.remaining = S.phasesAfter (Phase.Combat CombatStep.DeclareAttackers)
    }

-- mutateBoard with two more of alice's creatures on it, for the cases that ask
-- how many creatures may be declared rather than what one of them may do.
withTwoPikers :: Printing.Printing -> (ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId) -> (ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
withTwoPikers piker (host, board, spellId) =
  let (one, withOne) = S.addPermanent piker S.alice board
      (two, withTwo) = S.addPermanent piker S.alice withOne
   in (host, withTwo, spellId, one, two)

-- CR 614.12a's as-enters copy choice answered with `victim`, PINNED to that id
-- rather than searched for, so a mutation cannot be repaired by an answerer that
-- finds another eligible creature.
copying :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copying victim p = case p of
  Prompt.ChooseCopyTarget _ _ _ offered -> List.find (== victim) offered
  _ -> S.identityAnswer p

-- The Clone permanent, found by the printing behind it rather than by the name
-- it now shows: a copy of Falcon Abomination answers Game.cardOf with the copied
-- card, so a name search would find the original instead.
cloneOn :: GameState.GameState -> Maybe ObjectId.ObjectId
cloneOn gs =
  List.find
    (\oid -> fmap (S.nameOf . Printing.card) (Game.printingOfObject oid gs) == Just (CardName.MkCardName (Text.pack "Clone")))
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- The board every case above starts from: four Plains, one Falcon Abomination
-- settled on the battlefield, and Cubwarden in alice's hand. Four white mana is
-- exactly Cubwarden's mutate cost and its printed cost is {3}{W}, so BOTH
-- candidates are payable and only the announcement separates them.
mutateBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState, ObjectId.ObjectId)
mutateBoard plains falcon cubwarden =
  let base = S.landsFor plains S.alice 4 (Setup.emptyGame S.bothPlayers)
      (host, withHost) = S.addPermanent falcon S.alice base
      (board, spellId) = S.handOne cubwarden withHost
   in (host, board, spellId)

-- Cast Cubwarden for its mutate cost at `host`, put it on `side`, and drain the
-- stack: the spell first, then CR 702.140d's trigger, which CR 603.3 puts on the
-- stack at the next time a player would receive priority.
merging :: MutateSide.MutateSide -> ObjectId.ObjectId -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
merging side host board spellId =
  let cast = S.runPure (mutatingAt side host) board (S.cast S.alice spellId)
   in -- SIX passes over a stack of two, so every assertion reads a DRAINED
      -- stack: capping the resolutions at the expected count would let a
      -- trigger a wrong implementation added sit there unresolved, and the
      -- assertion that its effect did not happen would pass for that reason.
      -- Stack.resolveTop over an empty stack is a no-op.
      S.runPure (mutatingAt side host) cast (Monad.replicateM_ 6 (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)

-- CR 601.2b's announcement answered by NAMING the mutate cost rather than an
-- index, and CR 601.2c's target by FILTERING the offered set rather than
-- building a recipient: an answerer that hands back a Recipient.ToObject of the
-- same permanent is a different recipient, and CR 608.2b's re-read drops it
-- silently. CR 702.140c's side is the one thing two runs of this differ by.
mutatingAt :: MutateSide.MutateSide -> ObjectId.ObjectId -> Prompt.Prompt r -> r
mutatingAt side host p = case p of
  Prompt.ChooseCost _ _ _ candidates ->
    Maybe.fromMaybe (Cost.firstOffered candidates) (List.find ((== Just mutateCost) . Cost.Type.mana) candidates)
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((== Just host) . Recipient.objectOf) . snd) sets
  Prompt.ChooseMutateSide {} -> side
  _ -> S.identityAnswer p

-- Cubwarden's mutate cost, {2}{W}{W}, written as the announcement names it.
mutateCost :: ManaCost.ManaCost
mutateCost = ManaCost.MkManaCost [ManaSymbol.Generic 2, theWhite, theWhite]

theWhite :: ManaSymbol.ManaSymbol
theWhite = ManaSymbol.OfType (ManaType.Colored Color.White)

-- What CR 730.3's split puts into alice's graveyard, by name.
graveyardNames :: GameState.GameState -> [CardName.CardName]
graveyardNames gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers Zone.Graveyard S.alice gs)

-- The cards representing one permanent, top first, by name -- CR 730.2's order,
-- read through the classifier CR 712.21 and CR 730.3 share.
componentNames :: ObjectId.ObjectId -> GameState.GameState -> [CardName.CardName]
componentNames oid gs =
  foldMap
    (Maybe.mapMaybe (\component -> fmap (S.nameOf . Printing.card) (Game.printingOf (MergeComponent.printing component) gs)) . Foldable.toList . Game.componentsOf . Object.source)
    (Game.lookupObject oid gs)

-- The same board with one permanent summoning sick, which S.addPermanent does
-- not leave anything: CR 302.6's state is the paired boards' one difference.
sickened :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
sickened oid gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)
    }

-- The first of the two Cat tokens CR 702.140d's trigger created, which is the
-- token these cases merge with. By name, so nothing else on the board can stand
-- in for it.
catId :: GameState.GameState -> ObjectId.ObjectId
catId gs =
  Maybe.fromMaybe (ObjectId.MkObjectId 999) $
    List.find
      (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just (CardName.MkCardName (Text.pack "Cat Token")))
      (Game.zoneMembers Zone.Battlefield S.alice gs)

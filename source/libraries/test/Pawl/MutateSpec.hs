{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 702.140's mutate and CR 730's merged permanents: Pawl.Types.Keyword's
-- Mutate arm, the target slot and cost Pawl.Engine.Keyword mints from it,
-- Pawl.Engine.Cast's CR 601.2b stamp and CR 601.2c target, Pawl.Engine.Stack's
-- CR 702.140b/702.140c fork, Pawl.Engine.Event.merge, Pawl.Types.Source's
-- OfMerge arm and the projection read Pawl.Engine.Projection.View's
-- withMergedAbilities adds for CR 702.140e.
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
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.MutateSide as MutateSide
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Subtype as Subtype
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
    (Maybe.mapMaybe (\pid -> fmap (S.nameOf . Printing.card) (Game.printingOf pid gs)) . Foldable.toList . Game.componentsOf . Object.source)
    (Game.lookupObject oid gs)

-- The same board with one permanent summoning sick, which S.addPermanent does
-- not leave anything: CR 302.6's state is the paired boards' one difference.
sickened :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
sickened oid gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) oid (GameState.objects gs)
    }

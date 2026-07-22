{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Cross-cutting test fixtures, answerers, and assertions shared by two or more
-- spec modules -- the test suite's prelude. Imported "qualified ... as S"
-- everywhere (the one documented exception to alias-to-last-component; these
-- names appear on nearly every test line). Group-local helpers live with their
-- group, not here.
module Pawl.Support where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Card as Card
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Damage as Damage
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Expiry as Expiry
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified System.Random as Random

alice, bob :: PlayerId.PlayerId
alice = PlayerId.MkPlayerId 0
bob = PlayerId.MkPlayerId 1

bothPlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
bothPlayers = alice NonEmpty.:| [bob]

redRed :: Cards.Cards -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
redRed cards = Setup.mirror (Cards.redDeck cards) bothPlayers

greenBlack :: Cards.Cards -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
greenBlack cards = (alice, Cards.greenDeck cards) NonEmpty.:| [(bob, Cards.blackDeck cards)]

blueBlack :: Cards.Cards -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
blueBlack cards = (alice, Cards.blueDeck cards) NonEmpty.:| [(bob, Cards.blackDeck cards)]

matchups :: Cards.Cards -> [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]
matchups cards = [redRed cards, greenBlack cards, blueBlack cards]

-- A 60-basic-land mirror: no spell can be cast and no creature can attack, so the
-- only loss condition reachable is CR 704.5b deck-out. Used by the durable
-- lands-only-decks property.
landsOnly :: Cards.Cards -> NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
landsOnly cards = Setup.mirror (Deck.MkDeck (Map.singleton (Cards.mountainPrinting cards) 60)) bothPlayers

isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToPlayer _ -> False
  Recipient.ToObject _ -> False

-- Identity interpreter: shuffle returns ids unchanged; actions never occur here.
identityAnswer :: Prompt.Prompt r -> r
identityAnswer p = case p of
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0

-- Casts when legal, otherwise plays a land, otherwise passes.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          A.Cast _ -> True
          _ -> False
        isPlay a = case a of
          A.Play _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> case filter isPlay actions of
            h : _ -> h
            [] -> A.Pass
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0

-- Attacks with everything and blocks the first attacker with everything.
-- Deliberately maximal: it makes combat happen without the test having to
-- hand-build a Combat record.
aggressiveAnswer :: Prompt.Prompt r -> r
aggressiveAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction {} -> A.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (map (\b -> (b, a)) mine)
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0

-- Always plays a land when one is legal, otherwise passes.
playLandAnswer :: Prompt.Prompt r -> r
playLandAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.ChooseAction _ _ actions ->
    let isPlay a = case a of
          A.Play _ -> True
          A.Pass -> False
          A.Cast _ -> False
          A.Activate _ _ -> False
     in case filter isPlay actions of
          h : _ -> h
          [] -> A.Pass
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0

-- A StdGen-driven interpreter: random shuffle and random legal action.
randomAnswer :: Prompt.Prompt r -> State.State Random.StdGen r
randomAnswer p = case p of
  Prompt.DeclareAttackers _ _ ids -> do
    g <- State.get
    let (keep, g') = Random.uniformR (0, length ids) g
    State.put g'
    pure (take keep ids)
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> pure Map.empty
    a : _ -> do
      g <- State.get
      let (keep, g') = Random.uniformR (0, length mine) g
      State.put g'
      pure (Map.fromList (map (\b -> (b, a)) (take keep mine)))
  -- The damage division stays canonical rather than random: a random division
  -- would usually be illegal (it must total the attacker's power), and this
  -- property suite is not the place to test the rejection path.
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    pure $ case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.Shuffle ids -> do
    g <- State.get
    let (g1, g2) = Random.splitGen g
    State.put g2
    pure (shuffleWith g1 ids)
  Prompt.ChooseDiscard _ _ ids n -> pure (take (fromIntegral n) ids)
  Prompt.ChooseAction _ _ actions -> do
    g <- State.get
    let n = length actions
        (i, g') = Random.uniformR (0, max 0 (n - 1)) g
    State.put g'
    pure (pick actions (min (n - 1) (max 0 i)))
  Prompt.ChooseTargets _ _ _ sets ->
    let pickFrom s = do
          g <- State.get
          let xs = Set.toList s
              (i, g') = Random.uniformR (0, max 0 (length xs - 1)) g
          State.put g'
          pure $ case drop (min (max 0 i) (max 0 (length xs - 1))) xs of
            h : _ -> Just h
            [] -> Nothing
     in fmap (Map.mapMaybe id) (traverse pickFrom sets)
  Prompt.ChooseBasicLandTypes {} -> pure (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> pure Nothing
  Prompt.CastWhileSearching {} -> pure Nothing
  -- A small bounded X (0..3) so a variable-cost spell sometimes chooses nonzero,
  -- exercising substituteX under random play; payment rejects an unaffordable draw.
  Prompt.ChooseX {} -> do
    g <- State.get
    let (i, g') = Random.uniformR (0 :: Int, 3) g
    State.put g'
    pure (fromIntegral i)
  -- A deterministic prefix of the legal modes, keeping replay simple (the
  -- brief permits this in place of a genuinely random size-`count` subset).
  Prompt.ChooseModes _ _ _ legal count ->
    pure (Set.fromList (take (fromIntegral count) (Set.toAscList legal)))
  Prompt.ChooseCopyTarget {} -> pure Nothing
  Prompt.ChooseEntryOption {} -> pure 0
  Prompt.OrderTriggers _ _ sources -> pure (map fromIntegral (take (length sources) [0 :: Int ..]))
  Prompt.ChooseReplacement {} -> pure 0

-- Total index into a list; the engine always offers at least Pass, so the
-- fallback is unreachable in practice but keeps this free of partial functions.
pick :: [A.Action] -> Int -> A.Action
pick actions i = case drop i actions of
  h : _ -> h
  [] -> A.Pass

shuffleWith :: Random.StdGen -> [a] -> [a]
shuffleWith g xs =
  let unfoldInts :: Random.StdGen -> [Int]
      unfoldInts gen = let (v, gen') = Random.uniform gen in v : unfoldInts gen'
      insertByKey y ys = case ys of
        [] -> [y]
        z : zs -> if fst y <= fst z then y : z : zs else z : insertByKey y zs
      keys = take (length xs) (unfoldInts g)
   in map snd (foldr insertByKey [] (zip keys xs))

runRandomGame :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Int -> GameState.GameState
runRandomGame matchup s =
  snd (State.evalState (Engine.runMatch randomAnswer matchup) (Random.mkStdGen s))

-- A Piker put onto the battlefield under pid's control, untapped and Settled.
--
-- Any printing, on the battlefield under pid's control, untapped and Settled.
addCreature :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addCreature printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
          }
      )

-- Install a SetController continuous effect (CR 108.4) making pid the
-- controller of oid, and settle it (Sickness.Settled) so a test that exercises
-- control isolates control from summoning sickness.
giveControl :: ObjectId.ObjectId -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
giveControl oid pid gs =
  let (ts, g1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = oid,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = Modification.SetController pid,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
      settle o = o {Object.sickness = Sickness.Settled}
   in g1
        { GameState.continuousEffects = eff : GameState.continuousEffects g1,
          GameState.objects = Map.adjust settle oid (GameState.objects g1)
        }

addPiker :: Cards.Cards -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addPiker cards = addCreature (Cards.pikerPrinting cards)

-- Append a stored continuous effect affecting exactly `oid`, at timestamp `ts`.
-- Object id 998 is a stand-in source: nothing in these tests reads the
-- source's own characteristics. The general shape (ColorSpec, PowerToughnessSpec,
-- ProjectionSpec and ResolveSpec all grew their own copy of this before it moved
-- here).
withEffectAt :: ObjectId.ObjectId -> Timestamp.Timestamp -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffectAt oid ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 998,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

-- withEffectAt, allocating its own fresh timestamp -- the convenience shape for
-- a caller that doesn't care which timestamp the effect lands at.
withEffect :: ObjectId.ObjectId -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid m gs =
  let (ts, gs1) = Game.freshTimestamp gs
   in withEffectAt oid ts m gs1

-- The source stand-in for a targeting call whose spec is source-blind (every
-- spec but OpponentCreatureTarget). Object id 999 names nothing, the same
-- posture withEffectAt's 998 takes.
noSource :: ObjectId.ObjectId
noSource = ObjectId.MkObjectId 999

-- The M4h NonlandPermanentTarget fixture (CR 109.2/110.4): alice controls a Piker
-- (creature), a Mindslaver (a Legendary Artifact -- nonland, non-creature),
-- and a Mountain (land). Three permanents so a nonland-permanent legal set can
-- be distinguished from both "all permanents" and "creatures only".
boardWithCreatureArtifactLand :: Cards.Cards -> GameState.GameState
boardWithCreatureArtifactLand cards =
  let gs0 = Setup.emptyGame bothPlayers
      (_, gs1) = addPiker cards alice gs0
      (_, gs2) = addCreature (Cards.mindslaverPrinting cards) alice gs1
      (_, gs3) = addCreature (Cards.mountainPrinting cards) alice gs2
   in gs3

-- The creature on a `boardWithCreatureArtifactLand` board.
creatureId :: GameState.GameState -> ObjectId.ObjectId
creatureId gs = case filter (\oid -> Projection.isCreatureOf oid gs) (Set.toList (GameState.battlefield gs)) of
  oid : _ -> oid
  [] -> ObjectId.MkObjectId 999

-- The nonland, non-creature permanent (the artifact) on a
-- `boardWithCreatureArtifactLand` board.
artifactId :: GameState.GameState -> ObjectId.ObjectId
artifactId gs =
  let notLand oid = not (Set.member CardType.Land (Projection.cardTypesOf oid gs))
      notCreature oid = not (Projection.isCreatureOf oid gs)
      candidates = filter (\oid -> notLand oid && notCreature oid) (Set.toList (GameState.battlefield gs))
   in case candidates of
        oid : _ -> oid
        [] -> ObjectId.MkObjectId 999

-- A token built directly from effect-defined characteristics (Source.OfToken),
-- Settled so combat fixtures can attack/block with it. Bypasses createTokens; use
-- when a test needs a token on the board without resolving a maker.
addToken :: Card.Type.Card -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addToken card pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfToken card,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
          }
      )

-- One card of a printing in pid's library.
addLibraryCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addLibraryCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.library gs2)
          }
      )

-- One card of a printing in pid's graveyard.
addGraveyardCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addGraveyardCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Graveyard,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.graveyard = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.graveyard gs2)
          }
      )

-- One more card of a printing in pid's hand, APPENDED (contrast handOne, which
-- replaces the hand and sets up the phase for a cast).
addHandCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addHandCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )

-- Humility on the battlefield under bob's control (it is not a creature, so
-- AllCreatures does not touch it). Returns the updated state.
withHumility :: Cards.Cards -> GameState.GameState -> GameState.GameState
withHumility cards gs = snd (addCreature (Cards.humilityPrinting cards) bob gs)

-- alice controls n untapped basic lands of one printing, nothing else.
landsInPlay :: Printing.Printing -> Int -> GameState.GameState
landsInPlay land n =
  let add gs _ =
        let (oid, gs1) = Game.freshObjectId gs
            (ts, gs2) = Game.freshTimestamp gs1
            obj =
              Object.MkObject
                { Object.owner = alice,
                  Object.source = Source.OfCard land,
                  Object.zone = Zone.Battlefield,
                  Object.tapped = TapState.Untapped,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled,
                  Object.bindings = Map.empty,
                  Object.counters = Map.empty,
                  Object.timestamp = ts
                }
         in gs2
              { GameState.objects = Map.insert oid obj (GameState.objects gs2),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
              }
   in List.foldl' add (Setup.emptyGame bothPlayers) [1 .. n]

-- alice controls n untapped Mountains on the battlefield, nothing else.
mountainsInPlay :: Cards.Cards -> Int -> GameState.GameState
mountainsInPlay cards = landsInPlay (Cards.mountainPrinting cards)

-- Put one card of a printing into alice's hand in a main phase with priority.
handOne :: Printing.Printing -> GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handOne printing base =
  let (oid, gs1) = Game.freshObjectId base
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insert alice (Seq.singleton oid) (GameState.hand gs2),
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = alice,
            GameState.priority = Just alice
          },
        oid
      )

-- alice has n untapped Mountains in play and one Piker in hand, in a chosen phase.
pikerInHand :: Cards.Cards -> Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
pikerInHand cards n ph =
  let base = mountainsInPlay cards n
      (oid, gs1) = Game.freshObjectId base
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard (Cards.pikerPrinting cards),
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
      gs3 =
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insert alice (Seq.singleton oid) (GameState.hand gs2),
            GameState.phase = ph,
            GameState.activePlayer = alice,
            GameState.priority = Just alice
          }
   in (gs3, oid)

-- alice has n untapped Mountains in play and one Lightning Bolt in hand.
boltInHand :: Cards.Cards -> Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
boltInHand cards n ph =
  let (gs, oid) = handOne (Cards.lightningBoltPrinting cards) (mountainsInPlay cards n)
   in (gs {GameState.phase = ph}, oid)

-- alice is active with one Settled creature per printing in `mine`; bob defends
-- with one per printing in `theirs`. Returns their ids alongside the state, in
-- the order the printings were given.
combatBoardOf :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoardOf mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = addCreature p pid g in (ids ++ [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll alice mine (Setup.emptyGame bothPlayers)
      (yours, gs2) = addAll bob theirs gs1
   in ( gs2
          { GameState.activePlayer = alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            -- The steps after declare attackers, so a runStep-driven test (Tasks
            -- 2 and 4) can advance through combat. Direct-call tests ignore it.
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Ending EndingStep.EndStep,
                  Phase.Ending EndingStep.Cleanup
                ]
          },
        ours,
        yours
      )

-- alice is active with `a` Settled Pikers; bob defends with `b` Settled Pikers.
-- Returns the attackers' ids and the blockers' ids alongside the state.
combatBoard :: Cards.Cards -> Int -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoard cards a b = combatBoardOf (replicate a (Cards.pikerPrinting cards)) (replicate b (Cards.pikerPrinting cards))

-- Attack with everything, block per the given plan, then deal damage.
fightWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
fightWith answer gs =
  snd $ Engine.runGamePure answer gs $ do
    Combat.declareAttackers alice
    Combat.declareBlockers
    Damage.dealCombatDamage

-- Run a Game action purely under an answerer and keep only the final state. The
-- shape every direct-call test needs now that the change-and-emit funnels are
-- monadic (P5): `Event.destroy oid gs` becomes
-- `S.runPure S.identityAnswer gs (Event.destroy oid)`.
runPure :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> GameState.GameState
runPure answer gs game = snd (Engine.runGamePure answer gs game)

-- One CR 704 state-based-action pass, run purely. The direct replacement for the
-- pre-P5 pure `Sba.checkStateBasedActions gs`.
settleSba :: GameState.GameState -> GameState.GameState
settleSba gs = runPure identityAnswer gs Sba.checkStateBasedActions

-- Run whole steps through the engine while the current phase is in the combat
-- phase, stopping once combat is left or the game ends. Bounded so a bug cannot
-- loop forever.
runCombat :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runCombat answer gs0 =
  let go n g =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result g) || not (inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 24 gs0

inCombatPhase :: Phase.Phase -> Bool
inCombatPhase p = case p of
  Phase.Combat _ -> True
  _ -> False

lifeOf :: PlayerId.PlayerId -> GameState.GameState -> Maybe Integer
lifeOf pid gs = fmap Player.life (Map.lookup pid (GameState.players gs))

creaturesInPlay :: PlayerId.PlayerId -> GameState.GameState -> Int
creaturesInPlay pid gs =
  let isCreatureObject oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isCreature (Printing.card printing)
          Source.OfToken card -> Card.isCreature card
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
   in length (filter isCreatureObject (Game.zoneMembers Zone.Battlefield pid gs))

countByName :: Text.Text -> PlayerId.PlayerId -> GameState.GameState -> Int
countByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
          Source.OfToken card -> Card.Type.name card == wanted
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
      inLibrary = filter named (Game.zoneMembers Zone.Library pid gs)
      inHand = filter named (Game.zoneMembers Zone.Hand pid gs)
   in length inLibrary + length inHand

-- How many of pid's battlefield objects are copies of a card with this name.
countOnBattlefieldByName :: Text.Text -> PlayerId.PlayerId -> GameState.GameState -> Int
countOnBattlefieldByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
          Source.OfToken card -> Card.Type.name card == wanted
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
   in length (filter named (Game.zoneMembers Zone.Battlefield pid gs))

damageOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural.Natural
damageOf oid gs = fmap Object.damage (Game.lookupObject oid gs)

markDamage :: ObjectId.ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
markDamage oid n gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.damage = n}) oid (GameState.objects gs)}

-- The damage events recorded so far this turn, in order. Replaces the
-- GameState.damageEvents list P4 folded into the one turn-scoped log.
damageEventsOf :: GameState.GameState -> [DamageEvent.DamageEvent]
damageEventsOf gs = Maybe.mapMaybe Event.damageOf (Foldable.toList (GameState.events gs))

-- The zone changes recorded so far this turn, in order.
zoneChangesOf :: GameState.GameState -> [ZoneChange.ZoneChange]
zoneChangesOf gs = Maybe.mapMaybe Event.movedOf (Foldable.toList (GameState.events gs))

-- The characteristics of nothing: Projection.project on an id with no card in
-- Setup.emptyGame. The filler snapshot for a hand-built GameEvent.Moved whose
-- payload no assertion reads.
emptyCharacteristics :: PC.ProjectedCharacteristics
emptyCharacteristics = Projection.project (ObjectId.MkObjectId 999) (Setup.emptyGame bothPlayers)

-- A state carrying exactly one UNSCANNED event -- the hand-built-event fixture
-- shape a scan test needs (EventSpec and ModalSpec both build one).
withEvent :: GameEvent.GameEvent -> GameState.GameState -> GameState.GameState
withEvent event gs =
  gs
    { GameState.events = Seq.singleton event,
      GameState.scannedThrough = 0,
      GameState.damageScannedThrough = 0
    }

-- Seed a floating replacement directly into GameState (bypasses casting the
-- spell that would install it; use when a test needs one active without a
-- resolution).
addReplacement :: ActiveReplacement.ActiveReplacement -> GameState.GameState -> GameState.GameState
addReplacement active gs =
  gs {GameState.replacements = active : GameState.replacements gs}

-- Seed a regeneration shield directly (bypasses activating a regenerate ability;
-- use when a test needs a shield up without the activation). Since P5 a shield is
-- an ordinary floating replacement: CR 701.19a's "the next time ... this turn" is
-- exactly UntilEndOfTurn + Once.
addRegenShield :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addRegenShield oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      active =
        ActiveReplacement.MkActiveReplacement
          { ActiveReplacement.effect = ReplacementEffect.DestructionR DestructionRewrite.Regenerate,
            ActiveReplacement.source = oid,
            ActiveReplacement.timestamp = ts,
            ActiveReplacement.expiry = Expiry.AtCleanup,
            ActiveReplacement.uses = Uses.Once
          }
   in addReplacement active gs1

-- Set an object's tapped state to Tapped directly (bypasses a tap cost or
-- combat), so a test can set up a tapped permanent for an Untap effect.
tapObject :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tapObject oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}

-- Put `n` counters of a kind directly onto an object's per-incarnation state,
-- bypassing the PutCounters opcode -- so a projection or SBA test can set up
-- counters without resolving a spell.
addCounter :: CounterKind.CounterKind -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addCounter kind n oid gs =
  let bump obj = obj {Object.counters = Map.insertWith (+) kind n (Object.counters obj)}
   in gs {GameState.objects = Map.adjust bump oid (GameState.objects gs)}

tappedCount :: PlayerId.PlayerId -> GameState.GameState -> Int
tappedCount pid gs =
  let isTapped oid = case Game.lookupObject oid gs of
        Just obj -> Object.tapped obj == TapState.Tapped
        Nothing -> False
   in length (filter isTapped (Game.zoneMembers Zone.Battlefield pid gs))

handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

pikerCard :: Cards.Cards -> Card.Type.Card
pikerCard cards = Printing.card (Cards.pikerPrinting cards)

-- The printings M2a adds, paired with the single keyword each must carry.
m2aPrintings :: Cards.Cards -> [(Printing.Printing, Keyword.Keyword)]
m2aPrintings cards =
  [ (Cards.birdMaidenPrinting cards, Keyword.Flying),
    (Cards.nimbleBirdstickerPrinting cards, Keyword.Reach),
    (Cards.ogreSentryPrinting cards, Keyword.Defender),
    (Cards.windseekerCentaurPrinting cards, Keyword.Vigilance),
    (Cards.goblinChariotPrinting cards, Keyword.Haste)
  ]

-- A GameState with a single Mountain in alice's hand, in a chosen phase.
oneMountainState :: Cards.Cards -> Phase.Phase -> GameState.GameState
oneMountainState cards ph =
  let oid = ObjectId.MkObjectId 0
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard (Cards.mountainPrinting cards),
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = Timestamp.MkTimestamp 0
          }
   in GameState.MkGameState
        { GameState.objects = Map.singleton oid obj,
          GameState.library = Map.empty,
          GameState.hand = Map.singleton alice (Seq.singleton oid),
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.stack = [],
          GameState.players = Map.empty,
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.turnOrder = [alice],
          GameState.activePlayer = alice,
          GameState.phase = ph,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Just alice,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.nextObjectId = ObjectId.MkObjectId 1,
          GameState.nextTimestamp = Timestamp.MkTimestamp 1,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing
        }

drawStep :: Game.Type.Game ()
drawStep = Engine.runTurnBasedActions (Phase.Beginning BeginningStep.DrawStep)

-- bob's Piker on the battlefield; alice casts a Bolt at it under identityAnswer
-- (lookupMin prefers ToCreature over ToPlayer, and the Piker is the only
-- creature). Returns (pre-cast state, post-cast state, Bolt's hand id).
boltAtBobsPiker :: Cards.Cards -> (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
boltAtBobsPiker cards =
  let (_, withPiker) = addPiker cards bob (mountainsInPlay cards 1)
      (gs, oid) = handOne (Cards.lightningBoltPrinting cards) withPiker
   in (gs, snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid)), oid)

-- The single creature bob controls in a fixture built by (addPiker cards).
pikerOf :: GameState.GameState -> ObjectId.ObjectId
pikerOf gs = case Game.zoneMembers Zone.Battlefield bob gs of
  oid : _ -> oid
  [] -> ObjectId.MkObjectId 999

-- Put a fresh `printing` spell (owned by `pid`) onto the stack: a Stack-zone
-- object added to GameState.stack (mirrors EventSpec's inline placement). Used to
-- set up counter targets without paying to cast the victim.
spellOnStack :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
spellOnStack printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.stack = oid : GameState.stack gs2
          }
      )

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Cross-cutting test fixtures, answerers, and assertions shared by two or more
-- spec modules -- the test suite's prelude. Imported "qualified ... as S"
-- everywhere (the one documented exception to alias-to-last-component; these
-- names appear on nearly every test line). Group-local helpers live with their
-- group, not here.
module Pawl.Support where

import qualified Control.Monad.Trans.State.Strict as State
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
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone
import qualified System.Random as Random

alice, bob :: PlayerId.PlayerId
alice = PlayerId.MkPlayerId 0
bob = PlayerId.MkPlayerId 1

bothPlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
bothPlayers = alice NonEmpty.:| [bob]

redRed :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
redRed = Setup.mirror Cards.redDeck bothPlayers

greenBlack :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
greenBlack = (alice, Cards.greenDeck) NonEmpty.:| [(bob, Cards.blackDeck)]

matchups :: [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]
matchups = [redRed, greenBlack]

-- A 60-basic-land mirror: no spell can be cast and no creature can attack, so the
-- only loss condition reachable is CR 704.5b deck-out. Used by the durable
-- lands-only-decks property.
landsOnly :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)
landsOnly = Setup.mirror (Deck.MkDeck (Map.singleton Cards.mountainPrinting 60)) bothPlayers

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
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
          }
      )

addPiker :: PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addPiker = addCreature Cards.pikerPrinting

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
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.library gs2)
          }
      )

-- Humility on the battlefield under bob's control (it is not a creature, so
-- AllCreatures does not touch it). Returns the updated state.
withHumility :: GameState.GameState -> GameState.GameState
withHumility gs = snd (addCreature Cards.humilityPrinting bob gs)

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
                  Object.targets = Map.empty,
                  Object.chosenSubtypes = Map.empty,
                  Object.timestamp = ts
                }
         in gs2
              { GameState.objects = Map.insert oid obj (GameState.objects gs2),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
              }
   in List.foldl' add (Setup.emptyGame bothPlayers) [1 .. n]

-- alice controls n untapped Mountains on the battlefield, nothing else.
mountainsInPlay :: Int -> GameState.GameState
mountainsInPlay = landsInPlay Cards.mountainPrinting

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
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
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
pikerInHand :: Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
pikerInHand n ph =
  let base = mountainsInPlay n
      (oid, gs1) = Game.freshObjectId base
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard Cards.pikerPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
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
boltInHand :: Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
boltInHand n ph =
  let (gs, oid) = handOne Cards.lightningBoltPrinting (mountainsInPlay n)
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
combatBoard :: Int -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoard a b = combatBoardOf (replicate a Cards.pikerPrinting) (replicate b Cards.pikerPrinting)

-- Attack with everything, block per the given plan, then deal damage.
fightWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
fightWith answer gs =
  snd $ Engine.runGamePure answer gs $ do
    Combat.declareAttackers alice
    Combat.declareBlockers
    Damage.dealCombatDamage

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
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
   in length (filter isCreatureObject (Game.zoneMembers Zone.Battlefield pid gs))

countByName :: Text.Text -> PlayerId.PlayerId -> GameState.GameState -> Int
countByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
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
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
   in length (filter named (Game.zoneMembers Zone.Battlefield pid gs))

damageOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural.Natural
damageOf oid gs = fmap Object.damage (Game.lookupObject oid gs)

markDamage :: ObjectId.ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
markDamage oid n gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.damage = n}) oid (GameState.objects gs)}

tappedCount :: PlayerId.PlayerId -> GameState.GameState -> Int
tappedCount pid gs =
  let isTapped oid = case Game.lookupObject oid gs of
        Just obj -> Object.tapped obj == TapState.Tapped
        Nothing -> False
   in length (filter isTapped (Game.zoneMembers Zone.Battlefield pid gs))

handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

pikerCard :: Card.Type.Card
pikerCard = Printing.card Cards.pikerPrinting

-- The printings M2a adds, paired with the single keyword each must carry.
m2aPrintings :: [(Printing.Printing, Keyword.Keyword)]
m2aPrintings =
  [ (Cards.birdMaidenPrinting, Keyword.Flying),
    (Cards.nimbleBirdstickerPrinting, Keyword.Reach),
    (Cards.ogreSentryPrinting, Keyword.Defender),
    (Cards.windseekerCentaurPrinting, Keyword.Vigilance),
    (Cards.goblinChariotPrinting, Keyword.Haste)
  ]

-- A GameState with a single Mountain in alice's hand, in a chosen phase.
oneMountainState :: Phase.Phase -> GameState.GameState
oneMountainState ph =
  let oid = ObjectId.MkObjectId 0
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard Cards.mountainPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
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
          GameState.damageEvents = [],
          GameState.zoneChanges = [],
          GameState.continuousEffects = [],
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
boltAtBobsPiker :: (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
boltAtBobsPiker =
  let (_, withPiker) = addPiker bob (mountainsInPlay 1)
      (gs, oid) = handOne Cards.lightningBoltPrinting withPiker
   in (gs, snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid)), oid)

-- The single creature bob controls in a fixture built by addPiker.
pikerOf :: GameState.GameState -> ObjectId.ObjectId
pikerOf gs = case Game.zoneMembers Zone.Battlefield bob gs of
  oid : _ -> oid
  [] -> ObjectId.MkObjectId 999

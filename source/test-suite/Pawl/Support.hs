{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Cross-cutting test fixtures, answerers, and assertions shared by two or more
-- spec modules -- the test suite's prelude. Imported "qualified ... as S"
-- everywhere (the one documented exception to alias-to-last-component; these
-- names appear on nearly every test line). Group-local helpers live with their
-- group, not here.
module Pawl.Support where

import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Numeric.Natural (Natural)
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Sba as Sba
import qualified Pawl.Engine.Script as Script
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.HandActionPerformer as HandActionPerformer
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Types.RestartSignal as RestartSignal
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Uses as Uses
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified System.Directory as Directory

alice, bob, carol, dave :: PlayerId.PlayerId
alice = PlayerId.MkPlayerId 0
bob = PlayerId.MkPlayerId 1
carol = PlayerId.MkPlayerId 2
dave = PlayerId.MkPlayerId 3

bothPlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
bothPlayers = alice NonEmpty.:| [bob]

-- CR 800.1: a multiplayer game is one that BEGINS with more than two players.
-- The two-player suite is not generalized -- two-player Magic is a supported,
-- correct configuration with its own rules (CR 102.2, CR 103.8a) and the
-- assertions describing it are accurate. A third seat is added alongside.
threePlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
threePlayers = alice NonEmpty.:| [bob, carol]

-- The deckless three-seat board: turn order [alice, bob, carol], alice active,
-- all three still playing. The base for unit-level departure, turn-order and
-- priority tests, mirroring what `Setup.emptyGame bothPlayers` is for two.
threePlayerGame :: GameState.GameState
threePlayerGame = Setup.emptyGame threePlayers

-- A fourth seat, alongside threePlayers, for cases where three seats cannot
-- distinguish two candidate answers (a departure walk with only one
-- still-playing seat left has nowhere else to land, whoever it's anchored on).
fourPlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
fourPlayers = alice NonEmpty.:| [bob, carol, dave]

-- The deckless four-seat board: turn order [alice, bob, carol, dave], alice
-- active, all four still playing. Mirrors threePlayerGame for a fourth seat.
fourPlayerGame :: GameState.GameState
fourPlayerGame = Setup.emptyGame fourPlayers

-- A throwaway directory holding `files` (name, contents), for the cases that
-- need a corpus other than the committed one. The label keeps concurrently
-- running cases in separate directories, since tasty runs them in parallel.
withCorpusDir :: String -> [(FilePath, Text.Text)] -> (FilePath -> IO a) -> IO a
withCorpusDir label files action = do
  tmp <- Directory.getTemporaryDirectory
  let dir = tmp <> "/pawl-corpus-" <> label
  Exception.bracket_
    ( do
        Directory.createDirectoryIfMissing True dir
        mapM_ (\(name, contents) -> TextIO.writeFile (dir <> "/" <> name) contents) files
    )
    (Directory.removeDirectoryRecursive dir)
    (action dir)

-- The committed Goblin Piker file, used as a known-good card in a throwaway
-- corpus. Read rather than inlined so no spec becomes a second source of truth
-- for a card's contents.
pikerJson :: IO Text.Text
pikerJson = do
  root <- Registry.defaultRoot
  TextIO.readFile (root <> "/goblin-piker.json")

-- The committed Wax // Wane file, pikerJson's two-faced counterpart: the only
-- card in the pool whose faces have names of their own (CR 709.4a), and so the
-- one a throwaway corpus needs to exercise a by-either-name lookup.
waxWaneJson :: IO Text.Text
waxWaneJson = do
  root <- Registry.defaultRoot
  TextIO.readFile (root <> "/wax-wane.json")

printingOf :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m Printing.Printing
printingOf s registry = fmap Printing.MkPrinting . cardOf s registry

-- How a spec case fetches a card: a Nothing becomes an assertion failure naming
-- the card, so a missing card fails the case that wanted it rather than
-- escaping as an exception. This is the adapter for everything inside a
-- Spec.it, and the reason those modules need no IO.
cardOf :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> String -> m Card.Type.Card
cardOf s registry name = do
  result <- Registry.named registry name
  case result of
    Nothing -> Spec.assertFailure s ("no such card: " <> name)
    Just card -> pure card

redRed :: (Monad m) => Cards.Fetch m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
redRed fetch = do
  deck <- Cards.redDeck fetch
  pure (Setup.mirror deck bothPlayers)

greenBlack :: (Monad m) => Cards.Fetch m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
greenBlack fetch = do
  green <- Cards.greenDeck fetch
  black <- Cards.blackDeck fetch
  pure ((alice, green) NonEmpty.:| [(bob, black)])

-- A 60-basic-land mirror: no spell can be cast and no creature can attack, so the
-- only loss condition reachable is CR 704.5b deck-out. EngineSpec plays it out.
landsOnly :: (Monad m) => Cards.Fetch m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
landsOnly fetch = do
  mountain <- fetch "Mountain"
  pure (Setup.mirror (Deck.fromCards (Map.singleton mountain 60)) bothPlayers)

-- CR 800.1: the three-seat twin of landsOnly. 60 basic lands each, so the only
-- reachable loss condition is CR 704.5b deck-out and the only reachable end is
-- CR 104.2a's last player standing. The seat count is what makes it a falsifier:
-- at two players the first deck-out ends the game, at three it must not.
threePlayerLandsOnly :: (Monad m) => Cards.Fetch m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
threePlayerLandsOnly fetch = do
  mountain <- fetch "Mountain"
  pure (Setup.mirror (Deck.fromCards (Map.singleton mountain 60)) threePlayers)

-- CR 800.1: the three-seat twin of redRed -- one red deck each for alice, bob and
-- carol. Setup.mirror is already NonEmpty-shaped, so the seat count is the only
-- difference. The three-seat setup rules (CR 103.5c's free first mulligan, CR
-- 103.8c's first draw) are what this exists to exercise.
threeWayMirror :: (Monad m) => Cards.Fetch m -> m (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
threeWayMirror fetch = do
  deck <- Cards.redDeck fetch
  pure (Setup.mirror deck threePlayers)

isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToPlaneswalker _ -> False
  Recipient.ToBattle _ -> False
  Recipient.ToPlayer _ -> False
  Recipient.ToObject _ -> False

-- The offered way of tapping a source whose YIELD is `wanted`, or the head where
-- it offers no such yield. What a Prompt.ChooseManaYield answerer that cares
-- about a colour wants: the prompt names a whole option, cost and all (#1117),
-- and a caller choosing a colour is indifferent to what it is charged.
--
-- The FIRST such option, where a source offers one yield for two costs (an
-- Urborg'd Mana Confluence). A fixture that cares which cost it pays picks the
-- candidate itself; ManaSpec's Urborg'd Mana Confluence is the one that does.
optionYielding :: Mana.Mana -> NonEmpty.NonEmpty ManaOption.ManaOption -> ManaOption.ManaOption
optionYielding wanted candidates =
  Maybe.fromMaybe
    (NonEmpty.head candidates)
    (List.find ((==) wanted . ManaOption.yield) (NonEmpty.toList candidates))

-- CR 601.2c: answer a Prompt.ChooseTargets offer by taking each slot's announced
-- number of recipients, the ones the predicate admits first and the smallest of
-- the rest after -- which is what almost every spec's aiming answerer wants now
-- that one slot may take several targets.
preferring ::
  (Recipient.Recipient -> Bool) ->
  Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient) ->
  Map.Map SlotName.SlotName (Set.Set Recipient.Recipient)
preferring wanted =
  fmap
    ( \(n, legal) ->
        let (yes, no) = List.partition wanted (Set.toAscList legal)
         in Set.fromList (take (Natural.toIntSaturating n) (yes <> no))
    )

-- Identity interpreter: shuffle returns ids unchanged; actions never occur here.
identityAnswer :: Prompt.Prompt r -> r
identityAnswer p = case p of
  -- CR 601.2: taking no action is always legal, and a fixture that wants one
  -- taken says so with its own interpreter -- which is what makes that answer
  -- discriminating.
  Prompt.ChooseAction {} -> A.Pass
  _ -> Script.declining p

-- Casts when legal, otherwise plays a land, otherwise passes.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.ChooseAction _ _ actions -> Script.castElsePlay actions
  _ -> identityAnswer p

-- Attacks with everything and blocks the first attacker with everything.
-- Deliberately maximal: it makes combat happen without the test having to
-- hand-build a Combat record.
aggressiveAnswer :: Prompt.Prompt r -> r
aggressiveAnswer p = case p of
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (fmap (\b -> (b, Set.singleton a)) mine)
  _ -> identityAnswer p

-- castAnswer's actions with aggressiveAnswer's combat: plays lands and casts
-- what it can afford, then attacks with everything and blocks with everything.
-- Neither half drives a game through combat on its own -- castAnswer never
-- declares an attacker, and aggressiveAnswer never takes an action, so it never
-- puts a creature onto the battlefield to attack with.
--
-- The two agree on every other prompt, so castAnswer is this answerer's exact
-- paired control: the same game with combat switched off, and the only
-- difference between the two is CR 508/509's declarations. EngineSpec's
-- whole-game combat case is that pair.
fightAnswer :: Prompt.Prompt r -> r
fightAnswer p = case p of
  Prompt.ChooseAction {} -> castAnswer p
  _ -> aggressiveAnswer p

-- Answers Prompt.ChooseDefender with `who` and everything else with
-- aggressiveAnswer -- the shared shape of CombatSpec's and GameSpec's M5.6d
-- defending-player fixtures. Its own type is the ordinary rank-1 `forall r.
-- PlayerId -> Prompt r -> r` (the implicit forall is outermost, quantifying
-- the whole arrow chain), so it needs no RankNTypes of its own; partially
-- applying `attackTo who` gives exactly the `forall r. Prompt.Prompt r -> r`
-- that runCombat expects, with GHC instantiating attackTo's `r` at the skolem
-- constant runCombat's argument type introduces. GameSpec's gate also needs to
-- decline blocks (CR 509.1 routing, not this module's concern), which stays a
-- group-local refinement rather than a second parameter here.
attackTo :: PlayerId.PlayerId -> Prompt.Prompt r -> r
attackTo who p = case p of
  Prompt.ChooseDefender {} -> who
  _ -> aggressiveAnswer p

-- Always plays a land when one is legal, otherwise passes.
playLandAnswer :: Prompt.Prompt r -> r
playLandAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    let isPlay a = case a of
          A.Play {} -> True
          A.Pass -> False
          A.Cast {} -> False
          A.TurnFaceUp {} -> False
          A.Unlock _ _ -> False
          A.Activate _ _ -> False
          A.DiscardFromHand _ -> False
          A.Plot _ -> False
          A.Foretell _ -> False
          A.Ignore _ -> False
          A.ActivateManaAbility _ -> False
     in case filter isPlay actions of
          h : _ -> h
          [] -> A.Pass
  _ -> identityAnswer p

-- Any printing, on the battlefield under pid's control, untapped and Settled.
addCreature :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addCreature printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
          }
      )

-- Install a SetController continuous effect (CR 108.4) making pid the
-- controller of oid, and settle it under pid (Sickness.Settled pid) so a test
-- that exercises control isolates control from summoning sickness.
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
      settle o = o {Object.sickness = Sickness.Settled pid}
   in g1
        { GameState.continuousEffects = eff : GameState.continuousEffects g1,
          GameState.objects = Map.adjust settle oid (GameState.objects g1)
        }

-- CR 611.2b's Master Thief shape: "for as long as you control this creature",
-- as an ordinary count. Shared by Pawl.ExpirySpec, Pawl.CardSpec,
-- Pawl.ActivateSpec and Pawl.PlayerEffectSpec -- the retired
-- StateCondition.YouControlSource's one replacement value.
youControlSource :: Condition.Type.Condition
youControlSource =
  Condition.Type.Compares
    ( Compares.MkCompares
        ( Quantity.Type.Count
            ( Count.Type.MkCount
                (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
                (Filter.Type.And [Filter.Type.IsSource, Filter.Type.ControlledBy PlayerRelation.You])
                Aggregation.Members
            )
        )
        Comparison.Exactly
        (Quantity.Type.Literal 1)
    )

-- Barbarian Outcast's migrated StateIs (retired StateCondition.YouControlNo
-- Swamp -- CR 603.8): "you control no Swamps" as a Count of exactly 0. Shared by
-- Pawl.CodecIntegrationSpec (round-trip) and Pawl.CardSpec (the decoded card
-- equals this value), so one fixture is what both the wire format and the
-- corpus are pinned against -- the shape youControlSource already has.
youControlNoSwamps :: Condition.Type.Condition
youControlNoSwamps =
  Condition.Type.Compares
    ( Compares.MkCompares
        ( Quantity.Type.Count
            ( Count.Type.MkCount
                (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
                (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
                Aggregation.Members
            )
        )
        Comparison.Exactly
        (Quantity.Type.Literal 0)
    )

-- Does a stored continuous effect target `target` specifically? Used to tell
-- "nothing was stored FOR THIS OBJECT" apart from an unrelated entry already
-- in play (S.giveControl's own AtCleanup SetController on the object whose
-- control moved, which is not what CR 611.2b's "never starts" is about).
-- Matches every Affected constructor explicitly (no wildcard): a GainControl
-- effect only ever stores TheseObjects, so a dynamic Matching (or
-- MatchingAnywhere) set is correctly False here, but an exhaustive case means a
-- future Affected constructor forces a decision at this site instead of
-- silently reading as "nothing stored".
continuousEffectAffects :: ObjectId.ObjectId -> ContinuousEffect.ContinuousEffect Card.Type.Card -> Bool
continuousEffectAffects target eff = case ContinuousEffect.affected eff of
  Affected.TheseObjects ids -> Set.member target ids
  Affected.Matching _ -> False
  Affected.MatchingAnywhere _ -> False
  Affected.Attached -> False
  Affected.AttachedPlayerControls _ -> False

-- Append a stored continuous effect affecting exactly `oid`, at timestamp `ts`.
-- Object id 998 is a stand-in source: nothing in these tests reads the
-- source's own characteristics. The general shape (ColorSpec, PowerToughnessSpec,
-- ProjectionSpec and ResolveSpec all grew their own copy of this before it moved
-- here).
withEffectAt :: ObjectId.ObjectId -> Timestamp.Timestamp -> Projection.Modification -> GameState.GameState -> GameState.GameState
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

-- withEffectAt, but naming a REAL source instead of the 998 stand-in. Needed
-- because a modification can read its own source's state: Modification's
-- SetLandSubtypeToChosen reads Object.chosenSubtype off the effect's source, and
-- a source that names no object answers Nothing for every board. Pair it with
-- withChosenSubtype below.
withEffectFromAt :: ObjectId.ObjectId -> ObjectId.ObjectId -> Timestamp.Timestamp -> Projection.Modification -> GameState.GameState -> GameState.GameState
withEffectFromAt src oid ts m gs =
  let eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = src,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.expiry = Expiry.AtCleanup,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs {GameState.continuousEffects = eff : GameState.continuousEffects gs}

-- CR 614.1c: stamp the basic land type a permanent's controller would have
-- chosen as it entered, without running the entry loop. A STATE fixture, the
-- shape `attach` and `withEffect` already have -- the cast-it-for-real proof
-- that the choice is actually MADE is Pawl.AuraSpec's whole-card Convincing
-- Mirage test, and this exists so a projection test does not have to cast.
withChosenSubtype :: Subtype.Subtype -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
withChosenSubtype subtype oid gs =
  let set obj = obj {Object.chosenSubtype = Just subtype}
   in gs {GameState.objects = Map.adjust set oid (GameState.objects gs)}

-- withEffectAt, allocating its own fresh timestamp -- the convenience shape for
-- a caller that doesn't care which timestamp the effect lands at.
withEffect :: ObjectId.ObjectId -> Projection.Modification -> GameState.GameState -> GameState.GameState
withEffect oid m gs =
  let (ts, gs1) = Game.freshTimestamp gs
   in withEffectAt oid ts m gs1

-- The one CR 103.5b performer (Pawl.Engine.Resolve.performHandAction), so a test
-- that only wants a game set up does not have to reach into Pawl.Engine.Resolve for it.
performer :: HandActionPerformer.HandActionPerformer
performer = Resolve.performHandAction

-- The source stand-in for a targeting call whose target slot is source-blind
-- (every slot but the opponent's-creatures one, Pawl.ResolveSpec's CR 115.1a
-- cases). Object id 999 names nothing, the same posture withEffectAt's 998
-- takes.
--
-- SOURCE-BLIND is now a claim about the BOARD as well as about the slot: CR
-- 702.11d's "hexproof from [quality]" makes Target.legalRecipients read the
-- source's characteristics, and this id has none to read, so every quality is
-- vacuously unmatched. A case that puts such a candidate on the board must pass a
-- real object (S.spellOnStack) or it passes for the wrong reason -- see
-- Pawl.TargetSpec's rule 702.11d cases, which do.
noSource :: ObjectId.ObjectId
noSource = ObjectId.MkObjectId 999

-- The M4h NonlandPermanentTarget fixture (CR 109.2/110.4): alice controls a Piker
-- (creature), a Mindslaver (a Legendary Artifact -- nonland, non-creature),
-- and a Mountain (land). Three permanents so a nonland-permanent legal set can
-- be distinguished from both "all permanents" and "creatures only".
boardWithCreatureArtifactLand :: Printing.Printing -> Printing.Printing -> Printing.Printing -> GameState.GameState
boardWithCreatureArtifactLand creature artifact land =
  let gs0 = Setup.emptyGame bothPlayers
      (_, gs1) = addCreature creature alice gs0
      (_, gs2) = addCreature artifact alice gs1
      (_, gs3) = addCreature land alice gs2
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
            Object.enteredUnder = Nothing,
            Object.source = Source.OfToken card,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
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
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.library = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.library gs2)
          }
      )

-- One card of a printing in pid's graveyard, ON TOP of whatever is already
-- there -- CR 404.1, and the end Pawl.Engine.Game.insertIntoZone puts a real
-- arrival at, so a fixture built by repeated calls has the order a game would
-- have produced. That is load-bearing for CR 404.2's "the top creature card"
-- (Pawl.Engine.Cost.topExileCandidate), which reads the LAST member.
addGraveyardCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addGraveyardCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Graveyard,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.graveyard = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.graveyard gs2)
          }
      )

-- One card of a printing OWNED by pid, in exile. Exile is shared (CR 400.1:
-- "the other zones are shared by all players"), so unlike the graveyard and
-- library helpers above this one files the id in the single GameState.exile set
-- and the owner is carried only on the object -- which is exactly how
-- Game.zoneMembers Zone.Exile then reports whose card it is.
--
-- Face up, per CR 406.3's default ("exiled cards are, by default, kept face
-- up"). A card exiled FACE DOWN is not built here at all: Pawl.ExileSpec gets
-- one by casting Ignorant Bliss, so the rider it sets goes through the real
-- funnel rather than being written onto a hand-built object.
addExiledCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addExiledCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Exile,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.exile = Set.insert oid (GameState.exile gs2)
          }
      )

-- A creature of a printing on the battlefield under `pid`, WITH the CR 603.6a
-- enters event crafted alongside it, so Engine.placePendingTriggers finds its
-- SelfEnters trigger pending. addCreature alone puts the permanent there without
-- an event, and no trigger fires off a board that was simply arranged.
--
-- The from-zone is the stack, which is where a resolved creature spell comes
-- from (CR 608.3) -- unread by any trigger condition here, but the honest value.
entersWithTrigger :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
entersWithTrigger printing pid gs0 =
  let (oid, gs1) = addCreature printing pid gs0
      entered = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
   in (oid, withEvents [GameEvent.Moved (Moved.MkMoved entered (Projection.project oid gs1))] gs1)

-- One more card of a printing in pid's hand, APPENDED (contrast handOne, which
-- replaces the hand and sets up the phase for a cast).
addHandCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
addHandCard printing pid gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.hand = Map.insertWith (Seq.><) pid (Seq.singleton oid) (GameState.hand gs2)
          }
      )

-- Humility on the battlefield under bob's control (it is not a creature, so
-- a Matching (HasCardType Creature) affected set does not touch it). Returns
-- the updated state.
withHumility :: Printing.Printing -> GameState.GameState -> GameState.GameState
withHumility humility gs = snd (addCreature humility bob gs)

-- The TargetSlot declared under the "target" slot name, read straight out of a
-- JSON-loaded printing's spell (Modal.allTargetSlots, keyed by SlotName) -- so a
-- gate test exercises the codec's parse of the committed card data, never a
-- hand-built TargetSlot (P9 Task 5: Terror, Reprisal).
spellTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
spellTargetSlot printing =
  Map.lookup
    (SlotName.MkSlotName (Text.pack "target"))
    (Modal.allTargetSlots (Face.spell (Card.combined (Printing.card printing))))

-- alice controls n untapped basic lands of one printing, nothing else. The
-- two-seat board; landsFor below is the same lands on a board a caller supplies,
-- which is how a three-seat case gets mana.
landsInPlay :: Printing.Printing -> Int -> GameState.GameState
landsInPlay land n = landsFor land alice n (Setup.emptyGame bothPlayers)

-- n untapped basic lands of one printing under pid, added to an existing board.
landsFor :: Printing.Printing -> PlayerId.PlayerId -> Int -> GameState.GameState -> GameState.GameState
landsFor land pid n base =
  let add gs _ =
        let (oid, gs1) = Game.freshObjectId gs
            (ts, gs2) = Game.freshTimestamp gs1
            obj =
              Object.MkObject
                { Object.owner = pid,
                  Object.enteredUnder = Nothing,
                  Object.source = Source.OfCard land,
                  Object.zone = Zone.Battlefield,
                  Object.tapped = TapState.Untapped,
                  Object.facing = Facing.FaceUp,
                  Object.exiledFaceDown = False,
                  Object.damage = 0,
                  Object.sickness = Sickness.Settled pid,
                  Object.bindings = Map.empty,
                  Object.counters = Map.empty,
                  Object.counterTimestamps = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.chosenColor = Nothing,
                  Object.chosenSubtype = Nothing,
                  Object.chosenNames = Set.empty,
                  Object.chosenPlayer = Nothing,
                  Object.timestamp = ts,
                  Object.face = Nothing,
                  Object.turnedOverAt = Nothing,
                  Object.worldSince = Nothing,
                  Object.playableFromExile = Nothing,
                  Object.plotted = Nothing,
                  Object.foretold = Nothing,
                  Object.ringBearerFor = Nothing,
                  Object.protector = Nothing,
                  Object.ventureRoom = Nothing,
                  Object.unlockedHalves = Set.empty,
                  Object.designations = Set.empty,
                  Object.kicked = False,
                  Object.announcedX = Nothing,
                  Object.detainedUntil = Set.empty,
                  Object.doesNotUntapNext = False,
                  Object.exertedBy = Set.empty
                }
         in gs2
              { GameState.objects = Map.insert oid obj (GameState.objects gs2),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
              }
   in List.foldl' add base [1 .. n]

-- Put one card of a printing into alice's hand in a main phase with priority.
handOne :: Printing.Printing -> GameState.GameState -> (GameState.GameState, ObjectId.ObjectId)
handOne printing base =
  let (oid, gs1) = Game.freshObjectId base
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
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

-- alice has n untapped lands of the given printing in play and one Piker in
-- hand, in a chosen phase.
pikerInHand :: Printing.Printing -> Printing.Printing -> Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
pikerInHand land piker n ph =
  let base = landsInPlay land n
      (oid, gs1) = Game.freshObjectId base
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard piker,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
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

-- alice has n untapped lands of the given printing in play and one Lightning
-- Bolt in hand.
boltInHand :: Printing.Printing -> Printing.Printing -> Int -> Phase.Phase -> (GameState.GameState, ObjectId.ObjectId)
boltInHand land bolt n ph =
  let (gs, oid) = handOne bolt (landsInPlay land n)
   in (gs {GameState.phase = ph}, oid)

-- alice is active with one Settled creature per printing in `mine`; bob defends
-- with one per printing in `theirs`. Returns their ids alongside the state, in
-- the order the printings were given.
combatBoardOf :: [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoardOf mine theirs =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = addCreature p pid g in (ids <> [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll alice mine (Setup.emptyGame bothPlayers)
      (yours, gs2) = addAll bob theirs gs1
   in ( gs2
          { GameState.activePlayer = alice,
            GameState.phase = Phase.Combat CombatStep.DeclareAttackers,
            -- The board sits AFTER the beginning of combat step, so CR 703.4h has
            -- already happened and the defending player is settled. alice is
            -- active, so by CR 506.2's second sentence bob is the defending
            -- player. Stated rather than derived, because a direct-call test
            -- never runs the turn-based action that would fill this in.
            GameState.combat = Combat.emptyCombat {Combat.Type.defender = Just bob},
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
combatBoard :: Printing.Printing -> Int -> Int -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId])
combatBoard piker a b = combatBoardOf (replicate a piker) (replicate b piker)

-- CR 800.1: the three-seat twin of combatBoardOf. alice is active with one
-- Settled creature per printing in `mine`; bob gets one per printing in `theirs`
-- and carol one per printing in `others`, so BOTH opponents are legal
-- defending-player choices and both can block.
--
-- Positioned at the BEGINNING of combat, unlike combatBoardOf: Combat.defender is
-- Nothing, and running the step is what fills it (CR 703.4h). A test that wants a
-- particular defender without running the step sets the field itself.
threePlayerCombat :: [Printing.Printing] -> [Printing.Printing] -> [Printing.Printing] -> (GameState.GameState, [ObjectId.ObjectId], [ObjectId.ObjectId], [ObjectId.ObjectId])
threePlayerCombat mine theirs others =
  let addAll pid ps gs =
        List.foldl'
          (\(ids, g) p -> let (oid, g1) = addCreature p pid g in (ids <> [oid], g1))
          ([], gs)
          ps
      (ours, gs1) = addAll alice mine threePlayerGame
      (yours, gs2) = addAll bob theirs gs1
      (hers, gs3) = addAll carol others gs2
   in ( gs3
          { GameState.activePlayer = alice,
            GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.remaining =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareAttackers,
                  Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain,
                  Phase.Ending EndingStep.EndStep,
                  Phase.Ending EndingStep.Cleanup
                ]
          },
        ours,
        yours,
        hers
      )

-- Attack with everything, block per the given plan, then deal damage.
fightWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
fightWith answer gs =
  snd . Engine.runGamePure answer gs $ do
    Combat.declareAttackers alice
    Combat.declareBlockers
    Damage.dealCombatDamage

-- Run a Game action purely under an answerer and keep only the final state. The
-- shape every direct-call test needs now that the change-and-emit funnels are
-- monadic (P5): `Event.destroy oid gs` becomes
-- `S.runPure S.identityAnswer gs (Event.destroy regenerability [oid])`.
runPure :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> GameState.GameState
runPure answer gs game = snd (Engine.runGamePure answer gs game)

-- runPure, keeping the action's RESULT alongside the final state -- the shape a
-- test needs when the door under test answers with a value (Pawl.Engine.Cost.pay's
-- Payment) and not only with a board.
runPureWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> (a, GameState.GameState)
runPureWith = Engine.runGamePure

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

-- Run whole steps until `step` is the current phase, WITHOUT running it, so a
-- test can play that one step itself under a different answerer. Bounded so a
-- bug cannot loop forever. Stops early if combat is left, so a caller that names
-- a step this combat never reaches gets the state at the exit rather than a
-- hang.
runToStep :: Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToStep step answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || GameState.phase g == step
          || not (inCombatPhase (GameState.phase g))
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 8 gs0

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
          Source.OfCard printing -> Card.isCreature (combinedFace printing)
          Source.OfToken card -> Card.isCreature (Card.combined card)
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
   in length (filter isCreatureObject (Game.zoneMembers Zone.Battlefield pid gs))

-- The name on a card's combined face, which for every card in this pool is its
-- only face and so its only name. NOT "the name a card answers to" in general:
-- CR 709.4a gives a split card two names and no combined one, so a card that
-- prints two faces answers here with the two RENDERED as one string. A test
-- about a multi-named object wants Pawl.Engine.Projection.namesOf; every caller
-- here is comparing against a one-faced card's printed name.
nameOf :: Card.Type.Card -> CardName.CardName
nameOf = Face.name . Card.combined

-- The printed characteristics a PRINTING carries, which is the seam every test
-- that reaches past Printing.card goes through. Card.combined for nameOf's
-- reason: a characteristic belongs to a face, and which face a card shows is
-- Pawl.Engine.Card's question.
--
-- Named for the view it builds. Pawl.Engine.Game.faceOf answers a DIFFERENT
-- question -- which face is this OBJECT showing (CR 709.3b) -- and the two
-- carried one name between them.
combinedFace :: Printing.Printing -> Face.Face Card.Type.Card
combinedFace = Card.combined . Printing.card

countByName :: CardName.CardName -> PlayerId.PlayerId -> GameState.GameState -> Int
countByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> nameOf (Printing.card printing) == wanted
          Source.OfToken card -> nameOf card == wanted
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
      inLibrary = filter named (Game.zoneMembers Zone.Library pid gs)
      inHand = filter named (Game.zoneMembers Zone.Hand pid gs)
   in length inLibrary + length inHand

-- How many of pid's battlefield objects are copies of a card with this name.
countOnBattlefieldByName :: CardName.CardName -> PlayerId.PlayerId -> GameState.GameState -> Int
countOnBattlefieldByName wanted pid gs =
  let named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> nameOf (Printing.card printing) == wanted
          Source.OfToken card -> nameOf card == wanted
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
   in length (filter named (Game.zoneMembers Zone.Battlefield pid gs))

damageOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural
damageOf oid gs = fmap Object.damage (Game.lookupObject oid gs)

-- Projection.powerOf and Projection.toughnessOf, paired -- the shape an
-- Aura/pump-effect assertion wants (a single "is it a 4/2" check) rather than
-- two separate equalities. Nothing if either projects to Nothing.
powerToughnessOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe (Integer, Integer)
powerToughnessOf oid gs = (,) <$> Projection.powerOf oid gs <*> Projection.toughnessOf oid gs

markDamage :: ObjectId.ObjectId -> Natural -> GameState.GameState -> GameState.GameState
markDamage oid n gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.damage = n}) oid (GameState.objects gs)}

-- The events recorded so far this turn, in order, WITHOUT the EventGroup each
-- carries. Which events were simultaneous is a question only
-- Pawl.Engine.Event.eventTriggers asks (CR 603.10a), and it reads the log itself;
-- an assertion about what happened wants the events alone.
eventsOf :: GameState.GameState -> [GameEvent.GameEvent]
eventsOf = fmap snd . Foldable.toList . GameState.events

-- The damage events recorded so far this turn, in order. Replaces the
-- GameState.damageEvents list P4 folded into the one turn-scoped log.
damageEventsOf :: GameState.GameState -> [DamageEvent.DamageEvent]
damageEventsOf gs = Maybe.mapMaybe Event.damageOf (eventsOf gs)

-- The zone changes recorded so far this turn, in order.
zoneChangesOf :: GameState.GameState -> [ZoneChange.ZoneChange]
zoneChangesOf gs = Maybe.mapMaybe Event.movedOf (eventsOf gs)

-- Who revealed what, so far this turn, in order (CR 701.20a). Projects the
-- snapshot down to the card's NAMES: a reveal shows every characteristic, but
-- the name is what identifies the card to the table and the only part an
-- assertion can write down legibly. Plural because CR 709.4a's is (a split card
-- revealed from a library shows both halves' names); every caller so far reveals
-- a one-named card. A test that needs more reads Event.revealOf directly.
revealsOf :: GameState.GameState -> [(PlayerId.PlayerId, Set.Set CardName.CardName)]
revealsOf gs = fmap (fmap PC.names) (Maybe.mapMaybe Event.revealOf (eventsOf gs))

-- The battlefield objects that are tokens (CR 111.1) rather than cards.
tokensOf :: GameState.GameState -> [ObjectId.ObjectId]
tokensOf gs = filter isToken (Set.toList (GameState.battlefield gs))
  where
    isToken oid = case fmap Object.source (Game.lookupObject oid gs) of
      Just (Source.OfToken _) -> True
      _ -> False

-- The creatures DECLARED as attackers so far this turn, in order (CR 508.2b).
-- Deliberately not "who is attacking", which is Combat.attackers: CR 508.3a's
-- last sentence turns on the difference, since a creature put onto the
-- battlefield attacking is in that record and never appears here.
attackerDeclarationsOf :: GameState.GameState -> [ObjectId.ObjectId]
attackerDeclarationsOf gs = Maybe.mapMaybe declared (eventsOf gs)
  where
    declared event = case event of
      GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid _ _) -> Just oid
      _ -> Nothing

-- The characteristics of nothing: Projection.project on an id with no card in
-- Setup.emptyGame. The filler snapshot for a hand-built GameEvent.Moved whose
-- payload no assertion reads.
emptyCharacteristics :: PC.ProjectedCharacteristics
emptyCharacteristics = Projection.project (ObjectId.MkObjectId 999) (Setup.emptyGame bothPlayers)

-- A state whose UNSCANNED event log IS this list, in this order -- the
-- hand-built-event fixture shape a scan test needs. Takes the whole list rather
-- than one event because it REPLACES the log: the one-event `withEvent` this
-- replaces read like an append, so nesting two calls silently dropped the first
-- (#164), and the two-event case had to be spelled with a trailing
-- Event.recordEvent.
--
-- Each event gets its OWN EventGroup, which is what a hand-built list means: the
-- fixture states a sequence, and nothing in it says two of the events were the
-- single event CR 704.3 or CR 608.2f describes. A test that wants simultaneity
-- goes through the funnels that stamp it (Event.simultaneously), since a fixture
-- asserting it directly would be asserting the answer.
--
-- GameState.nextEventGroup is advanced PAST them for the same reason: a test that
-- goes on to run a real funnel is continuing the sequence, so what that funnel
-- records must land in a LATER group. Leaving the counter where it was would put
-- the fixture's last event and the funnel's first in one group and claim they
-- were simultaneous.
withEvents :: [GameEvent.GameEvent] -> GameState.GameState -> GameState.GameState
withEvents events gs =
  gs
    { GameState.events = Seq.fromList (zipWith (\n event -> (EventGroup.MkEventGroup n, event)) [0 ..] events),
      GameState.nextEventGroup = EventGroup.MkEventGroup (Natural.length events),
      GameState.eventGroupDepth = 0,
      GameState.scannedThrough = 0,
      GameState.damageScannedThrough = 0,
      -- Rewriting the log rewrites the groups, so any sample Event.recordEvent
      -- took is now filed under a group number this fixture has just reused for a
      -- different event. Cleared rather than left standing, which sends
      -- Event.eventTriggers to its live reading -- the only honest one for an event
      -- no funnel recorded.
      GameState.battlefieldWhenTriggered = Map.empty
    }

-- Set the monarch directly, for tests that need the designation without
-- resolving the effect that grants it.
withMonarch :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
withMonarch pid gs = gs {GameState.monarch = Just pid}

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
            -- CR 109.5: the shield's "you" is whoever controls the permanent it
            -- is on. Nothing reads it -- a DestructionR carries no pattern --
            -- so this is honesty rather than behaviour.
            ActiveReplacement.controller = Maybe.fromMaybe alice (Projection.controllerOf oid gs),
            ActiveReplacement.timestamp = ts,
            ActiveReplacement.expiry = Expiry.AtCleanup,
            ActiveReplacement.uses = Uses.Once,
            ActiveReplacement.origin = ReplacementOrigin.Other,
            ActiveReplacement.rider = Nothing
          }
   in addReplacement active gs1

-- Seed a stored player effect directly into GameState (bypasses resolving the
-- spell that would install it; use when a test needs one active without a
-- resolution). Object id 998 is the stand-in source, the withEffectAt posture --
-- nothing here reads the source's own characteristics.
addPlayerEffect ::
  Expiry.Expiry ->
  AffectedPlayers.AffectedPlayers PlayerId.PlayerId ->
  PlayerEffect.PlayerEffect ->
  PlayerId.PlayerId ->
  GameState.GameState ->
  GameState.GameState
addPlayerEffect expiry scope effect controller gs =
  let (ts, gs1) = Game.freshTimestamp gs
      active =
        ActivePlayerEffect.MkActivePlayerEffect
          { ActivePlayerEffect.source = ObjectId.MkObjectId 998,
            ActivePlayerEffect.controller = controller,
            ActivePlayerEffect.timestamp = ts,
            ActivePlayerEffect.expiry = expiry,
            ActivePlayerEffect.scope = scope,
            ActivePlayerEffect.effect = effect
          }
   in gs1 {GameState.playerEffects = active : GameState.playerEffects gs1}

-- Set an object's tapped state to Tapped directly (bypasses a tap cost or
-- combat), so a test can set up a tapped permanent for an Untap effect.
tapObject :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tapObject oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}

-- Put `n` counters of a kind directly onto an object's per-incarnation state,
-- bypassing the PutCounters opcode -- so a projection or SBA test can set up
-- counters without resolving a spell. Stamps the kind (CR 613.7c) as the real
-- funnel does, so the placement orders after everything already on the board.
addCounter :: CounterKind.CounterKind Keyword.Keyword -> Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addCounter kind n oid gs =
  let (ts, gs1) = Game.freshTimestamp gs
      bump obj =
        obj
          { Object.counters = Map.insertWith (+) kind n (Object.counters obj),
            Object.counterTimestamps = Map.insert kind ts (Object.counterTimestamps obj)
          }
   in gs1 {GameState.objects = Map.adjust bump oid (GameState.objects gs1)}

-- CR 303.4b: attach `rider` to `host` directly, without casting. A STATE fixture
-- (the shape addCreature and withEffect already have), not a synthetic card --
-- every printing a caller passes is real. Type-agnostic on purpose: CR 400.7's
-- reset is a property of the field, so the CR 400.7 test does not need an Aura.
--
-- Tagged ToCreature, which is the tag the real attach paths store for a
-- CREATURE-enchanting Aura: those have a Pool.Creatures enchant slot, and
-- Target's candidates for that pool are ToCreature -- so an SBA that re-checks
-- the attachment against the enchant slot (Pawl.Engine.Sba.stillLegalEnchant) sees what
-- casting would have left.
--
-- WRONG for Convincing Mirage, whose "Enchant land" is a Pool.Permanents slot
-- narrowed by a Land filter, and whose candidates are therefore ToObject. That
-- costs nothing for a rule that reads only WHICH object is named -- CR 704.5n,
-- CR 704.5p, CR 400.7, and Affected.Attached, which goes through
-- Recipient.objectOf -- which is every caller that attaches to a non-creature
-- today. A test that puts Convincing Mirage in front of stillLegalEnchant must
-- CAST it instead; Pawl.AuraSpec's whole-card case is that cast.
attach :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
attach rider host = attachTo rider (Recipient.ToCreature host)

-- attach, to whatever a Recipient names -- CR 303.4's "attached to an object or
-- player". The door an enchant-player Aura (CR 702.5d) needs, since no ObjectId
-- names its host.
attachTo :: ObjectId.ObjectId -> Recipient.Recipient -> GameState.GameState -> GameState.GameState
attachTo rider host gs =
  let set obj = obj {Object.attachedTo = Just host}
   in gs {GameState.objects = Map.adjust set rider (GameState.objects gs)}

-- Put `n` counters of a player-counter kind directly onto a player, bypassing
-- the diversion/effect that would add them -- so an SBA or cost test can set up
-- poison or energy without resolving anything.
addPlayerCounter :: PlayerCounterKind.PlayerCounterKind -> Natural -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addPlayerCounter kind n pid gs =
  let bump player = player {Player.counters = Map.insertWith (+) kind n (Player.counters player)}
   in gs {GameState.players = Map.adjust bump pid (GameState.players gs)}

-- How many counters of a kind an object has (absent kind = zero). The object
-- mirror of playerCounterOf below.
counterOf :: CounterKind.CounterKind Keyword.Keyword -> ObjectId.ObjectId -> GameState.GameState -> Natural
counterOf kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

-- Is this object still on the battlefield? A zone read, not a set-membership
-- one, so it stays correct for an object whose incarnation changed (CR 400.7).
onBattlefield :: ObjectId.ObjectId -> GameState.GameState -> Bool
onBattlefield oid gs = fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Battlefield

-- How many counters of a kind a player has (absent kind = zero).
playerCounterOf :: PlayerCounterKind.PlayerCounterKind -> PlayerId.PlayerId -> GameState.GameState -> Natural
playerCounterOf kind pid gs =
  maybe 0 (Map.findWithDefault 0 kind . Player.counters) (Map.lookup pid (GameState.players gs))

tappedCount :: PlayerId.PlayerId -> GameState.GameState -> Int
tappedCount pid gs =
  let isTapped oid = case Game.lookupObject oid gs of
        Just obj -> Object.tapped obj == TapState.Tapped
        Nothing -> False
   in length (filter isTapped (Game.zoneMembers Zone.Battlefield pid gs))

handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

-- The cards M2a adds, paired with the single keyword each must carry. Named
-- rather than loaded here so Pawl.Support stays pure: the caller loads them.
m2aKeywords :: [(String, Keyword.Keyword)]
m2aKeywords =
  [ ("Bird Maiden", Keyword.Flying),
    ("Nimble Birdsticker", Keyword.Reach),
    ("Ogre Sentry", Keyword.Defender),
    ("Windseeker Centaur", Keyword.Vigilance),
    ("Goblin Chariot", Keyword.Haste)
  ]

-- A GameState with a single Mountain in alice's hand, in a chosen phase.
oneMountainState :: Printing.Printing -> Phase.Phase -> GameState.GameState
oneMountainState mountain ph =
  let oid = ObjectId.MkObjectId 0
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard mountain,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = Timestamp.MkTimestamp 0,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in GameState.MkGameState
        { GameState.objects = Map.singleton oid obj,
          GameState.library = Map.empty,
          GameState.hand = Map.singleton alice (Seq.singleton oid),
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.phasedOut = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.players = Map.empty,
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.nextEventGroup = EventGroup.first,
          GameState.eventGroupDepth = 0,
          GameState.lastKnown = Map.empty,
          GameState.scannedThrough = 0,
          GameState.battlefieldWhenTriggered = Map.empty,
          GameState.controlSample = Map.empty,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.pendingPreventionRiders = Seq.empty,
          GameState.ambientAmounts = Map.empty,
          GameState.pendingEntryEffects = Seq.empty,
          GameState.playerEffects = [],
          GameState.blockRequirements = [],
          GameState.ignoredAbilities = [],
          GameState.turnOrder = [alice],
          GameState.activePlayer = alice,
          GameState.phase = ph,
          GameState.remaining = Turn.laterPhases,
          GameState.priority = Just alice,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.restartSignal = RestartSignal.Playing,
          GameState.nextObjectId = ObjectId.MkObjectId 1,
          GameState.nextTimestamp = Timestamp.MkTimestamp 1,
          GameState.lastChoice = Timestamp.MkTimestamp 0,
          GameState.drewFromEmpty = mempty,
          GameState.landsPlayed = mempty,
          GameState.drawsThisTurn = mempty,
          GameState.speedIncreasedThisTurn = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.daytime = Nothing,
          GameState.spellsCastLastTurn = 0,
          GameState.exiledUntilMonarch = Map.empty,
          GameState.haunting = Map.empty,
          GameState.exiledWith = Map.empty,
          GameState.extraTurns = [],
          GameState.turnAnchor = Nothing
        }

drawStep :: Game.Type.Game ()
drawStep = Engine.runTurnBasedActions (Phase.Beginning BeginningStep.DrawStep)

-- CR 709.3's half-choice, made where the card leaves exactly one answer: the
-- name of the object's sole castable face. Read off that card's own data, so it
-- is never a stand-in for a name a test failed to give.
--
-- A card with SEVERAL halves is a real choice, and this refuses to make it --
-- LOUDLY, naming the card. A helper that quietly did nothing is how a future
-- multi-face test would pass while exercising nothing. Such a test names the
-- half and calls Pawl.Engine.Cast directly.
soleFaceName :: ObjectId.ObjectId -> GameState.GameState -> CardName.CardName
soleFaceName oid gs = case fmap Card.castableFaces (Game.cardOf oid gs) of
  Just [face] -> Face.name face
  _ ->
    error
      ( "Pawl.Support: "
          <> show (fmap nameOf (Game.cardOf oid gs))
          <> " does not offer exactly one castable half -- name the half and call Pawl.Engine.Cast directly"
      )

-- Pawl.Engine.Cast.castSpell of that one half, FACE UP -- CR 702.37c's
-- face-down cast is a second cast of the same card, so a test that wants it
-- names it (Pawl.FaceDownSpec) rather than getting it from a helper that cannot
-- know which was meant.
cast :: PlayerId.PlayerId -> ObjectId.ObjectId -> Game.Type.Game ()
cast pid oid = do
  gs <- State.get
  Cast.castSpell pid oid (soleFaceName oid gs) Facing.FaceUp

-- `cast`'s predicate half: Pawl.Engine.Cast.castable asked of that same half.
castable :: PlayerId.PlayerId -> ObjectId.ObjectId -> GameState.GameState -> Bool
castable pid oid gs = Cast.castable pid oid (soleFaceName oid gs) Facing.FaceUp gs

-- The name a single-face printing carries -- what a test naming the half of an
-- A.Cast action holds in scope.
printingName :: Printing.Printing -> CardName.CardName
printingName = nameOf . Printing.card

-- Is this action a cast of that object, whichever half? An answerer that pins
-- a cast to one CARD asks this rather than building the action, since CR
-- 709.3's choice among a card's halves is a separate question it has no answer
-- for.
isCastOf :: ObjectId.ObjectId -> A.Action -> Bool
isCastOf oid action = case action of
  A.Cast o _ _ -> o == oid
  A.Pass -> False
  A.Play {} -> False
  A.Activate _ _ -> False
  A.TurnFaceUp {} -> False
  A.Unlock _ _ -> False
  A.DiscardFromHand _ -> False
  A.Plot _ -> False
  A.Foretell _ -> False
  A.Ignore _ -> False
  A.ActivateManaAbility _ -> False

-- bob's Piker on the battlefield; alice casts a Bolt at it under identityAnswer
-- (lookupMin prefers ToCreature over ToPlayer, and the Piker is the only
-- creature). Returns (pre-cast state, post-cast state, Bolt's hand id).
boltAtBobsPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
boltAtBobsPiker piker land bolt =
  let (_, withPiker) = addCreature piker bob (landsInPlay land 1)
      (gs, oid) = handOne bolt withPiker
   in (gs, snd (Engine.runGamePure identityAnswer gs (cast alice oid)), oid)

-- The single creature bob controls in a fixture built by (addCreature piker bob).
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
            Object.enteredUnder = Nothing,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.stack = oid : GameState.stack gs2
          }
      )

-- Drive Pawl.Engine.Count.evaluate with the per-object quantity reader wired the way
-- the library wires it -- Pawl.Engine.Quantity.evaluate, which is where that knot is
-- tied (Pawl.Engine.Count cannot import it). Shared by every spec that drives the fold
-- directly, so the injection they test is the injection the engine makes.
countOf ::
  Count.ViewOf ->
  Filter.Context ->
  GameState.GameState ->
  Count.Type.Count Quantity.Type.Quantity ->
  Maybe Integer
countOf viewOf context gs =
  Count.evaluate
    viewOf
    -- The library reads each member against the RESOLVING object's announced X
    -- (Pawl.Engine.Quantity's Count arm); a fixture has no resolving object, so
    -- each member stands in for itself. A member with no object at all -- a
    -- Scope.InHistory candidate, whose view is a CR 608.2h snapshot -- gets
    -- noSource, which names nothing and so binds nothing.
    (\mOid view -> Quantity.evaluateAgainst viewOf context gs (Maybe.fromMaybe noSource mOid) mOid (Just view))
    context
    gs

-- Shared by Pawl.CountSpec and Pawl.ConditionSpec: a stub ViewOf, so a
-- Pawl.Engine.Count.evaluate fold is exercised apart from any real projection. Every
-- id gets a view carrying exactly the card types, subtypes and controller it
-- was registered with; an id absent from the table has no view.
stubView ::
  [(ObjectId.ObjectId, Set.Set CardType.CardType, Set.Set Subtype.Subtype, Maybe PlayerId.PlayerId)] ->
  Count.ViewOf
stubView table oid =
  let match (o, _, _, _) = o == oid
   in case filter match table of
        (o, ts, ss, ctrl) : _ ->
          Just
            Filter.MkView
              { -- CR 201.1: the table registers no name, and no Count in the pool
                -- filters by one, so HasName is vacuously False against this stub.
                Filter.names = Set.empty,
                Filter.cardTypes = ts,
                Filter.supertypes = Set.empty,
                Filter.colors = Set.empty,
                Filter.subtypes = ss,
                Filter.keywords = Set.empty,
                Filter.power = Nothing,
                Filter.toughness = Nothing,
                Filter.manaValue = Nothing,
                Filter.controller = ctrl,
                -- CR 108.3: the table registers no owner, and no Count in the
                -- pool filters by one, so this stub answers Nothing and OwnedBy
                -- is vacuously False against it.
                Filter.owner = Nothing,
                Filter.identity = Just o,
                Filter.playerIdentity = Nothing,
                Filter.attacking = False,
                Filter.blocking = False,
                Filter.blocked = False,
                Filter.attackedThisTurn = False,
                Filter.milledThisTurn = False,
                Filter.attachedToView = Nothing,
                Filter.attachedTo = Nothing,
                Filter.canHostSubject = False,
                Filter.token = False,
                Filter.tapped = False,
                Filter.counters = Map.empty,
                Filter.ringBearerFor = Nothing,
                Filter.designations = Set.empty,
                Filter.kicked = False,
                -- CR 602.1: the table registers no abilities either, for the
                -- reason `owner` above gives -- no Count in the pool filters on
                -- one.
                Filter.nonManaActivatedAbility = False
              }
        [] -> Nothing

-- Every card pawl ships, or a failure naming every file that would not load.
--
-- Takes no registry: the lint is about the BUNDLED corpus by definition, so
-- being handed a pool to inspect would let a caller point it somewhere else and
-- quietly stop checking what ships.
allPrintings :: Spec.Spec IO n -> IO [Printing.Printing]
allPrintings s = do
  root <- Registry.defaultRoot
  loaded <- Registry.loadRoot root
  case [path <> ": " <> Text.unpack reason | (path, Left reason) <- loaded] of
    [] -> pure [Printing.MkPrinting card | (_, Right card) <- loaded]
    errs -> Spec.assertFailure s (List.intercalate "\n" errs)

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
import qualified Pawl.Cost as Cost
import qualified Pawl.Count as Count
import qualified Pawl.Damage as Damage
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Extra.Int as Int
import qualified Pawl.Filter as Filter
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Combat as Combat.Type
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.Comparison as Comparison
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Condition as Condition.Type
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.DamageEvent as DamageEvent
import qualified Pawl.Type.Deck as Deck
import qualified Pawl.Type.DestructionRewrite as DestructionRewrite
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Expiry as Expiry
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.HandActionPerformer as HandActionPerformer
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.OptionalDecision as OptionalDecision
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.PlayerEffect as PlayerEffect
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.PlayerScope as PlayerScope
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.RestartSignal as RestartSignal
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Slug as Slug.Type
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.StaticAbility as StaticAbility
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Uses as Uses
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified System.Random as Random

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

redRed :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
redRed registry = do
  deck <- Cards.redDeck registry
  pure (Setup.mirror deck bothPlayers)

greenBlack :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
greenBlack registry = do
  green <- Cards.greenDeck registry
  black <- Cards.blackDeck registry
  pure ((alice, green) NonEmpty.:| [(bob, black)])

blueBlack :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
blueBlack registry = do
  blue <- Cards.blueDeck registry
  black <- Cards.blackDeck registry
  pure ((alice, blue) NonEmpty.:| [(bob, black)])

matchups :: Registry.Type.Registry -> IO [NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck)]
matchups registry = do
  rr <- redRed registry
  gb <- greenBlack registry
  bb <- blueBlack registry
  -- CR 800.1: the three-seat matchup. Every invariant that holds at two seats
  -- must hold at three, and this is the cheapest possible broad falsifier for
  -- that -- one list entry buys a whole played-out three-player game per seed.
  tw <- threeWayMirror registry
  pure [rr, gb, bb, tw]

-- A 60-basic-land mirror: no spell can be cast and no creature can attack, so the
-- only loss condition reachable is CR 704.5b deck-out. Used by the durable
-- lands-only-decks property.
landsOnly :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
landsOnly registry = do
  mountain <- Registry.printing registry "Mountain"
  pure (Setup.mirror (Deck.MkDeck (Map.singleton mountain 60)) bothPlayers)

-- CR 800.1: the three-seat twin of landsOnly. 60 basic lands each, so the only
-- reachable loss condition is CR 704.5b deck-out and the only reachable end is
-- CR 104.2a's last player standing. The seat count is what makes it a falsifier:
-- at two players the first deck-out ends the game, at three it must not.
threePlayerLandsOnly :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
threePlayerLandsOnly registry = do
  mountain <- Registry.printing registry "Mountain"
  pure (Setup.mirror (Deck.MkDeck (Map.singleton mountain 60)) threePlayers)

-- CR 800.1: the three-seat twin of redRed -- one red deck each for alice, bob and
-- carol. Setup.mirror is already NonEmpty-shaped, so the seat count is the only
-- difference. The three-seat setup rules (CR 103.5c's free first mulligan, CR
-- 103.8c's first draw) are what this exists to exercise.
threeWayMirror :: Registry.Type.Registry -> IO (NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck))
threeWayMirror registry = do
  deck <- Cards.redDeck registry
  pure (Setup.mirror deck threePlayers)

isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToPlayer _ -> False
  Recipient.ToObject _ -> False

-- Identity interpreter: shuffle returns ids unchanged; actions never occur here.
identityAnswer :: Prompt.Prompt r -> r
identityAnswer p = case p of
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseAction {} -> A.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseAttachment _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a "may" changes nothing, the least-eventful default
  -- (mirrors MulliganAction -> Nothing). A test that wants the option TAKEN says
  -- so with its own interpreter, which is what makes that answer discriminating.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  -- CR 118.13a: the head is a legal answer -- every offered route is payable --
  -- and is the least eventful default, matching Replay.defaultAnswer.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> NonEmpty.head offers

-- Casts when legal, otherwise plays a land, otherwise passes.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
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
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseAttachment _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a "may" changes nothing, the least-eventful default
  -- (mirrors MulliganAction -> Nothing). A test that wants the option TAKEN says
  -- so with its own interpreter, which is what makes that answer discriminating.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  -- CR 118.13a: the head is a legal answer -- every offered route is payable --
  -- and is the least eventful default, matching Replay.defaultAnswer.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> NonEmpty.head offers

-- Attacks with everything and blocks the first attacker with everything.
-- Deliberately maximal: it makes combat happen without the test having to
-- hand-build a Combat record.
aggressiveAnswer :: Prompt.Prompt r -> r
aggressiveAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseAction {} -> A.Pass
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (fmap (\b -> (b, a)) mine)
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseAttachment _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a "may" changes nothing, the least-eventful default
  -- (mirrors MulliganAction -> Nothing). A test that wants the option TAKEN says
  -- so with its own interpreter, which is what makes that answer discriminating.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  -- CR 118.13a: the head is a legal answer -- every offered route is payable --
  -- and is the least eventful default, matching Replay.defaultAnswer.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> NonEmpty.head offers

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
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
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
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseAttachment _ _ _ candidates -> NonEmpty.head candidates
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  Prompt.MulliganAction {} -> Nothing
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a "may" changes nothing, the least-eventful default
  -- (mirrors MulliganAction -> Nothing). A test that wants the option TAKEN says
  -- so with its own interpreter, which is what makes that answer discriminating.
  Prompt.ChooseOptional {} -> OptionalDecision.Declines
  -- CR 118.13a: the head is a legal answer -- every offered route is payable --
  -- and is the least eventful default, matching Replay.defaultAnswer.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> NonEmpty.head offers

-- A StdGen-driven interpreter: random shuffle and random legal action.
randomAnswer :: Prompt.Prompt r -> State.State Random.StdGen r
randomAnswer p = case p of
  Prompt.Concede _ -> pure Concession.Continues
  -- CR 507.1: the one interpreter with a real generator, so the defending-player
  -- choice is a genuine draw rather than the head of the list. S.pickFrom keeps
  -- an out-of-range draw total.
  Prompt.ChooseDefender _ _ candidates -> do
    g <- State.get
    let players = NonEmpty.toList candidates
        (i, g') = Random.uniformR (0, length players - 1) g
    State.put g'
    pure (pickFrom candidates i)
  -- Same shape for mana sources: a random legal source, so a random game that
  -- mixes a creature mana source with lands really does explore both (#12).
  Prompt.ChooseManaSource _ _ candidates -> do
    g <- State.get
    let sources = NonEmpty.toList candidates
        (i, g') = Random.uniformR (0, length sources - 1) g
    State.put g'
    pure (pickFrom candidates i)
  -- And the same for WHICH type a multi-type source makes, so a random game
  -- exercises every colour an any-colour source can produce.
  Prompt.ChooseManaType _ _ _ candidates -> do
    g <- State.get
    let types = NonEmpty.toList candidates
        (i, g') = Random.uniformR (0, length types - 1) g
    State.put g'
    pure (pickFrom candidates i)
  -- CR 701.34a: a random subset of each offered list, so a random game explores
  -- proliferating and declining alike -- "any number" includes none and all.
  Prompt.ChooseProliferate _ _ oids pids -> do
    g <- State.get
    let (howManyObjects, g') = Random.uniformR (0, length oids) g
        (howManyPlayers, g'') = Random.uniformR (0, length pids) g'
    State.put g''
    pure (Set.fromList (take howManyObjects oids), Set.fromList (take howManyPlayers pids))
  -- CR 704.5j: a random survivor, so a random game does not always keep the
  -- oldest legend.
  Prompt.ChooseLegend _ _ candidates -> do
    g <- State.get
    let legends = NonEmpty.toList candidates
        (i, g') = Random.uniformR (0, length legends - 1) g
    State.put g'
    pure (pickFrom candidates i)
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
      pure (Map.fromList (fmap (\b -> (b, a)) (take keep mine)))
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
  -- CR 729.2: the one interpreter that carries actual randomness, so this is the
  -- one that answers the first-player roll with a real draw rather than the head
  -- of the order.
  Prompt.RandomFirstPlayer order -> do
    g <- State.get
    let players = NonEmpty.toList order
        (i, g') = Random.uniformR (0, length players - 1) g
    State.put g'
    pure (pickFrom order i)
  Prompt.ChooseDiscard _ _ ids n -> pure (List.genericTake n ids)
  Prompt.ChooseAction _ _ actions -> do
    g <- State.get
    let n = length actions
        (i, g') = Random.uniformR (0, max 0 (n - 1)) g
    State.put g'
    pure (pick actions (min (n - 1) (max 0 i)))
  Prompt.ChooseTargets _ _ _ sets ->
    let pickFromSet s = do
          g <- State.get
          let xs = Set.toList s
              (i, g') = Random.uniformR (0, max 0 (length xs - 1)) g
          State.put g'
          pure $ case drop (min (max 0 i) (max 0 (length xs - 1))) xs of
            h : _ -> Just h
            [] -> Nothing
     in fmap (Map.mapMaybe id) (traverse pickFromSet sets)
  Prompt.ChooseBasicLandTypes {} -> pure (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> pure Nothing
  Prompt.CastWhileSearching {} -> pure Nothing
  -- A small bounded X (0..3) so a variable-cost spell sometimes chooses nonzero,
  -- exercising substituteX under random play; payment rejects an unaffordable draw.
  Prompt.ChooseX {} -> do
    g <- State.get
    let (i, g') = Random.uniformR (0 :: Int, 3) g
    State.put g'
    pure (Int.toNaturalSaturating i)
  -- A deterministic prefix of the legal modes, keeping replay simple (the
  -- brief permits this in place of a genuinely random size-`count` subset).
  Prompt.ChooseModes _ _ _ legal count ->
    pure (Set.fromList (List.genericTake count (Set.toAscList legal)))
  Prompt.ChooseCopyTarget {} -> pure Nothing
  Prompt.ChooseEntryOption {} -> pure 0
  Prompt.OrderTriggers _ _ sources -> pure (zipWith const [0 ..] sources)
  Prompt.ChooseReplacement {} -> pure 0
  -- CR 603.7c: a random minted token, so a random game does not always bind the
  -- first one a doubled Create produced.
  Prompt.ChooseBoundToken _ _ _ candidates -> do
    g <- State.get
    let tokens = NonEmpty.toList candidates
        (i, g') = Random.uniformR (0, length tokens - 1) g
    State.put g'
    pure (pickFrom candidates i)
  -- CR 701.3a: a random destination, so a random game does not always move an
  -- Aura onto the first permanent offered.
  Prompt.ChooseAttachment _ _ _ candidates -> do
    g <- State.get
    let destinations = NonEmpty.toList candidates
        (i, g') = Random.uniformR (0, length destinations - 1) g
    State.put g'
    pure (pickFrom candidates i)
  Prompt.ChooseSacrifices _ _ _ candidates count -> pure (Set.fromList (List.genericTake count candidates))
  Prompt.ChooseCost _ _ _ candidates -> pure (Cost.firstOffered candidates)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> pure (List.genericTake count hand)
  Prompt.MulliganAction {} -> pure Nothing
  Prompt.OpeningHandAction {} -> pure Nothing
  -- CR 603.5: a random answer, so a random game exercises both taking and
  -- declining a printed "may" (the ChooseProliferate posture).
  Prompt.ChooseOptional {} -> do
    g <- State.get
    let (takeIt, g') = Random.uniform g
    State.put g'
    pure (if takeIt then OptionalDecision.Exercises else OptionalDecision.Declines)
  -- CR 118.13a: deterministic rather than random, the ChooseSacrifices and
  -- ChooseCost posture -- no Phyrexian card is in any deck Pawl.Cards builds, so
  -- a random draw here would explore nothing and only complicate replay.
  Prompt.AnnouncePhyrexianPayment _ _ _ _ offers -> pure (NonEmpty.head offers)

-- Total index into a non-empty run of candidates -- a turn order, the tokens one
-- Create minted: an out-of-range draw falls back to the head, which the NonEmpty
-- guarantees exists (no partial functions).
pickFrom :: NonEmpty.NonEmpty a -> Int -> a
pickFrom order i = case drop i (NonEmpty.toList order) of
  h : _ -> h
  [] -> NonEmpty.head order

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
      keys = zipWith const (unfoldInts g) xs
   in fmap snd (foldr insertByKey [] (zip keys xs))

runRandomGame :: NonEmpty.NonEmpty (PlayerId.PlayerId, Deck.Deck) -> Int -> GameState.GameState
runRandomGame matchup s =
  snd (State.evalState (Engine.runMatch randomAnswer matchup) (Random.mkStdGen s))

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
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = ts
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
  Condition.Type.MkCondition
    ( Quantity.Type.Count
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.And [Filter.Type.IsSource, Filter.Type.ControlledBy PlayerRelation.You])
            Aggregation.Objects
        )
    )
    Comparison.Exactly
    (Quantity.Type.Literal 1)

-- Barbarian Outcast's migrated StateIs (retired StateCondition.YouControlNo
-- Swamp -- CR 603.8): "you control no Swamps" as a Count of exactly 0. Shared by
-- Pawl.CodecSpec (round-trip) and Pawl.CardSpec (the decoded card equals this
-- value), so one fixture is what both the wire format and the corpus are pinned
-- against -- the shape youControlSource already has.
youControlNoSwamps :: Condition.Type.Condition
youControlNoSwamps =
  Condition.Type.MkCondition
    ( Quantity.Type.Count
        ( Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
            Aggregation.Objects
        )
    )
    Comparison.Exactly
    (Quantity.Type.Literal 0)

-- Does a stored continuous effect target `target` specifically? Used to tell
-- "nothing was stored FOR THIS OBJECT" apart from an unrelated entry already
-- in play (S.giveControl's own AtCleanup SetController on the object whose
-- control moved, which is not what CR 611.2b's "never starts" is about).
-- Matches every Affected constructor explicitly (no wildcard): a GainControl
-- effect only ever stores TheseObjects, so a dynamic Matching set is correctly
-- False here, but an exhaustive case means a future Affected constructor forces a
-- decision at this site instead of silently reading as "nothing stored".
continuousEffectAffects :: ObjectId.ObjectId -> ContinuousEffect.ContinuousEffect -> Bool
continuousEffectAffects target eff = case ContinuousEffect.affected eff of
  Affected.TheseObjects ids -> Set.member target ids
  Affected.Matching _ -> False
  Affected.Attached -> False

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

-- The one CR 103.5b performer (Pawl.Resolve.performHandAction), so a test
-- that only wants a game set up does not have to reach into Pawl.Resolve for it.
performer :: HandActionPerformer.HandActionPerformer
performer = Resolve.performHandAction

-- The source stand-in for a targeting call whose spec is source-blind (every
-- spec but OpponentCreatureTarget). Object id 999 names nothing, the same
-- posture withEffectAt's 998 takes.
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
            Object.source = Source.OfToken card,
            Object.zone = Zone.Battlefield,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
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
            Object.attachedTo = Nothing,
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
            Object.attachedTo = Nothing,
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
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = ts
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

-- The "target" slot's TargetSpec, read straight out of a JSON-loaded printing's
-- spell (Modal.allTargetSpecs, keyed by SlotName) -- so a gate test exercises
-- the codec's parse of the committed card data, never a hand-built TargetSpec
-- (P9 Task 5: Terror, Reprisal).
spellTargetSpec :: Printing.Printing -> Maybe TargetSpec.TargetSpec
spellTargetSpec printing =
  Map.lookup
    (SlotName.MkSlotName (Text.pack "target"))
    (Modal.allTargetSpecs (Card.Type.spell (Printing.card printing)))

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
                  Object.sickness = Sickness.Settled alice,
                  Object.bindings = Map.empty,
                  Object.counters = Map.empty,
                  Object.attachedTo = Nothing,
                  Object.timestamp = ts
                }
         in gs2
              { GameState.objects = Map.insert oid obj (GameState.objects gs2),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs2)
              }
   in List.foldl' add (Setup.emptyGame bothPlayers) [1 .. n]

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
            Object.sickness = Sickness.Settled alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
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
            Object.source = Source.OfCard piker,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled alice,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
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
-- test needs when the door under test answers with a value (Pawl.Cost.pay's
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
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
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
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
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
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
   in length (filter named (Game.zoneMembers Zone.Battlefield pid gs))

damageOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe Natural.Natural
damageOf oid gs = fmap Object.damage (Game.lookupObject oid gs)

-- Projection.powerOf and Projection.toughnessOf, paired -- the shape an
-- Aura/pump-effect assertion wants (a single "is it a 4/2" check) rather than
-- two separate equalities. Nothing if either projects to Nothing.
powerToughnessOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe (Integer, Integer)
powerToughnessOf oid gs = (,) <$> Projection.powerOf oid gs <*> Projection.toughnessOf oid gs

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

-- Who revealed what, so far this turn, in order (CR 701.20a). Projects the
-- snapshot down to the card's NAME: a reveal shows every characteristic, but the
-- name is what identifies the card to the table and the only part an assertion
-- can write down legibly. A test that needs more reads Event.revealOf directly.
revealsOf :: GameState.GameState -> [(PlayerId.PlayerId, Text.Text)]
revealsOf gs = fmap (fmap PC.name) (Maybe.mapMaybe Event.revealOf (Foldable.toList (GameState.events gs)))

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
attackerDeclarationsOf gs = Maybe.mapMaybe declared (Foldable.toList (GameState.events gs))
  where
    declared event = case event of
      GameEvent.AttackerDeclared oid -> Just oid
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
withEvents :: [GameEvent.GameEvent] -> GameState.GameState -> GameState.GameState
withEvents events gs =
  gs
    { GameState.events = Seq.fromList events,
      GameState.scannedThrough = 0,
      GameState.damageScannedThrough = 0
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
            ActiveReplacement.timestamp = ts,
            ActiveReplacement.expiry = Expiry.AtCleanup,
            ActiveReplacement.uses = Uses.Once
          }
   in addReplacement active gs1

-- Seed a stored player effect directly into GameState (bypasses resolving the
-- spell that would install it; use when a test needs one active without a
-- resolution). Object id 998 is the stand-in source, the withEffectAt posture --
-- nothing here reads the source's own characteristics.
addPlayerEffect ::
  Expiry.Expiry ->
  PlayerScope.PlayerScope ->
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
-- counters without resolving a spell.
addCounter :: CounterKind.CounterKind -> Natural.Natural -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
addCounter kind n oid gs =
  let bump obj = obj {Object.counters = Map.insertWith (+) kind n (Object.counters obj)}
   in gs {GameState.objects = Map.adjust bump oid (GameState.objects gs)}

-- CR 303.4b: attach `rider` to `host` directly, without casting. A STATE fixture
-- (the shape addCreature and withEffect already have), not a synthetic card --
-- every printing a caller passes is real. Type-agnostic on purpose: CR 400.7's
-- reset is a property of the field, so the CR 400.7 test does not need an Aura.
attach :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
attach rider host gs =
  let set obj = obj {Object.attachedTo = Just host}
   in gs {GameState.objects = Map.adjust set rider (GameState.objects gs)}

-- Put `n` counters of a player-counter kind directly onto a player, bypassing
-- the diversion/effect that would add them -- so an SBA or cost test can set up
-- poison or energy without resolving anything.
addPlayerCounter :: PlayerCounterKind.PlayerCounterKind -> Natural.Natural -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addPlayerCounter kind n pid gs =
  let bump player = player {Player.counters = Map.insertWith (+) kind n (Player.counters player)}
   in gs {GameState.players = Map.adjust bump pid (GameState.players gs)}

-- How many counters of a kind an object has (absent kind = zero). The object
-- mirror of playerCounterOf below.
counterOf :: CounterKind.CounterKind -> ObjectId.ObjectId -> GameState.GameState -> Natural.Natural
counterOf kind oid gs =
  maybe 0 (Map.findWithDefault 0 kind . Object.counters) (Game.lookupObject oid gs)

-- Is this object still on the battlefield? A zone read, not a set-membership
-- one, so it stays correct for an object whose incarnation changed (CR 400.7).
onBattlefield :: ObjectId.ObjectId -> GameState.GameState -> Bool
onBattlefield oid gs = fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Battlefield

-- How many counters of a kind a player has (absent kind = zero).
playerCounterOf :: PlayerCounterKind.PlayerCounterKind -> PlayerId.PlayerId -> GameState.GameState -> Natural.Natural
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

-- LABELED SYNTHETIC: an emblem's characteristics are only its abilities (CR
-- 114.3), but pawl models no planeswalker/Ring to mint one, so tests use this
-- fixture -- an Elspeth-style anthem, "creatures you control get +1/+1". Built
-- by overriding a vanilla card's static abilities; the residual printed fields
-- are inert for a command-zone object (never projected as a permanent). (#125)
anthemEmblemCard :: Printing.Printing -> Card.Type.Card
anthemEmblemCard piker =
  (Printing.card piker)
    { Card.Type.staticAbilities =
        [ StaticAbility.MkStaticAbility
            { StaticAbility.affected =
                Affected.Matching
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You]),
              StaticAbility.modifications =
                NonEmpty.singleton (Modification.ModifyPowerToughness (Quantity.Type.Literal 1) (Quantity.Type.Literal 1))
            }
        ]
    }

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
            Object.source = Source.OfCard mountain,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = Timestamp.MkTimestamp 0
          }
   in GameState.MkGameState
        { GameState.objects = Map.singleton oid obj,
          GameState.library = Map.empty,
          GameState.hand = Map.singleton alice (Seq.singleton oid),
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.command = mempty,
          GameState.stack = [],
          GameState.players = Map.empty,
          GameState.manaPool = Map.empty,
          GameState.combat = Combat.emptyCombat,
          GameState.events = Seq.empty,
          GameState.lastKnown = Map.empty,
          GameState.scannedThrough = 0,
          GameState.damageScannedThrough = 0,
          GameState.delayedTriggers = Seq.empty,
          GameState.continuousEffects = [],
          GameState.replacements = [],
          GameState.playerEffects = [],
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
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty,
          GameState.pendingControl = Map.empty,
          GameState.activeControl = Nothing,
          GameState.monarch = Nothing,
          GameState.exiledUntilMonarch = Map.empty
        }

drawStep :: Game.Type.Game ()
drawStep = Engine.runTurnBasedActions (Phase.Beginning BeginningStep.DrawStep)

-- bob's Piker on the battlefield; alice casts a Bolt at it under identityAnswer
-- (lookupMin prefers ToCreature over ToPlayer, and the Piker is the only
-- creature). Returns (pre-cast state, post-cast state, Bolt's hand id).
boltAtBobsPiker :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
boltAtBobsPiker piker land bolt =
  let (_, withPiker) = addCreature piker bob (landsInPlay land 1)
      (gs, oid) = handOne bolt withPiker
   in (gs, snd (Engine.runGamePure identityAnswer gs (Cast.castSpell alice oid)), oid)

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
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.attachedTo = Nothing,
            Object.timestamp = ts
          }
   in ( oid,
        gs2
          { GameState.objects = Map.insert oid obj (GameState.objects gs2),
            GameState.stack = oid : GameState.stack gs2
          }
      )

-- Drive Pawl.Count.evaluate with the per-object quantity reader wired the way
-- the library wires it -- Pawl.Quantity.evaluate, which is where that knot is
-- tied (Pawl.Count cannot import it). Shared by every spec that drives the fold
-- directly, so the injection they test is the injection the engine makes.
countOf ::
  Count.ViewOf ->
  Filter.Context ->
  GameState.GameState ->
  Count.Type.Count Quantity.Type.Quantity ->
  Maybe Integer
countOf viewOf context gs = Count.evaluate viewOf (Quantity.evaluate viewOf context gs) context gs

-- Shared by Pawl.CountSpec and Pawl.ConditionSpec: a stub ViewOf, so a
-- Pawl.Count.evaluate fold is exercised apart from any real projection. Every
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
              { Filter.cardTypes = ts,
                Filter.supertypes = Set.empty,
                Filter.colors = Set.empty,
                Filter.subtypes = ss,
                Filter.power = Nothing,
                Filter.controller = ctrl,
                Filter.identity = Just o,
                Filter.playerIdentity = Nothing,
                Filter.attacking = False,
                Filter.blocking = False,
                Filter.attackedThisTurn = False,
                Filter.attachedToCreature = False,
                Filter.token = False
              }
        [] -> Nothing

-- Every card file in a registry's root, by slug. The corpus-wide checks need
-- the directory listing rather than a hand-kept list: a file nobody loads is
-- exactly the file a hand-kept list forgets. Kept as a String list because that
-- is what the sweeps feed back to Registry.card; the enumeration itself is the
-- library's since #167.
corpusSlugs :: Registry.Type.Registry -> IO [String]
corpusSlugs registry = fmap (fmap (Text.unpack . Slug.Type.toText)) (Registry.slugs registry)

allPrintings :: Registry.Type.Registry -> IO [Printing.Printing]
allPrintings = Registry.printings

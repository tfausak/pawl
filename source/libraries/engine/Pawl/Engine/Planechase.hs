-- | CR 901, the Planechase variant: the planar deck, CR 103.7's starting plane,
-- CR 701.31's planeswalk, and CR 901.9's planar die with the two things it can
-- set off -- CR 311.7's chaos and CR 901.8's planeswalking ability.
--
-- No GameSettings field, Pawl.Engine.Vanguard's posture: every rule here is
-- stated of a player's planar deck or a plane card, so having a planar deck is
-- the whole of being in the variant (isPlanechase).
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * CR 312's phenomena: the encounter trigger (CR 312.5) and CR 704.6f's
--     planeswalk away. A phenomenon turned face up here stays face up and does
--     nothing (#4309).
--   * CR 901.11's planeswalk triggers and durations -- "when you planeswalk to"
--     and "until a player planeswalks" (#4310).
--   * CR 901.12's Two-Headed Giant, CR 901.14's Grand Melee and CR 901.15's
--     single planar deck options, and CR 801.18's range-of-influence exemption
--     (#4312).
module Pawl.Engine.Planechase where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Plane as Plane
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Clause as Clause
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Effect as Effect
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.ManaAbilityPerformer as ManaAbilityPerformer
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.PlanarDieFace as PlanarDieFace
import qualified Pawl.Types.PlanarDieRolled as PlanarDieRolled
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerLimit as TriggerLimit
import qualified Pawl.Types.TriggerSource as TriggerSource
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 901.3: this player's planar deck, top first.
deckOf :: PlayerId -> GameState -> [ObjectId]
deckOf pid gs = foldMap Foldable.toList (Map.lookup pid (GameState.planarDecks gs))

-- | CR 103.3a: shuffle this player's planar deck.
shufflePlanarDeck :: PlayerId -> Game ()
shufflePlanarDeck pid = do
  gs <- State.get
  let ids = deckOf pid gs
  Monad.unless (null ids) $ do
    answer <- Game.ask (Prompt.Shuffle ids)
    let shuffled = Game.honourShuffle ids answer
    State.modify' (\g -> g {GameState.planarDecks = Map.insert pid (Seq.fromList shuffled) (GameState.planarDecks g)})

-- | CR 901.1: is this a Planechase game? Some player brought a planar deck.
isPlanechase :: GameState -> Bool
isPlanechase gs = not (Map.null (GameState.planarDecks gs))

-- | CR 901.4: the face-up plane and phenomenon cards.
faceUp :: GameState -> [ObjectId]
faceUp gs = filter (`Plane.isPlanarCard` gs) (Set.toAscList (GameState.command gs))

-- | CR 701.31a: may this player planeswalk? Only in a Planechase game, and only
-- the planar controller.
canPlaneswalk :: PlayerId -> GameState -> Bool
canPlaneswalk pid gs = isPlanechase gs && pid == Plane.planarController gs

-- | CR 701.31b: put each face-up plane and phenomenon card on the bottom of its
-- owner's planar deck, then turn the top card of this player's planar deck
-- face up. Nothing for a player CR 701.31a does not let planeswalk.
planeswalk :: PlayerId -> Game ()
planeswalk pid = do
  gs <- State.get
  Monad.when (canPlaneswalk pid gs) $ do
    Monad.mapM_ toBottom (faceUp gs)
    Monad.void (turnUpTop pid)

-- | CR 103.7: the starting player turns up planar cards until a plane is face
-- up, putting each phenomenon on the bottom. No ability triggers (CR 901.5):
-- nothing here records an event. Bounded by the deck's size, so a deck of
-- phenomena alone ends with none face up rather than looping.
setStartingPlane :: PlayerId -> Game ()
setStartingPlane pid = do
  gs <- State.get
  let go :: Int -> Game ()
      go n = Monad.when (n > 0) $ do
        turned <- turnUpTop pid
        g <- State.get
        case turned of
          Just oid | maybe False Plane.isPhenomenonFace (Game.faceOf oid g) -> toBottom oid >> go (n - 1)
          _ -> pure ()
  go (length (deckOf pid gs))

-- | CR 901.10 / 901.10a, run as these players leave the game and read against
-- `before`, the board as it was: if a face-up plane or phenomenon card they own
-- left with them, the planar controller -- already CR 800.4p's heir -- turns up
-- the top card of their planar deck, and if a face-up plane left, every
-- planeswalking ability on the stack ceases to exist. Not a state-based action.
-- A phenomenon leaving alone spares the ability, CR 901.10a naming a plane;
-- nothing observes that gate while phenomena are inert (gap #4309).
ownersLeft :: GameState -> [PlayerId] -> Game ()
ownersLeft before pids = do
  let owned oid = maybe False (\obj -> List.elem (Object.owner obj) pids) (Game.lookupObject oid before)
      left = filter owned (faceUp before)
      isPlane oid = maybe False (not . Plane.isPhenomenonFace) (Game.faceOf oid before)
  Monad.unless (null left) $ do
    Monad.when (any isPlane left) (State.modify' ceasePlaneswalking)
    gs <- State.get
    Monad.void (turnUpTop (Plane.planarController gs))

-- CR 901.10a: every planeswalking ability on the stack ceases to exist. Read as
-- CR 901.8's own inherent ability, which is the rule's and not a card's.
ceasePlaneswalking :: GameState -> GameState
ceasePlaneswalking gs =
  let walking oid = case fmap Object.source (Game.lookupObject oid gs) of
        Just (Source.OfInherentTrigger inherent) -> InherentTriggerSource.ability inherent == planeswalkingAbility
        _ -> False
      cease g oid = maybe g (\_ -> let g1 = Game.removeFromZones oid g in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}) (Game.lookupObject oid g)
   in List.foldl' cease gs (filter walking (GameState.stack gs))

-- CR 701.31b's second half: move the top card off the planar deck and turn it
-- face up, which is joining GameState.command. A fresh timestamp, since its
-- static abilities' effects begin now (CR 613.7a); CR 613.7f states the same of
-- a permanent turned face up.
turnUpTop :: PlayerId -> Game (Maybe ObjectId)
turnUpTop pid = do
  gs <- State.get
  case deckOf pid gs of
    [] -> pure Nothing
    top : rest -> do
      ts <- State.state Game.freshTimestamp
      State.modify' $ \g ->
        g
          { GameState.planarDecks = Map.insert pid (Seq.fromList rest) (GameState.planarDecks g),
            GameState.command = Set.insert top (GameState.command g),
            GameState.objects = Map.adjust (\o -> o {Object.timestamp = ts}) top (GameState.objects g)
          }
      pure (Just top)

-- CR 701.31b's first half: a face-up card goes to the bottom of its owner's
-- planar deck face down, and CR 311.6 / 312.6 make it a new object.
toBottom :: ObjectId -> Game ()
toBottom oid = do
  gs <- State.get
  case Game.lookupObject oid gs of
    Nothing -> pure ()
    Just obj -> do
      fresh <- State.state Game.freshObjectId
      let owner = Object.owner obj
      State.modify' $ \g ->
        g
          { GameState.command = Set.delete oid (GameState.command g),
            GameState.objects = Map.insert fresh (Object.newIncarnation obj) (Map.delete oid (GameState.objects g)),
            GameState.planarDecks = Map.insertWith (flip (Seq.><)) owner (Seq.singleton fresh) (GameState.planarDecks g)
          }

-- | CR 901.9: how many times this player has rolled the planar die this turn.
-- A fold over the event log, which is exactly "this turn"
-- (Pawl.Engine.Coin.flipsThisTurn's reason). Every roll is the special action's
-- today, so the count is CR 901.9's "times they have previously taken this
-- action".
rollsThisTurn :: PlayerId -> GameState -> Natural
rollsThisTurn pid gs =
  let rolled logged = case LoggedEvent.event logged of
        GameEvent.PlanarDieRolled r -> PlanarDieRolled.roller r == pid
        _ -> False
   in Natural.length (filter rolled (Foldable.toList (GameState.events gs)))

-- | CR 901.9: {1} for each earlier roll this turn, so the first is free.
rollCost :: PlayerId -> GameState -> Cost Keyword
rollCost pid gs =
  let n = rollsThisTurn pid gs
   in Cost.Type.MkCost
        { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic n | n > 0]),
          Cost.Type.components = []
        }

-- | CR 116.2i / 901.9: may this player roll the planar die now? The active
-- player, in a main phase of their turn with an empty stack, able to pay.
canRoll :: PlayerId -> GameState -> Bool
canRoll pid gs =
  isPlanechase gs
    && Turn.sorcerySpeedWindow pid gs
    && Cost.canPay PaymentSubject.ForNeither pid (GameState.nextObjectId gs) (rollCost pid gs) gs

-- | CR 901.3a: a six-sided die with one Planeswalker face, one chaos face and
-- four blank ones. Which number stands for which face is pawl's; anything the
-- answerer returns outside 1 to 6 is blank.
faceOf :: Natural -> PlanarDieFace.PlanarDieFace
faceOf n = case n of
  5 -> PlanarDieFace.Chaos
  6 -> PlanarDieFace.Planeswalker
  _ -> PlanarDieFace.Blank

-- | CR 116.2i / 901.9: pay, then roll the planar die. The roll records
-- DiceRolled (CR 901.9d) and PlanarDieRolled, which is what CR 311.7's chaos
-- abilities and CR 901.8's planeswalking ability trigger on (inherentPending).
-- A payment that fails restores the state, Pawl.Engine.Companion.take's
-- posture, and nothing is rolled.
roll :: ManaAbilityPerformer.ManaAbilityPerformer -> PlayerId -> Game ()
roll perform pid = do
  before <- State.get
  Monad.when (canRoll pid before) $ do
    noSource <- State.state Game.freshObjectId
    paid <- Cost.payAction perform before PaymentSubject.ForNeither 0 pid noSource (rollCost pid before)
    Monad.forM_ paid $ \_ -> do
      rolled <- Game.ask (Prompt.RollDie 6)
      State.modify' (Event.recordEvent (GameEvent.DiceRolled pid))
      State.modify' (Event.recordEvent (GameEvent.PlanarDieRolled (PlanarDieRolled.MkPlanarDieRolled pid (faceOf rolled))))

-- | CR 901.8: the planeswalking ability, "Whenever you roll the Planeswalker
-- symbol on the planar die, planeswalk." It has no source and its roller
-- controls it, so it is gathered here as a TriggerSource.Sourceless entry, the
-- Pawl.Engine.Rad.inherentPending posture.
inherentPending :: [GameEvent] -> GameState -> [PendingTrigger]
inherentPending events _ =
  let rolledPlaneswalker event = case event of
        GameEvent.PlanarDieRolled r | PlanarDieRolled.face r == PlanarDieFace.Planeswalker -> Just (PlanarDieRolled.roller r)
        _ -> Nothing
   in fmap (\pid -> PendingTrigger.MkPendingTrigger TriggerSource.Sourceless pid planeswalkingAbility Map.empty Nothing Nothing 1) (Maybe.mapMaybe rolledPlaneswalker events)

-- | CR 901.8's text. The condition is never matched -- inherentPending gathers
-- the ability off the event -- and PlayerRollsDice is the nearest description.
planeswalkingAbility :: TriggeredAbility Card (GrantedAbility.GrantedAbility Card)
planeswalkingAbility =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.PlayerRollsDice PlayerRelation.You,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton Effect.Planeswalk))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing,
      TriggeredAbility.name = Nothing,
      TriggeredAbility.limit = TriggerLimit.Unlimited
    }

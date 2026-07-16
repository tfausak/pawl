module Pawl.Setup where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Turn as Turn
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Printing (Printing)
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone

startingLife :: Integer
startingLife = 20

-- 36 Mountain / 24 Goblin Piker: enough lands to cast reliably, enough Pikers
-- that a random game actually exercises casting.
deckList :: [Printing]
deckList = replicate 36 Card.mountainPrinting ++ replicate 24 Card.pikerPrinting

deckSize :: Int
deckSize = length deckList

openingHand :: Int
openingHand = 7

-- Takes a NonEmpty so the active player is total (no partial head).
emptyGame :: NonEmpty.NonEmpty PlayerId -> GameState
emptyGame order =
  let order_ = NonEmpty.toList order
      newPlayer pid =
        ( pid,
          Player.MkPlayer
            { Player.life = startingLife,
              Player.status = Status.Playing
            }
        )
   in GameState.MkGameState
        { GameState.objects = Map.empty,
          GameState.library = Map.empty,
          GameState.hand = Map.empty,
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.stack = [],
          GameState.players = Map.fromList (map newPlayer order_),
          GameState.manaPool = Map.empty,
          GameState.turnOrder = order_,
          GameState.activePlayer = NonEmpty.head order,
          GameState.phase = Turn.firstPhase,
          GameState.priority = Nothing,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.nextObjectId = ObjectId.MkObjectId 0,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty
        }

createCard :: PlayerId -> Printing -> Game ObjectId
createCard pid printing = do
  gs <- State.get
  let (oid, gs1) = Game.freshObjectId gs
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.source = Source.OfCard printing,
            Object.zone = Zone.Library,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Sick
          }
      gs2 =
        gs1
          { GameState.objects = Map.insert oid obj (GameState.objects gs1),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs1)
          }
  State.put gs2
  pure oid

shuffleLibrary :: PlayerId -> Game ()
shuffleLibrary pid = do
  gs <- State.get
  let ids = Game.zoneMembers Zone.Library pid gs
  shuffled <- Trans.lift (Program.prompt (Prompt.Shuffle ids))
  State.put gs {GameState.library = Map.insert pid (Seq.fromList shuffled) (GameState.library gs)}

drawCard :: PlayerId -> Game ()
drawCard pid = do
  gs <- State.get
  case Game.zoneMembers Zone.Library pid gs of
    [] -> pure ()
    top : _ -> State.put (Game.changeZone top Zone.Hand gs)

newGame :: NonEmpty.NonEmpty PlayerId -> Game ()
newGame order = Monad.forM_ (NonEmpty.toList order) $ \pid -> do
  Monad.forM_ deckList (createCard pid)
  shuffleLibrary pid
  Monad.replicateM_ openingHand (drawCard pid)

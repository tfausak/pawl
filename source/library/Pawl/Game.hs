module Pawl.Game where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Pawl.Type.Card (Card)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Object (Object)
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.ObjectId as ObjectId
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Timestamp as Timestamp
import Pawl.Type.Zone (Zone)
import qualified Pawl.Type.Zone as Zone

lookupObject :: ObjectId -> GameState -> Maybe Object
lookupObject oid gs = Map.lookup oid (GameState.objects gs)

objectCount :: GameState -> Int
objectCount gs = Map.size (GameState.objects gs)

freshObjectId :: GameState -> (ObjectId, GameState)
freshObjectId gs =
  let ObjectId.MkObjectId n = GameState.nextObjectId gs
   in (ObjectId.MkObjectId n, gs {GameState.nextObjectId = ObjectId.MkObjectId (n + 1)})

freshTimestamp :: GameState -> (Timestamp.Timestamp, GameState)
freshTimestamp gs =
  let Timestamp.MkTimestamp n = GameState.nextTimestamp gs
   in (Timestamp.MkTimestamp n, gs {GameState.nextTimestamp = Timestamp.MkTimestamp (n + 1)})

zoneMembers :: Zone -> PlayerId -> GameState -> [ObjectId]
zoneMembers zone pid gs =
  let perPlayer m = maybe [] (foldr (:) []) (Map.lookup pid m)
      ownedBy oid = case lookupObject oid gs of
        Just obj -> Object.owner obj == pid
        Nothing -> False
      ownedShared s = filter ownedBy (Set.toList s)
   in case zone of
        Zone.Library -> perPlayer (GameState.library gs)
        Zone.Hand -> perPlayer (GameState.hand gs)
        Zone.Graveyard -> perPlayer (GameState.graveyard gs)
        Zone.Battlefield -> ownedShared (GameState.battlefield gs)
        Zone.Exile -> ownedShared (GameState.exile gs)
        Zone.Stack -> filter ownedBy (GameState.stack gs)

removeFromZones :: PlayerId -> ObjectId -> GameState -> GameState
removeFromZones pid oid gs =
  gs
    { GameState.library = Map.adjust (Seq.filter (/= oid)) pid (GameState.library gs),
      GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs),
      GameState.graveyard = Map.adjust (Seq.filter (/= oid)) pid (GameState.graveyard gs),
      GameState.battlefield = Set.delete oid (GameState.battlefield gs),
      GameState.exile = Set.delete oid (GameState.exile gs),
      GameState.stack = filter (/= oid) (GameState.stack gs)
    }

insertIntoZone :: Zone -> PlayerId -> ObjectId -> GameState -> GameState
insertIntoZone zone pid oid gs = case zone of
  Zone.Library -> gs {GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs)}
  Zone.Hand -> gs {GameState.hand = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.hand gs)}
  Zone.Graveyard -> gs {GameState.graveyard = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.graveyard gs)}
  Zone.Battlefield -> gs {GameState.battlefield = Set.insert oid (GameState.battlefield gs)}
  Zone.Exile -> gs {GameState.exile = Set.insert oid (GameState.exile gs)}
  Zone.Stack -> gs {GameState.stack = oid : GameState.stack gs}

-- The card an object is a copy of. Nothing when the id is unknown.
cardOf :: ObjectId -> GameState -> Maybe Card
cardOf oid gs = case lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.source obj of
    Source.OfCard printing -> Just (Printing.card printing)
    Source.OfAbility _ _ -> Nothing
    Source.OfTrigger _ _ -> Nothing

-- Who controls an object (CR 108.4). Nothing when the id is unknown.
--
-- Owner stands in for controller: nothing in M1b can change control, so the two
-- are provably identical, and a real field would be dead state that every
-- fixture must maintain and no card could observe drifting.
--
-- EXPIRES at M3 (Mindslaver). Everything that needs a controller calls this and
-- never Object.owner, so that change is one function rather than every call site.
controllerOf :: ObjectId -> GameState -> Maybe PlayerId
controllerOf oid gs = fmap Object.owner (lookupObject oid gs)

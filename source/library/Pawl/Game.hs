module Pawl.Game where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Quantity as Quantity
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Card as Card
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Object (Object)
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.ObjectId as ObjectId
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Toughness as Toughness
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

-- The single zone-change primitive (CR 400.7): the source object ceases; a NEW
-- object with a fresh id is created in the destination, carrying owner and
-- source forward and resetting per-incarnation state. No-op if the id is unknown.
changeZone :: ObjectId -> Zone -> GameState -> GameState
changeZone oid dest gs = case lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let pid = Object.owner obj
        (newId, gs1) = freshObjectId gs
        newObj = obj {Object.zone = dest, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Sick, Object.targets = Map.empty}
        gs2 = removeFromZones pid oid gs1
        gs3 = gs2 {GameState.objects = Map.insert newId newObj (Map.delete oid (GameState.objects gs2))}
     in insertIntoZone dest pid newId gs3

-- The card an object is a copy of. Nothing when the id is unknown.
cardOf :: ObjectId -> GameState -> Maybe Card
cardOf oid gs = case lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.source obj of
    Source.OfCard printing -> Just (Printing.card printing)

-- Nothing when the object has no power at all (a land), or when the value cannot
-- be determined yet (a Star, once M3's layer system exists).
powerOf :: ObjectId -> GameState -> Maybe Integer
powerOf oid gs = case fmap Card.power (cardOf oid gs) of
  Just (Just (Power.MkPower quantity)) -> Quantity.evaluate gs oid quantity
  _ -> Nothing

toughnessOf :: ObjectId -> GameState -> Maybe Integer
toughnessOf oid gs = case fmap Card.toughness (cardOf oid gs) of
  Just (Just (Toughness.MkToughness quantity)) -> Quantity.evaluate gs oid quantity
  _ -> Nothing

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

-- The keywords an object currently has (CR 702). Empty when the id is unknown.
--
-- A function, not a field read, and that is the whole point. Today this is
-- provably Card.keywords of the object's printing -- nothing in M2a grants or
-- removes an ability -- so reading the field directly from Pawl.Combat would
-- compile and pass every test. It would also be wrong in a dozen call sites at
-- once the moment Magical Hack and Humility arrive.
--
-- EXPIRES at M3: layer 6 grants and removes abilities, at which point this
-- consults the layer system. Everything that needs a keyword calls this and never
-- Card.keywords, so that change is one function rather than every call site. Same
-- move as controllerOf, and as M1a's Mana.manaSources.
keywordsOf :: ObjectId -> GameState -> Set Keyword
keywordsOf oid gs = maybe Set.empty Card.keywords (cardOf oid gs)

hasKeyword :: Keyword -> ObjectId -> GameState -> Bool
hasKeyword keyword oid gs = Set.member keyword (keywordsOf oid gs)

module Pawl.Engine.Players where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import Pawl.Types.Recipient (Recipient)
import qualified Pawl.Types.Recipient as Recipient
import Pawl.Types.SlotName (SlotName)

-- | What a position asking "which players?" can read, so that 'named' answers
-- every PlayerRef the same way wherever it is asked. The two askers differ only
-- here: a count (Pawl.Engine.Count.playersFor) reads its slots off a
-- Filter.Context and its controllers off the caller's view, and a resolution
-- (Pawl.Engine.Resolve.Slots.playerRefPlayers) off CR 608.2b's legal slots and
-- CR 608.2h's last known information.
data Reads = MkReads
  { -- | CR 109.5's "you". Nothing is an unframed evaluation, which only a
    -- perspective-free reference can answer.
    perspective :: Maybe PlayerId,
    -- | Whether the evaluation carries bindings at all, so that a slot naming
    -- nobody can exclude nobody. A reader with no source has none, and an
    -- excluding reference names nobody there (Pawl.Engine.Mana.recipientsOf).
    bound :: Bool,
    -- | CR 601.2c: the players a slot names, Nothing where it is not bound.
    slotPlayers :: SlotName -> Maybe [PlayerId],
    -- | The ONE object a slot names, Nothing where it names none or several.
    slotObject :: SlotName -> Maybe ObjectId,
    -- | CR 613.1b's controller of an object, as this position sees it.
    controllerOf :: ObjectId -> Maybe PlayerId,
    -- | CR 108.3's owner of an object, as this position sees it.
    ownerOf :: ObjectId -> Maybe PlayerId
  }

-- | What a RESOLUTION reads (CR 608.2): its controller as "you", the slots it
-- filled (CR 608.2b's legal ones), and the controller and owner reads the
-- caller supplies -- CR 608.2h's last known information, from a module that
-- sees the projection.
resolution :: (ObjectId -> Maybe PlayerId) -> (ObjectId -> Maybe PlayerId) -> Map.Map SlotName (Set Recipient) -> PlayerId -> Reads
resolution controllerRead ownerRead slots controller =
  MkReads
    { perspective = Just controller,
      bound = True,
      slotPlayers = \slot -> fmap (Maybe.mapMaybe Recipient.playerOf . Set.toList) (Map.lookup slot slots),
      slotObject = \slot -> Map.lookup slot slots >>= Binding.onlyOne >>= Recipient.objectOf,
      controllerOf = controllerRead,
      ownerOf = ownerRead
    }

-- | CR 102.1 / 801.10 / 801.11: the players still in the game that the
-- perspective reaches. A departed player keeps their row in GameState.players,
-- so this is Game.stillPlaying rather than the map's keys; an unframed
-- evaluation has no controller to measure range from, and cuts nothing else.
table :: Maybe PlayerId -> GameState -> [PlayerId]
table viewer gs = maybe (Game.stillPlaying gs) (`Game.reachableBy` gs) viewer

-- | The players on 'table' standing in this relation to the perspective,
-- judged once through PlayerRelation.holds. Nothing where the relation needs a
-- perspective and there is none.
--
-- "You" names the perspective outright: CR 801.2b keeps a player in their own
-- range, so the range cut has nothing to remove, and CR 800.4i reads a
-- departed player through their last known information rather than dropping
-- them -- Specific's posture.
related :: Maybe PlayerId -> GameState -> PlayerRelation.PlayerRelation -> Maybe [PlayerId]
related viewer gs relation = case viewer of
  Just you -> case relation of
    PlayerRelation.You -> Just [you]
    _ -> Just (filter (PlayerRelation.holds (Game.teams gs) relation you) (table viewer gs))
  Nothing
    | PlayerRelation.perspectiveFree relation -> Just (table viewer gs)
    | otherwise -> Nothing

-- | The players a PlayerRef names, in PlayerId order, or Nothing where the
-- position cannot answer it. THE one reading of every PlayerRef arm; a caller
-- with an ordering rule imposes it, and a caller with no use for
-- "unanswerable" reads Nothing as nobody.
named :: Reads -> GameState -> PlayerRef.PlayerRef -> Maybe [PlayerId]
named given gs ref =
  let you = perspective given
      -- CR 702.26b: a baked object that is phased out names nothing.
      unlessPhasedOut oid = if Map.member oid (GameState.phasedOut gs) then Nothing else Just oid
      -- A slot naming nobody excludes nobody; with no bindings at all, nothing
      -- was there to exclude anybody.
      excluding name = if bound given then Just (Maybe.fromMaybe [] (slotPlayers given name)) else Nothing
   in case ref of
        PlayerRef.Relative relation -> related you gs relation
        -- Every player minus every player the slot names (CR 104.2c's winning
        -- team is several). A source that existed and then ceased, which CR
        -- 729.5 leaves a resumed resolution holding, is not looked up: the
        -- resolution's context carries the slot, read through
        -- Pawl.Engine.Resolve.liveBindings. Pawl.OutsideTheGameSpec's Synthetic
        -- Subgame Tithe case proves it.
        PlayerRef.EachPlayerExcept name -> do
          excluded <- excluding name
          fmap (filter (`notElem` excluded)) (related you gs PlayerRelation.AnyPlayer)
        -- CR 702.116a's "each opponent other than defending player": the arm
        -- above narrowed by CR 102.2 / 102.3.
        PlayerRef.EachOpponentExcept name -> do
          excluded <- excluding name
          fmap (filter (`notElem` excluded)) (related you gs PlayerRelation.Opponent)
        -- ONE player or none: declining a slot that names several is CR
        -- 601.2c's own answer, the one Binding.onlyOne gives every other such
        -- reader. A slot's target is in range already (CR 801.4).
        PlayerRef.InSlot name -> case slotPlayers given name of
          Just [pid] -> Just [pid]
          _ -> Nothing
        -- InSlot's plural, off the same read: every player the slot names.
        -- Jungle Wayfinder's CR 603.5 "may" seats and Bellowing Mauler's
        -- Binding.gatePlayers are read this way.
        PlayerRef.EachInSlot name -> slotPlayers given name
        -- The baked seat. Not filtered against the roster: it names one specific
        -- player, and what a departed seat can still be TRUE of is the reader's
        -- question -- CR 725.4 takes the crown off a player as they leave, so
        -- Quantity.IsMonarch reads 0 for one and Garland's duration ends.
        PlayerRef.Specific pid -> Just [pid]
        -- The fold's own candidate, which is a fact about the member being read
        -- rather than about the board. Pawl.Engine.Quantity answers it where the
        -- view is, Quantity.forCandidate substitutes it in a SCOPE, and the
        -- Effect.Search arm's ownersFor substitutes the searcher; what reaches
        -- here has no candidate of either kind.
        PlayerRef.Candidate -> Nothing
        -- CR 613.1b / CR 608.2h: the controller of the object a slot names, as
        -- the position sees it -- layer 2 decides who controls a permanent, and
        -- the clause naming the player generally moved it first.
        -- Pawl.ResolveSpec's "bob, who controlled the bounced creature, went 20
        -- -> 19" (Vapor Snag) proves the resolution's last-known road, and
        -- Flunk's "that creature's controller's hand" (Pawl.CountSpec) a count's.
        PlayerRef.ControllerOfBound slot -> fmap pure (slotObject given slot >>= controllerOf given)
        -- CR 108.3's owner, one field over. The Deck of Many Things' 20 band is
        -- the producer (Pawl.CardSpec): reanimating an opponent's creature and
        -- having that opponent lose the game is where owner and controller part.
        PlayerRef.OwnerOfBound slot -> fmap pure (slotObject given slot >>= ownerOf given)
        -- CR 611.2b: the two arms above, BAKED -- the object named outright, so a
        -- stored duration asks who controls or owns it NOW. Unanswered while the
        -- object is phased out, which CR 702.26b treats as not existing. Proved
        -- by
        -- data/scenarios/cr-611-2b-a-stored-duration-reads-its-target-s-controller-and-owner-after-resolution.json
        -- and, for phasing,
        -- data/scenarios/phasing/cr-702-26f-a-duration-reading-its-permanent-s-controller-ends-when-it-phases-out.json.
        PlayerRef.ControllerOfObject oid -> fmap pure (unlessPhasedOut oid >>= controllerOf given)
        PlayerRef.OwnerOfObject oid -> fmap pure (unlessPhasedOut oid >>= ownerOf given)
        -- CR 614.1c / CR 702.174b: the player the object a slot names chose,
        -- read off Object.chosenPlayer rather than any view -- a choice is a
        -- record, not a characteristic (CR 707.2) -- through CR 608.2h's
        -- look-back. Pawl.CastSpec's Scrapshooter killed in response still has
        -- the promised opponent draw.
        PlayerRef.ChosenPlayerOfBound slot -> fmap pure (slotObject given slot >>= (`Game.chosenPlayerWithLastKnown` gs))
        -- CR 508.6: the players controlling a creature attacking the player a
        -- slot names, narrowed by the printed relation -- Curse of Vitality's
        -- "each opponent attacking that player". The LIVE combat record (CR
        -- 608.2c): the sentence is present tense, so a creature CR 506.4 removed
        -- from combat has taken its controller out. AttackTarget.OfPlayer alone,
        -- CR 508.1b listing player, planeswalker and battle separately.
        -- Unanswered where the slot names no one player.
        PlayerRef.Attacking (AttackingPlayers.MkAttackingPlayers relation slot) -> do
          attacked <- case slotPlayers given slot of
            Just [pid] -> Just pid
            _ -> Nothing
          let sentAt = Map.keys (Map.filter (== AttackTarget.OfPlayer attacked) (Combat.attackers (GameState.combat gs)))
              attackers = Maybe.mapMaybe (controllerOf given) sentAt
          fmap (filter (`elem` attackers)) (related you gs relation)

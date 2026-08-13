-- | CR 903, the Commander variant, as far as one commander in the command zone
-- reaches it: CR 903.3's designation, CR 903.6's starting zone, CR 903.8's cost
-- increase, CR 903.9's replacement sending a commander back to the command zone
-- instead of anywhere else, and CR 903.10a's twenty-one-damage loss.
--
-- A FORMAT and not a card, which is what makes this module the right home for
-- all five. No card's printed text says "cast this from the command zone" or
-- "this costs {2} more" -- rule 903 says both, of whatever card the player
-- designated. So none of this is a Pawl.Types.CastingPermission, a
-- Pawl.Types.PlayerEffect or a printed replacement: those are things a CARD
-- carries, and reading them here would be the rules core learning from data a
-- rule it already knows. Pawl.Engine.Cast.legendaryRestrictionOk makes the same
-- argument for CR 205.4e.
--
-- The designation is a PRINTING on the player, for the reason
-- Pawl.Types.Player.commander gives: CR 400.7 mints a fresh object id on every
-- zone change and a commander crosses zones constantly, so nothing keyed to an
-- object could survive its first cast.
--
-- WHAT IS NOT IMPLEMENTED, none of which the pool can reach:
--
--   * CR 903.4's colour identity and CR 903.5's singleton deck construction
--     (#940) -- both are deck-legality rules, and pawl validates no deck.
--   * Two commanders via partner or a background (#939).
--   * The Brawl and Oathbreaker variants (CR 903.12 and beyond).
module Pawl.Engine.Commander where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | CR 903.3: record which card this player designated as their commander.
-- Called once per player by Pawl.Engine.Setup.createDeck, from the Deck.
designate :: PlayerId -> Printing.Printing -> GameState -> GameState
designate pid printing gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.commander = Just printing}) pid (GameState.players gs)}

-- | CR 903.3: is this object its owner's commander?
--
-- Asked of the OWNER and never the controller, because rule 903.3 designates a
-- card from the deck its owner brought -- a stolen commander is still its owner's
-- commander, which is also what makes CR 903.9 send it to its owner's command
-- zone rather than the thief's.
--
-- Matches on the PRINTING, which is exact under CR 903.5's singleton rule and is
-- the only thing that survives CR 400.7's fresh id. Pawl does not enforce rule
-- 903.5 (#940), so a deck holding two copies of its commander would have both
-- answer True here.
isCommander :: ObjectId -> GameState -> Bool
isCommander oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> case Object.source obj of
    Source.OfCard printing ->
      fmap Player.commander (Map.lookup (Object.owner obj) (GameState.players gs)) == Just (Just printing)
    _ -> False

-- | CR 903.10a's key: the owner of the commander that dealt this damage, or
-- Nothing when the source was not a commander at all.
--
-- The OWNER and not the object, for `isCommander`'s reason: rule 903.3's
-- designation survives CR 400.7's fresh incarnations and Player.commander is
-- one printing per player, so the owner names exactly one commander (#939 is
-- the partner/background widening).
commanderOwnerOf :: ObjectId -> GameState -> Maybe PlayerId
commanderOwnerOf oid gs
  | isCommander oid gs = fmap Object.owner (Game.lookupObject oid gs)
  | otherwise = Nothing

-- | CR 903.10a / CR 704.6c: "a player who's been dealt 21 or more combat damage
-- by the same commander over the course of the game loses the game". The
-- predicate Pawl.Engine.Sba.losesNow reads, kept here for the reason this
-- module's header gives -- Sba owns WHEN a state-based action is checked, not
-- what each one means, the same split it takes with CR 704.5z and
-- Pawl.Engine.Speed.
--
-- The MAXIMUM over the tally and never its sum, which is the whole of "by the
-- SAME commander": two commanders that between them dealt 24 have killed
-- nobody.
--
-- ">= 21" and not "== 21", because rule 903.10a says "21 or more" and one
-- damage event can carry the difference on its own.
lethalDamage :: PlayerId -> GameState -> Bool
lethalDamage pid gs = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player -> any (>= 21) (Map.elems (Player.commanderDamage player))

-- | CR 903.8: "a commander's owner may cast it from the command zone for its mana
-- cost plus {2} for each previous time they cast it from the command zone this
-- game". This is the {2}-per-previous-cast part, as an amount of GENERIC mana.
--
-- Zero when the object is not a commander, when its owner is not the caster, or
-- when it has never been cast from there -- so every ordinary spell in every
-- ordinary game gets 0 and the tax costs nothing to ask about.
--
-- Read as a CR 601.2f cost INCREASE by Pawl.Engine.Cost, alongside the ones cards
-- generate. It belongs on that side of rule 601.2f rather than being folded into
-- the mana cost, because rule 903.8 says "plus {2}" -- an increase applied after
-- the cost is determined, so a cost reduction applies to the total afterwards
-- exactly as CR 601.2f orders them.
tax :: PlayerId -> ObjectId -> GameState -> Natural
tax pid oid gs
  | not (canCastFromCommandZone pid oid gs) = 0
  -- Rule 903.8 taxes casting it FROM THE COMMAND ZONE, so the object has to be
  -- there NOW. Without this the tax would follow the card everywhere: a commander
  -- returned to its owner's hand and cast from there is cast for its printed cost,
  -- and so is one flickered back onto the battlefield. It also keeps every
  -- ACTIVATED ability off the tax, since Pawl.Engine.Cost.total is asked about
  -- those too and a commander on the battlefield is not in the command zone.
  | not (Set.member oid (GameState.command gs)) = 0
  | otherwise = 2 * castCount pid gs

-- | CR 903.8's permission half: may this player cast this object from the command
-- zone? Only its OWNER may, and only if it is their commander -- rule 903.8 says
-- "a commander's owner may cast it from the command zone", and nothing else in
-- the pool is castable from there at all.
canCastFromCommandZone :: PlayerId -> ObjectId -> GameState -> Bool
canCastFromCommandZone pid oid gs =
  isCommander oid gs && fmap Object.owner (Game.lookupObject oid gs) == Just pid

-- | CR 903.8's "each previous time they cast it from the command zone this game".
castCount :: PlayerId -> GameState -> Natural
castCount pid gs = maybe 0 Player.commanderCasts (Map.lookup pid (GameState.players gs))

-- | CR 903.8's tax as a function on ONE candidate cost: add {2} per previous cast
-- to its mana part, or leave it alone when there is no tax to add.
--
-- Takes the PRE-MOVE object, because `tax` asks whether it is in the command zone
-- and CR 601.2a's move has not happened yet at the only call site
-- (Pawl.Engine.Cast.castSpell), which is the whole reason this exists rather than
-- the tax being left to Cost.total.
--
-- A cost with no mana part (Nothing, CR 118.6a's unpayable) stays unpayable: fmap
-- leaves it alone rather than inventing a {2} cost for it.
taxCandidates :: PlayerId -> ObjectId -> GameState -> Cost.Cost Keyword.Keyword -> Cost.Cost Keyword.Keyword
taxCandidates pid oid gs cost =
  let owed = tax pid oid gs
      add (ManaCost.MkManaCost symbols) = ManaCost.MkManaCost (symbols <> [ManaSymbol.Generic owed])
   in if owed == 0 then cost else cost {Cost.mana = fmap add (Cost.mana cost)}

-- | CR 903.9a: the commanders their owners may put into the command zone right
-- now, paired with the owner to ask.
--
-- Rule 903.9a has two conditions and both are here. The object must be IN a
-- graveyard or in exile, and it must have arrived there "since the last time
-- state-based actions were checked" -- which is what keeps a declining owner from
-- being asked again on every subsequent check, and what makes the offer land once
-- per arrival. The watermark is GameState.damageScannedThrough, which is not a
-- damage-only mark despite its name: it is how far the STATE-BASED ACTION CHECK
-- has consumed the event log, the same boundary CR 704.5h reads and the same one
-- rule 903.9a's "since the last time" names.
--
-- Keyed off the ZoneChange's NEW id (ZoneChange.object), because that is the
-- incarnation now sitting in the graveyard; ZoneChange.departed named the one that
-- left the battlefield and no longer exists.
--
-- NOT a replacement effect. Rule 903.9a says "this is a state-based action" in so
-- many words, which is why it is classified in Pawl.Engine.Sba beside CR 704.5's
-- own list rather than installed as a Pawl.Types.ReplacementEffect. Its sibling CR
-- 903.9b -- hand and library, which IS a replacement, and an explicit exception to
-- CR 614.5 at that -- is not implemented (#942).
returnable :: GameState -> [(PlayerId, ObjectId)]
returnable gs =
  let arrivals = Maybe.mapMaybe arrivalOf (Event.unscannedSbaEvents gs)
      arrivalOf event = case event of
        GameEvent.Moved (Moved.MkMoved zc _)
          | elem (ZoneChange.to zc) [Zone.Graveyard, Zone.Exile] -> Just (ZoneChange.object zc)
        _ -> Nothing
      stillThere oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> elem (Object.zone obj) [Zone.Graveyard, Zone.Exile]
      offer oid = case Game.lookupObject oid gs of
        Just obj | isCommander oid gs && stillThere oid -> Just (Object.owner obj, oid)
        _ -> Nothing
   in Maybe.mapMaybe offer (List.nub arrivals)

-- | CR 903.8's counter, bumped as the cast is announced. Called by
-- Pawl.Engine.Cast only when the spell left the COMMAND ZONE -- a commander cast
-- from a hand or a graveyard makes no later cast dearer, which is what rule
-- 903.8's "from the command zone" restricts.
recordCast :: PlayerId -> GameState -> GameState
recordCast pid gs =
  gs
    { GameState.players =
        Map.adjust (\p -> p {Player.commanderCasts = Player.commanderCasts p + 1}) pid (GameState.players gs)
    }

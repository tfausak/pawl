{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.InZone where

import qualified Data.Text as Text
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

-- | CR 400.1: "each player has their own library, hand, and graveyard. The other
-- zones are shared by all players." Exhaustive rather than a fallthrough, so a
-- new zone has to say which half of that sentence it is in and @-Werror@ is what
-- asks.
shared :: Zone.Zone -> Bool
shared zone = case zone of
  Zone.Library -> False
  Zone.Hand -> False
  Zone.Graveyard -> False
  Zone.Battlefield -> True
  Zone.Stack -> True
  Zone.Exile -> True
  Zone.Command -> True

-- | The invariant Pawl.Types.InZone states, enforced HERE, this being where a
-- scope enters the engine at all: a shared zone is undivided, so "one player's
-- share of the battlefield" names nothing, and the reference beside such a zone
-- can only be the whole table. It reads both fields, which is what
-- 'Fields.objectWith' is for.
--
-- The question a card means when it writes one is CONTROL or OWNERSHIP of the
-- objects in the zone rather than a share of the zone itself -- Nightmare's
-- "Swamps you control" is 'PlayerRef.EachPlayer' with a
-- @Filter.ControlledBy You@ conjunct -- and both have their own spelling in the
-- Filter, which is where CR 110.2 and CR 108.3 can come apart. Without this the
-- pairing decoded and Pawl.Engine.Count answered it off Game.zoneMembers, which
-- slices the shared battlefield by OWNER: a number, silently, for a question the
-- rules do not have (#161).
undividedShared :: InZone.InZone -> Either Text.Text InZone.InZone
undividedShared inZone
  | shared (InZone.zone inZone) && InZone.player inZone /= PlayerRef.EachPlayer =
      Left (Text.pack ("InZone: CR 400.1 makes " <> show (InZone.zone inZone) <> " shared by all players, so it cannot be scoped to " <> show (InZone.player inZone)))
  | otherwise = Right inZone

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec InZone.InZone
codec = Fields.objectWith undividedShared $ do
  zone <- Fields.required "zone" Zone.codec InZone.zone
  player <- Fields.required "player" PlayerRef.codec InZone.player
  pure InZone.MkInZone {InZone.zone = zone, InZone.player = player}

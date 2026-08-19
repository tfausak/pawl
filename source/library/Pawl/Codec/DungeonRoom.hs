{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DungeonRoom where

import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Face as Face

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (DungeonRoom.DungeonRoom card)
codec cardCodec = Fields.object $ do
  name <- Fields.required "name" AbilityName.codec DungeonRoom.name
  -- CR 309.4c: a room whose printed effect pawl cannot express writes no
  -- ability, and the default is Face.defaultSpell -- one empty mode, the same
  -- value a vanilla creature's spell takes.
  ability <- Fields.defaulted "ability" Face.defaultSpell (Modal.codec cardCodec) DungeonRoom.ability
  -- CR 309.4: the bottommost room has no arrows out of it, so an empty set is
  -- the common case and writes no key.
  exits <- Fields.defaulted "exits" Set.empty (Common.set RoomIndex.codec) DungeonRoom.exits
  pure
    DungeonRoom.MkDungeonRoom
      { DungeonRoom.name = name,
        DungeonRoom.ability = ability,
        DungeonRoom.exits = exits
      }

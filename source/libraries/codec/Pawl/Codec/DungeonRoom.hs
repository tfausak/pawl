module Pawl.Codec.DungeonRoom where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Face as Face

toJson :: (Eq card) => (card -> Value.Value) -> DungeonRoom.DungeonRoom card -> Value.Value
toJson codec room =
  Value.object . concat $
    [ Common.requiredPair "name" (Codec.encode AbilityName.codec) (DungeonRoom.name room),
      -- CR 309.4c: a room whose printed effect pawl cannot express writes no
      -- ability, and the default is Face.defaultSpell -- one empty mode, the same
      -- value a vanilla creature's spell takes.
      Common.optionalPair "ability" Face.defaultSpell (Modal.toJson codec) (DungeonRoom.ability room),
      -- CR 309.4: the bottommost room has no arrows out of it, so an empty set is
      -- the common case and writes no key.
      Common.optionalPair "exits" Set.empty (Common.encodeSet (Codec.encode RoomIndex.codec)) (DungeonRoom.exits room)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (DungeonRoom.DungeonRoom card)
fromJson decode value = do
  ps <- Common.asObject value
  n <- Common.field "name" ps >>= Codec.decode AbilityName.codec
  a <- Common.defaultedField "ability" Face.defaultSpell (Modal.fromJson decode) ps
  e <- Common.defaultedField "exits" Set.empty (Common.decodeSet (Codec.decode RoomIndex.codec)) ps
  pure
    DungeonRoom.MkDungeonRoom
      { DungeonRoom.name = n,
        DungeonRoom.ability = a,
        DungeonRoom.exits = e
      }

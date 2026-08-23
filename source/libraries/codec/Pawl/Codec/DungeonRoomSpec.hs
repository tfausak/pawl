module Pawl.Codec.DungeonRoomSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.DungeonRoom as DungeonRoom
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.DungeonRoom as DungeonRoom
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RoomIndex as RoomIndex

-- | The `card` parameter is instantiated at 'Text.Text', TriggeredAbilitySpec's
-- posture: the codec reaches it only through the supplied Modal codec.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (DungeonRoom.DungeonRoom Text.Text)
codec = DungeonRoom.codec cardCodec

toJson :: DungeonRoom.DungeonRoom Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (DungeonRoom.DungeonRoom Text.Text)
fromJson = Codec.decode codec

drawOne :: Modal.Modal Text.Text
drawOne =
  Modal.MkModal
    ( Seq.singleton
        ( Mode.MkMode
            (Seq.singleton (Clause.MkClause Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Draw (PlayerQuantity.MkPlayerQuantity (PlayerRef.Relative PlayerRelation.You) (Quantity.Literal 1))))))
            Map.empty
        )
    )
    (ModeSelection.ChooseExactly 1)

-- One constructor, so two cases: a room with arrows and an ability, and the
-- bottommost room of a dungeon whose printed effect pawl cannot express -- which
-- writes neither optional key.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DungeonRoom" $ do
  Spec.it s "MkDungeonRoom, Lost Mine of Phandelver's Temple of Dumathoin" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( DungeonRoom.MkDungeonRoom
          { DungeonRoom.name = AbilityName.MkAbilityName (Text.pack "Temple of Dumathoin"),
            DungeonRoom.ability = drawOne,
            DungeonRoom.exits = Set.empty
          }
      )
      " {\"name\":\"Temple of Dumathoin\",\"ability\":{\"modes\":[{\"clauses\":[{\"effects\":[{\"type\":\"Draw\",\"value\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"quantity\":{\"type\":\"Literal\",\"value\":1}}}]}]}]}} "
  -- CR 309.5a: a room with two arrows out of it. The ability is left at its
  -- default, which is what a room whose printed effect pawl cannot express writes.
  Spec.it s "MkDungeonRoom, Cave Entrance's two arrows and no expressible ability" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( DungeonRoom.MkDungeonRoom
          { DungeonRoom.name = AbilityName.MkAbilityName (Text.pack "Cave Entrance"),
            DungeonRoom.ability = Face.defaultSpell,
            DungeonRoom.exits = Set.fromList [RoomIndex.MkRoomIndex 1, RoomIndex.MkRoomIndex 2]
          }
      )
      " {\"name\":\"Cave Entrance\",\"exits\":[1,2]} "

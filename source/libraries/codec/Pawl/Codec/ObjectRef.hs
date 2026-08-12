module Pawl.Codec.ObjectRef where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | Tagged like every other sum. The five arms were previously told apart by
-- JSON TYPE -- a string was a slot, an object the battlefield sweep's Filter,
-- and everything else an array leading with its own word -- which no schema can
-- state as a claim the decoder guarantees (#1304). The word an array led with
-- was already a tag in all but name; it now sits under @type@ where every other
-- sum's does, and the arms carrying no payload or one need no array at all.
--
-- Still a loose toJson\/fromJson pair rather than an 'Pawl.JsonCodec.Arm.tagged'
-- bundle: 'Pawl.Codec.PlayerRef' is not a bundle yet, so 'TopOfLibrary' has no
-- 'Codec.Codec' to hand 'Pawl.JsonCodec.Arm.payload' (#1263).
toJson :: ObjectRef.ObjectRef -> Value.Value
toJson r = case r of
  ObjectRef.InSlot n -> Common.tagged "InSlot" . Just $ Codec.encode SlotName.codec n
  ObjectRef.EachMatching f -> Common.tagged "EachMatching" . Just $ Codec.encode (Filter.codec Keyword.codec) f
  ObjectRef.EachCardInGraveyard s f ->
    Common.tagged "EachCardInGraveyard" . Just . Value.array $
      [Codec.encode PlayerScope.codec s, Codec.encode (Filter.codec Keyword.codec) f]
  ObjectRef.EachPlayer -> Common.nullary "EachPlayer"
  ObjectRef.TopOfLibrary p -> Common.tagged "TopOfLibrary" . Just $ PlayerRef.toJson p

fromJson :: Value.Value -> Either Text.Text ObjectRef.ObjectRef
fromJson value = do
  (t, mv) <- Common.asTagged value
  case t of
    "InSlot" -> Common.withValue mv $ fmap ObjectRef.InSlot . Codec.decode SlotName.codec
    "EachMatching" -> Common.withValue mv $ fmap ObjectRef.EachMatching . Codec.decode (Filter.codec Keyword.codec)
    "EachCardInGraveyard" -> case mv of
      Just (Value.Array (Array.MkArray [scope, filter_])) ->
        ObjectRef.EachCardInGraveyard
          <$> Codec.decode PlayerScope.codec scope
          <*> Codec.decode (Filter.codec Keyword.codec) filter_
      _ -> Left $ Text.pack "EachCardInGraveyard expects [playerScope, filter]"
    "EachPlayer" -> Right ObjectRef.EachPlayer
    "TopOfLibrary" -> Common.withValue mv $ fmap ObjectRef.TopOfLibrary . PlayerRef.fromJson
    _ -> Left . Text.pack $ "unknown ObjectRef: " <> t

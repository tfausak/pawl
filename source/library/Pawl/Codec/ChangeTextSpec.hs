module Pawl.Codec.ChangeTextSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ChangeText as ChangeText
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChangeText" $ do
  -- CR 612's text change, within one subtype family. The forbidden set is what
  -- the chooser may NOT pick, and it is written even when empty is the common
  -- case, because a required key says the family and the exclusions are two
  -- different questions.
  Spec.it s "MkChangeText, an empty forbidden set" $
    Common.assertCodec
      s
      ChangeText.codec
      ( ChangeText.MkChangeText
          { ChangeText.family = SubtypeFamily.BasicLandType,
            ChangeText.forbidden = Set.empty,
            ChangeText.slot = SlotName.MkSlotName (Text.pack "target")
          }
      )
      " {\"family\":{\"type\":\"BasicLandType\"},\"forbidden\":[],\"slot\":\"target\"} "
  -- The set is ascending and deduplicated on the wire, which Common.set's
  -- decoder additionally rejects a repeat for.
  Spec.it s "MkChangeText, forbidden subtypes are written in order" $
    Common.assertCodec
      s
      ChangeText.codec
      ( ChangeText.MkChangeText
          { ChangeText.family = SubtypeFamily.BasicLandType,
            ChangeText.forbidden = Set.fromList [Subtype.Island, Subtype.Forest],
            ChangeText.slot = SlotName.MkSlotName (Text.pack "target")
          }
      )
      " {\"family\":{\"type\":\"BasicLandType\"},\"forbidden\":[{\"type\":\"Forest\"},{\"type\":\"Island\"}],\"slot\":\"target\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChangeText.codec

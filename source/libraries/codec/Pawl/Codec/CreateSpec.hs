module Pawl.Codec.CreateSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Create as Create
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState

-- | The @card@ parameter is instantiated at 'Text.Text': this codec reaches it
-- only through the supplied codec, so any type proves the shape.
codec :: Codec.Codec (Create.Create Text.Text)
codec = Create.codec Common.text

-- CR 111.2's default creator under CR 109.5, which every case but the last
-- elides.
you :: PlayerRef.PlayerRef
you = PlayerRef.Relative PlayerRelation.You

plain :: EntryRiders.EntryRiders Quantity.Quantity
plain =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = False,
      EntryRiders.blocking = Nothing,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False,
      EntryRiders.exiledFaceDown = False,
      EntryRiders.faceDown = Nothing
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Create" $ do
  -- CR 111.1 with no rider, no slot and CR 111.2's default creator: two keys,
  -- which is most tokens in the pool.
  Spec.it s "MkCreate, every default elided" $
    Common.assertCodec
      s
      codec
      ( Create.MkCreate
          { Create.quantity = Quantity.Literal 2,
            Create.card = Text.pack "Spirit Token",
            Create.riders = plain,
            Create.slot = Nothing,
            Create.creator = you
          }
      )
      " {\"quantity\":{\"type\":\"Literal\",\"value\":2},\"card\":\"Spirit Token\"} "
  -- The slot alone. Under the positional payload this and the riders-alone case
  -- below were BOTH the three-element form, told apart by JSON TYPE; two named
  -- keys need no such argument.
  Spec.it s "MkCreate, slot alone" $
    Common.assertCodec
      s
      codec
      ( Create.MkCreate
          { Create.quantity = Quantity.Literal 1,
            Create.card = Text.pack "Spirit Token",
            Create.riders = plain,
            Create.slot = Just (SlotName.MkSlotName (Text.pack "token")),
            Create.creator = you
          }
      )
      " {\"quantity\":{\"type\":\"Literal\",\"value\":1},\"card\":\"Spirit Token\",\"slot\":\"token\"} "
  -- CR 110.5b's riders alone.
  Spec.it s "MkCreate, riders alone" $
    Common.assertCodec
      s
      codec
      ( Create.MkCreate
          { Create.quantity = Quantity.Literal 2,
            Create.card = Text.pack "Spirit Token",
            Create.riders = plain {EntryRiders.attacking = True},
            Create.slot = Nothing,
            Create.creator = you
          }
      )
      " {\"quantity\":{\"type\":\"Literal\",\"value\":2},\"card\":\"Spirit Token\",\"riders\":{\"attacking\":true}} "
  -- CR 111.2's creator alone, the key a card writes when the sentence names
  -- somebody other than "you".
  Spec.it s "MkCreate, a creator other than you" $
    Common.assertCodec
      s
      codec
      ( Create.MkCreate
          { Create.quantity = Quantity.Literal 1,
            Create.card = Text.pack "Centaur Token",
            Create.riders = plain,
            Create.slot = Nothing,
            Create.creator = PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "victim"))
          }
      )
      " {\"quantity\":{\"type\":\"Literal\",\"value\":1},\"card\":\"Centaur Token\",\"creator\":{\"type\":\"ControllerOfBound\",\"value\":\"victim\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

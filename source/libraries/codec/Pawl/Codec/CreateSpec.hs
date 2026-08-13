{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CreateSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Create as Create
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState

-- | The @card@ parameter is instantiated at 'Text.Text': this codec reaches it
-- only through the supplied codec, so any type proves the shape.
codec :: Codec.Codec (Create.Create Text.Text)
codec = Create.codec Common.text

plain :: EntryRiders.EntryRiders
plain =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = False,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Create" $ do
  -- CR 111.1 with neither rider: two keys, which is most tokens in the pool.
  Spec.it s "MkCreate, both defaults elided" $
    Common.assertCodec
      s
      codec
      ( Create.MkCreate
          { Create.quantity = Quantity.Literal 2,
            Create.card = Text.pack "Spirit Token",
            Create.riders = plain,
            Create.slot = Nothing
          }
      )
      """ {"quantity":{"type":"Literal","value":2},"card":"Spirit Token"} """
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
            Create.slot = Just (SlotName.MkSlotName (Text.pack "token"))
          }
      )
      """ {"quantity":{"type":"Literal","value":1},"card":"Spirit Token","slot":"token"} """
  -- CR 110.5b's riders alone.
  Spec.it s "MkCreate, riders alone" $
    Common.assertCodec
      s
      codec
      ( Create.MkCreate
          { Create.quantity = Quantity.Literal 2,
            Create.card = Text.pack "Spirit Token",
            Create.riders = plain {EntryRiders.attacking = True},
            Create.slot = Nothing
          }
      )
      """ {"quantity":{"type":"Literal","value":2},"card":"Spirit Token","riders":{"attacking":true}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

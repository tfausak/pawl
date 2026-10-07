module Pawl.Codec.OutsideObjectSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Codec.OutsideObject as OutsideObject
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.OutsideObject as OutsideObject
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId

codec :: Codec.Codec OutsideObject.OutsideObject
codec = OutsideObject.codec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OutsideObject" $ do
  -- CR 108.3b's owner, plus which printing the outer frame's card is. The
  -- default facing is elided, as Pawl.Codec.Object's own field is.
  Spec.it s "MkOutsideObject" $
    Common.assertCodec
      s
      codec
      ( OutsideObject.MkOutsideObject
          { OutsideObject.owner = PlayerId.MkPlayerId 0,
            OutsideObject.printing = PrintingId.MkPrintingId 1,
            OutsideObject.facing = Facing.FaceUp,
            OutsideObject.cards = PrintingId.MkPrintingId 1 NonEmpty.:| []
          }
      )
      " {\"owner\":0,\"printing\":1,\"cards\":[1]} "
  -- CR 708.2's status, the one thing carried besides the printing.
  Spec.it s "MkOutsideObject face down" $
    Common.assertCodec
      s
      codec
      ( OutsideObject.MkOutsideObject
          { OutsideObject.owner = PlayerId.MkPlayerId 0,
            OutsideObject.printing = PrintingId.MkPrintingId 1,
            OutsideObject.facing = Facing.faceDown FaceDownReason.Manifested,
            OutsideObject.cards = PrintingId.MkPrintingId 1 NonEmpty.:| []
          }
      )
      " {\"owner\":0,\"printing\":1,\"facing\":{\"type\":\"FaceDown\",\"value\":{\"reason\":{\"type\":\"Manifested\"},\"listed\":{}}},\"cards\":[1]} "
  -- CR 712.21 / 712.8g: a melded permanent, offered by its combined face and
  -- bringing both cards.
  Spec.it s "MkOutsideObject melded" $
    Common.assertCodec
      s
      codec
      ( OutsideObject.MkOutsideObject
          { OutsideObject.owner = PlayerId.MkPlayerId 0,
            OutsideObject.printing = PrintingId.MkPrintingId 3,
            OutsideObject.facing = Facing.FaceUp,
            OutsideObject.cards = PrintingId.MkPrintingId 1 NonEmpty.:| [PrintingId.MkPrintingId 2]
          }
      )
      " {\"owner\":0,\"printing\":3,\"cards\":[1,2]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

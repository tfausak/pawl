module Pawl.Codec.OutsideObjectSpec where

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
            OutsideObject.facing = Facing.FaceUp
          }
      )
      " {\"owner\":0,\"printing\":1} "
  -- CR 708.2's status, the one thing carried besides the printing.
  Spec.it s "MkOutsideObject face down" $
    Common.assertCodec
      s
      codec
      ( OutsideObject.MkOutsideObject
          { OutsideObject.owner = PlayerId.MkPlayerId 0,
            OutsideObject.printing = PrintingId.MkPrintingId 1,
            OutsideObject.facing = Facing.faceDown FaceDownReason.Manifested
          }
      )
      " {\"owner\":0,\"printing\":1,\"facing\":{\"type\":\"FaceDown\",\"value\":{\"reason\":{\"type\":\"Manifested\"},\"listed\":{}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec

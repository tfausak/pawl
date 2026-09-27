module Pawl.Codec.CopyOriginal where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CopyOriginal as CopyOriginal

codec :: Codec.Codec CopyOriginal.CopyOriginal
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "OfObject" ObjectRef.codec CopyOriginal.OfObject (\x -> case x of CopyOriginal.OfObject y -> Just y; _ -> Nothing),
      Arm.payload "Named" CardName.codec CopyOriginal.Named (\x -> case x of CopyOriginal.Named y -> Just y; _ -> Nothing)
    ]

tagOf :: CopyOriginal.CopyOriginal -> String
tagOf x = case x of
  CopyOriginal.OfObject {} -> "OfObject"
  CopyOriginal.Named {} -> "Named"

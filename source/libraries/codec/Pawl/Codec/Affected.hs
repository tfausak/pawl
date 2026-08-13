module Pawl.Codec.Affected where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Affected as Affected

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Affected.Affected
codec =
  Arm.tagged
    encode
    [ Arm.payload "TheseObjects" (Common.set ObjectId.codec) Affected.TheseObjects,
      Arm.payload "Matching" filterCodec Affected.Matching,
      Arm.payload "MatchingAnywhere" filterCodec Affected.MatchingAnywhere,
      Arm.nullary "Attached" Affected.Attached,
      Arm.payload "AttachedPlayerControls" filterCodec Affected.AttachedPlayerControls
    ]
  where
    filterCodec = Filter.codec Keyword.codec
    encode a = case a of
      Affected.TheseObjects ids -> Common.tagged "TheseObjects" . Just $ Codec.encode (Common.set ObjectId.codec) ids
      Affected.Matching f -> Common.tagged "Matching" . Just $ Codec.encode filterCodec f
      Affected.MatchingAnywhere f -> Common.tagged "MatchingAnywhere" . Just $ Codec.encode filterCodec f
      Affected.Attached -> Common.nullary "Attached"
      Affected.AttachedPlayerControls f -> Common.tagged "AttachedPlayerControls" . Just $ Codec.encode filterCodec f

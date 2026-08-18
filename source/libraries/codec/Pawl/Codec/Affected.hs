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
    [ Arm.payload "TheseObjects" (Common.set ObjectId.codec) Affected.TheseObjects (\x -> case x of Affected.TheseObjects y -> Just y; _ -> Nothing),
      Arm.payload "Matching" filterCodec Affected.Matching (\x -> case x of Affected.Matching y -> Just y; _ -> Nothing),
      Arm.payload "MatchingAnywhere" filterCodec Affected.MatchingAnywhere (\x -> case x of Affected.MatchingAnywhere y -> Just y; _ -> Nothing),
      Arm.payload "MatchingOffBattlefield" filterCodec Affected.MatchingOffBattlefield (\x -> case x of Affected.MatchingOffBattlefield y -> Just y; _ -> Nothing),
      Arm.nullary "Attached" Affected.Attached,
      Arm.payload "AttachedPlayerControls" filterCodec Affected.AttachedPlayerControls (\x -> case x of Affected.AttachedPlayerControls y -> Just y; _ -> Nothing)
    ]
  where
    filterCodec = Filter.codec Keyword.codec

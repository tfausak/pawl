module Pawl.Codec.PermissionCost where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.PermissionCost as PermissionCost

codec :: Codec.Codec PermissionCost.PermissionCost
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "InsteadOfManaCost" ManaCost.codec PermissionCost.InsteadOfManaCost (\x -> case x of PermissionCost.InsteadOfManaCost y -> Just y; _ -> Nothing),
      Arm.nullary "WaterbendManaValue" PermissionCost.WaterbendManaValue
    ]

tagOf :: PermissionCost.PermissionCost -> String
tagOf x = case x of
  PermissionCost.InsteadOfManaCost {} -> "InsteadOfManaCost"
  PermissionCost.WaterbendManaValue {} -> "WaterbendManaValue"

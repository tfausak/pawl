module Pawl.Codec.Check where

import qualified Pawl.Codec.CountIs as CountIs
import qualified Pawl.Codec.DamageIs as DamageIs
import qualified Pawl.Codec.LifeIs as LifeIs
import qualified Pawl.Codec.TappedIs as TappedIs
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Check as Check

-- | Keyed, as Pawl.Codec.Move is: @{"Life": {"player": "bob", "life": 18}}@.
codec :: Codec.Codec Check.Check
codec =
  Arm.keyed
    tagOf
    [ Arm.payload "Life" LifeIs.codec Check.Life (\x -> case x of Check.Life y -> Just y; _ -> Nothing),
      Arm.payload "Count" CountIs.codec Check.Count (\x -> case x of Check.Count y -> Just y; _ -> Nothing),
      Arm.payload "Damage" DamageIs.codec Check.Damage (\x -> case x of Check.Damage y -> Just y; _ -> Nothing),
      Arm.payload "Tapped" TappedIs.codec Check.Tapped (\x -> case x of Check.Tapped y -> Just y; _ -> Nothing)
    ]

tagOf :: Check.Check -> String
tagOf x = case x of
  Check.Life {} -> "Life"
  Check.Count {} -> "Count"
  Check.Damage {} -> "Damage"
  Check.Tapped {} -> "Tapped"

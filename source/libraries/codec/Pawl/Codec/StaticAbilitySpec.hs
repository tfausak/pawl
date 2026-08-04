{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.StaticAbilitySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.StaticAbility as StaticAbility
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.StaticAbility as StaticAbility

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StaticAbility" $ do
  -- A single-part static ability, e.g. a keyword granter.
  Spec.it s "one part" $
    Common.assertJsonCodec
      s
      StaticAbility.toJson
      StaticAbility.fromJson
      (StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying)))
      """ {"affected":{"type":"Attached"},"modifications":[{"type":"GainKeyword","value":{"type":"Flying"}}]} """
  -- Humility's shape: several parts under one affected set (CR 613.6).
  Spec.it s "several parts" $
    Common.assertJsonCodec
      s
      StaticAbility.toJson
      StaticAbility.fromJson
      ( StaticAbility.MkStaticAbility
          Affected.Attached
          (Modification.LoseAllAbilities NonEmpty.:| [Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)])
      )
      """ {"affected":{"type":"Attached"},"modifications":[{"type":"LoseAllAbilities"},{"type":"SetBasePowerToughness","value":[{"type":"Literal","value":1},{"type":"Literal","value":1}]}]} """
  -- CR 613.6 is why a static ability is one affected set and one or more parts, so
  -- the wire format is an array -- and an array can be empty. An ability with
  -- no parts does nothing, which no card means, so it is a decode FAILURE
  -- rather than a permanent that quietly under-performs its own text.
  Spec.it s "an empty modifications array is rejected" $
    Spec.assertBool
      s
      ( either
          (const True)
          (const False)
          ( StaticAbility.fromJson
              (Common.object [Common.pair "affected" (Common.tagged "Attached" Nothing), Common.pair "modifications" (Common.array [])])
          )
      )
      "an empty array does not decode"

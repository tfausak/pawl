{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.StaticAbilitySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Codec.StaticAbility as StaticAbility
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StaticAbility" $ do
  -- A single-part static ability, e.g. a keyword granter.
  Spec.it s "one part" $
    Common.assertJsonCodec
      s
      StaticAbility.toJson
      StaticAbility.fromJson
      (StaticAbility.MkStaticAbility Affected.Attached Nothing Nothing (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying)))
      """ {"affected":{"type":"Attached"},"modifications":[{"type":"GainKeyword","value":{"type":"Flying"}}]} """
  -- Humility's shape: several parts under one affected set (CR 613.6).
  Spec.it s "several parts" $
    Common.assertJsonCodec
      s
      StaticAbility.toJson
      StaticAbility.fromJson
      ( StaticAbility.MkStaticAbility
          Affected.Attached
          Nothing
          Nothing
          (Modification.LoseAllAbilities NonEmpty.:| [Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)])
      )
      """ {"affected":{"type":"Attached"},"modifications":[{"type":"LoseAllAbilities"},{"type":"SetBasePowerToughness","value":[{"type":"Literal","value":1},{"type":"Literal","value":1}]}]} """
  -- CR 604.2's "as long as" gate, Kird Ape's shape: the same ability plus a
  -- condition, so the key is present exactly when the clause is. The two cases
  -- above pin the absent half -- an encoder that always emitted the key would
  -- rewrite every card already committed.
  Spec.it s "an as-long-as condition" $
    Common.assertJsonCodec
      s
      StaticAbility.toJson
      StaticAbility.fromJson
      ( StaticAbility.MkStaticAbility
          Affected.Attached
          ( Just
              ( Condition.Compares
                  (Quantity.Count (Count.MkCount (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer) (Filter.HasSubtype Subtype.Forest) Aggregation.Objects))
                  Comparison.AtLeast
                  (Quantity.Literal 1)
              )
          )
          Nothing
          (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying))
      )
      """ {"affected":{"type":"Attached"},"condition":{"comparison":{"type":"AtLeast"},"measured":{"type":"Count","value":{"aggregation":{"type":"Objects"},"filter":{"type":"HasSubtype","value":{"type":"Forest"}},"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]}}},"threshold":{"type":"Literal","value":1}},"modifications":[{"type":"GainKeyword","value":{"type":"Flying"}}]} """
  -- Titania's Song's second sentence: CR 604.2's other override, and optional
  -- for the condition's reason -- absent means the effect ends with its
  -- permanent, which is every other ability in the pool.
  Spec.it s "a leaves-the-battlefield duration" $
    Common.assertJsonCodec
      s
      StaticAbility.toJson
      StaticAbility.fromJson
      ( StaticAbility.MkStaticAbility
          Affected.Attached
          Nothing
          (Just Duration.UntilEndOfTurn)
          (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying))
      )
      """ {"affected":{"type":"Attached"},"lingers":{"type":"UntilEndOfTurn"},"modifications":[{"type":"GainKeyword","value":{"type":"Flying"}}]} """
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
              (Value.object [Pair.fromString "affected" (Common.tagged "Attached" Nothing), Pair.fromString "modifications" (Value.array [])])
          )
      )
      "an empty array does not decode"

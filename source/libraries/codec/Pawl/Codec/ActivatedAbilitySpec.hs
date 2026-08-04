{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ActivatedAbilitySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec

-- | The `card` parameter is instantiated at 'Text.Text' throughout.
-- 'ActivatedAbility.toJson'/'ActivatedAbility.fromJson' reach it only through
-- the supplied Modal codec, so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Common.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: ActivatedAbility.ActivatedAbility Text.Text -> Value.Value
toJson = ActivatedAbility.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (ActivatedAbility.ActivatedAbility Text.Text)
fromJson = ActivatedAbility.fromJson cardFromJson

-- One constructor, so two cases: an equip ability (CR 702.6a) carrying
-- CR 307.5's printed SorcerySpeed rider, and CR 117.1b's default AnyTime,
-- whose key is elided.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActivatedAbility" $ do
  Spec.it s "MkActivatedAbility, Bonesplitter's Equip ability" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) [])
          ( Modal.MkModal
              ( Seq.singleton
                  ( Mode.MkMode
                      (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))
                      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
                      Optionality.Mandatory
                  )
              )
              (ModeSelection.ChooseExactly 1)
          )
          ActivationTiming.SorcerySpeed
      )
      """ {"cost":{"mana":[{"type":"Generic","value":1}]},"modal":{"modes":[{"effects":[{"type":"Attach","value":"target"}],"targetSpecs":[{"slot":"target","spec":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}]}]},"timing":{"type":"SorcerySpeed"}} """
  -- CR 117.1b: AnyTime is the default for every ability but equip, so its key
  -- stays out of the JSON.
  Spec.it s "an AnyTime ability omits the timing key, and an absent key decodes to AnyTime" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [])
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1))
          ActivationTiming.AnyTime
      )
      """ {"cost":{"mana":null},"modal":{"modes":[{}]}} """

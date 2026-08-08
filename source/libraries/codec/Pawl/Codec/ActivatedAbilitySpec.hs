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
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.TurnScope as TurnScope

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

-- One constructor, so three cases: an equip ability (CR 702.6a) carrying CR
-- 602.5d's printed SorcerySpeed clause, CR 602.2's default of no clause at all,
-- whose key is elided, and Kongming's Contraptions' TWO clauses, which is what
-- the key being an array is for.
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
                      (Seq.singleton (Clause.MkClause (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target"))))))
                      (Map.singleton (SlotName.MkSlotName (Text.pack "target")) (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.ControlledBy PlayerRelation.You))))
                      Optionality.Mandatory
                      Nothing
                  )
              )
              (ModeSelection.ChooseExactly 1)
          )
          [ActivationRestriction.SorcerySpeed]
          Nothing
      )
      """ {"cost":{"mana":[{"type":"Generic","value":1}]},"modal":{"modes":[{"clauses":[{"effects":[{"type":"Attach","value":"target"}]}],"targetSpecs":[{"slot":"target","spec":{"pool":{"type":"Creatures"},"filter":{"type":"ControlledBy","value":{"type":"You"}}}}]}]},"restrictions":[{"type":"SorcerySpeed"}]} """
  -- CR 602.5's conjunction, in the JSON: two clauses in printed order, which is
  -- the shape a single tagged object could not hold.
  Spec.it s "MkActivatedAbility, Kongming's Contraptions' two clauses" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [CostComponent.TapThis])
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing)) (ModeSelection.ChooseExactly 1))
          [ ActivationRestriction.DuringPhase (PhaseSelector.Step (Phase.Combat CombatStep.DeclareAttackers)) TurnScope.EachTurn,
            ActivationRestriction.AttackedThisStep
          ]
          Nothing
      )
      """ {"cost":{"mana":null,"components":[{"type":"TapThis"}]},"modal":{"modes":[{}]},"restrictions":[{"type":"DuringPhase","value":[{"type":"Step","value":{"type":"Combat","value":{"type":"DeclareAttackers"}}},{"type":"EachTurn"}]},{"type":"AttackedThisStep"}]} """
  -- CR 602.2: no rider is the default for nearly every ability, so the key stays
  -- out of the JSON.
  Spec.it s "an unrestricted ability omits the restrictions key, and an absent key decodes to none" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( ActivatedAbility.MkActivatedAbility
          (Cost.MkCost Nothing [])
          (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing)) (ModeSelection.ChooseExactly 1))
          []
          Nothing
      )
      """ {"cost":{"mana":null},"modal":{"modes":[{}]}} """

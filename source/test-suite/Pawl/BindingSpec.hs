-- Covers: Pawl.Types.Binding, Pawl.Engine.Binding
module Pawl.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype

sampleSnapshot :: PC.ProjectedCharacteristics
sampleSnapshot =
  PC.MkProjectedCharacteristics
    { PC.name = CardName.MkCardName $ Text.pack "Sample",
      PC.supertypes = Set.empty,
      PC.keywords = Map.empty,
      PC.colors = Set.empty,
      PC.power = Just 2,
      PC.toughness = Just 1,
      PC.loyalty = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.empty,
      PC.subtypes = Set.empty,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = []
    }

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Engine.Binding" $ do
  Spec.describe s "fromChoices merges a shared slot's target and subtypes" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
        r = Recipient.ToPlayer S.alice
        pair = (Subtype.Mountain, Subtype.Island)
        m = Binding.fromChoices (Map.singleton slot r) (Map.singleton slot pair) Nothing Set.empty

    Spec.it s "target projected" $ do
      Spec.assertEq s (Binding.targetsOf m) $ Map.singleton slot r

    Spec.it s "subtypes projected" $ do
      Spec.assertEq s (Binding.subtypesOf m) $ Map.singleton slot pair

  Spec.it s "fromChoices stores X under the reserved slot" $ do
    let m = Binding.fromChoices Map.empty Map.empty (Just 3) Set.empty
    Spec.assertEq s (Binding.amountOf Binding.variableX m) $ Just 3

  Spec.it s "amountOf is Nothing for an absent slot" $ do
    Spec.assertEq s (Binding.amountOf Binding.variableX Map.empty) Nothing

  Spec.it s "modesOf round-trips a stamped set of chosen modes" $ do
    let chosen = Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]
        m = Binding.fromChoices Map.empty Map.empty Nothing chosen
    Spec.assertEq s (Binding.modesOf m) chosen

  Spec.it s "modesOf is empty for an absent slot" $ do
    Spec.assertEq s (Binding.modesOf Map.empty) Set.empty

  Spec.it s "setCopy then copyOf round-trips the snapshot" $ do
    Spec.assertEq s (Binding.copyOf (Binding.setCopy sampleSnapshot Map.empty)) $ Just sampleSnapshot

  Spec.it s "no copy binding means copyOf is Nothing" $ do
    Spec.assertEq s (Binding.copyOf Map.empty) Nothing

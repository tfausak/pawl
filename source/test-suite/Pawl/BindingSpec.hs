-- Covers: Pawl.Types.Binding, Pawl.Engine.Binding
module Pawl.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

slotTokens :: SlotName.SlotName
slotTokens = SlotName.MkSlotName (Text.pack "tokens")

sampleSnapshot :: PC.ProjectedCharacteristics
sampleSnapshot =
  PC.MkProjectedCharacteristics
    { PC.name = CardName.MkCardName $ Text.pack "Sample",
      PC.supertypes = Set.empty,
      PC.keywords = Map.empty,
      PC.colors = Set.empty,
      PC.manaValue = Just 0,
      PC.power = Just 2,
      PC.toughness = Just 1,
      PC.loyalty = Nothing,
      PC.defense = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.empty,
      PC.subtypes = Set.empty,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = []
    }

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Engine.Binding" $ do
  Spec.it s "fromChoices projects a slot's chosen target" $ do
    let slot = SlotName.MkSlotName (Text.pack "target")
        r = Recipient.ToPlayer S.alice
        m = Binding.fromChoices (Map.singleton slot (Set.singleton r)) Nothing Seq.empty
    Spec.assertEq s (Binding.targetsOf m) $ Map.singleton slot (Set.singleton r)

  Spec.it s "fromChoices stores X under the reserved slot" $ do
    let m = Binding.fromChoices Map.empty (Just 3) Seq.empty
    Spec.assertEq s (Binding.amountOf Binding.variableX m) $ Just 3

  Spec.it s "amountOf is Nothing for an absent slot" $ do
    Spec.assertEq s (Binding.amountOf Binding.variableX Map.empty) Nothing

  -- CR 700.2d: the stamped selection is a Seq, so a repeated mode survives the
  -- round trip -- a Set here would silently collapse the two 2s into one.
  Spec.it s "modesOf round-trips a stamped sequence of chosen modes, repeats and all" $ do
    let chosen = Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2, ModeIndex.MkModeIndex 2]
        m = Binding.fromChoices Map.empty Nothing chosen
    Spec.assertEq s (Binding.modesOf m) chosen

  Spec.it s "modesOf is empty for an absent slot" $ do
    Spec.assertEq s (Binding.modesOf Map.empty) Seq.empty

  Spec.it s "setCopy then copyOf round-trips the snapshot" $ do
    Spec.assertEq s (Binding.copyOf (Binding.setCopy sampleSnapshot Map.empty)) $ Just sampleSnapshot

  Spec.it s "no copy binding means copyOf is Nothing" $ do
    Spec.assertEq s (Binding.copyOf Map.empty) Nothing

  -- The order is the order the tokens were minted, not an ObjectId order, so the
  -- descending fixture is what proves this is a Seq and not a Set: sorting it
  -- would be the engine reordering what it sacrifices.
  Spec.it s "toObjects then objectsOf round-trips the minted set in mint order" $ do
    let slot = SlotName.MkSlotName (Text.pack "tokens")
        minted = Seq.fromList [ObjectId.MkObjectId 9, ObjectId.MkObjectId 4]
    Spec.assertEq s (Binding.objectsOf slot (Map.singleton slot (Binding.toObjects minted))) $ Just minted

  Spec.it s "objectsOf is Nothing for an absent slot" $ do
    Spec.assertEq s (Binding.objectsOf (SlotName.MkSlotName (Text.pack "tokens")) Map.empty) Nothing

  -- A group binding and a target binding are different fields, so a reader can
  -- tell "those tokens" from "it" without a tag -- the reason Sacrifice may
  -- consult one and fall through to the other.
  Spec.it s "a group binding carries no target" $ do
    let slot = SlotName.MkSlotName (Text.pack "tokens")
        set_ = Map.singleton slot (Binding.toObjects (Seq.fromList [ObjectId.MkObjectId 1]))
    Spec.assertEq s (Binding.targetsOf set_) Map.empty

  -- Engine.placeOne joins a delayed ability's placement-time choices with the
  -- environment captured when it was armed, and the two can now carry DIFFERENT
  -- fields of one slot -- a target spec named for the slot a Create bound its
  -- tokens to. A whole-Binding left-biased union would drop the loser entirely;
  -- merging per field keeps both, which is what that join relies on.
  Spec.it s "mergeBinding keeps disjoint fields from both sides" $ do
    let merged =
          Binding.mergeBinding
            (Binding.toObject (ObjectId.MkObjectId 1))
            (Binding.toObjects (Seq.fromList [ObjectId.MkObjectId 2, ObjectId.MkObjectId 3]))
    Spec.assertEq s (Binding.objectsOf slotTokens (Map.singleton slotTokens merged)) $
      Just (Seq.fromList [ObjectId.MkObjectId 2, ObjectId.MkObjectId 3])

  Spec.it s "mergeBinding prefers the left side's target" $ do
    let merged =
          Binding.mergeBinding
            (Binding.toObject (ObjectId.MkObjectId 1))
            (Binding.toObject (ObjectId.MkObjectId 2))
    Spec.assertEq s (Binding.targetsOf (Map.singleton slotTokens merged)) $
      Map.singleton slotTokens (Set.singleton (Recipient.ToObject (ObjectId.MkObjectId 1)))

  Spec.it s "a target binding carries no group" $ do
    let slot = SlotName.MkSlotName (Text.pack "tokens")
        one = Map.singleton slot (Binding.toObject (ObjectId.MkObjectId 1))
    Spec.assertEq s (Binding.objectsOf slot one) Nothing

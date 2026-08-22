module Pawl.Codec.ModificationSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | The `ability` parameter is instantiated at @GrantedAbility Text@, which is
-- what every card position holds with `card` in turn instantiated at 'Text.Text'
-- -- the posture 'Pawl.Codec.ActivatedAbilitySpec' takes for the same reason.
codec :: Codec.Codec (Modification.Modification (GrantedAbility.GrantedAbility Text.Text))
codec = Modification.codec (GrantedAbility.codec Common.text)

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Modification" $ do
  -- layer 6 (Serpent's Gift).
  Spec.it s "GainKeyword" $
    Common.assertCodec
      s
      codec
      (Modification.GainKeyword Keyword.Deathtouch)
      " {\"type\":\"GainKeyword\",\"value\":{\"type\":\"Deathtouch\"}} "
  -- layer 6 (Humility).
  Spec.it s "LoseAllAbilities" $
    Common.assertCodec
      s
      codec
      Modification.LoseAllAbilities
      " {\"type\":\"LoseAllAbilities\"} "
  -- layer 7b (Humility 1/1; Opalescence mana value).
  Spec.it s "SetBasePowerToughness" $
    Common.assertCodec
      s
      codec
      (Modification.SetBasePowerToughness (SetBasePowerToughness.MkSetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1)))
      " {\"type\":\"SetBasePowerToughness\",\"value\":{\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":1}}} "
  -- layer 7c (Giant Growth +3/+3).
  Spec.it s "ModifyPowerToughness" $
    Common.assertCodec
      s
      codec
      (Modification.ModifyPowerToughness (ModifyPowerToughness.MkModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3)))
      " {\"type\":\"ModifyPowerToughness\",\"value\":{\"power\":{\"type\":\"Literal\",\"value\":3},\"toughness\":{\"type\":\"Literal\",\"value\":3}}} "
  -- layer 4, CR 305.7 set (Blood Moon -> Mountain).
  Spec.it s "SetLandSubtype" $
    Common.assertCodec
      s
      codec
      (Modification.SetLandSubtype Subtype.Mountain)
      " {\"type\":\"SetLandSubtype\",\"value\":{\"type\":\"Mountain\"}} "
  -- Nullary: the subtype is read from the effect's source at projection time,
  -- so there is nothing on the wire.
  Spec.it s "SetLandSubtypeToChosen" $
    Common.assertCodec
      s
      codec
      Modification.SetLandSubtypeToChosen
      " {\"type\":\"SetLandSubtypeToChosen\"} "
  -- layer 4, CR 305.7 add (Urborg -> Swamp).
  Spec.it s "AddLandSubtype" $
    Common.assertCodec
      s
      codec
      (Modification.AddLandSubtype Subtype.Swamp)
      " {\"type\":\"AddLandSubtype\",\"value\":{\"type\":\"Swamp\"}} "
  -- layer 4, CR 205.1a/205.1b set (Turn to Frog -> Frog).
  Spec.it s "SetCreatureSubtype" $
    Common.assertCodec
      s
      codec
      (Modification.SetCreatureSubtype Subtype.Frog)
      " {\"type\":\"SetCreatureSubtype\",\"value\":{\"type\":\"Frog\"}} "
  -- layer 4, CR 205.1b add (Life and Limb -> Saproling).
  Spec.it s "AddCreatureSubtype" $
    Common.assertCodec
      s
      codec
      (Modification.AddCreatureSubtype Subtype.Saproling)
      " {\"type\":\"AddCreatureSubtype\",\"value\":{\"type\":\"Saproling\"}} "
  -- layer 4, CR 205.1b add over the whole of CR 205.3m (Wings of Velis Vel).
  Spec.it s "AddEveryCreatureSubtype" $
    Common.assertCodec
      s
      codec
      Modification.AddEveryCreatureSubtype
      " {\"type\":\"AddEveryCreatureSubtype\"} "
  -- layer 4, CR 205.1b add outside the land and creature families (Ygra, Eater
  -- of All). A different subtype from every arm above, so a codec that crossed
  -- this arm with one of them cannot pass both.
  Spec.it s "AddSubtype" $
    Common.assertCodec
      s
      codec
      (Modification.AddSubtype Subtype.Food)
      " {\"type\":\"AddSubtype\",\"value\":{\"type\":\"Food\"}} "
  -- layer 4 (Opalescence -> Creature).
  Spec.it s "AddCardType" $
    Common.assertCodec
      s
      codec
      (Modification.AddCardType CardType.Creature)
      " {\"type\":\"AddCardType\",\"value\":{\"type\":\"Creature\"}} "
  -- layer 4, CR 205.1a set (Song of the Dryads -> land). A different card type
  -- from the add above, so a codec that crossed the two arms cannot pass both.
  Spec.it s "SetCardType" $
    Common.assertCodec
      s
      codec
      (Modification.SetCardType CardType.Land)
      " {\"type\":\"SetCardType\",\"value\":{\"type\":\"Land\"}} "
  -- layer 4, CR 205.4b grant (Leyline of Singularity -> legendary).
  Spec.it s "AddSupertype" $
    Common.assertCodec
      s
      codec
      (Modification.AddSupertype Supertype.Legendary)
      " {\"type\":\"AddSupertype\",\"value\":{\"type\":\"Legendary\"}} "
  -- layer 4, CR 205.4b removal (Arcum's Weathervane -> no longer snow).
  Spec.it s "RemoveSupertype" $
    Common.assertCodec
      s
      codec
      (Modification.RemoveSupertype Supertype.Snow)
      " {\"type\":\"RemoveSupertype\",\"value\":{\"type\":\"Snow\"}} "
  -- layer 3, CR 612 (Magical Hack: from -> to).
  Spec.it s "ChangeSubtypeWord" $
    Common.assertCodec
      s
      codec
      (Modification.ChangeSubtypeWord (ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Mountain Subtype.Island))
      " {\"type\":\"ChangeSubtypeWord\",\"value\":{\"from\":{\"type\":\"Mountain\"},\"to\":{\"type\":\"Island\"}}} "
  -- layer 2, CR 613.1b: the PlayerId is BAKED at effect creation, unlike
  -- SetControllerToSource below.
  Spec.it s "SetController carries its baked PlayerId" $
    Common.assertCodec
      s
      codec
      (Modification.SetController (PlayerId.MkPlayerId 0))
      " {\"type\":\"SetController\",\"value\":0} "
  -- layer 2, CR 613.1b: the only shape a printed static ability can grant
  -- control with, since the controller is DERIVED at projection time rather
  -- than baked. Must not decode as SetController.
  Spec.it s "SetControllerToSource" $
    Common.assertCodec
      s
      codec
      Modification.SetControllerToSource
      " {\"type\":\"SetControllerToSource\"} "
  -- layer 5, CR 613.1e / 105.3: a SET, not an add.
  Spec.it s "SetColor carries its colour set" $
    Common.assertCodec
      s
      codec
      (Modification.SetColor (Set.singleton Color.Blue))
      " {\"type\":\"SetColor\",\"value\":[{\"type\":\"Blue\"}]} "
  -- layer 5, CR 613.1e / 105.3: an ADD, not a set.
  Spec.it s "AddColor" $
    Common.assertCodec
      s
      codec
      (Modification.AddColor (Set.singleton Color.Blue))
      " {\"type\":\"AddColor\",\"value\":[{\"type\":\"Blue\"}]} "
  -- layer 5, CR 613.1e / 105.3: an "in addition" colour too, but payload-free
  -- -- the colour is read at projection time off the effect's source.
  Spec.it s "AddChosenColor" $
    Common.assertCodec
      s
      codec
      Modification.AddChosenColor
      " {\"type\":\"AddChosenColor\"} "
  -- layer 7d, CR 613.4d: switches power and toughness. Payload-free.
  Spec.it s "SwitchPowerToughness" $
    Common.assertCodec
      s
      codec
      Modification.SwitchPowerToughness
      " {\"type\":\"SwitchPowerToughness\"} "
  -- layer 6, CR 613.1f: a whole quoted activated ability, Presence of Gond's
  -- "{T}: Create a 1/1 green Elf Warrior creature token" reduced to its cost.
  Spec.it s "GainAbility" $
    Common.assertCodec
      s
      codec
      ( Modification.GainAbility
          ( GrantedAbility.Activated
              ( ActivatedAbility.MkActivatedAbility
                  (Cost.MkCost Nothing [CostComponent.TapThis])
                  (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty)) (ModeSelection.ChooseExactly 1))
                  []
                  Nothing
              )
          )
      )
      " {\"type\":\"GainAbility\",\"value\":{\"type\":\"Activated\",\"value\":{\"cost\":{\"mana\":null,\"components\":[{\"type\":\"TapThis\"}]},\"modal\":{\"modes\":[{}]}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
  -- The Void instantiation Pawl.Types.ModifyTarget takes: one arm short, and the
  -- schema says so.
  Spec.it s "the grantless codec has a schema" $ Common.assertHasSchema s Modification.grantless

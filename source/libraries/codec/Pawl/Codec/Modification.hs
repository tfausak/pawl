module Pawl.Codec.Modification where

import qualified Data.Typeable as Typeable
import qualified Data.Void as Void
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Codec.TargetSlot as TargetSlot
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Modification as Modification

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- Takes the codec for the GRANTED ABILITY rather than for a card, since that is
-- what Pawl.Types.Modification is parametric in -- and because naming
-- Pawl.Codec.GrantedAbility here would close the same module cycle the type's
-- own parameter opens. 'grantless' below is this codec at the one position that
-- cannot hold a grant.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (Modification.Modification ability)
codec abilityCodec =
  Arm.tagged
    ( Arm.payload "GainAbility" abilityCodec Modification.GainAbility (\x -> case x of Modification.GainAbility y -> Just y; _ -> Nothing)
        : arms
    )

-- | The codec for a Modification in a position whose ability variable is Void
-- (Pawl.Types.ModifyTarget). Shares one arm list with 'codec' above, so the two
-- differ in exactly the arm the type says they differ in -- and the schema for a
-- ModifyTarget honestly omits the grant rather than describing an unsatisfiable
-- payload.
grantless :: Codec.Codec (Modification.Modification Void.Void)
grantless = Arm.tagged arms

-- | Every arm that carries no ability, hence polymorphic in the variable.
arms :: (Eq ability) => [Arm.Arm (Modification.Modification ability)]
arms =
  [ Arm.payload "GainKeyword" Keyword.codec Modification.GainKeyword (\x -> case x of Modification.GainKeyword y -> Just y; _ -> Nothing),
    Arm.payload "GainEnchant" TargetSlot.codec Modification.GainEnchant (\x -> case x of Modification.GainEnchant y -> Just y; _ -> Nothing),
    Arm.nullary "LoseAllAbilities" Modification.LoseAllAbilities,
    Arm.payload "SetBasePowerToughness" SetBasePowerToughness.codec Modification.SetBasePowerToughness (\x -> case x of Modification.SetBasePowerToughness y -> Just y; _ -> Nothing),
    Arm.payload "ModifyPowerToughness" ModifyPowerToughness.codec Modification.ModifyPowerToughness (\x -> case x of Modification.ModifyPowerToughness y -> Just y; _ -> Nothing),
    Arm.payload "SetLandSubtype" Subtype.codec Modification.SetLandSubtype (\x -> case x of Modification.SetLandSubtype y -> Just y; _ -> Nothing),
    Arm.nullary "SetLandSubtypeToChosen" Modification.SetLandSubtypeToChosen,
    Arm.payload "AddLandSubtype" Subtype.codec Modification.AddLandSubtype (\x -> case x of Modification.AddLandSubtype y -> Just y; _ -> Nothing),
    Arm.payload "SetCreatureSubtype" Subtype.codec Modification.SetCreatureSubtype (\x -> case x of Modification.SetCreatureSubtype y -> Just y; _ -> Nothing),
    Arm.payload "AddCreatureSubtype" Subtype.codec Modification.AddCreatureSubtype (\x -> case x of Modification.AddCreatureSubtype y -> Just y; _ -> Nothing),
    Arm.nullary "AddEveryCreatureSubtype" Modification.AddEveryCreatureSubtype,
    Arm.payload "AddSubtype" Subtype.codec Modification.AddSubtype (\x -> case x of Modification.AddSubtype y -> Just y; _ -> Nothing),
    Arm.payload "AddCardType" CardType.codec Modification.AddCardType (\x -> case x of Modification.AddCardType y -> Just y; _ -> Nothing),
    Arm.payload "SetCardType" CardType.codec Modification.SetCardType (\x -> case x of Modification.SetCardType y -> Just y; _ -> Nothing),
    Arm.payload "AddSupertype" Supertype.codec Modification.AddSupertype (\x -> case x of Modification.AddSupertype y -> Just y; _ -> Nothing),
    Arm.payload "RemoveSupertype" Supertype.codec Modification.RemoveSupertype (\x -> case x of Modification.RemoveSupertype y -> Just y; _ -> Nothing),
    Arm.payload "ChangeSubtypeWord" ChangeSubtypeWord.codec Modification.ChangeSubtypeWord (\x -> case x of Modification.ChangeSubtypeWord y -> Just y; _ -> Nothing),
    Arm.payload "SetController" PlayerId.codec Modification.SetController (\x -> case x of Modification.SetController y -> Just y; _ -> Nothing),
    Arm.nullary "SetControllerToSource" Modification.SetControllerToSource,
    Arm.payload "SetColor" colors Modification.SetColor (\x -> case x of Modification.SetColor y -> Just y; _ -> Nothing),
    Arm.payload "AddColor" colors Modification.AddColor (\x -> case x of Modification.AddColor y -> Just y; _ -> Nothing),
    Arm.nullary "AddChosenColor" Modification.AddChosenColor,
    Arm.nullary "SwitchPowerToughness" Modification.SwitchPowerToughness
  ]
  where
    colors = Common.set Color.codec

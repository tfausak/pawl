module Pawl.Codec.Modification where

import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Modification as Modification

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Modification.Modification
codec =
  Arm.tagged
    encode
    [ Arm.payload "GainKeyword" Keyword.codec Modification.GainKeyword,
      Arm.nullary "LoseAllAbilities" Modification.LoseAllAbilities,
      Arm.payload "SetBasePowerToughness" SetBasePowerToughness.codec Modification.SetBasePowerToughness,
      Arm.payload "ModifyPowerToughness" ModifyPowerToughness.codec Modification.ModifyPowerToughness,
      Arm.payload "SetLandSubtype" Subtype.codec Modification.SetLandSubtype,
      Arm.nullary "SetLandSubtypeToChosen" Modification.SetLandSubtypeToChosen,
      Arm.payload "AddLandSubtype" Subtype.codec Modification.AddLandSubtype,
      Arm.payload "SetCreatureSubtype" Subtype.codec Modification.SetCreatureSubtype,
      Arm.payload "AddCreatureSubtype" Subtype.codec Modification.AddCreatureSubtype,
      Arm.nullary "AddEveryCreatureSubtype" Modification.AddEveryCreatureSubtype,
      Arm.payload "AddCardType" CardType.codec Modification.AddCardType,
      Arm.payload "AddSupertype" Supertype.codec Modification.AddSupertype,
      Arm.payload "RemoveSupertype" Supertype.codec Modification.RemoveSupertype,
      Arm.payload "ChangeSubtypeWord" ChangeSubtypeWord.codec Modification.ChangeSubtypeWord,
      Arm.payload "SetController" PlayerId.codec Modification.SetController,
      Arm.nullary "SetControllerToSource" Modification.SetControllerToSource,
      Arm.payload "SetColor" colors Modification.SetColor,
      Arm.payload "AddColor" colors Modification.AddColor,
      Arm.nullary "AddChosenColor" Modification.AddChosenColor,
      Arm.nullary "SwitchPowerToughness" Modification.SwitchPowerToughness
    ]
  where
    colors = Common.set Color.codec
    encode m = case m of
      Modification.GainKeyword k -> Common.tagged "GainKeyword" . Just $ Codec.encode Keyword.codec k
      Modification.LoseAllAbilities -> Common.nullary "LoseAllAbilities"
      Modification.SetBasePowerToughness x -> Common.tagged "SetBasePowerToughness" . Just $ Codec.encode SetBasePowerToughness.codec x
      Modification.ModifyPowerToughness x -> Common.tagged "ModifyPowerToughness" . Just $ Codec.encode ModifyPowerToughness.codec x
      Modification.SetLandSubtype s -> Common.tagged "SetLandSubtype" . Just $ Codec.encode Subtype.codec s
      Modification.SetLandSubtypeToChosen -> Common.nullary "SetLandSubtypeToChosen"
      Modification.AddLandSubtype s -> Common.tagged "AddLandSubtype" . Just $ Codec.encode Subtype.codec s
      Modification.SetCreatureSubtype s -> Common.tagged "SetCreatureSubtype" . Just $ Codec.encode Subtype.codec s
      Modification.AddCreatureSubtype s -> Common.tagged "AddCreatureSubtype" . Just $ Codec.encode Subtype.codec s
      Modification.AddEveryCreatureSubtype -> Common.nullary "AddEveryCreatureSubtype"
      Modification.AddCardType c -> Common.tagged "AddCardType" . Just $ Codec.encode CardType.codec c
      Modification.AddSupertype t -> Common.tagged "AddSupertype" . Just $ Codec.encode Supertype.codec t
      Modification.RemoveSupertype t -> Common.tagged "RemoveSupertype" . Just $ Codec.encode Supertype.codec t
      Modification.ChangeSubtypeWord x -> Common.tagged "ChangeSubtypeWord" . Just $ Codec.encode ChangeSubtypeWord.codec x
      Modification.SetController p -> Common.tagged "SetController" . Just $ Codec.encode PlayerId.codec p
      Modification.SetControllerToSource -> Common.nullary "SetControllerToSource"
      Modification.SetColor cs -> Common.tagged "SetColor" . Just $ Common.encodeSet (Codec.encode Color.codec) cs
      Modification.AddColor cs -> Common.tagged "AddColor" . Just $ Common.encodeSet (Codec.encode Color.codec) cs
      Modification.AddChosenColor -> Common.nullary "AddChosenColor"
      Modification.SwitchPowerToughness -> Common.nullary "SwitchPowerToughness"

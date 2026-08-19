module Pawl.Codec.PlayerEffect where

import qualified Pawl.Codec.AddActivationCost as AddActivationCost
import qualified Pawl.Codec.AddSpellCost as AddSpellCost
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Codec.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.Codec.SpendManaAsThough as SpendManaAsThough
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerEffect as PlayerEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec PlayerEffect.PlayerEffect
codec =
  Arm.tagged
    [ Arm.nullary "CantCastSpells" PlayerEffect.CantCastSpells,
      Arm.payload "CantCastMoreThan" Common.natural PlayerEffect.CantCastMoreThan (\x -> case x of PlayerEffect.CantCastMoreThan y -> Just y; _ -> Nothing),
      Arm.nullary "CantCastChosenName" PlayerEffect.CantCastChosenName,
      Arm.nullary "CantPlayLandChosenName" PlayerEffect.CantPlayLandChosenName,
      Arm.payload "IncreaseSpellCost" IncreaseSpellCost.codec PlayerEffect.IncreaseSpellCost (\x -> case x of PlayerEffect.IncreaseSpellCost y -> Just y; _ -> Nothing),
      Arm.payload "ReduceSpellCost" ReduceSpellCost.codec PlayerEffect.ReduceSpellCost (\x -> case x of PlayerEffect.ReduceSpellCost y -> Just y; _ -> Nothing),
      Arm.payload "ReduceActivationCost" ReduceActivationCost.codec PlayerEffect.ReduceActivationCost (\x -> case x of PlayerEffect.ReduceActivationCost y -> Just y; _ -> Nothing),
      Arm.payload "AddActivationCost" AddActivationCost.codec PlayerEffect.AddActivationCost (\x -> case x of PlayerEffect.AddActivationCost y -> Just y; _ -> Nothing),
      Arm.payload "AddSpellCost" AddSpellCost.codec PlayerEffect.AddSpellCost (\x -> case x of PlayerEffect.AddSpellCost y -> Just y; _ -> Nothing),
      Arm.payload "PlayAdditionalLands" Common.natural PlayerEffect.PlayAdditionalLands (\x -> case x of PlayerEffect.PlayAdditionalLands y -> Just y; _ -> Nothing),
      Arm.nullary "NoMaximumHandSize" PlayerEffect.NoMaximumHandSize,
      Arm.payload "SetMaximumHandSize" Common.natural PlayerEffect.SetMaximumHandSize (\x -> case x of PlayerEffect.SetMaximumHandSize y -> Just y; _ -> Nothing),
      Arm.payload "DontLoseUnspentMana" ManaFilter.codec PlayerEffect.DontLoseUnspentMana (\x -> case x of PlayerEffect.DontLoseUnspentMana y -> Just y; _ -> Nothing),
      Arm.payload "SpendManaAsThough" SpendManaAsThough.codec PlayerEffect.SpendManaAsThough (\x -> case x of PlayerEffect.SpendManaAsThough y -> Just y; _ -> Nothing),
      Arm.payload "CantBeTargetedBy" PlayerScope.codec PlayerEffect.CantBeTargetedBy (\x -> case x of PlayerEffect.CantBeTargetedBy y -> Just y; _ -> Nothing),
      Arm.payload "CastAsThoughItHadFlash" filterCodec PlayerEffect.CastAsThoughItHadFlash (\x -> case x of PlayerEffect.CastAsThoughItHadFlash y -> Just y; _ -> Nothing),
      Arm.payload "CantBeCountered" filterCodec PlayerEffect.CantBeCountered (\x -> case x of PlayerEffect.CantBeCountered y -> Just y; _ -> Nothing),
      Arm.payload "DamageCantBePrevented" DamagePattern.codec PlayerEffect.DamageCantBePrevented (\x -> case x of PlayerEffect.DamageCantBePrevented y -> Just y; _ -> Nothing),
      Arm.nullary "CantSearchLibraries" PlayerEffect.CantSearchLibraries,
      Arm.nullary "CantBecomeMonarch" PlayerEffect.CantBecomeMonarch,
      Arm.payload "CantCastMatching" filterCodec PlayerEffect.CantCastMatching (\x -> case x of PlayerEffect.CantCastMatching y -> Just y; _ -> Nothing),
      Arm.nullary "CantPlayLands" PlayerEffect.CantPlayLands,
      Arm.payload "CastFromGraveyard" filterCodec PlayerEffect.CastFromGraveyard (\x -> case x of PlayerEffect.CastFromGraveyard y -> Just y; _ -> Nothing),
      Arm.nullary "PlayLandsFromGraveyard" PlayerEffect.PlayLandsFromGraveyard,
      Arm.payload "CastFromHandWithoutPayingManaCost" filterCodec PlayerEffect.CastFromHandWithoutPayingManaCost (\x -> case x of PlayerEffect.CastFromHandWithoutPayingManaCost y -> Just y; _ -> Nothing)
    ]
  where
    filterCodec = Filter.codec Keyword.codec

module Pawl.Codec.PlayerEffect where

import qualified Pawl.Codec.AddActivationCost as AddActivationCost
import qualified Pawl.Codec.AddSpellCost as AddSpellCost
import qualified Pawl.Codec.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Codec.CastFromZone as CastFromZone
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.IncreaseActivationCost as IncreaseActivationCost
import qualified Pawl.Codec.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.ModifiedRoll as ModifiedRoll
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Codec.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.Codec.SpendManaAsThough as SpendManaAsThough
import qualified Pawl.Codec.StatedFlip as StatedFlip
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerEffect as PlayerEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec PlayerEffect.PlayerEffect
codec =
  let filterCodec = Filter.codec Keyword.codec
   in Arm.tagged
        tagOf
        [ Arm.nullary "CantCastSpells" PlayerEffect.CantCastSpells,
          Arm.nullary "CantActivateAbilities" PlayerEffect.CantActivateAbilities,
          Arm.payload "CantCastMoreThan" Common.natural PlayerEffect.CantCastMoreThan (\x -> case x of PlayerEffect.CantCastMoreThan y -> Just y; _ -> Nothing),
          Arm.nullary "CantCastChosenName" PlayerEffect.CantCastChosenName,
          Arm.nullary "CantPlayLandChosenName" PlayerEffect.CantPlayLandChosenName,
          Arm.payload "IncreaseSpellCost" IncreaseSpellCost.codec PlayerEffect.IncreaseSpellCost (\x -> case x of PlayerEffect.IncreaseSpellCost y -> Just y; _ -> Nothing),
          Arm.payload "IncreaseActivationCost" IncreaseActivationCost.codec PlayerEffect.IncreaseActivationCost (\x -> case x of PlayerEffect.IncreaseActivationCost y -> Just y; _ -> Nothing),
          Arm.payload "ReduceSpellCost" ReduceSpellCost.codec PlayerEffect.ReduceSpellCost (\x -> case x of PlayerEffect.ReduceSpellCost y -> Just y; _ -> Nothing),
          Arm.payload "ReduceActivationCost" ReduceActivationCost.codec PlayerEffect.ReduceActivationCost (\x -> case x of PlayerEffect.ReduceActivationCost y -> Just y; _ -> Nothing),
          Arm.payload "AddActivationCost" AddActivationCost.codec PlayerEffect.AddActivationCost (\x -> case x of PlayerEffect.AddActivationCost y -> Just y; _ -> Nothing),
          Arm.payload "AddSpellCost" AddSpellCost.codec PlayerEffect.AddSpellCost (\x -> case x of PlayerEffect.AddSpellCost y -> Just y; _ -> Nothing),
          Arm.payload "PlayAdditionalLands" Common.natural PlayerEffect.PlayAdditionalLands (\x -> case x of PlayerEffect.PlayAdditionalLands y -> Just y; _ -> Nothing),
          Arm.nullary "NoMaximumHandSize" PlayerEffect.NoMaximumHandSize,
          Arm.payload "SetMaximumHandSize" Common.natural PlayerEffect.SetMaximumHandSize (\x -> case x of PlayerEffect.SetMaximumHandSize y -> Just y; _ -> Nothing),
          Arm.payload "IncreaseMaximumHandSize" Common.natural PlayerEffect.IncreaseMaximumHandSize (\x -> case x of PlayerEffect.IncreaseMaximumHandSize y -> Just y; _ -> Nothing),
          Arm.payload "ReduceMaximumHandSize" Common.natural PlayerEffect.ReduceMaximumHandSize (\x -> case x of PlayerEffect.ReduceMaximumHandSize y -> Just y; _ -> Nothing),
          Arm.payload "DontLoseUnspentMana" ManaFilter.codec PlayerEffect.DontLoseUnspentMana (\x -> case x of PlayerEffect.DontLoseUnspentMana y -> Just y; _ -> Nothing),
          Arm.nullary "LoseLifeForUnspentMana" PlayerEffect.LoseLifeForUnspentMana,
          Arm.payload "SpendManaAsThough" SpendManaAsThough.codec PlayerEffect.SpendManaAsThough (\x -> case x of PlayerEffect.SpendManaAsThough y -> Just y; _ -> Nothing),
          Arm.payload "CantBeTargetedBy" PlayerScope.codec PlayerEffect.CantBeTargetedBy (\x -> case x of PlayerEffect.CantBeTargetedBy y -> Just y; _ -> Nothing),
          Arm.payload "CastAsThoughItHadFlash" filterCodec PlayerEffect.CastAsThoughItHadFlash (\x -> case x of PlayerEffect.CastAsThoughItHadFlash y -> Just y; _ -> Nothing),
          Arm.payload "MayPlayAsThoughItHadFlash" filterCodec PlayerEffect.MayPlayAsThoughItHadFlash (\x -> case x of PlayerEffect.MayPlayAsThoughItHadFlash y -> Just y; _ -> Nothing),
          Arm.payload "CantBeCountered" filterCodec PlayerEffect.CantBeCountered (\x -> case x of PlayerEffect.CantBeCountered y -> Just y; _ -> Nothing),
          Arm.payload "DamageCantBePrevented" DamagePattern.codec PlayerEffect.DamageCantBePrevented (\x -> case x of PlayerEffect.DamageCantBePrevented y -> Just y; _ -> Nothing),
          Arm.payload "DamageCantBeRedirected" DamagePattern.codec PlayerEffect.DamageCantBeRedirected (\x -> case x of PlayerEffect.DamageCantBeRedirected y -> Just y; _ -> Nothing),
          Arm.payload "CantSearchLibraries" CantSearchLibraries.codec PlayerEffect.CantSearchLibraries (\x -> case x of PlayerEffect.CantSearchLibraries y -> Just y; _ -> Nothing),
          Arm.nullary "HasProtectionFromChosenName" PlayerEffect.HasProtectionFromChosenName,
          Arm.payload "HasProtectionFrom" filterCodec PlayerEffect.HasProtectionFrom (\x -> case x of PlayerEffect.HasProtectionFrom y -> Just y; _ -> Nothing),
          Arm.nullary "CantBecomeMonarch" PlayerEffect.CantBecomeMonarch,
          Arm.payload "CantCastMatching" filterCodec PlayerEffect.CantCastMatching (\x -> case x of PlayerEffect.CantCastMatching y -> Just y; _ -> Nothing),
          Arm.nullary "CastOnlyAtSorcerySpeed" PlayerEffect.CastOnlyAtSorcerySpeed,
          Arm.payload "CantPlayLands" filterCodec PlayerEffect.CantPlayLands (\x -> case x of PlayerEffect.CantPlayLands y -> Just y; _ -> Nothing),
          Arm.payload "CastFrom" CastFromZone.codec PlayerEffect.CastFrom (\x -> case x of PlayerEffect.CastFrom y -> Just y; _ -> Nothing),
          Arm.payload "PlayLandsFrom" InZone.codec PlayerEffect.PlayLandsFrom (\x -> case x of PlayerEffect.PlayLandsFrom y -> Just y; _ -> Nothing),
          Arm.payload "CastFromHandWithoutPayingManaCost" filterCodec PlayerEffect.CastFromHandWithoutPayingManaCost (\x -> case x of PlayerEffect.CastFromHandWithoutPayingManaCost y -> Just y; _ -> Nothing),
          Arm.payload "CantGetCounters" (Common.maybe PlayerCounterKind.codec) PlayerEffect.CantGetCounters (\x -> case x of PlayerEffect.CantGetCounters y -> Just y; _ -> Nothing),
          Arm.payload "StateCoinFlip" StatedFlip.codec PlayerEffect.StateCoinFlip (\x -> case x of PlayerEffect.StateCoinFlip y -> Just y; _ -> Nothing),
          Arm.payload "ModifyDieRoll" ModifiedRoll.codec PlayerEffect.ModifyDieRoll (\x -> case x of PlayerEffect.ModifyDieRoll y -> Just y; _ -> Nothing),
          Arm.payload "AdditionalVotes" Common.natural PlayerEffect.AdditionalVotes (\x -> case x of PlayerEffect.AdditionalVotes y -> Just y; _ -> Nothing),
          Arm.nullary "CantGainLife" PlayerEffect.CantGainLife,
          Arm.nullary "CantLoseLife" PlayerEffect.CantLoseLife
        ]

tagOf :: PlayerEffect.PlayerEffect -> String
tagOf x = case x of
  PlayerEffect.CantCastSpells {} -> "CantCastSpells"
  PlayerEffect.CantActivateAbilities {} -> "CantActivateAbilities"
  PlayerEffect.CantCastMoreThan {} -> "CantCastMoreThan"
  PlayerEffect.CantCastChosenName {} -> "CantCastChosenName"
  PlayerEffect.CantPlayLandChosenName {} -> "CantPlayLandChosenName"
  PlayerEffect.IncreaseSpellCost {} -> "IncreaseSpellCost"
  PlayerEffect.IncreaseActivationCost {} -> "IncreaseActivationCost"
  PlayerEffect.ReduceSpellCost {} -> "ReduceSpellCost"
  PlayerEffect.ReduceActivationCost {} -> "ReduceActivationCost"
  PlayerEffect.AddActivationCost {} -> "AddActivationCost"
  PlayerEffect.AddSpellCost {} -> "AddSpellCost"
  PlayerEffect.PlayAdditionalLands {} -> "PlayAdditionalLands"
  PlayerEffect.NoMaximumHandSize {} -> "NoMaximumHandSize"
  PlayerEffect.SetMaximumHandSize {} -> "SetMaximumHandSize"
  PlayerEffect.IncreaseMaximumHandSize {} -> "IncreaseMaximumHandSize"
  PlayerEffect.ReduceMaximumHandSize {} -> "ReduceMaximumHandSize"
  PlayerEffect.DontLoseUnspentMana {} -> "DontLoseUnspentMana"
  PlayerEffect.LoseLifeForUnspentMana {} -> "LoseLifeForUnspentMana"
  PlayerEffect.SpendManaAsThough {} -> "SpendManaAsThough"
  PlayerEffect.CantBeTargetedBy {} -> "CantBeTargetedBy"
  PlayerEffect.CastAsThoughItHadFlash {} -> "CastAsThoughItHadFlash"
  PlayerEffect.MayPlayAsThoughItHadFlash {} -> "MayPlayAsThoughItHadFlash"
  PlayerEffect.CantBeCountered {} -> "CantBeCountered"
  PlayerEffect.DamageCantBePrevented {} -> "DamageCantBePrevented"
  PlayerEffect.DamageCantBeRedirected {} -> "DamageCantBeRedirected"
  PlayerEffect.CantSearchLibraries {} -> "CantSearchLibraries"
  PlayerEffect.HasProtectionFromChosenName {} -> "HasProtectionFromChosenName"
  PlayerEffect.HasProtectionFrom {} -> "HasProtectionFrom"
  PlayerEffect.CantBecomeMonarch {} -> "CantBecomeMonarch"
  PlayerEffect.CantCastMatching {} -> "CantCastMatching"
  PlayerEffect.CastOnlyAtSorcerySpeed {} -> "CastOnlyAtSorcerySpeed"
  PlayerEffect.CantPlayLands {} -> "CantPlayLands"
  PlayerEffect.CastFrom {} -> "CastFrom"
  PlayerEffect.PlayLandsFrom {} -> "PlayLandsFrom"
  PlayerEffect.CastFromHandWithoutPayingManaCost {} -> "CastFromHandWithoutPayingManaCost"
  PlayerEffect.CantGetCounters {} -> "CantGetCounters"
  PlayerEffect.StateCoinFlip {} -> "StateCoinFlip"
  PlayerEffect.ModifyDieRoll {} -> "ModifyDieRoll"
  PlayerEffect.AdditionalVotes {} -> "AdditionalVotes"
  PlayerEffect.CantGainLife {} -> "CantGainLife"
  PlayerEffect.CantLoseLife {} -> "CantLoseLife"

module Pawl.Codec.PlayerEffect where

import qualified Pawl.Codec.AlternativeActivationCost as AlternativeActivationCost
import qualified Pawl.Codec.CantSearchLibraries as CantSearchLibraries
import qualified Pawl.Codec.CastFromZone as CastFromZone
import qualified Pawl.Codec.CostModifier as CostModifier
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.KeywordDesignator as KeywordDesignator
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.ModifiedRoll as ModifiedRoll
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.PlotFromZone as PlotFromZone
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
          Arm.payload "CantActivateAbilities" (Common.maybe (KeywordDesignator.codec Keyword.codec)) PlayerEffect.CantActivateAbilities (\x -> case x of PlayerEffect.CantActivateAbilities y -> Just y; _ -> Nothing),
          Arm.payload "CantCastMoreThan" Common.natural PlayerEffect.CantCastMoreThan (\x -> case x of PlayerEffect.CantCastMoreThan y -> Just y; _ -> Nothing),
          Arm.payload "ModifyCost" CostModifier.codec PlayerEffect.ModifyCost (\x -> case x of PlayerEffect.ModifyCost y -> Just y; _ -> Nothing),
          Arm.payload "AlternativeActivationCost" AlternativeActivationCost.codec PlayerEffect.AlternativeActivationCost (\x -> case x of PlayerEffect.AlternativeActivationCost y -> Just y; _ -> Nothing),
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
          Arm.payload "ActivateKeywordAtInstantSpeed" (KeywordDesignator.codec Keyword.codec) PlayerEffect.ActivateKeywordAtInstantSpeed (\x -> case x of PlayerEffect.ActivateKeywordAtInstantSpeed y -> Just y; _ -> Nothing),
          Arm.payload "ActivateLoyaltyAtInstantSpeed" filterCodec PlayerEffect.ActivateLoyaltyAtInstantSpeed (\x -> case x of PlayerEffect.ActivateLoyaltyAtInstantSpeed y -> Just y; _ -> Nothing),
          Arm.payload "CantBeCountered" filterCodec PlayerEffect.CantBeCountered (\x -> case x of PlayerEffect.CantBeCountered y -> Just y; _ -> Nothing),
          Arm.payload "DamageCantBePrevented" DamagePattern.codec PlayerEffect.DamageCantBePrevented (\x -> case x of PlayerEffect.DamageCantBePrevented y -> Just y; _ -> Nothing),
          Arm.payload "DamageCantBeRedirected" DamagePattern.codec PlayerEffect.DamageCantBeRedirected (\x -> case x of PlayerEffect.DamageCantBeRedirected y -> Just y; _ -> Nothing),
          Arm.payload "CantSearchLibraries" CantSearchLibraries.codec PlayerEffect.CantSearchLibraries (\x -> case x of PlayerEffect.CantSearchLibraries y -> Just y; _ -> Nothing),
          Arm.payload "HasProtectionFrom" filterCodec PlayerEffect.HasProtectionFrom (\x -> case x of PlayerEffect.HasProtectionFrom y -> Just y; _ -> Nothing),
          Arm.nullary "CantBecomeMonarch" PlayerEffect.CantBecomeMonarch,
          Arm.nullary "CantSetSchemesInMotion" PlayerEffect.CantSetSchemesInMotion,
          Arm.nullary "CantAttackWithCreatures" PlayerEffect.CantAttackWithCreatures,
          Arm.payload "CantCastMatching" filterCodec PlayerEffect.CantCastMatching (\x -> case x of PlayerEffect.CantCastMatching y -> Just y; _ -> Nothing),
          Arm.nullary "CastOnlyAtSorcerySpeed" PlayerEffect.CastOnlyAtSorcerySpeed,
          Arm.payload "CantPlayLands" filterCodec PlayerEffect.CantPlayLands (\x -> case x of PlayerEffect.CantPlayLands y -> Just y; _ -> Nothing),
          Arm.payload "CastFrom" CastFromZone.codec PlayerEffect.CastFrom (\x -> case x of PlayerEffect.CastFrom y -> Just y; _ -> Nothing),
          Arm.payload "PlayLandsFrom" InZone.codec PlayerEffect.PlayLandsFrom (\x -> case x of PlayerEffect.PlayLandsFrom y -> Just y; _ -> Nothing),
          Arm.payload "PlotFrom" PlotFromZone.codec PlayerEffect.PlotFrom (\x -> case x of PlayerEffect.PlotFrom y -> Just y; _ -> Nothing),
          Arm.payload "CastFromHandWithoutPayingManaCost" filterCodec PlayerEffect.CastFromHandWithoutPayingManaCost (\x -> case x of PlayerEffect.CastFromHandWithoutPayingManaCost y -> Just y; _ -> Nothing),
          Arm.payload "CantGetCounters" (Common.maybe PlayerCounterKind.codec) PlayerEffect.CantGetCounters (\x -> case x of PlayerEffect.CantGetCounters y -> Just y; _ -> Nothing),
          Arm.payload "StateCoinFlip" StatedFlip.codec PlayerEffect.StateCoinFlip (\x -> case x of PlayerEffect.StateCoinFlip y -> Just y; _ -> Nothing),
          Arm.payload "ModifyDieRoll" ModifiedRoll.codec PlayerEffect.ModifyDieRoll (\x -> case x of PlayerEffect.ModifyDieRoll y -> Just y; _ -> Nothing),
          Arm.payload "AdditionalVotes" Common.natural PlayerEffect.AdditionalVotes (\x -> case x of PlayerEffect.AdditionalVotes y -> Just y; _ -> Nothing),
          Arm.payload "AdditionalSurveilCards" Common.natural PlayerEffect.AdditionalSurveilCards (\x -> case x of PlayerEffect.AdditionalSurveilCards y -> Just y; _ -> Nothing),
          Arm.nullary "CantGainLife" PlayerEffect.CantGainLife,
          Arm.nullary "CantLoseLife" PlayerEffect.CantLoseLife
        ]

tagOf :: PlayerEffect.PlayerEffect -> String
tagOf x = case x of
  PlayerEffect.CantCastSpells {} -> "CantCastSpells"
  PlayerEffect.CantActivateAbilities {} -> "CantActivateAbilities"
  PlayerEffect.CantCastMoreThan {} -> "CantCastMoreThan"
  PlayerEffect.ModifyCost {} -> "ModifyCost"
  PlayerEffect.AlternativeActivationCost {} -> "AlternativeActivationCost"
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
  PlayerEffect.ActivateKeywordAtInstantSpeed {} -> "ActivateKeywordAtInstantSpeed"
  PlayerEffect.ActivateLoyaltyAtInstantSpeed {} -> "ActivateLoyaltyAtInstantSpeed"
  PlayerEffect.CantBeCountered {} -> "CantBeCountered"
  PlayerEffect.DamageCantBePrevented {} -> "DamageCantBePrevented"
  PlayerEffect.DamageCantBeRedirected {} -> "DamageCantBeRedirected"
  PlayerEffect.CantSearchLibraries {} -> "CantSearchLibraries"
  PlayerEffect.HasProtectionFrom {} -> "HasProtectionFrom"
  PlayerEffect.CantBecomeMonarch {} -> "CantBecomeMonarch"
  PlayerEffect.CantSetSchemesInMotion {} -> "CantSetSchemesInMotion"
  PlayerEffect.CantAttackWithCreatures {} -> "CantAttackWithCreatures"
  PlayerEffect.CantCastMatching {} -> "CantCastMatching"
  PlayerEffect.CastOnlyAtSorcerySpeed {} -> "CastOnlyAtSorcerySpeed"
  PlayerEffect.CantPlayLands {} -> "CantPlayLands"
  PlayerEffect.CastFrom {} -> "CastFrom"
  PlayerEffect.PlayLandsFrom {} -> "PlayLandsFrom"
  PlayerEffect.PlotFrom {} -> "PlotFrom"
  PlayerEffect.CastFromHandWithoutPayingManaCost {} -> "CastFromHandWithoutPayingManaCost"
  PlayerEffect.CantGetCounters {} -> "CantGetCounters"
  PlayerEffect.StateCoinFlip {} -> "StateCoinFlip"
  PlayerEffect.ModifyDieRoll {} -> "ModifyDieRoll"
  PlayerEffect.AdditionalVotes {} -> "AdditionalVotes"
  PlayerEffect.AdditionalSurveilCards {} -> "AdditionalSurveilCards"
  PlayerEffect.CantGainLife {} -> "CantGainLife"
  PlayerEffect.CantLoseLife {} -> "CantLoseLife"

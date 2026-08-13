module Pawl.Codec.PlayerEffect where

import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.DamagePattern as DamagePattern
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.Codec.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.PlayerEffect as PlayerEffect

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- The multi-payload arms keep a 'Common.tuple'. Under the #1305 decision they
-- owe records of their own, like every other multi-payload arm.
codec :: Codec.Codec PlayerEffect.PlayerEffect
codec =
  Arm.tagged
    encode
    [ Arm.nullary "CantCastSpells" PlayerEffect.CantCastSpells,
      Arm.payload "CantCastMoreThan" Common.natural PlayerEffect.CantCastMoreThan,
      Arm.nullary "CantCastChosenName" PlayerEffect.CantCastChosenName,
      Arm.nullary "CantPlayLandChosenName" PlayerEffect.CantPlayLandChosenName,
      Arm.payload "IncreaseSpellCost" (Common.tuple filterCodec Common.natural) (uncurry PlayerEffect.IncreaseSpellCost),
      Arm.payload "ReduceSpellCost" (Common.tuple filterCodec ManaCost.codec) (uncurry PlayerEffect.ReduceSpellCost),
      Arm.payload "ReduceActivationCost" ReduceActivationCost.codec PlayerEffect.ReduceActivationCost,
      Arm.payload "AddActivationCost" (Common.tuple filterCodec (Common.list (CostComponent.codec Keyword.codec))) (uncurry PlayerEffect.AddActivationCost),
      Arm.payload "PlayAdditionalLands" Common.natural PlayerEffect.PlayAdditionalLands,
      Arm.nullary "NoMaximumHandSize" PlayerEffect.NoMaximumHandSize,
      Arm.payload "SetMaximumHandSize" Common.natural PlayerEffect.SetMaximumHandSize,
      Arm.payload "DontLoseUnspentMana" ManaFilter.codec PlayerEffect.DontLoseUnspentMana,
      Arm.payload "CantBeTargetedBy" PlayerScope.codec PlayerEffect.CantBeTargetedBy,
      Arm.payload "CastAsThoughItHadFlash" filterCodec PlayerEffect.CastAsThoughItHadFlash,
      Arm.payload "CantBeCountered" filterCodec PlayerEffect.CantBeCountered,
      Arm.payload "DamageCantBePrevented" DamagePattern.codec PlayerEffect.DamageCantBePrevented,
      Arm.nullary "CantSearchLibraries" PlayerEffect.CantSearchLibraries,
      Arm.nullary "CantBecomeMonarch" PlayerEffect.CantBecomeMonarch,
      Arm.payload "CantCastMatching" filterCodec PlayerEffect.CantCastMatching,
      Arm.nullary "CantPlayLands" PlayerEffect.CantPlayLands,
      Arm.payload "CastFromGraveyard" filterCodec PlayerEffect.CastFromGraveyard
    ]
  where
    filterCodec = Filter.codec Keyword.codec
    encode e = case e of
      PlayerEffect.CantCastSpells -> Common.nullary "CantCastSpells"
      PlayerEffect.CantCastMoreThan n -> Common.tagged "CantCastMoreThan" . Just $ Common.encodeNatural n
      PlayerEffect.CantCastChosenName -> Common.nullary "CantCastChosenName"
      PlayerEffect.CantPlayLandChosenName -> Common.nullary "CantPlayLandChosenName"
      PlayerEffect.IncreaseSpellCost c n -> Common.tagged "IncreaseSpellCost" . Just . Value.array $ [Codec.encode filterCodec c, Common.encodeNatural n]
      PlayerEffect.ReduceSpellCost c m -> Common.tagged "ReduceSpellCost" . Just . Value.array $ [Codec.encode filterCodec c, Codec.encode ManaCost.codec m]
      PlayerEffect.ReduceActivationCost x -> Common.tagged "ReduceActivationCost" . Just $ Codec.encode ReduceActivationCost.codec x
      PlayerEffect.AddActivationCost c cs -> Common.tagged "AddActivationCost" . Just . Value.array $ [Codec.encode filterCodec c, Codec.encode (Common.list (CostComponent.codec Keyword.codec)) cs]
      PlayerEffect.PlayAdditionalLands n -> Common.tagged "PlayAdditionalLands" . Just $ Common.encodeNatural n
      PlayerEffect.NoMaximumHandSize -> Common.nullary "NoMaximumHandSize"
      PlayerEffect.SetMaximumHandSize n -> Common.tagged "SetMaximumHandSize" . Just $ Common.encodeNatural n
      PlayerEffect.DontLoseUnspentMana f -> Common.tagged "DontLoseUnspentMana" . Just $ Codec.encode ManaFilter.codec f
      PlayerEffect.CantBeTargetedBy sc -> Common.tagged "CantBeTargetedBy" . Just $ Codec.encode PlayerScope.codec sc
      PlayerEffect.CastAsThoughItHadFlash c -> Common.tagged "CastAsThoughItHadFlash" . Just $ Codec.encode filterCodec c
      PlayerEffect.CantBeCountered c -> Common.tagged "CantBeCountered" . Just $ Codec.encode filterCodec c
      PlayerEffect.DamageCantBePrevented p -> Common.tagged "DamageCantBePrevented" . Just $ Codec.encode DamagePattern.codec p
      PlayerEffect.CantSearchLibraries -> Common.nullary "CantSearchLibraries"
      PlayerEffect.CantBecomeMonarch -> Common.nullary "CantBecomeMonarch"
      PlayerEffect.CantCastMatching c -> Common.tagged "CantCastMatching" . Just $ Codec.encode filterCodec c
      PlayerEffect.CantPlayLands -> Common.nullary "CantPlayLands"
      PlayerEffect.CastFromGraveyard c -> Common.tagged "CastFromGraveyard" . Just $ Codec.encode filterCodec c

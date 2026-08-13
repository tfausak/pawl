module Pawl.Codec.Pool where

import qualified Pawl.Codec.GraveyardScope as GraveyardScope
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Pool as Pool

-- | Tagged rather than nullary-only, because CR 400.1's per-player graveyard
-- makes one arm carry a payload: the GraveyardScope saying whose. The nullary arms
-- are unaffected, since Common.nullary IS Common.tagged with no value.
codec :: Codec.Codec Pool.Pool
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Creatures" Pool.Creatures,
      Arm.nullary "Players" Pool.Players,
      Arm.nullary "AnyTarget" Pool.AnyTarget,
      Arm.nullary "Permanents" Pool.Permanents,
      Arm.nullary "Spells" Pool.Spells,
      Arm.nullary "Abilities" Pool.Abilities,
      Arm.nullary "SpellsAndPermanents" Pool.SpellsAndPermanents,
      Arm.payload "CardsInGraveyard" GraveyardScope.codec Pool.CardsInGraveyard,
      -- Nullary: CR 400.1's shared zones have no per-player copy for a payload
      -- to select among.
      Arm.nullary "CardsInExile" Pool.CardsInExile
    ]
  where
    encode p = case p of
      Pool.Creatures -> Common.nullary "Creatures"
      Pool.Players -> Common.nullary "Players"
      Pool.AnyTarget -> Common.nullary "AnyTarget"
      Pool.Permanents -> Common.nullary "Permanents"
      Pool.Spells -> Common.nullary "Spells"
      Pool.Abilities -> Common.nullary "Abilities"
      Pool.SpellsAndPermanents -> Common.nullary "SpellsAndPermanents"
      Pool.CardsInGraveyard scope -> Common.tagged "CardsInGraveyard" . Just $ Codec.encode GraveyardScope.codec scope
      Pool.CardsInExile -> Common.nullary "CardsInExile"

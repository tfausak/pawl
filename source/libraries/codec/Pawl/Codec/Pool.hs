module Pawl.Codec.Pool where

import qualified Pawl.Codec.GraveyardScope as GraveyardScope
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Pool as Pool

-- | Tagged rather than nullary-only, because CR 400.1's per-player graveyard
-- makes one arm carry a payload: the GraveyardScope saying whose. The nullary arms
-- are unaffected, since Common.nullary IS Common.tagged with no value.
codec :: Codec.Codec Pool.Pool
codec =
  Arm.tagged
    [ Arm.nullary "Creatures" Pool.Creatures,
      Arm.nullary "Players" Pool.Players,
      Arm.nullary "AnyTarget" Pool.AnyTarget,
      Arm.nullary "Permanents" Pool.Permanents,
      Arm.nullary "Spells" Pool.Spells,
      Arm.nullary "Abilities" Pool.Abilities,
      Arm.nullary "SpellsAndPermanents" Pool.SpellsAndPermanents,
      Arm.payload "CardsInGraveyard" GraveyardScope.codec Pool.CardsInGraveyard (\x -> case x of Pool.CardsInGraveyard y -> Just y; _ -> Nothing),
      -- Nullary: CR 400.1's shared zones have no per-player copy for a payload
      -- to select among.
      Arm.nullary "CardsInExile" Pool.CardsInExile
    ]

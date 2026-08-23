module Pawl.Codec.AffectedPlayers where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers

-- | Tagged, in Pawl.Codec.GraveyardScope's shape for the same pair of readings: a
-- nested PlayerScope, or the thing the second arm names.
--
-- PARAMETRIC in that second thing, because both of the type's instantiations are
-- now serialized: a CARD writes @AffectedPlayers SlotName@ (Pawl.Codec.AffectPlayers
-- passes Pawl.Codec.SlotName's codec) and the store holds @AffectedPlayers PlayerId@
-- on Pawl.Types.ActivePlayerEffect (Pawl.Codec.ActivePlayerEffect passes
-- Pawl.Codec.PlayerId's). Neither instantiation gains an arm it cannot answer,
-- so no lint is owed to keep a card from writing a seat.
codec ::
  (Typeable.Typeable player) =>
  Codec.Codec player ->
  Codec.Codec (AffectedPlayers.AffectedPlayers player)
codec playerCodec =
  Arm.tagged
    [ Arm.payload "Scoped" PlayerScope.codec AffectedPlayers.Scoped (\x -> case x of AffectedPlayers.Scoped y -> Just y; _ -> Nothing),
      Arm.payload "Named" playerCodec AffectedPlayers.Named (\x -> case x of AffectedPlayers.Named y -> Just y; _ -> Nothing)
    ]

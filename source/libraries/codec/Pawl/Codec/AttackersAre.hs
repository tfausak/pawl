{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackersAre where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackersAre as AttackersAre

-- | Keyed by attacker, as Pawl.Codec.Move's blocks are: @{"$bear": "$bob"}@.
codec :: Codec.Codec AttackersAre.AttackersAre
codec = Fields.object $ do
  attackers <- Fields.required "attackers" (Common.textMap Reference.toText Reference.fromText Reference.codec) AttackersAre.attackers
  pure AttackersAre.MkAttackersAre {AttackersAre.attackers = attackers}

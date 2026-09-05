module Pawl.Codec.CastFromSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CastFrom as CastFrom
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastFrom as CastFrom
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Zone as Zone

-- Decodes a payload, discarding the parse and the decode error alike:
-- Pawl.Codec.InZoneSpec's helper, for the one case here that asks only whether
-- the value was accepted.
decodes :: String -> Bool
decodes = Either.isRight . (\t -> Common.parse (Text.pack t) >>= Codec.decode CastFrom.codec)

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastFrom" $ do
  -- Archfiend's Vessel's "you cast it from YOUR graveyard": both halves are CR
  -- 109.5's "you", and the caster is elided because that is the default.
  Spec.it s "elides a caster of You" $
    Common.assertCodec
      s
      CastFrom.codec
      ( CastFrom.MkCastFrom
          { CastFrom.caster = PlayerRef.Relative PlayerRelation.You,
            CastFrom.from = InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.You)
          }
      )
      " {\"from\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"zone\":{\"type\":\"Graveyard\"}}} "
  -- Breathless Knight's "you cast it from A graveyard": the caster is still CR
  -- 109.5's "you" and the zone's owner is the whole table, which is the pairing
  -- one reference could not state.
  Spec.it s "states a caster that is not the zone's owner" $
    Common.assertCodec
      s
      CastFrom.codec
      ( CastFrom.MkCastFrom
          { CastFrom.caster = PlayerRef.Relative PlayerRelation.You,
            CastFrom.from = InZone.MkInZone Zone.Graveyard PlayerRef.EachPlayer
          }
      )
      " {\"from\":{\"player\":{\"type\":\"EachPlayer\"},\"zone\":{\"type\":\"Graveyard\"}}} "
  -- Fblthp, the Lost's agentless "was cast from your library": the halves
  -- constrained the OTHER way round, and the case that makes the caster a field
  -- rather than a default nobody overrides.
  Spec.it s "states an agentless caster over one player's zone" $
    Common.assertCodec
      s
      CastFrom.codec
      ( CastFrom.MkCastFrom
          { CastFrom.caster = PlayerRef.EachPlayer,
            CastFrom.from = InZone.MkInZone Zone.Library (PlayerRef.Relative PlayerRelation.You)
          }
      )
      " {\"caster\":{\"type\":\"EachPlayer\"},\"from\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"zone\":{\"type\":\"Library\"}}} "
  -- CR 400.1's invariant is inherited rather than restated: `from` is
  -- Pawl.Codec.InZone's codec, so the pairing it rejects is rejected here. The
  -- accepted payload above and this one differ in the ZONE alone.
  Spec.it s "CR 400.1 rejects one player's share of a shared zone" $
    Spec.assertBool
      s
      (not (decodes " {\"from\":{\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"zone\":{\"type\":\"Exile\"}}} "))
      "a shared zone paired with a relative reference is rejected"
  Spec.it s "has a schema" $ Common.assertHasSchema s CastFrom.codec

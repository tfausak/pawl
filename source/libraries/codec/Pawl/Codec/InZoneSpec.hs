module Pawl.Codec.InZoneSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Zone as Zone

-- Decodes a payload, discarding the parse and the decode error alike: the cases
-- below ask only whether the value was accepted.
decodes :: String -> Bool
decodes = Either.isRight . (\t -> Common.parse (Text.pack t) >>= Codec.decode InZone.codec)

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.InZone" $ do
  -- CR 400.1: the battlefield is shared, so EachPlayer is what most counts say.
  Spec.it s "MkInZone, both keys" $
    Common.assertCodec
      s
      InZone.codec
      (InZone.MkInZone {InZone.zone = Zone.Battlefield, InZone.player = PlayerRef.EachPlayer})
      " {\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"EachPlayer\"}} "
  -- CR 400.1's per-player half: a graveyard is one player's, so every reference
  -- naming players is a question that zone can answer.
  Spec.it s "a per-player zone takes any reference" $
    Common.assertCodec
      s
      InZone.codec
      (InZone.MkInZone {InZone.zone = Zone.Graveyard, InZone.player = PlayerRef.Relative PlayerRelation.Opponent})
      " {\"zone\":{\"type\":\"Graveyard\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"Opponent\"}}} "
  -- CR 400.1's invariant, at the point card data enters the engine. The two
  -- payloads differ in the zone alone: the pairing above is the same reference
  -- over a zone its owner has a copy of.
  Spec.it s "CR 400.1 rejects one player's share of a shared zone" $ do
    Spec.assertBool
      s
      (not (decodes " {\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"Opponent\"}}} "))
      "a shared zone paired with a relative reference is rejected"
    Spec.assertBool
      s
      (not (decodes " {\"zone\":{\"type\":\"Exile\"},\"player\":{\"type\":\"InSlot\",\"value\":\"target\"}} "))
      "a shared zone paired with a slot is rejected"
    Spec.assertBool
      s
      (not (decodes " {\"zone\":{\"type\":\"Stack\"},\"player\":{\"type\":\"Candidate\"}} "))
      "a shared zone paired with the fold's candidate is rejected"
    Spec.assertBool
      s
      (not (decodes " {\"zone\":{\"type\":\"Command\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "))
      "the command zone is shared too"
    -- The other three zones CR 400.1 gives each player their own copy of, so a
    -- rejection reading the ZONE wrongly is caught rather than passing as a
    -- stricter version of the same rule.
    Spec.assertBool
      s
      (decodes " {\"zone\":{\"type\":\"Library\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} ")
      "a library is one player's"
    Spec.assertBool
      s
      (decodes " {\"zone\":{\"type\":\"Hand\"},\"player\":{\"type\":\"InSlot\",\"value\":\"target\"}} ")
      "and so is a hand"
  Spec.it s "has a schema" $ Common.assertHasSchema s InZone.codec

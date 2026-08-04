-- | Where the card knot is tied: @Pawl.Codec.Face@ carries the 28 printed
-- characteristics and is parametric in the card codec, and this module passes
-- its own 'toJson'\/'fromJson' back in.
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core.
module Pawl.Codec.Card where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Face as Face
import qualified Pawl.Codec.Layout as Layout
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Layout as Layout

toJson :: Card.Card -> Value.Value
toJson c =
  Common.object $
    Common.requiredPair "faces" (Common.encodeNonEmpty (Face.toJson toJson)) (Card.faces c)
      -- CR 709-722: Normal is the absence of a card saying otherwise, so it is
      -- a default rather than a required key and 227 single-face files say
      -- nothing about it.
      <> Common.optionalPair "layout" Layout.Normal Layout.toJson (Card.layout c)

fromJson :: Value.Value -> Either Text.Text Card.Card
fromJson value = do
  ps <- Common.asObject value
  -- Common.decodeNonEmpty rejects an empty array, which is where the
  -- at-least-one-face invariant is enforced -- the UnsafeX posture
  -- Pawl.Types.Modal's modes field documents.
  faces <- Common.field "faces" ps >>= Common.decodeNonEmpty (Face.fromJson fromJson)
  layout <- Common.defaultedField "layout" Layout.Normal Layout.fromJson ps
  pure Card.MkCard {Card.layout = layout, Card.faces = faces}

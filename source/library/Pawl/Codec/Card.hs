{-# LANGUAGE ApplicativeDo #-}

-- | Where the card knot is tied: @Pawl.Codec.Face@ carries the printed
-- characteristics and is parametric in the card codec, and this module passes
-- its own back in.
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core.
module Pawl.Codec.Card where

import qualified Pawl.Codec.Face as Face
import qualified Pawl.Codec.Layout as Layout
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Layout as Layout

-- | RECURSIVE: 'codec' names itself, which is the knot. It terminates for
-- Pawl.Codec.Effect's reason -- 'Fields.object' reaches WHNF as a
-- 'Codec.MkCodec' without forcing its field list, and 'Define.define' registers
-- this type's name before running the schema body, so the re-entry emits a
-- @$ref@.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec Card.Card
codec = Fields.object $ do
  -- Common.nonEmpty rejects an empty array, which is where the at-least-one-face
  -- invariant is enforced -- the UnsafeX posture Pawl.Types.Modal's modes field
  -- documents.
  faces <- Fields.required "faces" (Common.nonEmpty (Face.codec codec)) Card.faces
  -- CR 709-722: Normal is the absence of a card saying otherwise, so it is a
  -- default rather than a required key and most single-face files say nothing
  -- about it.
  layout <- Fields.defaulted "layout" Layout.Normal Layout.codec Card.layout
  pure Card.MkCard {Card.layout = layout, Card.faces = faces}

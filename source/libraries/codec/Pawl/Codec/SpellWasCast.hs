{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SpellWasCast where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SpellWasCast as SpellWasCast

-- | A bare object keyed by the record's field names, replacing the positional
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec SpellWasCast.SpellWasCast
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec SpellWasCast.player
  spell <- Fields.required "spell" ObjectId.codec SpellWasCast.spell
  characteristics <- Fields.required "characteristics" ProjectedCharacteristics.codec SpellWasCast.characteristics
  pure
    SpellWasCast.MkSpellWasCast
      { SpellWasCast.player = player,
        SpellWasCast.spell = spell,
        SpellWasCast.characteristics = characteristics
      }

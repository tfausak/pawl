{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FullText where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FullText as FullText

codec :: (Typeable.Typeable ability) => Codec.Codec ability -> Codec.Codec (FullText.FullText ability)
codec abilityCodec = Fields.object $ do
  graveyard <- Fields.required "graveyard" PlayerRef.codec FullText.graveyard
  alsoHas <- Fields.required "alsoHas" (Common.list abilityCodec) FullText.alsoHas
  pure FullText.MkFullText {FullText.graveyard = graveyard, FullText.alsoHas = alsoHas}

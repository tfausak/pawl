{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivateManaAbilities where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivateManaAbilities as ActivateManaAbilities

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's ActivateManaAbilities arm.
codec :: Codec.Codec ActivateManaAbilities.ActivateManaAbilities
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ActivateManaAbilities.filter
  player <- Fields.required "player" PlayerRef.codec ActivateManaAbilities.player
  pure
    ActivateManaAbilities.MkActivateManaAbilities
      { ActivateManaAbilities.filter = filter_,
        ActivateManaAbilities.player = player
      }

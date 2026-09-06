{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChooseCardName where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChooseCardName as ChooseCardName

-- | Both fields are REQUIRED. The chooser has no default worth writing: CR
-- 109.5's "you" is a reading of the printed words rather than an absence, so a
-- card that means it says so, exactly as Pawl.Codec.Search's two PlayerRefs do.
codec :: Codec.Codec ChooseCardName.ChooseCardName
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec ChooseCardName.player
  restriction <- Fields.required "restriction" (Filter.codec Keyword.codec) ChooseCardName.restriction
  pure
    ChooseCardName.MkChooseCardName
      { ChooseCardName.player = player,
        ChooseCardName.restriction = restriction
      }

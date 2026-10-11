{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChosenPermanents where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.HowMany as HowMany
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChosenPermanents as ChosenPermanents
import qualified Pawl.Types.HowMany as HowMany
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
--
-- @chooser@ and @count@ are defaulted, so the printed choice of one permanent
-- addressed to the resolving controller writes neither key.
codec :: Codec.Codec ChosenPermanents.ChosenPermanents
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenPermanents.filter
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ChosenPermanents.chooser
  count <- Fields.defaulted "count" HowMany.One HowMany.codec ChosenPermanents.count
  pure
    ChosenPermanents.MkChosenPermanents
      { ChosenPermanents.filter = filter_,
        ChosenPermanents.chooser = chooser,
        ChosenPermanents.count = count
      }

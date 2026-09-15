{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChosenPermanent where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChosenPermanent as ChosenPermanent
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
--
-- @chooser@ is defaulted rather than required, so the printed choice addressed to
-- the resolving controller writes no key at all.
codec :: Codec.Codec ChosenPermanent.ChosenPermanent
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenPermanent.filter
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ChosenPermanent.chooser
  pure
    ChosenPermanent.MkChosenPermanent
      { ChosenPermanent.filter = filter_,
        ChosenPermanent.chooser = chooser
      }

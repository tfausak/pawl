{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MakeForetold where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MakeForetold as MakeForetold

-- | The cards are required and CR 702.143d's cost defaults to absent: rule
-- 702.143d's second sentence is a "may", and a card that states only the first
-- (The Foretold Soldier) leaves the exiled card its own printed foretell cost.
codec :: Codec.Codec MakeForetold.MakeForetold
codec = Fields.object $ do
  cards <- Fields.required "cards" ObjectRef.codec MakeForetold.cards
  manaCostReducedBy <- Fields.defaulted "manaCostReducedBy" Nothing (Common.maybe ManaCost.codec) MakeForetold.manaCostReducedBy
  pure
    MakeForetold.MkMakeForetold
      { MakeForetold.cards = cards,
        MakeForetold.manaCostReducedBy = manaCostReducedBy
      }

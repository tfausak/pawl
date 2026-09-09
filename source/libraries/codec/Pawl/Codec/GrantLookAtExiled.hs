{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.GrantLookAtExiled where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GrantLookAtExiled as GrantLookAtExiled

-- | The cards are required and CR 702.75a's rider defaults to absent: the
-- keyword is what asks for it, and a card that spells the grant out (Extract
-- Power) gives CR 406.3's permission alone.
codec :: Codec.Codec GrantLookAtExiled.GrantLookAtExiled
codec = Fields.object $ do
  cards <- Fields.required "cards" ObjectRef.codec GrantLookAtExiled.cards
  followsExiler <- Fields.defaulted "followsExiler" False Common.boolean GrantLookAtExiled.followsExiler
  pure
    GrantLookAtExiled.MkGrantLookAtExiled
      { GrantLookAtExiled.cards = cards,
        GrantLookAtExiled.followsExiler = followsExiler
      }

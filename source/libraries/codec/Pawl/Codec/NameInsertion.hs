{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.NameInsertion where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.NameInsertion as NameInsertion

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec NameInsertion.NameInsertion
codec = Fields.object $ do
  word <- Fields.required "word" Common.text NameInsertion.word
  after <- Fields.required "after" Common.natural NameInsertion.after
  pure NameInsertion.MkNameInsertion {NameInsertion.word = word, NameInsertion.after = after}

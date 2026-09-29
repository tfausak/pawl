{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Timed where

import qualified Pawl.Codec.Entry as Entry
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.Codec.When as When
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Timed as Timed

-- | One flat object: the moment's keys, an optional @source@, and @do@ or
-- @check@.
codec :: Codec.Codec Timed.Timed
codec = Fields.object $ do
  when <- Fields.contramap Timed.when When.fields
  source <- Fields.defaulted "source" Nothing (Common.maybe Reference.codec) Timed.source
  entry <- Fields.contramap Timed.entry Entry.fields
  pure Timed.MkTimed {Timed.when = when, Timed.source = source, Timed.entry = entry}

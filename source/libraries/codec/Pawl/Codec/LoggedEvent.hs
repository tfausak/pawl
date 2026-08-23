{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LoggedEvent where

import qualified Pawl.Codec.EventGroup as EventGroup
import qualified Pawl.Codec.GameEvent as GameEvent
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LoggedEvent as LoggedEvent

codec :: Codec.Codec LoggedEvent.LoggedEvent
codec = Fields.object $ do
  group <- Fields.required "group" EventGroup.codec LoggedEvent.group
  event <- Fields.required "event" GameEvent.codec LoggedEvent.event
  pure LoggedEvent.MkLoggedEvent {LoggedEvent.group = group, LoggedEvent.event = event}

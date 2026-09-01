{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.GameSettings where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GameSettings as GameSettings

-- | An OBJECT and not a bare boolean, though the record holds one field today:
-- each further option of CR 800.2 is another field here (#175), and a wire
-- shape that changed kind when the second one landed would break every
-- document written before it.
--
-- 'Fields.required' rather than a default, for Pawl.Codec.Player's reason: a
-- document that does not say which options are in play is one whose reader
-- would have to guess, and CR 800.2's options are settled before the game
-- begins.
codec :: Codec.Codec GameSettings.GameSettings
codec = Fields.object $ do
  brawl <- Fields.required "brawl" Common.boolean GameSettings.brawl
  pure GameSettings.MkGameSettings {GameSettings.brawl = brawl}

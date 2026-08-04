module Pawl.Types.CardError where

import qualified Pawl.Types.CardName as CardName

-- | Why a registry could not answer with a card.
--
-- Deliberately says nothing about files. This type belongs to the registry
-- INTERFACE, so every way of answering has to be able to speak it: a map-backed
-- registry has no path to report and no file name to disagree with. Whatever a
-- particular registry knows about its own failure goes in Invalid's message.
--
-- Two constructors, so a caller wanting "unknown card X, did you mean...?" need
-- not string-match a `show` to tell it from "that card is broken" (#167).
data CardError
  = Missing CardName.CardName
  | -- | Invalid rather than Corrupt: corruption suggests damaged bytes, which is
    -- one file-specific cause among several. A card that parses and then fails
    -- validation is equally unusable and is not corrupt.
    Invalid CardName.CardName String
  deriving (Eq, Show)

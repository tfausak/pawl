{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChooseBetween where

import qualified Data.Text as Text
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChooseBetween as ChooseBetween

-- | The bare object the enclosing tag already carried, now with a record behind
-- it to name -- which is what gives it a schema definition.
--
-- The @least <= most@ check is 'Fields.objectWith''s, not a field's: it reads
-- the assembled record rather than any one key. Not a schema constraint either,
-- for the reason that function's haddock gives -- the rule being enforced is a
-- rule of Magic.
codec :: Codec.Codec ChooseBetween.ChooseBetween
codec = Fields.objectWith check $ do
  least <- Fields.required "least" Common.natural ChooseBetween.least
  most <- Fields.required "most" Common.natural ChooseBetween.most
  pure
    ChooseBetween.MkChooseBetween
      { ChooseBetween.least = least,
        ChooseBetween.most = most
      }
  where
    check cb =
      if ChooseBetween.least cb > ChooseBetween.most cb
        then Left (Text.pack "ModeSelection: least must not exceed most")
        else Right cb

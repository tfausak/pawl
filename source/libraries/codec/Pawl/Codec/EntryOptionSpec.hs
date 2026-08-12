{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EntryOptionSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.Keyword as Keyword

-- CR 208.2b / 614.1c: one of the shapes an as-enters choice offers. EntryOption
-- has one constructor, so both shapes ride one case -- a plain option and one
-- carrying a nonempty keyword set.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryOption" $ do
  Spec.it s "MkEntryOption round-trips with an empty keyword set" $
    Common.assertJsonCodec
      s
      EntryOption.toJson
      EntryOption.fromJson
      (EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty})
      """ {"power":3,"toughness":3} """
  Spec.it s "MkEntryOption round-trips with its keyword set" $
    Common.assertJsonCodec
      s
      EntryOption.toJson
      EntryOption.fromJson
      (EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender})
      """ {"power":1,"toughness":6,"keywords":[{"type":"Defender"}]} """

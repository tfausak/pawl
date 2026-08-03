module Pawl.Codec.EntryRewriteSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Keyword as Keyword

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRewrite" $ do
  -- CR 707.5: Clone becomes a copy as it enters, with no producer-visible
  -- payload of its own.
  Spec.it s "AsCopy (Clone)" $
    Common.assertJsonCodec s EntryRewrite.toJson EntryRewrite.fromJson EntryRewrite.AsCopy "{\"type\":\"AsCopy\"}"
  -- CR 208.2b: Primal Plasma's three P/T-and-keyword choices, here narrowed to
  -- the two that show the keyword union isn't lost on the wire.
  Spec.it s "ChoiceOf (Primal Plasma)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      ( EntryRewrite.ChoiceOf
          [ EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty},
            EntryOption.MkEntryOption {EntryOption.power = 1, EntryOption.toughness = 6, EntryOption.keywords = Set.singleton Keyword.Defender}
          ]
      )
      "{\"type\":\"ChoiceOf\",\"value\":[{\"power\":3,\"toughness\":3,\"keywords\":[]},{\"power\":1,\"toughness\":6,\"keywords\":[{\"type\":\"Defender\"}]}]}"
  -- CR 614.1c / 105.1: Painter's Servant's as-enters colour choice, payload-free
  -- because the five colours are always the whole offer.
  Spec.it s "ChooseColor (Painter's Servant)" $
    Common.assertJsonCodec s EntryRewrite.toJson EntryRewrite.fromJson EntryRewrite.ChooseColor "{\"type\":\"ChooseColor\"}"
  Spec.it s "ChooseBasicLandType (Convincing Mirage)" $
    Common.assertJsonCodec s EntryRewrite.toJson EntryRewrite.fromJson EntryRewrite.ChooseBasicLandType "{\"type\":\"ChooseBasicLandType\"}"
  -- CR 614.1c / 306.5b: a planeswalker's intrinsic entry-with-counters rewrite.
  Spec.it s "WithCounters (planeswalker loyalty)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      (EntryRewrite.WithCounters CounterKind.Loyalty 3)
      "{\"type\":\"WithCounters\",\"value\":[{\"type\":\"Loyalty\"},3]}"
  -- CR 616.1b: Gather Specimens' control rewrite, payload-free for the reason
  -- Pawl.Types.EntryRewrite gives -- CR 109.5 derives the player.
  Spec.it s "UnderSourceControl (Gather Specimens)" $
    Common.assertJsonCodec s EntryRewrite.toJson EntryRewrite.fromJson EntryRewrite.UnderSourceControl "{\"type\":\"UnderSourceControl\"}"

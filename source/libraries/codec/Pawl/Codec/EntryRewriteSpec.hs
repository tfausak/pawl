{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EntryRewriteSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRewrite" $ do
  -- CR 707.5: Clone becomes a copy as it enters, with no producer-visible
  -- payload of its own.
  Spec.it s "AsCopy (Clone)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      EntryRewrite.AsCopy
      """ {"type":"AsCopy"} """
  -- CR 208.2b: two P/T-and-keyword choices, enough to show the keyword union
  -- isn't lost on the wire.
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
      """ {"type":"ChoiceOf","value":[{"power":3,"toughness":3},{"power":1,"toughness":6,"keywords":[{"type":"Defender"}]}]} """
  -- CR 614.1c / 105.1: an as-enters colour choice, payload-free because the
  -- five colours are always the whole offer.
  Spec.it s "ChooseColor (Painter's Servant)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      EntryRewrite.ChooseColor
      """ {"type":"ChooseColor"} """
  Spec.it s "ChooseBasicLandType (Convincing Mirage)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      EntryRewrite.ChooseBasicLandType
      """ {"type":"ChooseBasicLandType"} """
  -- CR 614.1c / 201.4a: an as-enters name choice, carrying the restriction on
  -- which cards' names may be chosen -- Null Chamber's "other than a basic land
  -- card name", which is a supertype and a card type together.
  Spec.it s "ChooseCardNames (Null Chamber)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      (EntryRewrite.ChooseCardNames (Filter.Not (Filter.And [Filter.HasSupertype Supertype.Basic, Filter.HasCardType CardType.Land])))
      """ {"type":"ChooseCardNames","value":{"type":"Not","value":{"type":"And","value":[{"type":"HasSupertype","value":{"type":"Basic"}},{"type":"HasCardType","value":{"type":"Land"}}]}}} """
  -- CR 614.1c / 306.5b: a planeswalker's intrinsic entry-with-counters rewrite.
  Spec.it s "WithCounters (planeswalker loyalty)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      (EntryRewrite.WithCounters CounterKind.Loyalty 3)
      """ {"type":"WithCounters","value":[{"type":"Loyalty"},3]} """
  -- CR 702.136a: riot's rewrite, payload-free because rule 702.136a fixes both
  -- halves. Minted from a keyword rather than written by a card, and round-tripped
  -- anyway, because every arm of this type is.
  Spec.it s "Riot (Zhur-Taa Goblin)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      EntryRewrite.Riot
      """ {"type":"Riot"} """
  -- CR 616.1b: a control rewrite, payload-free because CR 109.5 derives the
  -- player.
  Spec.it s "UnderSourceControl (Gather Specimens)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      EntryRewrite.UnderSourceControl
      """ {"type":"UnderSourceControl"} """
  -- CR 614.1c / 701.21a: an as-enters sacrifice of any number, carrying both the
  -- criterion the chosen permanents must match -- Shimatsu the Bloodcloaked's is
  -- "any number of permanents", the empty conjunction -- and the counter kind the
  -- count buys.
  Spec.it s "SacrificeAnyNumber (Shimatsu the Bloodcloaked)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      (EntryRewrite.SacrificeAnyNumber (Filter.And []) (Just CounterKind.PlusOnePlusOne))
      """ {"type":"SacrificeAnyNumber","value":[{"type":"And","value":[]},{"type":"PlusOnePlusOne"}]} """
  -- The same rewrite buying no counters: Wood Elemental's count is read back by
  -- a characteristic-defining ability instead (CR 208.2a), so the second element
  -- is null. Its criterion is the narrowing one -- an untapped Forest (CR 110.5).
  Spec.it s "SacrificeAnyNumber with no counters (Wood Elemental)" $
    Common.assertJsonCodec
      s
      EntryRewrite.toJson
      EntryRewrite.fromJson
      (EntryRewrite.SacrificeAnyNumber (Filter.And [Filter.HasSubtype Subtype.Forest, Filter.Not Filter.IsTapped]) Nothing)
      """ {"type":"SacrificeAnyNumber","value":[{"type":"And","value":[{"type":"HasSubtype","value":{"type":"Forest"}},{"type":"Not","value":{"type":"IsTapped"}}]},null]} """

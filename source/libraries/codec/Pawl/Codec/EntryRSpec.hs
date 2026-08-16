{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EntryRSpec where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.EntryR as EntryR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryR" $ do
  -- CR 110.5b, as a permanent enters tapped.
  Spec.it s "MkEntryR" $
    Common.assertCodec
      s
      (EntryR.codec (Effect.codec Card.codec))
      ( EntryR.MkEntryR
          { EntryR.matching = Filter.IsSource,
            EntryR.rewrite = EntryRewrite.Tapped
          }
      )
      """ {"matching":{"type":"IsSource"},"rewrite":{"type":"Tapped"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s (EntryR.codec (Effect.codec Card.codec))

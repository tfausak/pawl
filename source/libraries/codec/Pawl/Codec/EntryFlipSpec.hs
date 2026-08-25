module Pawl.Codec.EntryFlipSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.EntryFlip as EntryFlip
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.Keyword as Keyword

-- CR 614.1c with CR 705.2: the two options a winnerless coin flip picks between.
-- Molten Sentry's own pair, and DISTINCT on both axes -- a codec that read one
-- key for both faces round-trips a symmetric pair by accident.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryFlip" $ do
  Spec.it s "MkEntryFlip round-trips Molten Sentry's two faces" $
    Common.assertCodec
      s
      EntryFlip.codec
      ( EntryFlip.MkEntryFlip
          { EntryFlip.heads = EntryOption.MkEntryOption {EntryOption.power = 5, EntryOption.toughness = 2, EntryOption.keywords = Set.singleton Keyword.Haste},
            EntryFlip.tails = EntryOption.MkEntryOption {EntryOption.power = 2, EntryOption.toughness = 5, EntryOption.keywords = Set.singleton Keyword.Defender}
          }
      )
      " {\"heads\":{\"power\":5,\"toughness\":2,\"keywords\":[{\"type\":\"Haste\"}]},\"tails\":{\"power\":2,\"toughness\":5,\"keywords\":[{\"type\":\"Defender\"}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s EntryFlip.codec

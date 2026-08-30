{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MovedKinds where

import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MovedKinds as MovedKinds

-- | The tag that picks an arm is written by Pawl.Codec.MoveCounters' `kinds`
-- field, which defaults to a chosen kind and a count of one -- what "move a
-- counter" means -- so data/cards/agents-toolkit.json writes no key at all.
--
-- The named arm's payload is Pawl.Codec.EntryRiders' `counter` and not a second
-- codec for the same pair: Pawl.JsonSchema.Define keys a definition by the
-- type's Typeable name, so two codecs for one type quietly share one name and
-- the loser's wire spelling stops matching the schema the corpus is checked
-- against.
codec :: Codec.Codec MovedKinds.MovedKinds
codec =
  Arm.tagged
    [ Arm.nullary "Every" MovedKinds.Every,
      Arm.payload "Named" EntryRiders.counter (uncurry MovedKinds.Named) (\x -> case x of MovedKinds.Named kind quantity -> Just (kind, quantity); _ -> Nothing),
      Arm.payload "Chosen" Quantity.codec MovedKinds.Chosen (\x -> case x of MovedKinds.Chosen quantity -> Just quantity; _ -> Nothing),
      Arm.nullary "AnyNumber" MovedKinds.AnyNumber
    ]

{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Modal where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Mode as Mode
import qualified Pawl.Codec.ModeSelection as ModeSelection
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.ModeSelection as ModeSelection

-- | CR 700.2 via Pawl.Types.Modal's header: "A non-modal payload is one Mode
-- with ChooseExactly 1", so this is what a card that says nothing about modes
-- means.
defaultSelection :: ModeSelection.ModeSelection
defaultSelection = ModeSelection.ChooseExactly 1

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- The at-least-one-mode invariant is 'Fields.objectWith''s check rather than a
-- field's: it reads the assembled record, and it is a rule of Magic rather than
-- a property of the wire, so it has no schema representation.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (Modal.Modal card)
codec cardCodec = Fields.objectWith check $ do
  modes <- Fields.required "modes" (Common.seq (Mode.codec cardCodec)) Modal.modes
  selection <- Fields.defaulted "selection" defaultSelection ModeSelection.codec Modal.selection
  pure Modal.MkModal {Modal.modes = modes, Modal.selection = selection}
  where
    check m =
      if Seq.null (Modal.modes m)
        then Left (Text.pack "modal has no modes")
        else Right m

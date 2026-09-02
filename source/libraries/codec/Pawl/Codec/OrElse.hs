{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OrElse where

import qualified Pawl.Codec.ClauseIndex as ClauseIndex
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.OrElse as OrElse
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | The chooser is ELIDED when it is the unmarked "you" (CR 405.4), the way
-- Pawl.Codec.Optionality elides its asker, so a card whose either-or is
-- announced by the resolving controller writes only the sibling's ordinal.
codec :: Codec.Codec OrElse.OrElse
codec = Fields.object $ do
  sibling <- Fields.required "sibling" ClauseIndex.codec OrElse.sibling
  chooser <- Fields.defaulted "chooser" defaultChooser PlayerRef.codec OrElse.chooser
  pure OrElse.MkOrElse {OrElse.sibling = sibling, OrElse.chooser = chooser}

-- | CR 608.2d: who announces the branch unless the card names somebody else.
defaultChooser :: PlayerRef.Type.PlayerRef
defaultChooser = PlayerRef.Type.Relative PlayerRelation.You

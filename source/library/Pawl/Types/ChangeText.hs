module Pawl.Types.ChangeText where

import qualified Data.Set as Set
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

-- | The payload of Pawl.Types.Effect's ChangeText arm (#1305): CR 612's text
-- change over the slot's object, within one subtype family.
--
-- The forbidden set is what the chooser may NOT pick, which is why it is a set
-- of subtypes rather than a second family.
data ChangeText = MkChangeText
  { family :: SubtypeFamily.SubtypeFamily,
    forbidden :: Set.Set Subtype.Subtype,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)

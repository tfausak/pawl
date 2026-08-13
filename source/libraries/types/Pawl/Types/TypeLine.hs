module Pawl.Types.TypeLine where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

data TypeLine = MkTypeLine
  { supertypes :: Set.Set Supertype.Supertype,
    types :: Set.Set CardType.CardType,
    subtypes :: Set.Set Subtype.Subtype
  }
  deriving (Eq, Ord, Show)

-- | CR 114.3 \/ 114.5: no types at all -- what an emblem's face carries, since
-- "an emblem has no characteristics other than the abilities defined by the
-- effect that created it" and "Emblem isn't a card type". Malformed for a CARD,
-- which is what Pawl.Codec.TypeLine's CR 205.1 guard says; a face reaches this
-- value only by leaving the field out, and Pawl.CardSpec's lint holds that to
-- emblems.
empty :: TypeLine
empty =
  MkTypeLine
    { supertypes = Set.empty,
      types = Set.empty,
      subtypes = Set.empty
    }

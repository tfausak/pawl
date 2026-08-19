module Pawl.JsonSchema.Name where

import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

-- | The name a schema definition is filed under in @$defs@.
--
-- Names are derived from 'Typeable.TypeRep', never written by hand.
newtype Name = MkName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

typeName :: (Typeable.Typeable a) => proxy a -> Name
typeName = MkName . Text.pack . show . Typeable.typeRep

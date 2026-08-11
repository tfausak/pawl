-- | The name a schema definition is filed under in @$defs@.
--
-- Names are derived from 'Typeable.TypeRep', never written by hand: GHC has
-- auto-derived 'Typeable.Typeable' for every type since 7.10, so this costs the
-- type's own module nothing, a rename cannot leave a stale string behind, and a
-- parameterised type names each of its instantiations separately.
--
-- They are unqualified. Pawl declares one type per module, with the module
-- named for the type, so bare names are unique by construction and
-- qualification would only lengthen every reference. 'Typeable.tyConModule' is
-- there if that ever stops holding.
module Pawl.JsonSchema.Name where

import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

newtype Name = MkName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | Shown rather than rendered structurally: 'show' already parenthesizes a
-- nested type application, and the characters it writes that a URI fragment
-- cannot carry -- the space in @Face Card@ -- are percent-encoded where the
-- name becomes a @$ref@, in 'Pawl.JsonSchema.Define.fragment'. Substituting
-- them here instead would map two distinct types onto one name.
typeName :: (Typeable.Typeable a) => Typeable.Proxy a -> Name
typeName = MkName . Text.pack . show . Typeable.typeRep

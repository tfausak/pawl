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

typeName :: (Typeable.Typeable a) => Typeable.Proxy a -> Name
typeName = fromTypeRep . Typeable.typeRep

-- | Renders the representation structurally rather than showing it. 'show'
-- writes a type application with a space, and a space is not a character a URI
-- fragment can carry unencoded; an underscore is.
fromTypeRep :: Typeable.TypeRep -> Name
fromTypeRep rep =
  MkName
    . Text.intercalate (Text.pack "_")
    $ Text.pack (Typeable.tyConName (Typeable.typeRepTyCon rep))
      : fmap (unwrap . fromTypeRep) (Typeable.typeRepArgs rep)

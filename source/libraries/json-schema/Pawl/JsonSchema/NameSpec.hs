module Pawl.JsonSchema.NameSpec where

import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonSchema.Name" $ do
  let name :: (Typeable.Typeable a) => Typeable.Proxy a -> Text.Text
      name = Name.unwrap . Name.typeName

  Spec.it s "uses the bare constructor name" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy Bool)) (Text.pack "Bool")

  Spec.it s "writes an argument as an application" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy (Maybe Bool))) (Text.pack "Maybe Bool")

  Spec.it s "writes two arguments as one application" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy (Either Bool Char))) (Text.pack "Either Bool Char")

  -- Which is why the name is not built by joining arguments: flattening would
  -- give @Maybe (Maybe Bool)@ and @(Maybe Maybe) Bool@ the same name.
  Spec.it s "parenthesizes a nested argument" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy (Maybe (Maybe Bool)))) (Text.pack "Maybe (Maybe Bool)")

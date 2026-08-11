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

  Spec.it s "joins one argument with an underscore" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy (Maybe Bool))) (Text.pack "Maybe_Bool")

  Spec.it s "joins two arguments with underscores" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy (Either Bool Char))) (Text.pack "Either_Bool_Char")

  Spec.it s "recurses into nested arguments" $ do
    Spec.assertEq s (name (Typeable.Proxy :: Typeable.Proxy (Maybe (Maybe Bool)))) (Text.pack "Maybe_Maybe_Bool")

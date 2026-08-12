{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultilineStrings #-}

module Pawl.JsonCodec.FieldsSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Spec as Spec

data Example = MkExample
  { size :: Integer,
    label :: Maybe Integer
  }
  deriving (Eq, Show)

size' :: Codec.Codec Integer
size' = Common.integer

codec :: Codec.Codec Example
codec = Fields.object $ do
  s <- Fields.required "size" size' size
  l <- Fields.defaulted "label" Nothing (Common.maybe size') label
  pure MkExample {size = s, label = l}

-- | Rejects a zero 'size', proving 'objectWith' runs its check on the
-- assembled record.
nonZeroSize :: Example -> Either Text.Text Example
nonZeroSize e =
  if size e == 0
    then Left (Text.pack "size must not be zero")
    else Right e

codecWith :: Codec.Codec Example
codecWith = Fields.objectWith nonZeroSize $ do
  s <- Fields.required "size" size' size
  l <- Fields.defaulted "label" Nothing (Common.maybe size') label
  pure MkExample {size = s, label = l}

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonCodec.Fields" $ do
  Spec.it s "writes a required field" $
    Common.assertCodec s codec MkExample {size = 1, label = Nothing} """ {"size":1} """

  Spec.it s "writes a defaulted field only when it differs" $
    Common.assertCodec s codec MkExample {size = 1, label = Just 2} """ {"size":1,"label":2} """

  Spec.it s "reads an explicit null as the default" $
    Common.assertFromJson s (Codec.decode codec) """ {"size":1,"label":null} """ MkExample {size = 1, label = Nothing}

  Spec.it s "has a schema" $
    Common.assertHasSchema s codec

  Spec.describe s "objectWith" $ do
    Spec.it s "round trips a value the check accepts" $
      Common.assertCodec s codecWith MkExample {size = 1, label = Nothing} """ {"size":1} """

    Spec.it s "rejects a value the check rejects" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode codecWith =<< Common.parse (Text.pack """ {"size":0} """)))
        "expected a decode failure"

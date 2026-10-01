module Pawl.Codec.ViewIsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ViewIs as ViewIs
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reply as Reply.Type
import qualified Pawl.Types.View as View.Type
import qualified Pawl.Types.ViewIs as ViewIs.Type
import qualified Pawl.Types.Zone as Zone.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ViewIs" $ do
  Spec.it s "a view with no subject" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Stack Nothing Nothing Nothing (Reply.Type.Array [])) " {\"of\":\"Stack\",\"is\":[]} "
  Spec.it s "a player's zone" $
    Common.assertCodec s ViewIs.codec (ViewIs.Type.MkViewIs View.Type.Zone (Just (Label.Type.MkLabel (Text.pack "bob"))) Nothing (Just Zone.Type.Hand) (Reply.Type.Array [Reply.Type.Text (Text.pack "Hill Giant")])) " {\"of\":\"Zone\",\"player\":\"bob\",\"zone\":\"Hand\",\"is\":[\"Hill Giant\"]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ViewIs.codec

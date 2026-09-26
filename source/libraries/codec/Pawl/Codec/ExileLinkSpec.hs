module Pawl.Codec.ExileLinkSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ExileLink as ExileLink
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ExileLink as ExileLink
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExileLink" $ do
  Spec.it s "MkExileLink, an unnamed ability" $
    Common.assertCodec
      s
      ExileLink.codec
      (ExileLink.MkExileLink {ExileLink.source = ObjectId.MkObjectId 7, ExileLink.ability = Nothing})
      " {\"source\":7} "

  Spec.it s "MkExileLink, a named ability" $
    Common.assertCodec
      s
      ExileLink.codec
      (ExileLink.MkExileLink {ExileLink.source = ObjectId.MkObjectId 7, ExileLink.ability = Just (AbilityName.MkAbilityName (Text.pack "warden"))})
      " {\"source\":7,\"ability\":\"warden\"} "

  Spec.it s "has a schema" $
    Common.assertHasSchema s ExileLink.codec

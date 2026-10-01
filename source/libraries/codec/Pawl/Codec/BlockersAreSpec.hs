module Pawl.Codec.BlockersAreSpec where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.BlockersAre as BlockersAre
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BlockersAre as BlockersAre.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockersAre" $ do
  Spec.it s "blocked" $
    Common.assertCodec
      s
      BlockersAre.codec
      (BlockersAre.Type.MkBlockersAre (labelled "bear") (Just (Set.fromList [labelled "wall", labelled "guard"])))
      " {\"attacker\":\"$bear\",\"blockers\":[\"$guard\",\"$wall\"]} "
  Spec.it s "blocked by nothing" $
    Common.assertCodec s BlockersAre.codec (BlockersAre.Type.MkBlockersAre (labelled "bear") (Just Set.empty)) " {\"attacker\":\"$bear\",\"blockers\":[]} "
  Spec.it s "unblocked is null" $
    Common.assertCodec s BlockersAre.codec (BlockersAre.Type.MkBlockersAre (labelled "bear") Nothing) " {\"attacker\":\"$bear\",\"blockers\":null} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s BlockersAre.codec

labelled :: String -> Reference.Type.Reference
labelled = Reference.Type.Labelled . Label.Type.MkLabel . Text.pack

module Pawl.Codec.AttackersAreSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.AttackersAre as AttackersAre
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackersAre as AttackersAre.Type
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackersAre" $ do
  Spec.it s "keyed by attacker" $
    Common.assertCodec
      s
      AttackersAre.codec
      ( AttackersAre.Type.MkAttackersAre
          ( Map.fromList
              [ (labelled "bear", labelled "bob"),
                (Reference.Type.Printed (CardName.Type.MkCardName (Text.pack "Goblin Piker")) 2, Reference.Type.Printed (CardName.Type.MkCardName (Text.pack "Jace Beleren")) 1)
              ]
          )
      )
      " {\"attackers\":{\"$bear\":\"$bob\",\"Goblin Piker#2\":\"Jace Beleren\"}} "
  Spec.it s "nothing attacking" $
    Common.assertCodec s AttackersAre.codec (AttackersAre.Type.MkAttackersAre Map.empty) " {\"attackers\":{}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s AttackersAre.codec

labelled :: String -> Reference.Type.Reference
labelled = Reference.Type.Labelled . Label.Type.MkLabel . Text.pack

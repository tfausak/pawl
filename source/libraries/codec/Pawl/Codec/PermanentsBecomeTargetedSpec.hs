module Pawl.Codec.PermanentsBecomeTargetedSpec where

import qualified Pawl.Codec.PermanentsBecomeTargeted as PermanentsBecomeTargeted
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PermanentsBecomeTargeted as PermanentsBecomeTargeted
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.StackObjectKind as StackObjectKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PermanentsBecomeTargeted" $ do
  -- Professor Hojo's, both fields narrowed.
  Spec.it s "MkPermanentsBecomeTargeted, creatures you control and an activated ability" $
    Common.assertCodec
      s
      PermanentsBecomeTargeted.codec
      ( PermanentsBecomeTargeted.MkPermanentsBecomeTargeted
          { PermanentsBecomeTargeted.filter = Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You],
            PermanentsBecomeTargeted.kind = Just StackObjectKind.ActivatedAbility
          }
      )
      " {\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}]},\"kind\":{\"type\":\"ActivatedAbility\"}} "
  -- The kind elided, which is the wider "a spell or ability" reading.
  Spec.it s "MkPermanentsBecomeTargeted, no kind at all" $
    Common.assertCodec
      s
      PermanentsBecomeTargeted.codec
      ( PermanentsBecomeTargeted.MkPermanentsBecomeTargeted
          { PermanentsBecomeTargeted.filter = Filter.IsSource,
            PermanentsBecomeTargeted.kind = Nothing
          }
      )
      " {\"filter\":{\"type\":\"IsSource\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PermanentsBecomeTargeted.codec

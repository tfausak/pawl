module Pawl.Codec.ManaAbilityResolvedSpec where

import qualified Pawl.Codec.ManaAbilityResolved as ManaAbilityResolved
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaAbilityResolved as ManaAbilityResolved
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaAbilityResolved" $ do
  -- One Llanowar Elves' worth: a single mana off the permanent that made it.
  Spec.it s "MkManaAbilityResolved, one mana" $
    Common.assertCodec
      s
      ManaAbilityResolved.codec
      (ManaAbilityResolved.MkManaAbilityResolved {ManaAbilityResolved.permanent = ObjectId.MkObjectId 9, ManaAbilityResolved.amount = 1})
      " {\"amount\":1,\"permanent\":9} "
  -- Sol Ring's "{T}: Add {C}{C}", where CR 106.12a's type set would answer one
  -- and this amount answers two.
  Spec.it s "MkManaAbilityResolved, two mana" $
    Common.assertCodec
      s
      ManaAbilityResolved.codec
      (ManaAbilityResolved.MkManaAbilityResolved {ManaAbilityResolved.permanent = ObjectId.MkObjectId 4, ManaAbilityResolved.amount = 2})
      " {\"amount\":2,\"permanent\":4} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaAbilityResolved.codec

{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BecameDesignatedSpec where

import qualified Pawl.Codec.BecameDesignated as BecameDesignated
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BecameDesignated as BecameDesignated
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BecameDesignated" $ do
  -- CR 701's designations.
  Spec.it s "MkBecameDesignated, both keys" $
    Common.assertCodec
      s
      BecameDesignated.codec
      ( BecameDesignated.MkBecameDesignated
          { BecameDesignated.designation = Designation.Renowned,
            BecameDesignated.object = ObjectId.MkObjectId 1
          }
      )
      """ {"designation":{"type":"Renowned"},"object":1} """
  Spec.it s "has a schema" $ Common.assertHasSchema s BecameDesignated.codec

{-# LANGUAGE MultilineStrings #-}

module Pawl.JsonCodec.ArmSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec

data Example
  = Plain
  | Sized Integer
  | Loose (Maybe Integer)
  deriving (Eq, Show)

size :: Codec.Codec Integer
size = Common.integer

codec :: Codec.Codec Example
codec =
  Arm.tagged
    encode
    [ Arm.nullary "Plain" Plain,
      Arm.payload "Sized" size Sized,
      Arm.optionalPayload "Loose" size Loose
    ]
  where
    encode x = case x of
      Plain -> Common.nullary "Plain"
      Sized n -> Common.tagged "Sized" . Just $ Codec.encode size n
      Loose Nothing -> Common.nullary "Loose"
      Loose (Just n) -> Common.tagged "Loose" . Just $ Codec.encode size n

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonCodec.Arm" $ do
  Spec.it s "round trips a nullary arm" $
    Common.assertCodec s codec Plain """ {"type":"Plain"} """

  Spec.it s "round trips a payload arm" $
    Common.assertCodec s codec (Sized 2) """ {"type":"Sized","value":2} """

  Spec.it s "names the type in an unknown tag's error" $
    Spec.assertEq
      s
      (Common.parse (Text.pack """ {"type":"Nope"} """) >>= Codec.decode codec)
      (Left (Text.pack "unknown Example: Nope"))

  Spec.it s "rejects a payload arm with no value" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"Sized"} """) >>= Codec.decode codec))
      "expected a decode failure"

  -- The shape 'Arm.optionalPayload' exists for: BOTH forms decode under one
  -- tag, unlike 'payload', which rejects the value-absent one.
  Spec.it s "round trips an optional-payload arm with and without a value" $ do
    Common.assertCodec s codec (Loose (Just 2)) """ {"type":"Loose","value":2} """
    Common.assertCodec s codec (Loose Nothing) """ {"type":"Loose"} """

  Spec.it s "has a schema" $
    Common.assertHasSchema s codec

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
  deriving (Eq, Ord, Show)

-- | An all-nullary sum, for 'Arm.enum'. Separate from 'Example' because that
-- one carries payloads and so cannot be 'Enum'.
data Flat
  = First
  | Middle
  | Last
  deriving (Bounded, Enum, Eq, Ord, Show)

flatCodec :: Codec.Codec Flat
flatCodec = Arm.enum

size :: Codec.Codec Integer
size = Common.integer

codec :: Codec.Codec Example
codec =
  Arm.tagged
    [ Arm.nullary "Plain" Plain,
      Arm.payload "Sized" size Sized (\x -> case x of Sized n -> Just n; _ -> Nothing),
      Arm.optionalPayload "Loose" size Loose (\x -> case x of Loose mn -> Just mn; _ -> Nothing)
    ]

-- | An arm list that deliberately OMITS a constructor, to pin what 'Arm.tagged'
-- does when nothing matches.
partialCodec :: Codec.Codec Example
partialCodec = Arm.tagged [Arm.nullary "Plain" Plain]

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

  -- Every constructor, not a representative one: the whole point of 'Arm.enum'
  -- is that the arm list IS @[minBound ..]@, so the exhaustive assertion is the
  -- one that would catch a derivation covering only part of the type.
  Spec.it s "enum round trips every constructor, tagged by its name" $ do
    Common.assertCodec s flatCodec First """ {"type":"First"} """
    Common.assertCodec s flatCodec Middle """ {"type":"Middle"} """
    Common.assertCodec s flatCodec Last """ {"type":"Last"} """

  -- An unknown tag still fails rather than falling through to some arm, and the
  -- error names the type the same way a hand-written 'tagged' does.
  Spec.it s "enum rejects an unknown tag" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"Absent"} """) >>= Codec.decode flatCodec))
      "expected a decode failure"

  -- The cost of deriving the encoder: 'Arm.tagged' cannot see that its arm list
  -- misses a constructor. What it must NOT do is write something that decodes to
  -- another value, so the unmatched case is @{}@ -- the one object
  -- 'Common.asTagged' rejects -- and the round trip fails loudly instead.
  Spec.it s "an unmatched value encodes as a document that will not decode" $ do
    Spec.assertEq s (Common.render (Codec.encode partialCodec (Sized 1))) (Text.pack "{}")
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode partialCodec (Codec.encode partialCodec (Sized 1))))
      "expected the unmatched encoding to fail to decode"
    -- The arms it DOES name are unaffected.
    Common.assertCodec s partialCodec Plain """ {"type":"Plain"} """

  Spec.it s "enum has a schema" $ Common.assertHasSchema s flatCodec

  Spec.it s "has a schema" $
    Common.assertHasSchema s codec

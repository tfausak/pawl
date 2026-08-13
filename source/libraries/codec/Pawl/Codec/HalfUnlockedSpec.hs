{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.HalfUnlockedSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.HalfUnlocked as HalfUnlocked
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.HalfUnlocked as HalfUnlocked
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.HalfUnlocked" $ do
  -- CR 709.5e, with CR 709.5i's completion flag.
  Spec.it s "MkHalfUnlocked, a door that completes the card" $
    Common.assertCodec
      s
      HalfUnlocked.codec
      ( HalfUnlocked.MkHalfUnlocked
          { HalfUnlocked.object = ObjectId.MkObjectId 1,
            HalfUnlocked.name = CardName.MkCardName (Text.pack "Trapped Entryway"),
            HalfUnlocked.fully = True
          }
      )
      """ {"object":1,"name":"Trapped Entryway","fully":true} """
  -- The flag defaults to nothing: it is REQUIRED, since False is a real answer
  -- (a door that opened but left the other locked) rather than an absence.
  Spec.it s "MkHalfUnlocked, a door that does not" $
    Common.assertCodec
      s
      HalfUnlocked.codec
      ( HalfUnlocked.MkHalfUnlocked
          { HalfUnlocked.object = ObjectId.MkObjectId 1,
            HalfUnlocked.name = CardName.MkCardName (Text.pack "Trapped Entryway"),
            HalfUnlocked.fully = False
          }
      )
      """ {"object":1,"name":"Trapped Entryway","fully":false} """
  Spec.it s "has a schema" $ Common.assertHasSchema s HalfUnlocked.codec

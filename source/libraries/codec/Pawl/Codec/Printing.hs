module Pawl.Codec.Printing where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Printing as Printing

-- | The whole record, which is what a card FILE holds and what
-- Pawl.CodecIntegrationSpec's honesty round trip over the corpus reads.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- a @$defs@ entry under this newtype's own name.
codec :: Codec.Codec Printing.Printing
codec = Common.wrapper Card.codec Printing.MkPrinting Printing.card

-- | A printing in a game state's intern table: its NAME where a resolver can
-- reproduce it, and the whole record where none can.
--
-- The resolver is the portability knob and the decoder's only source of cards:
-- one answering 'Nothing' for everything inlines the table, giving a
-- self-contained state, with no second code path. @pawl:registry@ sits ABOVE
-- @pawl:codec@, so a decoder cannot call a registry and the caller hands the
-- lookup in instead.
--
-- @Named@ FAILS to decode when the resolver does not know the name, rather than
-- guessing: a state is only as portable as the registry it was written against,
-- and this is what makes the format say so.
--
-- @Inline@ carries every printing the resolver does not reproduce EXACTLY, which
-- covers both the printings no registry could hold and a name it answers
-- differently for. Two producers make the arm necessary rather than defensive:
-- Pawl.Engine.Ring.theRingEmblem builds The Ring's emblem as a function of the
-- temptation count, and Pawl.Engine.Event.resolveTokens hands back a card that
-- has been through applyReplacements. Neither is in @data\/cards@.
--
-- The equality check is what makes @Named@ safe: a resolver answering with a
-- DIFFERENT card under this name would decode to that other card, so the name is
-- not written. Pawl.Codec.PrintingSpec's "a name the resolver answers
-- differently for is written out in full" is that case.
reference :: (CardName.Type.CardName -> Maybe Card.Type.Card) -> Codec.Codec Printing.Printing
reference resolve = Arm.anonymous [named, inline]
  where
    -- FIRST, so that Arm.tagged's take-the-first-matching-arm encoder prefers a
    -- name; `inline` matches everything.
    named =
      Arm.MkArm
        { Arm.tag = "Named",
          Arm.decodeValue = \mv -> do
            name <- Common.withValue mv (Codec.decode CardName.codec)
            case resolve name of
              Nothing -> Left (Text.pack ("no such card: " <> Text.unpack (CardName.Type.unwrap name)))
              Just card -> Right (Printing.MkPrinting card),
          Arm.valueSchema = Arm.RequiredValue (Codec.schema CardName.codec),
          Arm.projectValue = \printing ->
            let name = firstFaceName (Printing.card printing)
             in if resolve name == Just (Printing.card printing)
                  then Just (Just (Codec.encode CardName.codec name))
                  else Nothing
        }
    inline = Arm.payload "Inline" codec id Just

-- | The name a printing is written under. The FIRST face's, not the joined
-- name: CR 709.4a gives a split card two names and no combined one, and
-- Pawl.Registry.index keys each face name separately, so the first face's name
-- is a key every registry has while the joined one -- Pawl.Registry.filedAs, a
-- filing convention -- is a key none of them has.
firstFaceName :: Card.Type.Card -> CardName.Type.CardName
firstFaceName = Face.name . NonEmpty.head . Card.Type.faces

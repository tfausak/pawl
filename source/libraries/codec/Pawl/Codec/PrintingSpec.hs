module Pawl.Codec.PrintingSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Pawl.Codec.CardSpec as CardSpec
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Layout as Layout
import qualified Pawl.Types.Printing as Printing

-- | The whole record, spelled once: 'Printing.codec' writes it, and so does
-- 'Printing.reference'\'s Inline arm.
mountainJson :: String
mountainJson = "{\"faces\":[{\"name\":\"Mountain\",\"typeLine\":{\"supertypes\":[{\"type\":\"Basic\"}],\"types\":[{\"type\":\"Land\"}],\"subtypes\":[{\"type\":\"Mountain\"}]}}]}"

mountainName :: CardName.CardName
mountainName = CardName.MkCardName (Text.pack "Mountain")

-- | A resolver that knows the one card, which is what makes 'Printing.reference'
-- write a name rather than the record.
knowsMountain :: CardName.CardName -> Maybe Card.Card
knowsMountain name
  | name == mountainName = Just CardSpec.mountainCard
  | otherwise = Nothing

-- | The same NAME, a different card: no type line at all, where
-- 'CardSpec.mountainCard' is a basic land. A resolver answering with this is
-- the case the encoder's equality check exists for.
impostor :: Card.Card
impostor = Card.MkCard Layout.Normal (NonEmpty.singleton (CardSpec.bareFace mountainName))

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Printing" $ do
  -- MkPrinting just delegates to Card's own codec, so 'CardSpec.mountainCard' is
  -- reused rather than a second synthetic Card being built here. The
  -- registry-backed round-trip over every real Printing stays in
  -- Pawl.CodecIntegrationSpec, which this sublibrary sits above.
  Spec.it s "MkPrinting delegates to Card's own codec" $
    Common.assertCodec
      s
      Printing.codec
      (Printing.MkPrinting CardSpec.mountainCard)
      (" " <> mountainJson <> " ")
  -- CR 709.4a: the FIRST face's name, which is a key Pawl.Registry.index has,
  -- rather than the joined name, which is a filing convention and a key it does
  -- not have. A single-faced card cannot tell those two apart, so what this case
  -- pins is the tag and the shape; Pawl.CodecIntegrationSpec drives the corpus.
  Spec.it s "a printing the resolver knows is written as its name" $
    Common.assertCodec
      s
      (Printing.reference knowsMountain)
      (Printing.MkPrinting CardSpec.mountainCard)
      " {\"type\":\"Named\",\"value\":\"Mountain\"} "
  -- The same printing against a resolver that answers for nothing: the whole
  -- record, under the Inline tag. This is the portability knob -- a state
  -- encoded through such a resolver depends on no registry at all.
  Spec.it s "a printing no resolver can reproduce is written out in full" $
    Common.assertCodec
      s
      (Printing.reference (const Nothing))
      (Printing.MkPrinting CardSpec.mountainCard)
      (" {\"type\":\"Inline\",\"value\":" <> mountainJson <> "} ")
  -- The pair to the case above, differing in exactly one thing: the resolver
  -- ANSWERS, and with the wrong card. Writing the name would decode to the
  -- impostor, so the encoder compares what came back and inlines instead.
  Spec.it s "a name the resolver answers differently for is written out in full" $
    Common.assertCodec
      s
      (Printing.reference (const (Just impostor)))
      (Printing.MkPrinting CardSpec.mountainCard)
      (" {\"type\":\"Inline\",\"value\":" <> mountainJson <> "} ")
  -- Not a round trip: a Named printing the resolver cannot answer for has no
  -- card to decode to, and guessing is the one thing this format exists to
  -- prevent.
  Spec.it s "a Named printing fails to decode when the resolver cannot answer" $
    Spec.assertBool
      s
      (either (const True) (const False) (Codec.decode (Printing.reference (const Nothing)) (Common.tagged "Named" (Just (Value.text (Text.pack "Mountain"))))))
      "an unknown name must fail rather than decode to something else"
  -- And the resolver that DOES answer decodes to what it answered, so the case
  -- above fails for the missing name rather than for the arm being unreachable.
  Spec.it s "a Named printing decodes to whatever the resolver answers" $
    Spec.assertEq
      s
      (Codec.decode (Printing.reference knowsMountain) (Common.tagged "Named" (Just (Value.text (Text.pack "Mountain")))))
      (Right (Printing.MkPrinting CardSpec.mountainCard))

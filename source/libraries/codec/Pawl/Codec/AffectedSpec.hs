{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.AffectedSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Affected" $ do
  Spec.it s "TheseObjects" $
    Common.assertCodec
      s
      Affected.codec
      (Affected.TheseObjects (Set.fromList [ObjectId.MkObjectId 1, ObjectId.MkObjectId 2]))
      """ {"type":"TheseObjects","value":[1,2]} """
  Spec.it s "Matching" $
    Common.assertCodec
      s
      Affected.codec
      (Affected.Matching (Filter.HasCardType CardType.Creature))
      """ {"type":"Matching","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "MatchingAnywhere" $
    Common.assertCodec
      s
      Affected.codec
      (Affected.MatchingAnywhere (Filter.HasCardType CardType.Creature))
      """ {"type":"MatchingAnywhere","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  -- An "each other" card text as Not IsSource nested inside Matching -- the
  -- composed form the bare atom cases above do not exercise.
  Spec.it s "Matching, Opalescence's \"each other\" shape" $
    Common.assertCodec
      s
      Affected.codec
      ( Affected.Matching
          ( Filter.And
              [ Filter.HasCardType CardType.Enchantment,
                Filter.Not (Filter.HasSubtype Subtype.Mountain),
                Filter.Not Filter.IsSource
              ]
          )
      )
      """ {"type":"Matching","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Enchantment"}},{"type":"Not","value":{"type":"HasSubtype","value":{"type":"Mountain"}}},{"type":"Not","value":{"type":"IsSource"}}]}} """
  Spec.it s "Attached" $
    Common.assertCodec
      s
      Affected.codec
      Affected.Attached
      """ {"type":"Attached"} """
  -- CR 303.4m through a player.
  Spec.it s "AttachedPlayerControls" $
    Common.assertCodec
      s
      Affected.codec
      (Affected.AttachedPlayerControls (Filter.HasCardType CardType.Creature))
      """ {"type":"AttachedPlayerControls","value":{"type":"HasCardType","value":{"type":"Creature"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Affected.codec

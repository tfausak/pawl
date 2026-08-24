module Pawl.Codec.ManaAdditionSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ManaAddition as ManaAddition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaRestriction as ManaRestriction
import qualified Pawl.Types.ManaRetention as ManaRetention
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaAddition" $ do
  -- CR 109.5's unwritten "you": Llanowar Elves' "Add {G}" names nobody, so the
  -- key is absent from the wire in both directions.
  Spec.it s "MkManaAddition, the unwritten recipient" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.Relative PlayerRelation.You,
            ManaAddition.production = ManaProduction.OfType (ManaType.Colored Color.Green),
            ManaAddition.retention = ManaRetention.Ordinary,
            ManaAddition.restriction = Nothing
          }
      )
      " {\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}} "
  -- Shizuko, Caller of Autumn's "that player adds": the one printing that writes
  -- the key, over the slot CR 603.2b's step event bound.
  Spec.it s "MkManaAddition, a named recipient" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")),
            ManaAddition.production = ManaProduction.OfType (ManaType.Colored Color.Green),
            ManaAddition.retention = ManaRetention.Ordinary,
            ManaAddition.restriction = Nothing
          }
      )
      " {\"player\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}}} "
  -- The pair the defaulted key needs: an absent @retention@ IS Ordinary (both
  -- cases above), and Shizuko's third sentence is the one printing that writes
  -- it. Nothing forces this case -- the codec's arm list is not exhaustiveness
  -- checked (#1715) -- so deleting the field from Pawl.Codec.ManaAddition shows
  -- up here or nowhere.
  Spec.it s "MkManaAddition, a retention written on the wire" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")),
            ManaAddition.production = ManaProduction.OfType (ManaType.Colored Color.Green),
            ManaAddition.retention = ManaRetention.UntilEndOfTurn,
            ManaAddition.restriction = Nothing
          }
      )
      " {\"player\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Green\"}}},\"retention\":{\"type\":\"UntilEndOfTurn\"}} "
  -- The same pair for CR 106.6's spending restriction, and it needs its own case
  -- for the reason the retention one gives: nothing forces the field (#1715).
  -- Geosurge writes it on a spell and Mishra's Workshop on a mana ability.
  Spec.it s "MkManaAddition, a spending restriction written on the wire" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.Relative PlayerRelation.You,
            ManaAddition.production = ManaProduction.OfType (ManaType.Colored Color.Red),
            ManaAddition.retention = ManaRetention.Ordinary,
            ManaAddition.restriction = Just (ManaRestriction.onlyCasts (Filter.Or [Filter.HasCardType CardType.Artifact, Filter.HasCardType CardType.Creature]))
          }
      )
      " {\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}}},\"restriction\":{\"casts\":{\"type\":\"Or\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}},{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}]}}} "
  -- CR 106.6's other subject, and the pair that keeps the two halves of
  -- Pawl.Types.ManaRestriction from collapsing on the wire: Omen Hawker's "spend
  -- this mana only to activate abilities" writes the OTHER key, with the trivial
  -- predicate, since the card says nothing about which abilities.
  Spec.it s "MkManaAddition, a restriction whose subject is an activation" $
    Common.assertCodec
      s
      ManaAddition.codec
      ( ManaAddition.MkManaAddition
          { ManaAddition.player = PlayerRef.Relative PlayerRelation.You,
            ManaAddition.production = ManaProduction.OfType ManaType.Colorless,
            ManaAddition.retention = ManaRetention.Ordinary,
            ManaAddition.restriction = Just (ManaRestriction.MkManaRestriction {ManaRestriction.casts = Nothing, ManaRestriction.activations = Just (Filter.And [])})
          }
      )
      " {\"production\":{\"type\":\"OfType\",\"value\":{\"type\":\"Colorless\"}},\"restriction\":{\"activations\":{\"type\":\"And\",\"value\":[]}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaAddition.codec

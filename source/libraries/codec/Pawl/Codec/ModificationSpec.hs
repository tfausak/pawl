{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ModificationSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Modification" $ do
  -- layer 6 (Serpent's Gift).
  Spec.it s "GainKeyword" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.GainKeyword Keyword.Deathtouch)
      """ {"type":"GainKeyword","value":{"type":"Deathtouch"}} """
  -- layer 6 (Humility).
  Spec.it s "LoseAllAbilities" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      Modification.LoseAllAbilities
      """ {"type":"LoseAllAbilities"} """
  -- layer 7b (Humility 1/1; Opalescence mana value).
  Spec.it s "SetBasePowerToughness" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.SetBasePowerToughness (Quantity.Literal 1) (Quantity.Literal 1))
      """ {"type":"SetBasePowerToughness","value":[{"type":"Literal","value":1},{"type":"Literal","value":1}]} """
  -- layer 7c (Giant Growth +3/+3).
  Spec.it s "ModifyPowerToughness" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.ModifyPowerToughness (Quantity.Literal 3) (Quantity.Literal 3))
      """ {"type":"ModifyPowerToughness","value":[{"type":"Literal","value":3},{"type":"Literal","value":3}]} """
  -- layer 4, CR 305.7 set (Blood Moon -> Mountain).
  Spec.it s "SetLandSubtype" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.SetLandSubtype Subtype.Mountain)
      """ {"type":"SetLandSubtype","value":{"type":"Mountain"}} """
  -- Nullary: the subtype is read from the effect's source at projection time,
  -- so there is nothing on the wire.
  Spec.it s "SetLandSubtypeToChosen" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      Modification.SetLandSubtypeToChosen
      """ {"type":"SetLandSubtypeToChosen"} """
  -- layer 4, CR 305.7 add (Urborg -> Swamp).
  Spec.it s "AddLandSubtype" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.AddLandSubtype Subtype.Swamp)
      """ {"type":"AddLandSubtype","value":{"type":"Swamp"}} """
  -- layer 4, CR 205.1a/205.1b set (Turn to Frog -> Frog).
  Spec.it s "SetCreatureSubtype" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.SetCreatureSubtype Subtype.Frog)
      """ {"type":"SetCreatureSubtype","value":{"type":"Frog"}} """
  -- layer 4, CR 205.1b add (Life and Limb -> Saproling).
  Spec.it s "AddCreatureSubtype" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.AddCreatureSubtype Subtype.Saproling)
      """ {"type":"AddCreatureSubtype","value":{"type":"Saproling"}} """
  -- layer 4 (Opalescence -> Creature).
  Spec.it s "AddCardType" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.AddCardType CardType.Creature)
      """ {"type":"AddCardType","value":{"type":"Creature"}} """
  -- layer 4, CR 205.4b grant (Leyline of Singularity -> legendary).
  Spec.it s "AddSupertype" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.AddSupertype Supertype.Legendary)
      """ {"type":"AddSupertype","value":{"type":"Legendary"}} """
  -- layer 4, CR 205.4b removal (Thermal Flux -> isn't snow).
  Spec.it s "RemoveSupertype" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.RemoveSupertype Supertype.Snow)
      """ {"type":"RemoveSupertype","value":{"type":"Snow"}} """
  -- layer 3, CR 612 (Magical Hack: from -> to).
  Spec.it s "ChangeSubtypeWord" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.ChangeSubtypeWord Subtype.Mountain Subtype.Island)
      """ {"type":"ChangeSubtypeWord","value":[{"type":"Mountain"},{"type":"Island"}]} """
  -- layer 2, CR 613.1b: the PlayerId is BAKED at effect creation, unlike
  -- SetControllerToSource below.
  Spec.it s "SetController carries its baked PlayerId" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.SetController (PlayerId.MkPlayerId 0))
      """ {"type":"SetController","value":0} """
  -- layer 2, CR 613.1b: the only shape a printed static ability can grant
  -- control with, since the controller is DERIVED at projection time rather
  -- than baked. Must not decode as SetController.
  Spec.it s "SetControllerToSource" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      Modification.SetControllerToSource
      """ {"type":"SetControllerToSource"} """
  -- layer 5, CR 613.1e / 105.3: a SET, not an add.
  Spec.it s "SetColor carries its colour set" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.SetColor (Set.singleton Color.Blue))
      """ {"type":"SetColor","value":[{"type":"Blue"}]} """
  -- layer 5, CR 613.1e / 105.3: an ADD, not a set.
  Spec.it s "AddColor" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      (Modification.AddColor (Set.singleton Color.Blue))
      """ {"type":"AddColor","value":[{"type":"Blue"}]} """
  -- layer 5, CR 613.1e / 105.3: an "in addition" colour too, but payload-free
  -- -- the colour is read at projection time off the effect's source.
  Spec.it s "AddChosenColor" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      Modification.AddChosenColor
      """ {"type":"AddChosenColor"} """
  -- layer 7d, CR 613.4d: switches power and toughness. Payload-free.
  Spec.it s "SwitchPowerToughness" $
    Common.assertJsonCodec
      s
      Modification.toJson
      Modification.fromJson
      Modification.SwitchPowerToughness
      """ {"type":"SwitchPowerToughness"} """

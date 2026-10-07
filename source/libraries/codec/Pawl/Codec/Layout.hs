module Pawl.Codec.Layout where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Layout as Layout

codec :: Codec.Codec Layout.Layout
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "Normal" Layout.Normal,
      Arm.nullary "Split" Layout.Split,
      Arm.nullary "Room" Layout.Room,
      Arm.nullary "Flip" Layout.Flip,
      Arm.nullary "Adventure" Layout.Adventure,
      Arm.nullary "Omen" Layout.Omen,
      Arm.nullary "Preparation" Layout.Preparation,
      Arm.nullary "Transforming" Layout.Transforming,
      Arm.nullary "ModalDoubleFaced" Layout.ModalDoubleFaced,
      Arm.payload "Meld" CardName.codec Layout.Meld (\x -> case x of Layout.Meld y -> Just y; _ -> Nothing)
    ]

tagOf :: Layout.Layout -> String
tagOf x = case x of
  Layout.Normal -> "Normal"
  Layout.Split -> "Split"
  Layout.Room -> "Room"
  Layout.Flip -> "Flip"
  Layout.Adventure -> "Adventure"
  Layout.Omen -> "Omen"
  Layout.Preparation -> "Preparation"
  Layout.Transforming -> "Transforming"
  Layout.ModalDoubleFaced -> "ModalDoubleFaced"
  Layout.Meld {} -> "Meld"

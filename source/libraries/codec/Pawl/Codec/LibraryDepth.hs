module Pawl.Codec.LibraryDepth where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.LibraryDepth as LibraryDepth

codec :: Codec.Codec LibraryDepth.LibraryDepth
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "FromTop" Common.natural LibraryDepth.FromTop (\x -> case x of LibraryDepth.FromTop y -> Just y; _ -> Nothing),
      Arm.payload "AtRandomInTop" Common.natural LibraryDepth.AtRandomInTop (\x -> case x of LibraryDepth.AtRandomInTop y -> Just y; _ -> Nothing)
    ]

tagOf :: LibraryDepth.LibraryDepth -> String
tagOf x = case x of
  LibraryDepth.FromTop {} -> "FromTop"
  LibraryDepth.AtRandomInTop {} -> "AtRandomInTop"

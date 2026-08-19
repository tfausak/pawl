module Pawl.Extra.Word8 where

import qualified Data.Word as Word

-- | Converts a 'Word.Word8' into an 'Int'. This is total: every 'Word.Word8'
-- fits an 'Int'.
toInt :: Word.Word8 -> Int
toInt = fromEnum

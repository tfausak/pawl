module Pawl.Codec.KeywordCount where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.KeywordTally as KeywordTally
import qualified Pawl.Codec.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.KeywordCount as KeywordCount

-- | Tagged rather than a bare number, Pawl.Codec.DevourCount's reason: a
-- restated N carries no number at all. The keyword codec is a PARAMETER; see
-- Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (KeywordCount.KeywordCount keyword)
codec keywordCodec =
  Arm.tagged
    tagOf
    [ Arm.payload "Fixed" Common.natural KeywordCount.Fixed (\x -> case x of KeywordCount.Fixed y -> Just y; _ -> Nothing),
      Arm.nullary "Power" KeywordCount.Power,
      Arm.payload "Tally" (KeywordTally.codec keywordCodec) KeywordCount.Tally (\x -> case x of KeywordCount.Tally y -> Just y; _ -> Nothing),
      Arm.payload "PlayerCounters" PlayerCounterTally.codec KeywordCount.PlayerCounters (\x -> case x of KeywordCount.PlayerCounters y -> Just y; _ -> Nothing)
    ]

tagOf :: KeywordCount.KeywordCount keyword -> String
tagOf x = case x of
  KeywordCount.Fixed {} -> "Fixed"
  KeywordCount.Power {} -> "Power"
  KeywordCount.Tally {} -> "Tally"
  KeywordCount.PlayerCounters {} -> "PlayerCounters"

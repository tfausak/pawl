{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Cost where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Cost as Cost

-- | The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header. The 'Eq'
-- constraint is only for 'Fields.defaulted' on 'components', the same reason
-- the pre-'Codec' version needed it for 'Common.optionalPair'.
--
-- CR 118.6: 'mana' is REQUIRED, not defaulted, despite being a 'Maybe' --
-- Nothing and Just (MkManaCost []) are both real, distinct values, so there is
-- no single default an absent key could mean. A card file that forgets 'mana'
-- is malformed rather than unpayable-by-default.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Cost.Cost keyword)
codec keywordCodec = Fields.object $ do
  mana <- Fields.required "mana" (Common.maybe ManaCost.codec) Cost.mana
  components <- Fields.defaulted "components" [] (Common.list (CostComponent.codec keywordCodec)) Cost.components
  pure Cost.MkCost {Cost.mana = mana, Cost.components = components}

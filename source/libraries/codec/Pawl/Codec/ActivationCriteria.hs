{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivationCriteria where

import qualified Pawl.Codec.AbilityKind as AbilityKind
import qualified Pawl.Codec.KeywordDesignator as KeywordDesignator
import qualified Pawl.Codec.LoyaltyKind as LoyaltyKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivationCriteria as ActivationCriteria

-- | A bare object keyed by the record's field names, every one DEFAULTED to
-- Nothing -- every activated ability of a matching source, which is what most
-- printings say -- so a card file writes only the criteria it prints.
codec :: Codec.Codec ActivationCriteria.ActivationCriteria
codec = Fields.object $ do
  grantedBy <- Fields.defaulted "grantedBy" Nothing (Common.maybe KeywordDesignator.codec) ActivationCriteria.grantedBy
  whichKind <- Fields.defaulted "whichKind" Nothing (Common.maybe AbilityKind.codec) ActivationCriteria.whichKind
  whichLoyalty <- Fields.defaulted "whichLoyalty" Nothing (Common.maybe LoyaltyKind.codec) ActivationCriteria.whichLoyalty
  pure ActivationCriteria.MkActivationCriteria {ActivationCriteria.grantedBy = grantedBy, ActivationCriteria.whichKind = whichKind, ActivationCriteria.whichLoyalty = whichLoyalty}

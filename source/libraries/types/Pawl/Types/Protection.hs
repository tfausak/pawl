module Pawl.Types.Protection where

import qualified Pawl.Types.Filter as Filter

-- | The payload of Pawl.Types.Keyword's Protection arm: CR 702.16a's quality,
-- plus CR 702.16n's exception as one field on it.
--
-- PARAMETRIC in the keyword, Pawl.Types.Cycling's shape and for its reason: the
-- fields name Filters, a Filter can name a Keyword, and Keyword names THIS.
-- Only @Protection Keyword.Keyword@ is ever written.
data Protection keyword = MkProtection
  { -- | CR 702.16a's "[quality]", which every protection ability states.
    quality :: Filter.Filter keyword,
    -- | CR 702.16n's "this effect doesn't remove [those]": the attached
    -- permanents this instance leaves standing, Nothing where the ability says
    -- no such thing. Spectra Ward's "Auras" is the only printing of rule
    -- 702.16n's all-Auras form -- Scryfall o:"this effect doesn't remove",
    -- 2026-09-08, fifteen cards, of which the rest state rule 702.16n's "this
    -- Aura" or rule 702.16p. White Ward's "this Aura" is a bare Filter.IsSource,
    -- which Pawl.Engine.Keyword.grantedBy bakes into the granter's id.
    --
    -- On the KEYWORD rather than on the row Pawl.Engine.Keyword mints from it,
    -- because rule 702.16n's last sentence has another instance of protection
    -- from the same quality remove those permanents as normal -- which needs the
    -- two instances to be distinct keys in a permanent's keyword map.
    --
    -- Read by the STANDING halves of CR 702.16c and CR 702.16d alone
    -- (Pawl.Engine.AttachRestriction.removesGiven) and not by the
    -- becoming-attached one, since rule 702.16n says only that the specified
    -- permanents are not put into their owners' graveyards, where rule 702.16p
    -- states a becoming-attached sentence of its own.
    --
    -- Not implemented: rule 702.16p, whose "already attached to" is a moment
    -- rather than a state (#3046).
    spares :: Maybe (Filter.Filter keyword)
  }
  deriving (Eq, Ord, Show)

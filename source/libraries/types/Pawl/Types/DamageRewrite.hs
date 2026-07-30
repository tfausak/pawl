module Pawl.Types.DamageRewrite where

-- CR 615.1: how a prevention rewrites a damage event. PreventAll cancels it
-- outright (Fog) -- CR 615.6, a prevented event never happens. CR 615.7's shared
-- N-damage shield and the prevent-the-next-N shape are card-driven, not
-- structure-blocked (#58).
data DamageRewrite = PreventAll
  deriving (Eq, Ord, Show)

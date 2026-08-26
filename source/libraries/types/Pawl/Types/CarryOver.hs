module Pawl.Types.CarryOver where

-- | CR 400.7a: whether the object arriving from this zone change is the
-- permanent a permanent SPELL became, and so keeps the effects that changed the
-- spell's characteristics or controller. CR 400.7's default is the opposite --
-- a moved object is a new object with no memory of the old one -- so this says
-- which of the two a move is.
--
-- A named sum rather than a Bool, for Pawl.Types.CoinFace's reason and one more:
-- Pawl.Engine.Event.changeZoneAttaching already takes a Bool for CR 406.3's
-- "exiled face down", and two adjacent Bools are two arguments the type checker
-- cannot tell apart at a call site.
--
-- Carried is Pawl.Engine.Stack's two permanent-spell branches and nothing else.
-- CR 400.7b (static-ability ability grants) and CR 400.7c (prevention effects)
-- are separate exceptions with separate carriers and are not this (#634).
data CarryOver
  = Carried
  | NotCarried
  deriving (Eq, Ord, Show)

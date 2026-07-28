module Pawl.Type.ZoneChangeSubject where

-- CR 614.1a: WHICH object a zone-change redirect is about, as distinct from
-- Pawl.Type.ZoneChangePattern's `whoseObject`, which asks whose it is.
--
-- Rest in Peace and Leyline of the Void are AnyObject -- "if a card ... would be
-- put into a graveyard from anywhere" names no particular object. CR 702.34a's
-- second static ability is TheSource -- "exile THIS CARD instead of putting it
-- anywhere else any time it would leave the stack" -- the same self-scoping that
-- EntryR (CR 614.1c) and DestructionR (CR 201.5) carry implicitly by having no
-- pattern at all.
--
-- "The source" is the replacement effect's own source object, which for the
-- flashback ability is the spell on the stack. That makes the distinction load
-- bearing rather than cosmetic: without it a flashback spell on the stack would
-- redirect every OTHER card its controller owns that headed for a graveyard
-- while it sat there.
data ZoneChangeSubject
  = AnyObject
  | TheSource
  deriving (Eq, Ord, Show)

module Pawl.Types.Source where

import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.InherentTriggerSource as InherentTriggerSource
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource

-- | What is behind an object. The three card-shaped constructors name their
-- printing by id rather than carrying it (#1592), so the rules distinction
-- between them is the only distinction left: a card, a token and an emblem are
-- three different things under CR 108, CR 111 and CR 114, and the engine cases
-- on which of the three it has.
data Source
  = -- | CR 108: a card, named by its entry in GameState.printings.
    OfCard PrintingId.PrintingId
  | -- | CR 111.3/111.6: a token -- a permanent not represented by a card. Its
    -- characteristics ARE a Card (CR 111.3: effect-defined values are functionally
    -- equivalent to printed ones), interned like any other printing, and carrying
    -- no print-level data because a token is not a card (CR 111.6).
    OfToken PrintingId.PrintingId
  | -- | CR 602: an activated ability on the stack, which
    -- Pawl.Types.ActivatedAbilitySource carries and documents.
    OfAbility ActivatedAbilitySource.ActivatedAbilitySource
  | -- | CR 603.3: a triggered ability on the stack, which
    -- Pawl.Types.TriggeredAbilitySource carries and documents.
    OfTrigger TriggeredAbilitySource.TriggeredAbilitySource
  | -- | CR 114: an emblem -- an object in the command zone whose only
    -- characteristics are its abilities (CR 114.3). Its characteristics ARE a
    -- Card and are interned like a token's; unlike a token it is never a permanent
    -- (CR 114.5) and never on the battlefield. Owned and controlled by the player
    -- who created it (CR 114.2 / 109.4c).
    OfEmblem PrintingId.PrintingId
  | -- | CR 725.2 / CR 702.179d: a triggered ability with no object source, which
    -- Pawl.Types.InherentTriggerSource carries and documents.
    OfInherentTrigger InherentTriggerSource.InherentTriggerSource
  deriving (Eq, Ord, Show)

module Pawl.Types.PendingDamageEffect where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

-- | CR 614.1a: the effects a DamageRewrite.RunEffects applied and has not run
-- yet -- Kill-Suit Cultist's "destroy that creature instead".
-- Pawl.Types.PendingEntryEffect's twin one event class over, and queued for the
-- same reason: the module that applies a replacement (Pawl.Engine.Event) is
-- below the module that can run a card's effects.
--
-- A program plus the environment it must resolve in, kept together because the
-- environment cannot be re-derived at the drain: the installing spell or ability
-- is long gone (CR 400.7), so neither the slots it named nor CR 109.5's "you"
-- survive on the board. The same posture Pawl.Types.PreventionRider takes, and
-- for the same reason.
--
-- `targets` is Pawl.Types.ActiveReplacement.slots, which is what makes "that
-- creature" nameable turns later; `controller` is who performs the effects,
-- which is the ROW's controller and not the damage's source; `source` is CR
-- 113.7's source of the effect that created the replacement.
data PendingDamageEffect = MkPendingDamageEffect
  { effects :: Seq.Seq (Effect.Effect Card.Card (GrantedAbility.GrantedAbility Card.Card)),
    targets :: Map.Map SlotName.SlotName (Set.Set Recipient.Recipient),
    controller :: PlayerId.PlayerId,
    source :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)

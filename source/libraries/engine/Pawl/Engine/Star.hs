-- CR 208.2's printed star: substituting the value a characteristic-defining
-- ability supplies into a box, and asking whether a box holds one. Structural
-- only -- nothing here reads a board, which Pawl.Engine.Quantity's evaluation
-- does.
--
-- A module of its own for a MODULE CYCLE rather than for cohesion, as
-- Pawl.Engine.QuantitySlot is. Both functions lived in Pawl.Engine.Quantity
-- until CR 709.4c's split-card merge came to ask them: that merge is in
-- Pawl.Engine.Card, which Pawl.Engine.Game imports, and Pawl.Engine.Quantity
-- reads a board and so sits above Game.
module Pawl.Engine.Star where

import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.Plus as Plus
import Pawl.Types.Quantity (Quantity)
import qualified Pawl.Types.Quantity as Quantity

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through Plus so 1+* becomes 1+<the count>.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  Quantity.Plus (Plus.MkPlus a b) -> Quantity.Plus (Plus.MkPlus (substituteStar star a) (substituteStar star b))
  -- The same descent Plus takes, for CR 208.2's reason: a printed star inside a
  -- halving is still the value the CDA supplies. No card prints one there --
  -- Malignus' star is the whole P/T box and its CDA carries the halving.
  Quantity.Halved (Halved.MkHalved rounding inner) -> Quantity.Halved (Halved.MkHalved rounding (substituteStar star inner))
  -- Plus's descent, for Plus's reason: a star under a minus sign is still the
  -- star the characteristic-defining ability defines.
  Quantity.Negate a -> Quantity.Negate (substituteStar star a)
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.Power -> quantity
  Quantity.Toughness -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.Count _ -> quantity
  Quantity.ManaCount _ -> quantity
  Quantity.LifeTotal _ -> quantity
  Quantity.Speed _ -> quantity
  Quantity.IsMonarch _ -> quantity
  Quantity.IsStartingPlayer _ -> quantity
  Quantity.IsActivePlayer _ -> quantity
  Quantity.PlayerCounters {} -> quantity
  Quantity.ObjectCounters _ -> quantity
  Quantity.ObjectCountersOfAnyKind -> quantity
  Quantity.HasDesignation _ -> quantity
  Quantity.ClassLevel -> quantity
  Quantity.WasKicked -> quantity
  -- CR 702.33f's read, WasKicked's arm above in every respect: the Cost it
  -- carries is the IDENTIFIER of one kicker ability, matched against the spell's
  -- own record by equality, never an instruction this traversal descends into.
  Quantity.TimesKickedWith _ -> quantity
  Quantity.TagWasSpent {} -> quantity
  Quantity.WasToken -> quantity
  Quantity.WasBlocking -> quantity
  Quantity.DamageDealtToThisTurn -> quantity
  Quantity.OpponentsAttacked _ -> quantity
  Quantity.CardsDiscardedThisTurn _ -> quantity
  Quantity.LifeGainedThisTurn _ -> quantity
  Quantity.PlayersDealtDamageThisTurn _ -> quantity
  Quantity.DamageDealtToPlayersThisTurn _ -> quantity
  Quantity.SpellsCastLastTurn _ -> quantity
  Quantity.DungeonsCompleted _ -> quantity
  Quantity.CompletedDungeon {} -> quantity
  Quantity.EnteredThisTurn -> quantity
  Quantity.EnteredFrom _ -> quantity
  Quantity.WasCastFrom _ -> quantity
  Quantity.BlockersBeyondFirst -> quantity
  -- No card prints CR 702.184a's ability at all -- Keyword.Station mints it --
  -- so a printed P/T box can never contain this arm.
  Quantity.StationMeasure -> quantity
  -- No descent, for the Count arm's reason: CR 604.3 makes a CDA a static
  -- ability with no resolution and so no slots, and Pawl.CardSpec's
  -- powerToughnessSlots keeps a slot-naming quantity out of a printed P/T.
  Quantity.AgainstSlot {} -> quantity

-- Does a printed box hold CR 208.2's star anywhere inside it? The three
-- calculations descend for substituteStar's reason: 1+* is a star box, and the
-- star is what a characteristic-defining ability fills in later.
--
-- Asked by Pawl.Engine.Resolve.Effect.bakeTokenCharacteristics, which must tell a star
-- (keep it -- CR 208.2's value arrives at layer 7a, so there is nothing to settle
-- at creation) from a computed box (settle it -- CR 111.3 defines the token's
-- values once, as the effect resolves), and by Pawl.Engine.Card's CR 709.4c
-- merge, which asks it of each half's box to find the half whose
-- characteristic-defining ability defines that box.
containsStar :: Quantity -> Bool
containsStar quantity = case quantity of
  Quantity.Star -> True
  Quantity.Plus (Plus.MkPlus a b) -> containsStar a || containsStar b
  Quantity.Halved (Halved.MkHalved _ inner) -> containsStar inner
  Quantity.Negate a -> containsStar a
  -- No descent into a Count: CR 208.2a's star is a printed box's own symbol, and
  -- Pawl.Types.Count has no room for one -- its per-member quantity is read
  -- against another object, where a star would be that object's box and not this
  -- one's.
  _ -> False

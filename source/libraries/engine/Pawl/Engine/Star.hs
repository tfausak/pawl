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

import Pawl.Types.Quantity (Quantity)
import qualified Pawl.Types.Quantity as Quantity

-- CR 208.2: resolve a printed star to the quantity a characteristic-defining
-- ability supplies, recursing through a calculation so 1+* becomes 1+<the count>.
substituteStar :: Quantity -> Quantity -> Quantity
substituteStar star quantity = case quantity of
  Quantity.Star -> star
  -- A printed star inside a calculation is still the value the CDA supplies.
  -- No card prints one under anything but a Plus -- Malignus' star is the whole
  -- P/T box and its CDA carries the halving.
  Quantity.Arithmetic arithmetic -> Quantity.Arithmetic (fmap (substituteStar star) arithmetic)
  Quantity.Literal _ -> quantity
  Quantity.ManaValue -> quantity
  Quantity.Power -> quantity
  Quantity.Toughness -> quantity
  Quantity.Intensity -> quantity
  Quantity.InSlot _ -> quantity
  Quantity.WasBound _ -> quantity
  Quantity.BoundCount _ -> quantity
  Quantity.UniqueVowelsOnSticker _ -> quantity
  Quantity.Count _ -> quantity
  Quantity.ManaCount _ -> quantity
  Quantity.LifeTotal _ -> quantity
  Quantity.StartingLifeTotal _ -> quantity
  Quantity.Speed _ -> quantity
  Quantity.IsMonarch _ -> quantity
  Quantity.HasPlayerDesignation {} -> quantity
  Quantity.IsStartingPlayer _ -> quantity
  Quantity.IsActivePlayer _ -> quantity
  Quantity.PlayerCounters {} -> quantity
  Quantity.Devotion {} -> quantity
  Quantity.PartySize _ -> quantity
  Quantity.ObjectCounters _ -> quantity
  Quantity.ObjectCountersOfAnyKind -> quantity
  Quantity.LettersOnNameStickers _ -> quantity
  Quantity.NameStickers -> quantity
  Quantity.PowerOfStickers -> quantity
  Quantity.ToughnessOfStickers -> quantity
  Quantity.HasDesignation _ -> quantity
  Quantity.DesignationValue _ -> quantity
  Quantity.StoredResultsOfSameValue -> quantity
  Quantity.ClassLevel -> quantity
  Quantity.WasForetold -> quantity
  Quantity.TributeWasPaid -> quantity
  -- CR 601.2b: the keywords it designates are identifiers Keyword.designates
  -- matches, never instructions this traversal descends into.
  Quantity.TimesPaid _ -> quantity
  Quantity.CastUsing _ -> quantity
  Quantity.TagWasSpent {} -> quantity
  Quantity.TagWasSpentOfOwnColor {} -> quantity
  Quantity.ChosenColorsItIs -> quantity
  Quantity.ManaSpent -> quantity
  Quantity.WasToken -> quantity
  Quantity.WasAttacking -> quantity
  Quantity.WasBlocking -> quantity
  Quantity.WasBlockedThisTurn -> quantity
  Quantity.ControlGainedSinceLastUpkeep _ -> quantity
  Quantity.PlayedBy _ -> quantity
  Quantity.DamageDealtToThisTurn -> quantity
  Quantity.OpponentsAttacked _ -> quantity
  Quantity.AttackersDeclaredThisTurn _ -> quantity
  Quantity.AttackersDeclaredThisCombat -> quantity
  Quantity.AttackedInLastTurnOf _ -> quantity
  Quantity.AttackersInTheirLastTurn _ -> quantity
  Quantity.CardsDiscardedThisTurn _ -> quantity
  Quantity.CardsDrawnThisTurn _ -> quantity
  Quantity.BendingsThisTurn _ -> quantity
  Quantity.LifeGainedThisTurn _ -> quantity
  Quantity.PlayersDealtDamageThisTurn _ -> quantity
  Quantity.DamageDealtToPlayersThisTurn _ -> quantity
  Quantity.SpellsCastLastTurn _ -> quantity
  Quantity.SpellsCastThisTurn _ -> quantity
  Quantity.TimesResolvedThisTurn -> quantity
  Quantity.SpellsCastBefore -> quantity
  Quantity.PermanentsDiedThisTurn -> quantity
  Quantity.SpellsCastUsingThisTurn _ -> quantity
  Quantity.SubgamesThisMatch -> quantity
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
  -- No descent, the Count arm's reason again: the payload is read against the
  -- exiled card, where a star would be THAT card's box and not this one's.
  Quantity.AgainstCardsExiledWith {} -> quantity
  Quantity.AgainstLastCardExiledWith {} -> quantity
  -- CR 702.167c: AgainstCardsExiledWith's answer, over the craft link alone.
  Quantity.AgainstCraftMaterials {} -> quantity

-- Does a printed box hold CR 208.2's star anywhere inside it? A calculation
-- descends for substituteStar's reason: *+1 is a star box, and the star is what
-- a characteristic-defining ability fills in later -- proved by the scenario
-- consuming-blob-ooze-token-keeps-its-star-plus-one-box.
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
  Quantity.Arithmetic arithmetic -> any containsStar arithmetic
  -- No descent into a Count, nor into AgainstSlot or AgainstCardsExiledWith:
  -- CR 208.2a's star is a printed box's own symbol, and each of those three
  -- reads its payload against ANOTHER object, where a star would be that
  -- object's box and not this one's.
  _ -> False

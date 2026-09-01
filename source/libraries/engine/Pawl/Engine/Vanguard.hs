-- | CR 902, the Vanguard variant, and CR 313, the card type it is played with:
-- CR 902.3's card face up in the command zone, CR 902.4's starting life total,
-- CR 902.5's starting hand size, CR 902.5b's maximum hand size, and CR 313.2's
-- refusal to let the card leave that zone.
--
-- A FORMAT and not a card, which is Pawl.Engine.Commander's argument one variant
-- over: no card's printed text says "your starting hand size is three", and rule
-- 902.5 says it of whichever card the player brought. So none of this is a
-- Pawl.Types.PlayerEffect or a printed static ability.
--
-- What the variant needs and this module does NOT hold is CR 902.7's abilities
-- functioning from the command zone, which is where an emblem's are found for the
-- same reason (CR 114.4). The triggered half is Pawl.Engine.Event's command-zone
-- walk, which calls isVanguard below rather than growing a second reading of rule
-- 313; the static half needed nothing at all, Pawl.Engine.Projection's walk
-- already admitting an ability that names no zone.
--
-- There is no GameSettings field for "this is a Vanguard game", and CR 902 asks
-- for none: every rule in it is stated of a player's vanguard CARD, so having one
-- is the whole of being in the variant. That reading is exact today rather than
-- an approximation, and it rests on a capability pawl lacks rather than on a
-- claim about Magic -- Deck.vanguard is set by nothing but a Vanguard deck, and
-- no rule pawl implements asks whether a game is a Vanguard game without also
-- naming the card (#175).
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * CR 902.7's third limb, "its activated abilities may be activated":
--     Pawl.Engine.Activate reads abilities from the battlefield, a hand and a
--     graveyard and answers the empty list for the command zone, so a vanguard
--     that prints one is mute (#2888). The triggered and static limbs are done.
--   * CR 902.2's deck construction, as with every other format: pawl validates no
--     deck (#940), so a game where one player brought a vanguard and another did
--     not is playable here and is not a legal Vanguard game.
module Pawl.Engine.Vanguard where

import qualified Data.List as List
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.Vanguard as Vanguard
import qualified Pawl.Types.Zone as Zone

-- | CR 313.1: does this face print the vanguard card type? A CLASSIFICATION over
-- the printed type line, the same closed-half question
-- Pawl.Engine.Card.isPermanentType asks, and never a case on which card it is.
isVanguardFace :: Face.Face Card.Card -> Bool
isVanguardFace face = Set.member CardType.Vanguard (TypeLine.types (Face.typeLine face))

-- | CR 902.6: is this object a vanguard card? Read off the PRINTED face rather
-- than through the projection, for Pawl.Types.Face.vanguard's reason: rule 313.2
-- keeps the card in the command zone, where nothing in the pool can copy it or
-- change its type.
--
-- No designation on the player, which is where this parts from
-- Pawl.Engine.Commander.isCommander: a commander is an ordinary card that must be
-- singled out by rule 903.3, where a vanguard announces itself by its card type,
-- and rule 313.2 means the object never changes zones and so never trades its id
-- under CR 400.7.
isVanguard :: ObjectId -> GameState -> Bool
isVanguard oid gs = maybe False isVanguardFace (Game.faceOf oid gs)

-- | CR 902.3 \/ 902.6: this player's vanguard card, which is theirs to own and
-- sits in the command zone all game.
--
-- The FIRST by id where a player somehow has two, which no legal Vanguard game
-- has (CR 902.1's "one face-up vanguard card"); pawl validates no deck (#940), so
-- the tie is broken rather than reported.
vanguardOf :: PlayerId -> GameState -> Maybe ObjectId
vanguardOf pid gs = List.find (\oid -> isVanguard oid gs) (Game.zoneMembers Zone.Command pid gs)

-- | CR 313.6 \/ 313.7: the modifiers this player's vanguard prints, if they have
-- one. THE one reader of Face.vanguard in the engine, so that the three rules
-- that consult the two numbers cannot disagree about which card they came from.
modifiersOf :: PlayerId -> GameState -> Maybe Vanguard.Vanguard
modifiersOf pid gs = do
  oid <- vanguardOf pid gs
  face <- Game.faceOf oid gs
  Face.vanguard face

-- | CR 902.5 \/ 902.5b: apply CR 313.6's hand modifier to a base of seven -- both
-- the starting hand size and the maximum hand size, which the two rules state in
-- the same words over the same modifier.
--
-- CR 107.1b is the floor, which is what the saturating conversion performs, and
-- none of the exceptions that rule carves out is a hand size --
-- Pawl.Engine.PlayerEffect.maximumHandSize's reduction arm reads it the same way.
-- No vanguard in the pool can reach it: the largest negative modifier ever
-- printed is Gerrard's -4, so the floor is a regression fence rather than a
-- proved behaviour.
--
-- A player with no vanguard gets the base back unchanged, which is every game
-- outside the variant.
handSize :: Natural -> PlayerId -> GameState -> Natural
handSize base pid gs =
  Integer.toNaturalSaturating (toInteger base + maybe 0 Vanguard.handModifier (modifiersOf pid gs))

-- | CR 902.4: CR 313.7's life modifier, applied to a starting life total by
-- Pawl.Engine.Setup.startingLife. Zero for a player with no vanguard.
--
-- An Integer and not a floor, unlike the hand size above: CR 902.4 says "20 plus
-- or minus the life modifier" and states no minimum, and CR 104.3b speaks of a
-- life total of "0 or less", so pawl's is an Integer. A vanguard whose modifier
-- took a player below zero would have them lose to that rule at once, which is
-- what it says happens; none has ever been printed, the largest negative in the
-- set being Ashnod's -8.
lifeModifierOf :: PlayerId -> GameState -> Integer
lifeModifierOf pid gs = maybe 0 Vanguard.lifeModifier (modifiersOf pid gs)

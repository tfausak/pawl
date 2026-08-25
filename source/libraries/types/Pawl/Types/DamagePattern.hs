module Pawl.Types.DamagePattern where

import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 614.1a / 615.1: which damage events a replacement or prevention
-- intercepts -- both, since this type is shared. Fog's prevention is
-- (Just Combat, And [], Nothing, Nothing, Nothing); Furnace of Rath's replacement
-- is (Nothing, And [], Nothing, Nothing, Nothing); Mending Hands' shield is
-- (Nothing, And [], Nothing, Just the chosen recipient, Nothing); Healing Grace's
-- adds the chosen SOURCE in the last field, and Dovin, Hand of Control's
-- by-direction names that field and no recipient; Stormwild Capridor's is
-- (Just Noncombat, And [], Just IsSource, Nothing, Nothing). Nothing means any
-- kind.
--
-- `whatSource` says WHAT the damage's source is (CR 120.1), as a Filter over its
-- characteristics: Luminesce's "black sources and red sources" is
-- `Or [HasColor Black, HasColor Red]`, and CR 609.7b's recheck is what evaluating
-- it at the event rather than at the shield's creation means. `And []` is the
-- trivial predicate, so a pattern that says nothing about the source needs no
-- "any source" arm -- ZoneChangePattern.whatObject's shape, for its reason.
--
-- CR 614.15's "this way" is spelled in that same Filter as `Filter.IsSource`,
-- the atom asking whether the candidate IS the evaluation's source object:
-- Galvanic Blast's metalcraft clause replaces the damage its own resolution
-- deals and nothing else. An identity enum beside the Filter would be a second
-- spelling of one relation (#163). The candidate is the damage's SOURCE rather
-- than the event's subject, which is what gives the atom something to compare.
--
-- `whichRecipient` is the permanent or player a prevention shield covers -- CR
-- 615.7's, and CR 615.3's unbounded one -- and Nothing means EVERY recipient
-- rather than a missing answer. Mending Hands and Selfless Squire each name the
-- one CR 115.4 recipient their resolution chose; Fog and Furnace of Rath name
-- none.
--
-- That Recipient is BAKED by the engine, never authored, exactly as
-- Pawl.Types.PhasePattern.whosePhase is: card data cannot name an ObjectId or a
-- PlayerId, so the only producers are Resolve's three arms that bake one
-- (PreventNextDamage, PreventAllDamage and RedirectDamage), which share one
-- `installDamageRow`. All three thread a printed KIND through -- Turn the
-- Tables' "all combat damage", Decorated Griffin's "the next 1 combat damage".
--
-- `whatRecipient` is the CARD-PRINTED half of that same question, and the two
-- are not one field: `whichRecipient` names an id the engine baked, where this
-- one describes the recipient by characteristic -- Stormwild Capridor's "dealt
-- to this creature" is `Just Filter.IsSource`, read against the candidate's own
-- source the way `whatSource` reads CR 614.15's "this way". Nothing admits EVERY
-- recipient, players included; `Just` admits only an OBJECT recipient matching
-- the filter, since a Filter describes objects and CR 120.3a's player is not one.
-- A Maybe rather than `And []`, for exactly that reason: the trivial filter would
-- otherwise have to mean "any object OR any player", which is not what any other
-- Filter position means.
--
-- `whichSource` is the ONE object a row watches -- a prevention shield's or CR
-- 614.9's redirection (Oracle's Attendants) -- baked as an id, and two
-- unlike questions land in it: CR 609.7a's player-CHOSEN source (Healing Grace's
-- "by a source of your choice", answered when the effect is created) and CR
-- 601.2c's TARGETED one (Dovin, Hand of Control's "prevent all damage ... dealt
-- by target permanent", declared on the stack). Nothing means EVERY source, the
-- way Nothing means every recipient above, rather than an unanswered choice.
--
-- It does not replace `whatSource` beside it, and CR 615.9 is why both are
-- written together: a CHOSEN source's PROPERTIES are rechecked when it would
-- deal damage (CR 609.7b), so a "red source of your choice" is an id here AND
-- the colour predicate there. Healing Grace names no property, so its shield
-- carries the trivial filter and this id -- and so does a targeted source, which
-- has no printed properties for CR 615.9 to recheck.
--
-- BAKED by the engine and never authored, `whichRecipient`'s reason exactly:
-- card data cannot name an ObjectId. Resolve's installDamageRow is the one
-- producer, and Pawl.CardSpec's engineOnlyOffends keeps the corpus off it.
--
-- Not implemented: a printed recipient condition naming a PLAYER, which is what
-- a static redirection ability needs -- "all damage that would be dealt to you
-- is dealt to this creature instead" (Palisade Giant, Pariah) (#1054).
data DamagePattern = MkDamagePattern
  { whichKind :: Maybe DamageKind.DamageKind,
    whatSource :: Filter.Filter Keyword.Keyword,
    whatRecipient :: Maybe (Filter.Filter Keyword.Keyword),
    whichRecipient :: Maybe Recipient.Recipient,
    whichSource :: Maybe ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)

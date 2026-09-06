module Pawl.Types.CopyException where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness

-- | CR 707.9: one exception to the copying process, the "except ..." clause of a
-- copy effect. Quicksilver Gargantuan's "except it's 7/7" is CR 707.9d's own
-- worked example; Dack's Duplicate's "except it has haste and dethrone" is CR
-- 707.9a's; Phyrexian Metamorph's "except it's an artifact in addition to its
-- other types" is the carve-out CR 707.9d's last two sentences make.
--
-- A LIST of these rides EntryRewrite.AsCopy and Pawl.Types.BecomeCopy rather than
-- one: the printed clauses are joined by "and" (Moritte of the Frost states
-- three), and CR 707.9f reads "any other exceptions that effect includes" --
-- plural, of one effect.
--
-- BOTH CARRIERS take the same list, since CR 707.9 is a rider on the copying
-- process rather than on the door the copy arrives by: Copycrook's "except it
-- has [ability]" and Unstable Shapeshifter's "except it has this ability" are the
-- same sentence over CR 707.5's entry replacement and CR 707.4's battlefield
-- change. Pawl.Types.CreateCopy takes none, and that is card-driven rather than
-- structural -- every printed "create a token copy ... except it has haste"
-- pairs the clause with a delayed sacrifice pawl cannot yet write (#2302).
--
-- Every arm writes into the COPIABLE snapshot, never into a CR 613 layer, which
-- is what CR 707.9a and CR 707.9b both require: the excepted value or ability
-- "becomes part of the copiable values" of the copy. A token copy of the copy
-- therefore inherits the excepted value with no further machinery, where a
-- layer-7b write on the object would be left behind (CR 707.2's exclusion of
-- "other effects").
--
-- Not implemented: CR 707.9c's exception that declines to copy a characteristic
-- (Vesuvan Doppelganger's "except it doesn't copy that creature's color"), the
-- SUBTYPE and SUPERTYPE halves of CR 707.9b's "in addition to its other types"
-- (Visage Bandit's "a Shapeshifter Rogue", Sakashima the Impostor's "legendary"),
-- and CR 707.9e's exception that is an additional effect rather than a
-- characteristic (Altered Ego's additional counters) (#1292).
data CopyException
  = -- | CR 707.9b: the copy's power and toughness are these numbers instead of
    -- the copied object's ("except it's 7/7").
    --
    -- Two Integers rather than a Quantity, the position
    -- Pawl.Types.EntryOption's own pair takes: every printing of this clause
    -- states two literals, and CR 614.12a settles the copy before the permanent
    -- enters, so there is no board for a variable to be measured against yet.
    --
    -- Applying this arm ALSO clears the copied characteristic-defining P/T,
    -- which is CR 707.9d: an effect that "provides a specific set of values for
    -- a certain characteristic" does not copy the CDA defining it. Without that
    -- half the CDA would win at layer 7a and a Gargantuan copying a Tarmogoyf
    -- would recompute rather than stay 7/7.
    SetPowerToughness SetPowerToughness.SetPowerToughness
  | -- | CR 707.9a: the copy gains these abilities as part of the copying process
    -- ("except it has haste and dethrone"), so they join the copiable values.
    --
    -- A Set, and one arm for the whole clause rather than one per keyword: the
    -- printed sentence names them joined by "and" (Dack's Duplicate states two).
    --
    -- CR 604.3a(2) makes an ability acquired this way CHARACTERISTIC-DEFINING,
    -- which falls out of writing it into the snapshot: the copy's keywords are
    -- in place before layer 4, so Pawl.Engine.Projection.applySubtypeDefining
    -- picks up an excepted changeling at CR 613.3's start-of-layer moment rather
    -- than in timestamp order (Omni-Changeling).
    GainKeywords (Set.Set Keyword.Keyword)
  | -- | CR 707.9a again, one ability kind over: the copy gains the ability this
    -- copy effect is written inside ("except it has this ability", Unstable
    -- Shapeshifter), so that ability joins the copiable values.
    --
    -- NULLARY, and self-referential by construction. The printed words are a
    -- reference and not a quotation, so there is nothing to carry: the ability is
    -- read off the resolving object at CR 608.2's execution
    -- (Pawl.Engine.Resolve.Effect's BecomeCopy arm), and what is written into the
    -- snapshot is that whole ability -- this exception included. So the copy's
    -- own instance is what "this ability" names the next time it resolves, which
    -- is what lets a Shapeshifter copy a second creature and a third.
    -- A payload would have to be the ability that contains it, which no finite
    -- value is.
    --
    -- Not implemented: the exception that QUOTES an ability instead of pointing
    -- at this one (Copycrook's "except it has 'Whenever this creature attacks, it
    -- connives'", Estrid's Invocation). That payload is a whole
    -- Pawl.Types.GrantedAbility, which this module may not name -- CR 707.9's
    -- exceptions ride Pawl.Types.Effect's BecomeCopy, and GrantedAbility reaches
    -- Effect -- so it wants this type parametric in the ability the way
    -- Pawl.Types.EntryRewrite is parametric in the effect (#1292).
    --
    -- Not implemented: the same words in an ACTIVATED ability (Dimir
    -- Doppelganger's "{1}{U}: ... becomes a copy of that card, except it has this
    -- ability"), which would write Pawl.Types.ProjectedCharacteristics'
    -- activatedAbilities instead (#3325).
    GainThisAbility
  | -- | CR 707.9b: the copy is these card types "in addition to its other types"
    -- (Phyrexian Metamorph's "except it's an artifact"), so they JOIN the copied
    -- type line rather than replacing it (CR 205.1b).
    --
    -- A Set for GainKeywords' reason: one printed clause names as many as it
    -- likes at once (Dollhouse of Horrors' "a 0/0 Construct artifact creature").
    -- CR 205.1a's replacement is a different sentence and no copy exception
    -- prints it.
    --
    -- NOTHING ELSE MOVES, and that is CR 707.9d's last two sentences rather than
    -- an omission: its strip of the copied object's characteristic-defining
    -- ability "does not apply to copy effects with exceptions that state the
    -- object is a certain card type, supertype, and/or subtype 'in addition to
    -- its other types'". So this arm must NOT clear characteristicPT the way
    -- SetPowerToughness above does, and it must not touch the keywords a CDA is
    -- written as -- a Metamorph copying a Tarmogoyf keeps the Goyf's CDA.
    AddCardTypes (Set.Set CardType.CardType)
  deriving (Eq, Ord, Show)

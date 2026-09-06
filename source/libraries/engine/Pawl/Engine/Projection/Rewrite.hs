-- CR 612.1 text-changing effects: rewriting every card-text position an
-- effect can name, one traversal per type. Pure over the text; the projection
-- that decides whether a rewrite applies is Pawl.Engine.Projection. Split out
-- of it for size.
module Pawl.Engine.Projection.Rewrite where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Subtype as Subtype
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AddActivationCost as AddActivationCost
import qualified Pawl.Types.AddSpellCost as AddSpellCost
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Amass as Amass
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CastFromZone as CastFromZone
import qualified Pawl.Types.ChangeText as ChangeText
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Conjure as Conjure
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.CounterPlacement as CounterPlacement
import qualified Pawl.Types.CounterR as CounterR
import qualified Pawl.Types.Create as Create
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.DamagePart as DamagePart
import qualified Pawl.Types.DamagePattern as DamagePattern
import qualified Pawl.Types.DamageR as DamageR
import qualified Pawl.Types.DamageRewrite as DamageRewrite
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.Destroy as Destroy
import qualified Pawl.Types.Discard as Discard
import qualified Pawl.Types.Draw as Draw
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard
import qualified Pawl.Types.EachCardInHand as EachCardInHand
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntryR as EntryR
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ForbidActivation as ForbidActivation
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.IncreaseActivationCost as IncreaseActivationCost
import qualified Pawl.Types.IncreaseSpellCost as IncreaseSpellCost
import qualified Pawl.Types.LookAt as LookAt
import qualified Pawl.Types.Meld as Meld
import qualified Pawl.Types.Mill as Mill
import qualified Pawl.Types.MillTally as MillTally
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.MovedKinds as MovedKinds
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated
import qualified Pawl.Types.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith
import qualified Pawl.Types.PlayerCounters as PlayerCounters
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerQuantity as PlayerQuantity
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.PutCounters as PutCounters
import qualified Pawl.Types.PutCountersFrom as PutCountersFrom
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.RandomCardInHand as RandomCardInHand
import qualified Pawl.Types.RedirectDamage as RedirectDamage
import qualified Pawl.Types.ReduceActivationCost as ReduceActivationCost
import qualified Pawl.Types.ReduceSpellCost as ReduceSpellCost
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.Replace as Replace
import Pawl.Types.ReplacementEffect (ReplacementEffect)
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.RequireAttack as RequireAttack
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.Reveal as Reveal
import qualified Pawl.Types.RollDie as RollDie
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Types.SpeedDecrease as SpeedDecrease
import qualified Pawl.Types.SpellCast as SpellCast
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype.Type
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily
import qualified Pawl.Types.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenR as TokenR
import qualified Pawl.Types.TopOfLibrary as TopOfLibrary
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnFaceDown as TurnFaceDown
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.TypeLine as TypeLine
import qualified Pawl.Types.WithCounters as WithCounters
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- CR 613.1f's grant carries a whole quoted ability, and a card's abilities are
-- written against a whole Card (CR 707.8a).
type Modification = Modification.Modification (GrantedAbility.GrantedAbility Card.Type.Card)

-- Apply text-changes to a modification's subtype words (CR 612.1/612.2).
--
-- CR 612.2 gates each arm: the arm's family is fixed by its constructor, and the
-- PAIR's family is read off the word being replaced, which is why no family tag
-- rides on the stored ChangeSubtypeWord. Exhaustive rather than a catch-all, which
-- is what had let GainKeyword go unrewritten while carrying a land-type word.
-- Descends into the quoted ability a CR 613.1f grant carries, through
-- rewriteGrantedAbility below -- which is mutually recursive with this, since
-- that ability's own clauses hold effects that can grant again.
rewriteModification :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modification -> Modification
rewriteModification pairs m =
  let -- `inFamily from` is CR 612.2's gate.
      swap inFamily from to s = if s == from && inFamily from then to else s
      apply1 acc (from, to) = case acc of
        Modification.SetLandSubtype s -> Modification.SetLandSubtype (swap Subtype.isLandType from to s)
        Modification.AddLandSubtype s -> Modification.AddLandSubtype (swap Subtype.isLandType from to s)
        -- CR 612.2's other named example: a Turn to Frog's Frog on the stack.
        Modification.SetCreatureSubtype s -> Modification.SetCreatureSubtype (swap Subtype.isCreatureType from to s)
        Modification.AddCreatureSubtype s -> Modification.AddCreatureSubtype (swap Subtype.isCreatureType from to s)
        -- Holds no word to swap: it names CR 205.3m's list, not a member of it.
        Modification.AddEveryCreatureSubtype -> acc
        -- Deliberately unrewritten. CR 612.2 changes only a word used in the
        -- correct way, and this arm carries no family to check the word against;
        -- the two family-tagged adds above are where a land-type or creature-type
        -- word rides, and Pawl.CardSpec keeps those words out of this one.
        Modification.AddSubtype _ -> acc
        -- CR 702.14a: "[type]walk" holds a land-type word, so a hacked Lord of
        -- Atlantis grants swampwalk. The GRANTER's text is what this reads, which
        -- is CR 612.3. Filter.rewriteKeyword since the word is inside a Filter; no
        -- family gate is restated there -- the word's use is its family.
        Modification.GainKeyword k -> Modification.GainKeyword (Filter.rewriteKeyword [(from, to)] k)
        -- Carries no word: the keyword is rule 702.34a's and the cost is the
        -- RECEIVER's mana cost, so neither half is text a pair could name --
        -- SetLandSubtypeToChosen's answer for the same reason.
        Modification.GainFlashbackAtManaCost -> acc
        -- CR 612.1 through the granted enchant's own Filter, which is text
        -- printed on the GRANTER (CR 612.3) exactly as the keyword above is.
        -- rewriteTargetSlot is the same descent a mode's target slots take.
        Modification.GainEnchant slot -> Modification.GainEnchant (rewriteTargetSlot [(from, to)] slot)
        -- CR 612.1 over the whole quoted ability: the words are printed on the
        -- GRANTER, so a text change affecting it rewrites them before the grant.
        Modification.GainAbility a -> Modification.GainAbility (rewriteGrantedAbility [(from, to)] a)
        -- Carries no word: the type is read off the source at projection time.
        Modification.SetLandSubtypeToChosen -> acc
        -- A control op carries no subtype word either.
        Modification.SetController _ -> acc
        Modification.SetControllerToSource -> acc
        -- An ability wipe names nothing at all, and neither does a P/T switch.
        Modification.LoseAllAbilities -> acc
        -- Names an ABILITY of the same card, which is no subtype word.
        Modification.LoseNamedAbility _ -> acc
        -- CR 612.3 through the removal's own keyword, exactly as GainKeyword
        -- above: "loses islandwalk" is text printed on the REMOVER, so a text
        -- change affecting it swaps the land-type word before the removal is
        -- read. Scarwood Hag's "loses forestwalk" is the printing that carries
        -- one, and it is not in the pool: every written keyword a card here
        -- removes is payload-free -- Sky Tether's flying, Melira's infect -- so
        -- this arm is a regression fence where the hacked Lord of Atlantis proves
        -- the grant above.
        Modification.LoseKeyword k -> Modification.LoseKeyword (Filter.rewriteKeyword [(from, to)] k)
        -- Nothing to rewrite, and not for want of a descent: CR 612.1 swaps a
        -- WORD, and a KeywordFamily holds none -- "all landwalk abilities" names
        -- CR 702.14a's generic term, which has no land type in it. A Hack on
        -- Hammerheim changes what its removal reaches not at all.
        Modification.LoseKeywordFamily _ -> acc
        Modification.SwitchPowerToughness -> acc
        -- Nothing to rewrite: two bare markers naming no subtype word.
        Modification.AssignCombatDamageWithToughness -> acc
        Modification.GrantsStationToughness -> acc
        -- CR 612.1 through both boxes: a Quantity.Count carries a Filter, so a
        -- hacked Aspect of Wolf counts the new type. rewriteQuantity is the same
        -- descent rewriteCondition and the CDA path take.
        Modification.SetBasePowerToughness pt ->
          Modification.SetBasePowerToughness
            pt
              { SetBasePowerToughness.power = rewriteQuantity [(from, to)] (SetBasePowerToughness.power pt),
                SetBasePowerToughness.toughness = rewriteQuantity [(from, to)] (SetBasePowerToughness.toughness pt)
              }
        Modification.ModifyPowerToughness pt ->
          Modification.ModifyPowerToughness
            pt
              { ModifyPowerToughness.power = rewriteQuantity [(from, to)] (ModifyPowerToughness.power pt),
                ModifyPowerToughness.toughness = rewriteQuantity [(from, to)] (ModifyPowerToughness.toughness pt)
              }
        -- CR 205.2a's card types are a different list from CR 205.3's subtypes, and
        -- CR 205.4a's supertypes a third, so these hold no word a pair could name.
        Modification.AddCardType _ -> acc
        Modification.SetCardType _ -> acc
        Modification.AddSupertype _ -> acc
        Modification.RemoveSupertype _ -> acc
        -- The two words of a STORED text change are its own resolution's choice
        -- (CR 608.2d). A text changer's PRINTED clause is reached by rewriteEffect.
        Modification.ChangeSubtypeWord {} -> acc
        -- CR 612.2 names colour words as a swappable family, but pawl's only text
        -- changer swaps subtypes, so no pair reaching here holds a colour word.
        Modification.SetColor _ -> acc
        Modification.AddColor _ -> acc
        Modification.AddChosenColor -> acc
   in List.foldl' apply1 m pairs

-- rewriteModification's sibling for the other half of a static ability. Under CR
-- 612.1 an ability's affected clause is rules text like any other, so a hacked
-- Kormus Bell animates Islands. Exhaustive over Affected: a new arm carrying a
-- Filter must break this build rather than silently keep the old word.
rewriteAffected :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Affected.Affected -> Affected.Affected
rewriteAffected pairs a = case a of
  Affected.Matching f -> Affected.Matching (Filter.rewrite pairs f)
  Affected.MatchingAnywhere f -> Affected.MatchingAnywhere (Filter.rewrite pairs f)
  Affected.MatchingOffBattlefield f -> Affected.MatchingOffBattlefield (Filter.rewrite pairs f)
  Affected.AttachedPlayerControls f -> Affected.AttachedPlayerControls (Filter.rewrite pairs f)
  -- A frozen id set names no word (CR 611.2c), and an attachment names none.
  Affected.TheseObjects _ -> a
  Affected.Attached -> a

-- CR 612.1's subtype word swap over a PlayerEffect, rewriteModification's sibling
-- for the CR 613.10/613.11 axis. An Artificial Evolution resolved at an
-- Edgewalker moves "Cleric spells you cast cost {W}{B} less to cast" onto the new
-- word, because the word naming which spells the ability discounts is text
-- printed on the permanent like any other.
--
-- HERE, beside the other printed-text rewrites, and NOT in
-- Pawl.Engine.PlayerEffect, which is otherwise the only module that may case on
-- this type: rewriteEffect's AffectPlayers arm has to reach the same descent for
-- a restriction a RESOLUTION stores (Liliana, Untouched by Death's -3), and
-- Pawl.Engine.PlayerEffect imports this module rather than the other way round.
-- The exception is the same one rewriteEffect itself takes on the Effect type:
-- this cases on STRUCTURE -- does this arm carry a Filter a swap could reach --
-- and never on which player effect it is, so no rule in the closed half learns an
-- effect's identity from it. Every other reader still asks a typed question of
-- Pawl.Engine.PlayerEffect and never sees a constructor, and this module's own
-- other handling of the rows stays opaque: they ride
-- ProjectedCharacteristics.playerAbilities so a copy acquires them (CR 707.2a),
-- and nothing there looks inside one.
--
-- The shape Pawl.Engine.CombatRestriction takes for a restriction -- destructure
-- the type, hand each inner value to the module that owns it -- with
-- Pawl.Engine.Filter.rewrite doing the descent.
--
-- Exhaustive rather than a catch-all, for rewriteModification's stated reason: a
-- later arm that can hold a word must break this build instead of silently
-- keeping the printed one.
--
-- CR 612.2's family gate is not restated at the Filter descent, for the reason
-- Filter.rewrite's own comment gives: a HasSubtype atom may name a word of any
-- family, so the family the word is used AS is the family it belongs to, and the
-- exact lookup already asks CR 612.2's question. A Magical Hack's land-type pair
-- therefore leaves Edgewalker's Cleric alone.
rewritePlayerEffect :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> PlayerEffect.PlayerEffect -> PlayerEffect.PlayerEffect
rewritePlayerEffect pairs effect = case effect of
  -- The arms carrying a Filter, which is the only place in this type a subtype
  -- word can hide. Thalia's "noncreature spells", Vedalken Orrery's "spells",
  -- Prowling Serpopard's "creature spells", Heartstone's "activated abilities of
  -- creatures", Damping Engine's "artifact, creature, or enchantment spells",
  -- Oppressive Rays' "enchanted creature" and Yawgmoth's Will's "spells" and
  -- Omniscience's "spells" name none today; Edgewalker's "Cleric spells" does on
  -- the printed road, and Liliana, Untouched by Death's "Zombie spells" does on
  -- the stored one.
  PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost f n) -> PlayerEffect.IncreaseSpellCost (IncreaseSpellCost.MkIncreaseSpellCost (Filter.rewrite pairs f) n)
  PlayerEffect.IncreaseActivationCost (IncreaseActivationCost.MkIncreaseActivationCost f kind n) -> PlayerEffect.IncreaseActivationCost (IncreaseActivationCost.MkIncreaseActivationCost (Filter.rewrite pairs f) kind n)
  PlayerEffect.ReduceSpellCost x -> PlayerEffect.ReduceSpellCost x {ReduceSpellCost.whichSpells = Filter.rewrite pairs (ReduceSpellCost.whichSpells x)}
  -- TWO Filters of its own, and both descend. The second names
  -- what the ability targets (Dwarven Mauler's "that target this creature",
  -- spelled Filter.IsSource), so no card in `data/cards/` puts a subtype word
  -- there today and neutralising that descent leaves the suite green -- it is
  -- here so that the card which does write one cannot silently keep the printed
  -- word.
  PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost f family kind targets cost floor_) -> PlayerEffect.ReduceActivationCost (ReduceActivationCost.MkReduceActivationCost (Filter.rewrite pairs f) family kind (fmap (Filter.rewrite pairs) targets) cost floor_)
  -- The two arms with a word in TWO places: their own criterion ("nontoken
  -- Rebels"), and the criterion inside each component they add ("sacrifice a
  -- LAND", "sacrifice a SWAMP"). Both descend, which is Filter.rewriteCost's
  -- reading of CR 612.2 carried to a component that is added to a cost rather
  -- than printed in one. The scale beside them names a COLOUR, which CR 612.2's
  -- subtype pairs cannot reach.
  PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost f components scale) -> PlayerEffect.AddActivationCost (AddActivationCost.MkAddActivationCost (Filter.rewrite pairs f) (fmap (Filter.rewriteComponent pairs) components) scale)
  PlayerEffect.AddSpellCost (AddSpellCost.MkAddSpellCost f components scale) -> PlayerEffect.AddSpellCost (AddSpellCost.MkAddSpellCost (Filter.rewrite pairs f) (fmap (Filter.rewriteComponent pairs) components) scale)
  PlayerEffect.CastAsThoughItHadFlash f -> PlayerEffect.CastAsThoughItHadFlash (Filter.rewrite pairs f)
  PlayerEffect.MayPlayAsThoughItHadFlash f -> PlayerEffect.MayPlayAsThoughItHadFlash (Filter.rewrite pairs f)
  PlayerEffect.CantBeCountered f -> PlayerEffect.CantBeCountered (Filter.rewrite pairs f)
  PlayerEffect.CantCastMatching f -> PlayerEffect.CantCastMatching (Filter.rewrite pairs f)
  PlayerEffect.CastFrom grant -> PlayerEffect.CastFrom grant {CastFromZone.matching = Filter.rewrite pairs (CastFromZone.matching grant)}
  PlayerEffect.CastFromHandWithoutPayingManaCost f -> PlayerEffect.CastFromHandWithoutPayingManaCost (Filter.rewrite pairs f)
  -- The rest name no word a subtype pair could reach. The two chosen-name arms
  -- carry nothing at all -- CR 201.4's names are read off the source's
  -- Object.chosenNames -- and CR 612.2's second sentence says a subtype swap
  -- could not touch a card name even if they did. A count, a mana filter and a
  -- player scope are not words either.
  PlayerEffect.CantCastSpells -> effect
  PlayerEffect.CantActivateAbilities -> effect
  PlayerEffect.CantCastMoreThan _ -> effect
  PlayerEffect.CantCastChosenName -> effect
  PlayerEffect.CantPlayLandChosenName -> effect
  PlayerEffect.PlayAdditionalLands _ -> effect
  PlayerEffect.NoMaximumHandSize -> effect
  PlayerEffect.SetMaximumHandSize _ -> effect
  PlayerEffect.IncreaseMaximumHandSize _ -> effect
  PlayerEffect.ReduceMaximumHandSize _ -> effect
  PlayerEffect.DontLoseUnspentMana _ -> effect
  PlayerEffect.SpendManaAsThough _ -> effect
  PlayerEffect.CantBeTargetedBy _ -> effect
  PlayerEffect.DamageCantBePrevented _ -> effect
  PlayerEffect.DamageCantBeRedirected _ -> effect
  PlayerEffect.CantSearchLibraries _ -> effect
  -- CR 702.16a's quality here is a chosen card NAME, and CR 612.2's second
  -- sentence keeps a subtype swap off a name.
  PlayerEffect.HasProtectionFromChosenName -> effect
  PlayerEffect.CantBecomeMonarch -> effect
  PlayerEffect.CastOnlyAtSorcerySpeed -> effect
  PlayerEffect.CantPlayLands -> effect
  PlayerEffect.PlayLandsFrom _ -> effect
  -- A counter KIND is not a word CR 612.2's subtype pairs could reach either.
  PlayerEffect.CantGetCounters _ -> effect
  -- Nor is a coin's face, or the two flags beside it.
  PlayerEffect.StateCoinFlip _ -> effect

-- CR 612's subtype word swap over an effect's AST. Cases on an effect's
-- STRUCTURE -- does this arm carry a word a swap could reach -- never on which
-- effect it is.
rewriteEffect :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteEffect pairs effect = case effect of
  -- CR 612.1 / 612.3 through rewriteModification, whose other arms Tidal Warrior
  -- proves and whose GRANT arm Presence of Gond proves. The two together -- a
  -- text change reaching a quoted ability a RESOLUTION granted -- is proved by
  -- Pawl.CounterspellSpec's evolved Clavileño, whose granted dies trigger mints a
  -- Vampire Elf Token where the printed word says Demon.
  Effect.ModifyTarget (ModifyTarget.MkModifyTarget duration modification ref) ->
    Effect.ModifyTarget (ModifyTarget.MkModifyTarget (rewriteDuration pairs duration) (rewriteModification pairs modification) (rewriteObjectRef pairs ref))
  -- CR 612.1 through BOTH halves of every clause: the recipient's ref, and the
  -- clause's own amount, whose Count may name a creature type -- Goblin War
  -- Strike's "damage equal to the number of Goblins you control". CR 120.2b's
  -- dealer is a slot name and CR 120.4a's excess rider a destination; neither is a
  -- word rule 612 can swap.
  Effect.DealDamage (DealDamage.MkDealDamage parts dealer excess) -> Effect.DealDamage (DealDamage.MkDealDamage (fmap (rewriteDamagePart pairs) parts) dealer excess)
  -- Two SlotNames and nothing else: no word a swap could reach.
  Effect.Fight _ -> effect
  -- CR 612.1: a text-changer's own restriction clause is text like any other.
  Effect.ChangeText (ChangeText.MkChangeText family forbidden slot) ->
    Effect.ChangeText (ChangeText.MkChangeText family (Set.map (swapWordIn family pairs) forbidden) slot)
  Effect.AddMana _ -> effect
  Effect.Search (Search.MkSearch searcher owner zones quantity filter_ upTo destination) -> Effect.Search (Search.MkSearch searcher owner zones (fmap (rewriteQuantity pairs) quantity) (Filter.rewrite pairs filter_) upTo destination)
  Effect.ExileAllGraveyards -> effect
  Effect.Proliferate -> effect
  -- CR 612.1: rule 201.4a's restriction is printed card text, so a text-changer
  -- rewrites it exactly as it rewrites a search's filter above.
  Effect.ChooseCardName restriction -> Effect.ChooseCardName (Filter.rewrite pairs restriction)
  -- CR 612.1 again: "a sorcery card you own from outside the game" is printed
  -- card text like the search's filter above, so a text-changer reaches it the
  -- same way. A REGRESSION FENCE rather than a proven behaviour -- no card in
  -- data/cards changes a word this filter names, so both readings leave the same
  -- board and mutating this line reddens nothing.
  Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame predicate reveal) -> Effect.FromOutsideTheGame (FromOutsideTheGame.MkFromOutsideTheGame (Filter.rewrite pairs predicate) reveal)
  Effect.ExileThisSpell -> effect
  Effect.Bolster quantity -> Effect.Bolster (rewriteQuantity pairs quantity)
  -- CR 612.1 / 612.2a: amass's subtype is a printed word of CR 205.3m's family,
  -- and the token's own name follows it.
  Effect.Amass (Amass.MkAmass quantity subtype) ->
    Effect.Amass (Amass.MkAmass (rewriteQuantity pairs quantity) (List.foldl' (\s (from, to) -> if s == from && Subtype.isCreatureType from then to else s) subtype pairs))
  Effect.Blight x -> Effect.Blight (rewritePlayerQuantity pairs x)
  Effect.TemptWithTheRing -> effect
  -- CR 612.2's gate, and this arm is where it bites rather than where it is
  -- restated: the payload IS a subtype word (CR 701.49d's quality), but a pair
  -- reaching it would have to come from a Pawl.Types.SubtypeFamily, and that type
  -- has only CR 205.3m's creature types and the basic land types -- the two
  -- families CR 612.2 names. CR 205.3p's dungeon type is in neither, so no swap
  -- this function can be given names it.
  Effect.Venture {} -> effect
  Effect.ExileHandThenDraw -> effect
  Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot filter_ quantity) -> Effect.PlayerSacrifices (PlayerSacrifices.MkPlayerSacrifices slot (Filter.rewrite pairs filter_) (rewriteQuantity pairs quantity))
  Effect.RestartGame exempt -> Effect.RestartGame (fmap (rewriteObjectRef pairs) exempt)
  Effect.ControlPlayerNextTurn _ -> effect
  Effect.Destroy (Destroy.MkDestroy ref regenerability mSlot mBuried mPermanents) -> Effect.Destroy (Destroy.MkDestroy (rewriteObjectRef pairs ref) regenerability mSlot mBuried mPermanents)
  -- CR 612.1: the ref may carry a Filter of printed card text, so a text-changer
  -- reaches it exactly as Destroy's above. A REGRESSION FENCE rather than a
  -- proven behaviour: Golgothian Sylex is the only card whose sacrifice carries a
  -- Filter at all, and CR 206.3b's names are not words CR 612.2's two families
  -- can swap, so mutating this line reddens nothing.
  Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect ref sacrificer) -> Effect.Sacrifice (SacrificeEffect.MkSacrificeEffect (rewriteObjectRef pairs ref) sacrificer)
  -- CR 612.1: the ref carries a Filter of printed card text, so a text-changer
  -- reaches it exactly as Destroy's above. The listed characteristics hold no word
  -- this rewrites: a type line is CR 205's, not CR 201.4a's changeable text.
  --
  -- A REGRESSION FENCE rather than a proven behaviour, the shape
  -- FromOutsideTheGame above records: the two filters this opcode carries in
  -- data/cards name a keyword family (Backslide, Weaver of Lies) and the source,
  -- neither of which rule 612 changes, so both readings leave the same board and
  -- mutating this line reddens nothing.
  Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown ref listed) -> Effect.TurnFaceDown (TurnFaceDown.MkTurnFaceDown (rewriteObjectRef pairs ref) listed)
  Effect.TurnFaceUp _ -> effect
  Effect.RemoveFromCombat ref -> Effect.RemoveFromCombat (rewriteObjectRef pairs ref)
  Effect.BecomesBlocked _ -> effect
  -- The riders' counter AMOUNTS are Quantities and take rewriteQuantity's
  -- descent, PutCounters' case below.
  --
  -- Not implemented: a CR 122.1b keyword counter named in the riders keeps its
  -- printed keyword through the swap (#1190).
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref zone riders mSlot mOrigin position duration) -> Effect.MoveToZone (MoveToZone.MkMoveToZone (rewriteObjectRef pairs ref) zone (rewriteEntryRiders pairs riders) mSlot mOrigin position duration)
  Effect.Draw x -> Effect.Draw x {Draw.quantity = rewriteQuantity pairs (Draw.quantity x)}
  Effect.Mill (Mill.MkMill ref quantity mTally mSlot) ->
    Effect.Mill (Mill.MkMill ref (rewriteQuantity pairs quantity) (fmap (\t -> t {MillTally.filter = Filter.rewrite pairs (MillTally.filter t)}) mTally) mSlot)
  Effect.Reveal (Reveal.MkReveal ref slot) -> Effect.Reveal (Reveal.MkReveal (rewriteObjectRef pairs ref) slot)
  Effect.LookAt (LookAt.MkLookAt ref slot) -> Effect.LookAt (LookAt.MkLookAt (rewriteObjectRef pairs ref) slot)
  Effect.Scry x -> Effect.Scry (rewritePlayerQuantity pairs x)
  Effect.Surveil x -> Effect.Surveil (rewritePlayerQuantity pairs x)
  Effect.Fateseal x -> Effect.Fateseal (rewritePlayerQuantity pairs x)
  Effect.Explore ref -> Effect.Explore (rewriteObjectRef pairs ref)
  -- The These arm's ref carries a Filter, so rule 612's text change reaches it
  -- exactly as Reveal's does; the Counted arm holds two slot NAMES and a count,
  -- and only the count is a word rule 612 can reach -- a slot name is not.
  Effect.Discard subject -> case subject of
    Discard.Counted x -> Effect.Discard (Discard.Counted x {CountedDiscard.quantity = rewriteQuantity pairs (CountedDiscard.quantity x)})
    Discard.These ref -> Effect.Discard (Discard.These (rewriteObjectRef pairs ref))
  Effect.LoseLife x -> Effect.LoseLife (rewritePlayerQuantity pairs x)
  Effect.GainLife x -> Effect.GainLife (rewritePlayerQuantity pairs x)
  Effect.ExchangeLifeTotals _ -> effect
  Effect.SetLifeTotal x -> Effect.SetLifeTotal (rewritePlayerQuantity pairs x)
  Effect.RedistributeLifeTotals -> effect
  Effect.IncreaseSpeed x -> Effect.IncreaseSpeed (rewritePlayerQuantity pairs x)
  Effect.DecreaseSpeed x -> Effect.DecreaseSpeed x {SpeedDecrease.quantity = rewriteQuantity pairs (SpeedDecrease.quantity x)}
  -- CR 612.2a: the token's creature types and its name are the same words, and
  -- they live in the defining card. The count and the riders' counter amounts are
  -- Quantities and take rewriteQuantity's descent, PutCounters' case below.
  --
  -- Not implemented: a CR 122.1b keyword counter named in the riders keeps its
  -- printed keyword (#1190).
  Effect.Create (Create.MkCreate quantity card riders slot creator) -> Effect.Create (Create.MkCreate (rewriteQuantity pairs quantity) (rewriteCard pairs card) (rewriteEntryRiders pairs riders) slot creator)
  Effect.Conjure (Conjure.MkConjure quantity card destination) -> Effect.Conjure (Conjure.MkConjure (rewriteQuantity pairs quantity) (rewriteCard pairs card) destination)
  -- CR 707.2 excludes text-changing effects from copiable values, so what the
  -- token becomes is not rewritten -- only the ref, the count and the riders'
  -- counter amounts are.
  --
  -- Not implemented: a CR 122.1b keyword counter named in the riders keeps its
  -- printed keyword, Create's arm above (#1190).
  Effect.CreateCopy (CreateCopy.MkCreateCopy quantity ref riders) -> Effect.CreateCopy (CreateCopy.MkCreateCopy (rewriteQuantity pairs quantity) (rewriteObjectRef pairs ref) (rewriteEntryRiders pairs riders))
  Effect.BecomeCopy (BecomeCopy.MkBecomeCopy original subject) ->
    Effect.BecomeCopy (BecomeCopy.MkBecomeCopy (rewriteObjectRef pairs original) (rewriteObjectRef pairs subject))
  -- BOTH refs, CreateCopy's reason: CR 707.2 keeps a text change out of the
  -- copiable values, so what the copy becomes is not rewritten, but CR 707.10d's
  -- description of the candidates ("each other creature you control") is card
  -- text like any other ref's. CR 707.10c's offer is no land type at all.
  Effect.CopyStackObject (CopyStackObject.MkCopyStackObject ref targets) ->
    Effect.CopyStackObject (CopyStackObject.MkCopyStackObject (rewriteObjectRef pairs ref) (rewriteCopyTargets pairs targets))
  -- CR 612.1 through the SHIELD a resolution installs: the row's duration, its
  -- CR 614.1 gate, and the replacement effect itself, which is where the word
  -- usually sits. rewritePrintedReplacement makes the same descent over a
  -- permanent's static row; a floating one is the same text one carrier over.
  -- CR 614.3's Uses is a count and CR 614.15's ReplacementOrigin is provenance,
  -- so neither holds a printed word, and both are passed through.
  --
  -- Destructured POSITIONALLY, the prevention arms' shape below: a new
  -- word-bearing field on the record is then a compile error here, where a record
  -- update over the whole value would have carried it through unrewritten and
  -- said nothing.
  --
  -- The EFFECT half is proved by Pawl.CounterspellSpec's evolved Moonmist, whose
  -- shield spares Goblins where the printed word says Werewolves. The DURATION
  -- and the CONDITION are REGRESSION FENCES rather than proved behaviours. The
  -- test a reader can re-run: scan data/cards/ for Effect.Replace and ask which
  -- rows hold a subtype in those two fields. On 2026-08-30 none did -- every
  -- duration was UntilEndOfTurn, which holds no word at all, and every printed
  -- CR 614.1 gate was a Compares over a count of one CARD TYPE, which rule 612.2
  -- does not swap -- so mutating either line away left the whole suite green. A
  -- card gating a shield on "if you control three or more Goblins" is what would
  -- prove them.
  Effect.Replace (Replace.MkReplace duration uses origin condition replacement) ->
    Effect.Replace (Replace.MkReplace (rewriteDuration pairs duration) uses origin (fmap (rewriteCondition pairs) condition) (rewriteReplacementEffect pairs replacement))
  Effect.SkipNextPhase {} -> effect
  -- CR 612.1 through every half of the shield that holds printed words: the
  -- objects it covers, the predicates describing its recipients and its source,
  -- the countdown's amount, the duration and the CR 615.5 rider. `kind`,
  -- `whoRecipient` and
  -- `direction` are not words a subtype swap can find -- a damage kind, CR
  -- 109.5's player relation and which side of the event the ref sits on.
  --
  -- Three of the Filters are PROVEN. PreventAllDamage's whatSource and
  -- PreventNextDamage's whatRecipient are proven by Pawl.ReplacementSpec's
  -- "Synthetic Warding Chant (CR 612.1)" group; PreventAllDamage's whatRecipient
  -- by that file's "Pack Leader (CR 611.2c)" group, where Artificial Evolution
  -- swaps the word before the attack trigger resolves. The refs and the two
  -- chosenSource fields are REGRESSION FENCES, TurnFaceDown's
  -- shape above: every ref data/cards writes at these positions is an InSlot, on
  -- which rewriteObjectRef is the identity, and every chosenSource it writes
  -- (Auriok Replica, Healing Grace, Samite Ministration) is the trivial
  -- `And []`, so mutating either line reddens nothing.
  Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage duration kind ref whatRecipient whoRecipient chosenSource quantity rider) ->
    Effect.PreventNextDamage (PreventNextDamage.MkPreventNextDamage (rewriteDuration pairs duration) kind (fmap (rewriteObjectRef pairs) ref) (fmap (Filter.rewrite pairs) whatRecipient) whoRecipient (fmap (Filter.rewrite pairs) chosenSource) (rewriteQuantity pairs quantity) (fmap (rewriteEffect pairs) rider))
  Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage duration kind ref whatRecipient direction chosenSource whatSource rider) ->
    Effect.PreventAllDamage (PreventAllDamage.MkPreventAllDamage (rewriteDuration pairs duration) kind (fmap (rewriteObjectRef pairs) ref) (fmap (Filter.rewrite pairs) whatRecipient) direction (fmap (Filter.rewrite pairs) chosenSource) (Filter.rewrite pairs whatSource) (fmap (rewriteEffect pairs) rider))
  -- CR 612.1 through every half of the redirection that holds printed words: the
  -- two ends of the rewrite, the predicate describing CR 609.7a's chosen source,
  -- and the duration. `kind` is not a word a subtype swap can find: it says
  -- combat (CR 510) or noncombat, which is a rules category rather than printed
  -- vocabulary.
  --
  -- Destructured POSITIONALLY, the prevention arms' shape directly above and
  -- Effect.Replace's above them: a new word-bearing field on the record is then a
  -- compile error here. Before the widening that filled these fields in, the arm
  -- matched `RedirectDamage {}` and rewrote nothing at all, which is how
  -- chosenSource came to be skipped in the first place; a record update over the
  -- whole value would have kept that failure mode for the next field.
  --
  -- Only chosenSource is PROVEN, by Pawl.ReplacementSpec's "Synthetic Turn the
  -- Blade (CR 612.1)" group. The two refs and the duration are REGRESSION
  -- FENCES, the neighbouring shields' shape: Turn the Tables writes an InSlot at
  -- each end and Oracle's Attendants an InSlot and an `EachMatching IsSource`, on
  -- all three of which rewriteObjectRef is the identity -- a slot name is not a
  -- word, and IsSource holds none -- and every redirect in data/cards/ writes
  -- UntilEndOfTurn, on which rewriteDuration is the identity too. So mutating any
  -- of those three lines reddens nothing.
  --
  -- Two halves outside that group, added with Harm's Way: the counted `amount`
  -- goes through rewriteQuantity, the shape PreventNextDamage's quantity takes
  -- above, and the recipient description `whatRecipient` through Filter.rewrite,
  -- which a subtype swap genuinely reaches -- a card covering "Goblins you
  -- control" would be its producer; Harm's Way's ControlledBy names no word, so
  -- both are regression fences too. `whoRecipient` is a PlayerRelation and
  -- holds no word.
  Effect.RedirectDamage (RedirectDamage.MkRedirectDamage duration kind amount from whatRecipient whoRecipient to chosenSource) ->
    Effect.RedirectDamage (RedirectDamage.MkRedirectDamage (rewriteDuration pairs duration) kind (fmap (rewriteQuantity pairs) amount) (fmap (rewriteObjectRef pairs) from) (fmap (Filter.rewrite pairs) whatRecipient) whoRecipient (rewriteObjectRef pairs to) (fmap (Filter.rewrite pairs) chosenSource))
  Effect.Counter (Counter.MkCounter ref mSlot mSources) -> Effect.Counter (Counter.MkCounter (rewriteObjectRef pairs ref) mSlot mSources)
  -- Not implemented: a CR 122.1b keyword counter named in the kind keeps its
  -- printed keyword through the swap, where Filter's HasCounters arm rewrites
  -- the same kind (#1840).
  Effect.PutCounters (PutCounters.MkPutCounters kind quantity ref) ->
    Effect.PutCounters (PutCounters.MkPutCounters kind (rewriteQuantity pairs quantity) (rewriteObjectRef pairs ref))
  -- The count is a Quantity and takes the same descent PutCounters' case above
  -- makes.
  --
  -- Not implemented: a CR 122.1b keyword counter named in the kind keeps its
  -- printed keyword through the swap, PutCounters' case above (#1840).
  Effect.RemoveCounters x -> Effect.RemoveCounters x {RemoveCounters.quantity = rewriteQuantity pairs (RemoveCounters.quantity x)}
  -- Only the destination descends: CR 122.8 names no count, so the ObjectRef is
  -- where a subtype word would otherwise hide.
  --
  -- Not implemented: a CR 122.1b keyword counter named in the kind -- rule
  -- 122.8's third sentence, which lets the card settle which kinds cross --
  -- keeps its printed keyword through the swap, PutCounters' case above (#1840).
  Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom from kind ref) ->
    Effect.PutCountersFrom (PutCountersFrom.MkPutCountersFrom from kind (rewriteObjectRef pairs ref))
  -- BOTH refs and the count. Filter.rewrite renames no slot, so the bound slot is
  -- not rewritten, but either ref may carry a filter -- Spike Cannibal's "all
  -- creatures" on the first side, Forgotten Ancient's "other creatures" on the
  -- second -- and a subtype word there is as changeable as any other (CR 612.1);
  -- the count is a Quantity and goes through rewriteMovedKinds, PutCounters' case
  -- above.
  -- Not implemented: a CR 122.1b keyword counter named in the kind keeps its
  -- printed keyword through the swap, PutCounters' case above (#1840).
  Effect.MoveCounters (MoveCounters.MkMoveCounters from kinds slot to) ->
    Effect.MoveCounters (MoveCounters.MkMoveCounters (rewriteObjectRef pairs from) (rewriteMovedKinds pairs kinds) slot (rewriteObjectRef pairs to))
  -- A player counter kind is a closed list (CR 122.1f, CR 122.1i, CR 107.14, and
  -- CR 122.1's bare first sentence) with no subtype word in it, so only the count
  -- descends.
  Effect.GainPlayerCounters x -> Effect.GainPlayerCounters x {PlayerCounters.quantity = rewriteQuantity pairs (PlayerCounters.quantity x)}
  Effect.RemovePlayerCounters x -> Effect.RemovePlayerCounters x {PlayerCounters.quantity = rewriteQuantity pairs (PlayerCounters.quantity x)}
  Effect.PayAnyEnergy _ -> effect
  Effect.Tap ref -> Effect.Tap (rewriteObjectRef pairs ref)
  Effect.Untap ref -> Effect.Untap (rewriteObjectRef pairs ref)
  Effect.Detain ref -> Effect.Detain (rewriteObjectRef pairs ref)
  Effect.Goad ref -> Effect.Goad (rewriteObjectRef pairs ref)
  Effect.MakePlotted ref -> Effect.MakePlotted (rewriteObjectRef pairs ref)
  Effect.DoesNotUntapNext ref -> Effect.DoesNotUntapNext (rewriteObjectRef pairs ref)
  Effect.Transform ref -> Effect.Transform (rewriteObjectRef pairs ref)
  Effect.Convert ref -> Effect.Convert (rewriteObjectRef pairs ref)
  -- CR 612.2a through the combined back face as well as the ref, Effect.Create's
  -- reason one opcode over: the face is card data the ability carries, and its
  -- words are the ability's words.
  Effect.Meld (Meld.MkMeld ref card) -> Effect.Meld (Meld.MkMeld (rewriteObjectRef pairs ref) (rewriteCard pairs card))
  Effect.PhaseOut ref -> Effect.PhaseOut (rewriteObjectRef pairs ref)
  Effect.AddPhases _ -> effect
  Effect.EndTurn -> effect
  Effect.EndCombatPhase -> effect
  Effect.GainControl (DurationRef.MkDurationRef duration ref) -> Effect.GainControl (DurationRef.MkDurationRef (rewriteDuration pairs duration) (rewriteObjectRef pairs ref))
  -- CR 612.1 through the only half that holds printed words: CR 603.7b's stated
  -- duration, whose "for as long as" clause is text like any other. The
  -- AbilityName is the arming effect's own pointer at a delayed ability and CR
  -- 603.7a's Onset is a moment, so neither is a word rule 612 can swap.
  --
  -- A REGRESSION FENCE rather than a proven behaviour: every ArmDelayedTrigger in
  -- data/cards/ writes either no duration or UntilEndOfTurn, on both of which
  -- rewriteDuration is the identity, so mutating this line reddens nothing.
  --
  -- Destructured POSITIONALLY, the shape Effect.Replace and Effect.RedirectDamage
  -- above take: a new word-bearing field on the record is then a compile error
  -- here, where the record update this arm used to be would have carried it
  -- through unrewritten and said nothing.
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name onset duration) ->
    Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger name onset (fmap (rewriteDuration pairs) duration))
  -- CR 612.1 through BOTH halves that hold printed text, the same descent every
  -- neighbouring arm here makes. The duration's "for as long as" clause is
  -- printed text, so a Magical Hack on the spell while it is on the stack changes
  -- which word the clause counts; the restriction's own Filter is printed text
  -- too, so an Artificial Evolution on Liliana, Untouched by Death changes which
  -- graveyard cards her -3 lets its controller cast. The players axis names no
  -- word -- an AffectedPlayers is a PlayerScope or a SlotName, and neither is a
  -- subtype.
  --
  -- rewritePlayerEffect is the SAME function the printed carrier's copy of that
  -- Filter goes through (Pawl.Engine.PlayerEffect.printedRows), which is why it
  -- lives beside rewriteModification rather than there.
  Effect.AffectPlayers x ->
    Effect.AffectPlayers
      x
        { AffectPlayers.duration = rewriteDuration pairs (AffectPlayers.duration x),
          AffectPlayers.effect = rewritePlayerEffect pairs (AffectPlayers.effect x)
        }
  Effect.RequireBlock (RequireBlock.MkRequireBlock duration blocker attacker) ->
    Effect.RequireBlock (RequireBlock.MkRequireBlock (rewriteDuration pairs duration) (rewriteObjectRef pairs blocker) (rewriteObjectRef pairs attacker))
  Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated duration ref) ->
    Effect.CantBeRegenerated (CantBeRegenerated.MkCantBeRegenerated (rewriteDuration pairs duration) (rewriteObjectRef pairs ref))
  Effect.ForbidBlock (ForbidBlock.MkForbidBlock duration ref) ->
    Effect.ForbidBlock (ForbidBlock.MkForbidBlock (rewriteDuration pairs duration) (rewriteObjectRef pairs ref))
  Effect.ForbidActivation (ForbidActivation.MkForbidActivation duration ref) ->
    Effect.ForbidActivation (ForbidActivation.MkForbidActivation (rewriteDuration pairs duration) (rewriteObjectRef pairs ref))
  -- CR 612.1 reaches the creatures' words on either arm -- a Named ref's Filters
  -- and a Matching class's -- and not the AimedAt: a PlayerScope prints no word
  -- a text-changing effect reaches, and the kinds are CR 506.3's list.
  Effect.ForbidAttack (ForbidAttack.MkForbidAttack duration affected aimedAt) ->
    let rewritten = case affected of
          RestrictedCreatures.Named ref -> RestrictedCreatures.Named (rewriteObjectRef pairs ref)
          RestrictedCreatures.Matching f -> RestrictedCreatures.Matching (Filter.rewrite pairs f)
     in Effect.ForbidAttack (ForbidAttack.MkForbidAttack (rewriteDuration pairs duration) rewritten aimedAt)
  -- CR 612.1 reaches the OBJECT axis's ref and not its player: a word swap
  -- changes card text, and the defender clause of Alluring Siren's sentence is
  -- "you" rather than any word a Filter could name.
  Effect.RequireAttack (RequireAttack.MkRequireAttack duration attacker defender) ->
    Effect.RequireAttack (RequireAttack.MkRequireAttack (rewriteDuration pairs duration) (rewriteObjectRef pairs attacker) defender)
  -- CR 114.3 leaves an emblem no type line and no name, so its ABILITIES are the
  -- whole of what CR 612.1 can reach.
  Effect.CreateEmblem card -> Effect.CreateEmblem (rewriteCard pairs card)
  Effect.BecomeMonarch {} -> effect
  Effect.TakeTheInitiative {} -> effect
  Effect.Designate (Designate.MkDesignate _ _) -> effect
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> effect
  Effect.Unsuspect ref -> Effect.Unsuspect (rewriteObjectRef pairs ref)
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> effect
  Effect.Evolve _ -> effect
  Effect.Mentor _ -> effect
  Effect.Train _ -> effect
  Effect.ItBecomes _ -> effect
  Effect.ExileUntilMonarch _ -> effect
  Effect.ExileHaunting {} -> effect
  Effect.Attach _ -> effect
  Effect.AttachTarget (AttachTarget.MkAttachTarget slot filter_) -> Effect.AttachTarget (AttachTarget.MkAttachTarget slot (Filter.rewrite pairs filter_))
  Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot filter_) -> Effect.AttachTargetToEach (AttachTarget.MkAttachTarget slot (Filter.rewrite pairs filter_))
  -- No Filter to rewrite: both fields are slot names.
  Effect.AttachBound {} -> effect
  Effect.PlaySubgame _ -> effect
  Effect.ChoosePlayer _ -> effect
  Effect.ChooseOpponentAtRandom _ -> effect
  -- CR 706.1's number of sides is a numeral rather than a computed count; how
  -- many dice, and the modifier added to each result, are the Quantities,
  -- PutCounters' descent above. The slots no word rule 612 can swap.
  Effect.RollDie x ->
    Effect.RollDie
      x
        { RollDie.count = rewriteQuantity pairs (RollDie.count x),
          RollDie.modifier = fmap (rewriteQuantity pairs) (RollDie.modifier x)
        }
  -- The number of coins is the Quantity, PutCounters' descent above; the reading
  -- and the slot name no word rule 612 can swap.
  Effect.FlipCoin x -> Effect.FlipCoin x {FlipCoin.count = rewriteQuantity pairs (FlipCoin.count x)}
  -- The number of turns is the Quantity, FlipCoin's descent above; the
  -- PlayerRef and the skips no word rule 612 can swap.
  Effect.TakeExtraTurn x -> Effect.TakeExtraTurn x {TakeExtraTurn.count = rewriteQuantity pairs (TakeExtraTurn.count x)}
  Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named ref) -> Effect.ShuffleIntoLibrary (ShuffleIntoLibrary.MkShuffleIntoLibrary named (rewriteObjectRef pairs ref))
  -- No ObjectRef to rewrite: the opcode names a library and no objects.
  Effect.Shuffle {} -> effect
  -- The ObjectRef alone: rule 612 swaps words in a card's TEXT, and a Filter the
  -- reference carries is card text (GrantPlayFromExile's arm below). The caster is
  -- a PlayerRef and the riders are CR 118.9's and CR 712.11a's, neither of which
  -- is a word rule 612 can name.
  Effect.OfferCast offer -> Effect.OfferCast offer {OfferCast.ref = rewriteObjectRef pairs (OfferCast.ref offer)}
  Effect.GrantPlayFromExile grant ->
    Effect.GrantPlayFromExile
      grant
        { GrantPlayFromExile.duration = rewriteDuration pairs (GrantPlayFromExile.duration grant),
          GrantPlayFromExile.ref = rewriteObjectRef pairs (GrantPlayFromExile.ref grant)
        }
  Effect.ForEach (ForEach.MkForEach ref slot body) ->
    Effect.ForEach (ForEach.MkForEach (rewriteObjectRef pairs ref) slot (fmap (rewriteEffect pairs) body))

-- CR 612.2 over one word whose family a card's text names rather than a
-- constructor -- a ChangeText's forbidden-word set.
swapWordIn :: SubtypeFamily.SubtypeFamily -> [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Subtype.Type.Subtype -> Subtype.Type.Subtype
swapWordIn family pairs word = List.foldl' step word pairs
  where
    step s (from, to) = if s == from && Subtype.inFamily family from then to else s

-- CR 612.1 through an ObjectRef. An InSlot names an object chosen at cast time,
-- and the player-naming arms hold no subtype word; only the Filters and the
-- Quantities can carry one -- the Filter that says what ends a walk of a library
-- included, and ChosenCardFromAmong's and RandomCardInHand's counts beside the
-- two library walks'.
rewriteObjectRef :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ObjectRef.ObjectRef -> ObjectRef.ObjectRef
rewriteObjectRef pairs ref = case ref of
  ObjectRef.InSlot _ -> ref
  ObjectRef.EachMatching f -> ObjectRef.EachMatching (Filter.rewrite pairs f)
  ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard s f) -> ObjectRef.EachCardInGraveyard (EachCardInGraveyard.MkEachCardInGraveyard s (Filter.rewrite pairs f))
  ObjectRef.EachCardInYourHand -> ref
  ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand s f) -> ObjectRef.EachCardInHand (EachCardInHand.MkEachCardInHand s (fmap (Filter.rewrite pairs) f))
  -- A REGRESSION FENCE rather than proven behaviour: the one printing that
  -- states this Filter is under a triggered ability no card in the pool changes
  -- the text of, so dropping the rewrite here leaves the suite green.
  ObjectRef.EachCardInYourLibrary f -> ObjectRef.EachCardInYourLibrary (fmap (Filter.rewrite pairs) f)
  ObjectRef.EachCardExiledWithSource f -> ObjectRef.EachCardExiledWithSource (fmap (Filter.rewrite pairs) f)
  ObjectRef.EachSpell f -> ObjectRef.EachSpell (Filter.rewrite pairs f)
  ObjectRef.EachOnStack f -> ObjectRef.EachOnStack (Filter.rewrite pairs f)
  ObjectRef.EachPlayer -> ref
  ObjectRef.EachOpponent -> ref
  ObjectRef.ChosenPlayer -> ref
  ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary p c) -> ObjectRef.TopOfLibrary (TopOfLibrary.MkTopOfLibrary p (rewriteQuantity pairs c))
  ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil p f c) -> ObjectRef.TopOfLibraryUntil (TopOfLibraryUntil.MkTopOfLibraryUntil p (Filter.rewrite pairs f) (rewriteQuantity pairs c))
  ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard c s f) -> ObjectRef.ChosenCardInGraveyard (ChosenCardInGraveyard.MkChosenCardInGraveyard c s (Filter.rewrite pairs f))
  ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand p f) -> ObjectRef.ChosenCardInHand (ChosenCardInHand.MkChosenCardInHand p (Filter.rewrite pairs f))
  ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong n f c w) -> ObjectRef.ChosenCardFromAmong (ChosenCardFromAmong.MkChosenCardFromAmong n (Filter.rewrite pairs f) (rewriteQuantity pairs c) w)
  ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong n f) -> ObjectRef.EachCardFromAmong (EachCardFromAmong.MkEachCardFromAmong n (Filter.rewrite pairs f))
  -- A REGRESSION FENCE rather than proven behaviour, EachCardInYourLibrary's
  -- reason: no card in the pool narrows a random pick by a land type, and no
  -- printing changes the text of the ones that write this ref.
  ObjectRef.RandomCardInHand (RandomCardInHand.MkRandomCardInHand p f c) -> ObjectRef.RandomCardInHand (RandomCardInHand.MkRandomCardInHand p (Filter.rewrite pairs f) (rewriteQuantity pairs c))
  ObjectRef.AnyNumberMatching f -> ObjectRef.AnyNumberMatching (Filter.rewrite pairs f)
  ObjectRef.ChosenPermanent f -> ObjectRef.ChosenPermanent (Filter.rewrite pairs f)
  ObjectRef.SourceAndChosenPermanent f -> ObjectRef.SourceAndChosenPermanent (Filter.rewrite pairs f)

-- CR 612.1 through CR 707.10d's description of the copies' candidates, which is
-- card text like any other ref's. The other two answers name nothing a land-type
-- swap can find.
rewriteCopyTargets :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> CopyTargets.CopyTargets -> CopyTargets.CopyTargets
rewriteCopyTargets pairs targets = case targets of
  CopyTargets.Copied -> targets
  CopyTargets.ChosenByController -> targets
  CopyTargets.ForEach ref -> CopyTargets.ForEach (rewriteObjectRef pairs ref)
  CopyTargets.Stated ref -> CopyTargets.Stated (rewriteObjectRef pairs ref)

-- CR 612.1/612.2a through the CARD an Effect.Create or an Effect.CreateEmblem
-- defines its token or emblem with: the type line, the name, and the rules text.
-- The NAME's change is gated on the type line, since CR 612.2a licenses it only
-- where the word is used as a creature type. The RULES TEXT is walked
-- unconditionally, and an emblem is why: CR 114.3 leaves it no type line at all.
-- Recursive, and terminating because a Card is a finite first-order value and
-- every step descends into a strict subterm. Every FACE, since a card's printed
-- subtypes, name and text are per-face (CR 712.8).
rewriteCard :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Card.Type.Card -> Card.Type.Card
rewriteCard pairs card = card {Card.Type.faces = fmap (rewriteFace pairs) (Card.Type.faces card)}

rewriteFace :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Face.Face Card.Type.Card -> Face.Face Card.Type.Card
rewriteFace pairs face = List.foldl' apply1 face pairs
  where
    apply1 f (from, to) =
      let pair = [(from, to)]
          typeLine = Face.typeLine f
          subtypes = TypeLine.subtypes typeLine
          renamed =
            if Set.notMember from subtypes
              then f
              else
                f
                  { Face.typeLine = typeLine {TypeLine.subtypes = Set.insert to (Set.delete from subtypes)},
                    Face.name = rewriteTokenName from to (Face.name f)
                  }
       in renamed
            { -- CR 702.14a's land-type word inside a landwalk. Set.map rather
              -- than Map.mapKeysWith (+), since a face's keywords are a Set.
              Face.keywords = Set.map (Filter.rewriteKeyword pair) (Face.keywords renamed),
              -- CR 208.2a's star, unevaluated as at layer 3.
              Face.characteristicPT = fmap (rewriteCharacteristicPT pair) (Face.characteristicPT renamed),
              -- CR 101.1's ceiling on X, whose Quantity can Count a criterion
              -- naming a land type word. A regression fence: neither printing
              -- pairs a bounded X with one -- both say "the greatest toughness
              -- among creatures you control" -- so no test can falsify it.
              Face.maximumX = fmap (rewriteQuantity pair) (Face.maximumX renamed),
              Face.spell = rewriteModal pair (Face.spell renamed),
              Face.activatedAbilities = fmap (rewriteActivatedAbility pair) (Face.activatedAbilities renamed),
              -- CR 604.2's static ability, on the card a token or emblem is
              -- defined with. A regression fence rather than a proved behaviour,
              -- as Face.maximumX above is: a walk of every defined face in
              -- data/cards for a replacementEffects key (2026-08-21) found none
              -- at all, so no board can tell this line from its absence.
              -- Pawl.ReplacementSpec's Dragonstorm Globe case is what proves
              -- rewritePrintedReplacement itself.
              Face.replacementEffects = fmap (rewritePrintedReplacement pair) (Face.replacementEffects renamed),
              Face.triggeredAbilities = fmap (rewriteTriggeredAbility pair) (Face.triggeredAbilities renamed),
              Face.delayedAbilities = fmap (rewriteTriggeredAbility pair) (Face.delayedAbilities renamed),
              Face.staticAbilities = fmap (rewriteStaticAbility pair) (Face.staticAbilities renamed)
            }

-- A whole static ability under CR 612.1, for the defined-card walk above. A
-- permanent's own statics are reached piecemeal instead, being read per layer.
rewriteStaticAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> StaticAbility.StaticAbility Card.Type.Card -> StaticAbility.StaticAbility Card.Type.Card
rewriteStaticAbility pairs sa =
  sa
    { StaticAbility.affected = rewriteAffected pairs (StaticAbility.affected sa),
      StaticAbility.condition = fmap (rewriteCondition pairs) (StaticAbility.condition sa),
      StaticAbility.modifications = fmap (rewriteModification pairs) (StaticAbility.modifications sa)
    }

-- CR 612.2a's name half, gated on both words being creature types -- CR 612.2
-- prohibits every other family. Text.replace, since CR 111.4's derived name
-- holds one word per subtype; it matches a substring, so a name holding the word
-- inside a longer one is over-reached (#644).
rewriteTokenName :: Subtype.Type.Subtype -> Subtype.Type.Subtype -> CardName.CardName -> CardName.CardName
rewriteTokenName from to name = case (Subtype.creatureTypeWord from, Subtype.creatureTypeWord to) of
  (Just f, Just t) -> CardName.MkCardName (Text.replace f t (CardName.unwrap name))
  _ -> name

-- CR 612.1 over an ACTIVATED ability printed on a permanent: the payload, CR
-- 702.178a's "as long as" gate, and the ACTIVATION COST (CR 118.1, CR 602.1a),
-- so a Magical Hack naming Forest moves which land Dark Heart of the Wood's cost
-- demands.
rewriteActivatedAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteActivatedAbility pairs ability =
  ability
    { ActivatedAbility.modal = rewriteModal pairs (ActivatedAbility.modal ability),
      ActivatedAbility.condition = fmap (rewriteCondition pairs) (ActivatedAbility.condition ability),
      ActivatedAbility.cost = Filter.rewriteCost pairs (ActivatedAbility.cost ability)
    }

-- CR 612.1 over a GRANTED ability (CR 613.1f), whichever of CR 113.3's two kinds
-- it is. The words are printed on the GRANTER, so a text change affecting that
-- permanent rewrites them before layer 6 hands them over.
rewriteGrantedAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> GrantedAbility.GrantedAbility Card.Type.Card -> GrantedAbility.GrantedAbility Card.Type.Card
rewriteGrantedAbility pairs granted = case granted of
  GrantedAbility.Activated a -> GrantedAbility.Activated (rewriteActivatedAbility pairs a)
  GrantedAbility.Triggered t -> GrantedAbility.Triggered (rewriteTriggeredAbility pairs t)

-- CR 612.1 over a TRIGGERED ability printed on a permanent. Three parts, not
-- just the payload: the CR 603.8 condition is where the word usually is, and CR
-- 603.4's intervening "if" shares rewriteCondition.
rewriteTriggeredAbility :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteTriggeredAbility pairs ability =
  ability
    { TriggeredAbility.condition = rewriteTriggerCondition pairs (TriggeredAbility.condition ability),
      TriggeredAbility.intervening = fmap (rewriteCondition pairs) (TriggeredAbility.intervening ability),
      TriggeredAbility.modal = rewriteModal pairs (TriggeredAbility.modal ability)
    }

-- CR 612.1 over a REPLACEMENT effect printed on a permanent, which CR 604.2
-- makes a static ability's continuous effect and so text in the same text box as
-- a triggered ability's. Two carriers: the ability's own "as long as" clause, and
-- the effect itself.
rewritePrintedReplacement :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> PrintedReplacement.PrintedReplacement Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> PrintedReplacement.PrintedReplacement Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewritePrintedReplacement pairs printed =
  printed
    { PrintedReplacement.condition = fmap (rewriteCondition pairs) (PrintedReplacement.condition printed),
      PrintedReplacement.effect = rewriteReplacementEffect pairs (PrintedReplacement.effect printed)
    }

-- CR 612.1 through the replacement effect itself: the EVENT PATTERN saying which
-- objects it watches, and the REWRITE it applies to one. Exhaustive rather than a
-- wildcard, in rewriteTriggerCondition's posture -- a later arm carrying a word
-- fails to compile here instead of silently keeping the printed one.
--
-- Classification, not identity: every arm is a CR 614.1 event class, and the
-- descent is by the field shapes those classes carry.
rewriteReplacementEffect :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> ReplacementEffect Card.Type.Card (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteReplacementEffect pairs effect = case effect of
  -- CR 400.3's owner and the destination Zone name no word; the moving object's
  -- Filter does.
  ReplacementEffect.ZoneChangeR r ->
    ReplacementEffect.ZoneChangeR
      r
        { ZoneChangeR.matching =
            (ZoneChangeR.matching r)
              { ZoneChangePattern.whatObject = Filter.rewrite pairs (ZoneChangePattern.whatObject (ZoneChangeR.matching r))
              }
        }
  ReplacementEffect.EntryR r ->
    ReplacementEffect.EntryR
      r
        { EntryR.matching = Filter.rewrite pairs (EntryR.matching r),
          EntryR.rewrite = rewriteEntryRewrite pairs (EntryR.rewrite r)
        }
  -- The pattern's two Filters, CR 614.9's printed destination, and CR 615.5's
  -- riders. The pattern's DamageKind, its PlayerRelation, its Recipient and its
  -- ObjectId are a rules category, a CR 109.5 relation and two baked identities,
  -- so none of the four holds a printed word.
  ReplacementEffect.DamageR r ->
    ReplacementEffect.DamageR
      r
        { DamageR.matching =
            (DamageR.matching r)
              { DamagePattern.whatSource = Filter.rewrite pairs (DamagePattern.whatSource (DamageR.matching r)),
                DamagePattern.whatRecipient = fmap (Filter.rewrite pairs) (DamagePattern.whatRecipient (DamageR.matching r))
              },
          DamageR.rewrite = rewriteDamageRewrite pairs (DamageR.rewrite r),
          DamageR.riders = fmap (rewriteEffect pairs) (DamageR.riders r)
        }
  -- CR 701.19a's regeneration and CR 122.1c's shield: two nullary rewrites with
  -- no pattern beside them, so there is nothing to swap. CR 122.1d's untap
  -- replacement is the same shape one event class over.
  ReplacementEffect.DestructionR _ -> effect
  ReplacementEffect.UntapR _ -> effect
  -- CR 614.1a / 120.4c: a LifeLossPattern is one CR 109.5 relation and one cause,
  -- and no arm of the rewrite names a Filter or a card. No printed word, so
  -- nothing to swap.
  ReplacementEffect.LifeLossR {} -> effect
  -- CR 614.1a / 119.10: a LifeGainR is one CR 109.5 relation and one scaling.
  -- No printed word, so nothing to swap.
  ReplacementEffect.LifeGainR {} -> effect
  -- A DrawR's pattern is one CR 109.5 relation, but the REWRITE can hold CR 400.11c's
  -- wish filter, which rewriteEffect's own Effect.FromOutsideTheGame arm swaps on
  -- the resolution road -- so it has to be swapped here too, or the same sentence
  -- would read one way as a spell and another as a floating row.
  ReplacementEffect.DrawR r -> ReplacementEffect.DrawR r {DrawR.rewrite = rewriteDrawRewrite pairs (DrawR.rewrite r)}
  -- A DrawCountR is one CR 109.5 relation, one count and one nullary rewrite; no
  -- Filter and no card, so no printed word to swap. DrawR's answer.
  ReplacementEffect.DrawCountR {} -> effect
  ReplacementEffect.CounterR r ->
    ReplacementEffect.CounterR
      r
        { CounterR.matching =
            (CounterR.matching r)
              { CounterPattern.whichKind = fmap (Filter.rewriteCounterKind pairs) (CounterPattern.whichKind (CounterR.matching r)),
                CounterPattern.onWhat = Filter.rewrite pairs (CounterPattern.onWhat (CounterR.matching r))
              }
        }
  -- CR 612.1 through both printed halves: the pattern's Filter over what the
  -- token is (Queen Allenal of Ruadach's "creature tokens") and the appended
  -- token's own text, rewriteCard's descent as for a Create's token.
  ReplacementEffect.TokenR r ->
    ReplacementEffect.TokenR
      r
        { TokenR.matching = (TokenR.matching r) {TokenPattern.whatToken = Filter.rewrite pairs (TokenPattern.whatToken (TokenR.matching r))},
          TokenR.plus = fmap (rewriteCard pairs) (TokenR.plus r)
        }
  ReplacementEffect.TurnUpR r ->
    ReplacementEffect.TurnUpR
      r
        { TurnUpR.matching = Filter.rewrite pairs (TurnUpR.matching r),
          TurnUpR.rewrite = case TurnUpR.rewrite r of
            TurnUpRewrite.WithCounters w -> TurnUpRewrite.WithCounters (rewriteWithCounters pairs w)
            TurnUpRewrite.MayAttachTo f -> TurnUpRewrite.MayAttachTo (Filter.rewrite pairs f)
        }
  -- A PhasePattern is a PhaseSelector and a baked seat (CR 614.1b / 500.11), both
  -- rules categories rather than printed words.
  ReplacementEffect.PhaseR _ -> effect

-- CR 612.1 through a damage REWRITE, rewriteEntryRewrite's twin one event class
-- over. Only CR 614.9's printed destination holds a Filter for a word to sit in;
-- the rest are numbers, a Scaling and a baked Recipient.
--
-- NO BOARD OBSERVES IT: the pool's one printed destination is
-- Filter.IsHostOfSource (Pariah), which names no subtype for CR 612.1 to swap,
-- so mutating this arm away leaves the suite green. The arm is the rule rather
-- than a proven behaviour -- a card redirecting to "the enchanted Goblin" would
-- be what proves it. Exhaustive rather than a wildcard,
-- rewriteReplacementEffect's posture.
rewriteDamageRewrite :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> DamageRewrite.DamageRewrite -> DamageRewrite.DamageRewrite
rewriteDamageRewrite pairs rewrite = case rewrite of
  DamageRewrite.RedirectMatching f -> DamageRewrite.RedirectMatching (Filter.rewrite pairs f)
  DamageRewrite.Redirect _ -> rewrite
  DamageRewrite.RedirectNext _ _ -> rewrite
  DamageRewrite.PreventAll -> rewrite
  DamageRewrite.PreventRemovingShieldCounter -> rewrite
  DamageRewrite.PreventNext _ -> rewrite
  DamageRewrite.PreventAllBut _ -> rewrite
  DamageRewrite.SetAmount _ -> rewrite
  DamageRewrite.Scale _ -> rewrite

-- CR 612.1 through what a CR 614.1a draw replacement does. The wish filter is the
-- one printed word a draw rewrite can hold, and it is the SAME Filter
-- rewriteEffect's Effect.FromOutsideTheGame arm swaps, so the two roads into the
-- game agree about what a text change did (CR 400.11c).
--
-- NO BOARD OBSERVES IT, rewriteDamageRewrite's position above: the pool's one
-- draw-replacing wish is Ring of Ma'rûf, whose filter is @And []@ and names no
-- subtype for CR 612.1 to swap, so neutralising the Filter.rewrite here leaves the
-- suite green. The arm is the rule rather than a proven behaviour -- a card
-- replacing a draw with "a Goblin card you own from outside the game", under a
-- text-changing effect, would be what proves it. Exhaustive rather than a
-- wildcard, rewriteReplacementEffect's posture.
rewriteDrawRewrite :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> DrawRewrite.DrawRewrite -> DrawRewrite.DrawRewrite
rewriteDrawRewrite pairs rewrite = case rewrite of
  DrawRewrite.GainLife _ -> rewrite
  DrawRewrite.FromOutsideTheGame payload -> DrawRewrite.FromOutsideTheGame payload {FromOutsideTheGame.filter = Filter.rewrite pairs (FromOutsideTheGame.filter payload)}

-- CR 612.1 through CR 707.9's "except ..." clause. Exhaustive for
-- rewriteReplacementEffect's reason.
--
-- NO BOARD OBSERVES IT, the ChoiceByCoinFlip arm's position below: only a
-- keyword that CARRIES a word changes, and neither producer of the CR 707.9a arm
-- names one (Dack's Duplicate grants haste and dethrone, Omni-Changeling
-- changeling). The arm is the rule rather than a proven behaviour -- an
-- "except it has islandwalk" would be what proves it. Neither of CR 707.9b's
-- arms names a word either: the pair is two literals, and the type clause names
-- CR 205.2a's card types, which CR 612.2's subtype swap does not reach.
rewriteCopyException :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> CopyException.CopyException -> CopyException.CopyException
rewriteCopyException pairs exception = case exception of
  CopyException.SetPowerToughness _ -> exception
  CopyException.GainKeywords keywords -> CopyException.GainKeywords (Set.map (Filter.rewriteKeyword pairs) keywords)
  -- CR 707.9b's type clause names CARD types (CR 205.2a's list), and CR 612.2's
  -- swap reaches only subtypes, so there is nothing here for a pair to change.
  CopyException.AddCardTypes _ -> exception

-- CR 612.1 through what a CR 614.1c/614.1d entry replacement does. Exhaustive for
-- rewriteReplacementEffect's reason.
rewriteEntryRewrite :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> EntryRewrite.EntryRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> EntryRewrite.EntryRewrite (Effect.Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
rewriteEntryRewrite pairs rewrite = case rewrite of
  -- The "which permanents" clause names a word, and so does CR 707.9a's gained
  -- ability -- a keyword, and a keyword may carry one (rewriteCopyException).
  EntryRewrite.AsCopy c ->
    EntryRewrite.AsCopy
      c
        { AsCopy.eligible = Filter.rewrite pairs (AsCopy.eligible c),
          AsCopy.exceptions = fmap (rewriteCopyException pairs) (AsCopy.exceptions c)
        }
  -- CR 702.14a's word again, this time inside a keyword an option grants.
  EntryRewrite.ChoiceOf os -> EntryRewrite.ChoiceOf (fmap (\o -> o {EntryOption.keywords = Set.map (Filter.rewriteKeyword pairs) (EntryOption.keywords o)}) os)
  -- The same word again, in an option a coin picks rather than a player. NO
  -- BOARD OBSERVES IT, and cannot: rewriteKeyword only ever changes a keyword
  -- that CARRIES a word (landwalk, typecycling, hexproof from, protection from),
  -- and Molten Sentry's two options grant haste and defender, which carry none.
  -- The arm is the rule (CR 612.1) rather than a proven behaviour -- an option
  -- granting islandwalk would be what proves it.
  EntryRewrite.ChoiceByCoinFlip f ->
    let rewriteOption o = o {EntryOption.keywords = Set.map (Filter.rewriteKeyword pairs) (EntryOption.keywords o)}
     in EntryRewrite.ChoiceByCoinFlip f {EntryFlip.heads = rewriteOption (EntryFlip.heads f), EntryFlip.tails = rewriteOption (EntryFlip.tails f)}
  -- CR 105.1's five colours, CR 305.6's five basic land types and CR 102.1's
  -- seats are the offers themselves, so none of the three prints a word the card
  -- chose.
  EntryRewrite.ChooseColor -> rewrite
  EntryRewrite.ChooseBasicLandType -> rewrite
  EntryRewrite.ChoosePlayer -> rewrite
  -- CR 702.155b's offer is a range of NUMBERS, read off the entering Saga's own
  -- chapter symbols (CR 714.2d), so the rewrite prints no word either.
  EntryRewrite.ReadAhead -> rewrite
  -- CR 201.4a's restriction on which names may be named.
  EntryRewrite.ChooseCardNames f -> EntryRewrite.ChooseCardNames (Filter.rewrite pairs f)
  EntryRewrite.ChooseCardName f -> EntryRewrite.ChooseCardName (Filter.rewrite pairs f)
  EntryRewrite.WithCounters w -> EntryRewrite.WithCounters (rewriteWithCounters pairs w)
  -- CR 702.14a's word once more, this time in a keyword the entry clause grants
  -- outright. Latent for ChoiceByCoinFlip's reason: Faerie Squadron grants flying,
  -- which carries no word, and an entry clause granting islandwalk would be what
  -- proves it.
  EntryRewrite.WithKeywords ks -> EntryRewrite.WithKeywords (Set.map (Filter.rewriteKeyword pairs) ks)
  EntryRewrite.UnderSourceControl -> rewrite
  EntryRewrite.SacrificeAnyNumber s ->
    EntryRewrite.SacrificeAnyNumber
      s
        { SacrificeAnyNumber.filter = Filter.rewrite pairs (SacrificeAnyNumber.filter s),
          SacrificeAnyNumber.kind = fmap (Filter.rewriteCounterKind pairs) (SacrificeAnyNumber.kind s)
        }
  -- CR 614.1c's "exile a [matching] card from your graveyard": Living Lore's says
  -- instant or sorcery, card types CR 612.1 reaches. Latent for RevealOrTapped's
  -- reason -- that EntryR matches Filter.IsSource, and CR 400.7 forbids carrying a
  -- text change onto the permanent before it entered.
  EntryRewrite.ExileFromGraveyard f -> EntryRewrite.ExileFromGraveyard (Filter.rewrite pairs f)
  -- Rules 702.136a, 702.98a and 702.54a state these three whole, bloodthirst's
  -- number included, so the card prints no word for CR 612.1 to reach.
  EntryRewrite.Riot -> rewrite
  EntryRewrite.Unleash -> rewrite
  EntryRewrite.Bloodthirst _ -> rewrite
  -- CR 702.150a states compleated whole, the symbol count being CR 118.13a's
  -- announced payment rather than card text, so there is no word here for CR
  -- 612.1 either.
  EntryRewrite.Compleated _ -> rewrite
  -- CR 614.1d's bare "enters tapped", and the life total CR 614.1c's alternative
  -- to it asks for: a tap status and a number.
  EntryRewrite.Tapped -> rewrite
  EntryRewrite.PayLifeOrTapped _ -> rewrite
  -- CR 614.1c's "reveal a [matching] card": Rustic Clachan's says Kithkin, a
  -- creature type word CR 612.2 licenses. Latent all the same: that EntryR
  -- matches Filter.IsSource, so a text change would have to be on the permanent
  -- before it entered, and CR 400.7 is what forbids carrying one there.
  EntryRewrite.RevealOrTapped f -> EntryRewrite.RevealOrTapped (Filter.rewrite pairs f)
  EntryRewrite.EntersTransformed -> rewrite
  -- CR 614.1c's "as this enters, [do something]", the payload shared with an
  -- ability's clauses.
  EntryRewrite.RunEffects es -> EntryRewrite.RunEffects (fmap (rewriteEffect pairs) es)

-- CR 122.1b's keyword counter is the one counter kind holding a word. The amount
-- holds a second, on the same axis Effect.PutCounters' quantity does: CR 614.1c
-- admits "a number of +1/+1 counters equal to the number of creature cards in all
-- graveyards" (Undergrowth Scavenger), and a Count inside it names a card type or
-- a subtype CR 612.2 licenses swapping.
--
-- EVERY KIND on the row, since the row carries a map of them, see #2314.
-- 'Map.mapKeysWith' and not 'Map.mapKeys': the swap can map two kinds onto one --
-- a hexproof from Islands counter and a hexproof from Swamps counter, hacked
-- Island -> Swamp -- and 'Map.mapKeys' would keep an arbitrary survivor, losing a
-- row. CR 122.1's last sentence is why merging is what the rules ask for rather
-- than a convenience: counters with the same name are interchangeable, so the two
-- rows are one tally of that one kind. Projection's
-- Modification.ChangeSubtypeWord arm above guards the same hazard the same way,
-- over a permanent's projected keywords instead of an entry row.
--
-- The combiner is Quantity.Plus, CR 208.2's composition: it needs no Num instance
-- (Pawl.Types.Quantity has none, deliberately), and it leaves the two amounts
-- unevaluated exactly as the rewrite found them, so a Count that must be read
-- against a board is still read against the board the entry happens on.
-- 'Map.mapKeysWith' passes the value at the GREATER of the two original keys
-- first, so the map's own ascending order is what Plus's left and right hold --
-- addition commutes (Pawl.Types.Plus), so this reads true rather than computing
-- anything different.
rewriteWithCounters :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> WithCounters.WithCounters -> WithCounters.WithCounters
rewriteWithCounters pairs w =
  WithCounters.MkWithCounters
    . Map.mapKeysWith (\greater lesser -> Quantity.Type.Plus (Plus.MkPlus lesser greater)) (Filter.rewriteCounterKind pairs)
    . fmap (rewriteQuantity pairs)
    $ WithCounters.counters w

-- The modal payload both abilities carry. Both halves of a mode: its clauses'
-- effects, and its TARGET SLOTS, whose Filter is the candidate set CR 601.2c
-- (imported by CR 602.2b) reads. The Pool is not an omission: it names a rules
-- category (CR 115) rather than a word printed on the card.
rewriteModal :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Modal.Type.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Modal.Type.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)
rewriteModal pairs modal =
  let rewriteClause c =
        c
          { Clause.effects = fmap (rewriteEffect pairs) (Clause.effects c),
            Clause.payGate = fmap (rewritePayGate pairs) (Clause.payGate c),
            Clause.condition = fmap (rewriteCondition pairs) (Clause.condition c)
          }
      rewriteMode m =
        m
          { Mode.clauses = fmap rewriteClause (Mode.clauses m),
            Mode.targetSlots = fmap (rewriteTargetSlot pairs) (Mode.targetSlots m)
          }
   in modal {Modal.Type.modes = fmap rewriteMode (Modal.Type.modes modal)}

-- CR 612.1 through the cost a clause offers as it resolves (CR 118.12).
-- Lithophage's "sacrifice this creature unless you sacrifice a Mountain" is the
-- producer: Magical Hack swaps the land type word, and CR 612.2 licenses it
-- because the word is used as a land type.
--
-- Written out field by field rather than as a record update, rewriteComponent's
-- posture one level up: `cost` and `perEach` are the fields a printed word can
-- reach -- `payer` names a player, `branch` and `obligation` name rules
-- categories, and `offeredAt` is CR 608.2e's ordinal -- so a later field carrying
-- one must fail to compile here instead of silently keeping the printed word.
--
-- `perEach` takes rewriteQuantity, rewriteEffect's own descent through a counted
-- amount: a gate scaled by "for each Elf you control" counts what a Magical Hack
-- made an Elf.
rewritePayGate :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> PayGate.PayGate -> PayGate.PayGate
rewritePayGate pairs gate =
  PayGate.MkPayGate
    { PayGate.payer = PayGate.payer gate,
      PayGate.cost = Filter.rewriteCost pairs (PayGate.cost gate),
      PayGate.branch = PayGate.branch gate,
      PayGate.obligation = PayGate.obligation gate,
      PayGate.perEach = fmap (rewriteQuantity pairs) (PayGate.perEach gate),
      PayGate.offeredAt = PayGate.offeredAt gate
    }

-- A single target slot under CR 612.1. Top-level because Pawl.Engine.Resolve
-- needs the same rewrite over a resolving spell's slots (CR 608.2b).
--
-- The slot's `amount` is descended into for the filter's reason: CR 202.3's
-- computed bound is a Quantity, and a Quantity reaches a Count's Filter, so a
-- word swap finds text there exactly as it finds it at the top level. A
-- REGRESSION FENCE rather than a proved behaviour, bakeSlot's posture one module
-- over: no committed bound reaches a Filter -- the pool's bounds name either
-- Quantity.LifeGainedThisTurn or Quantity.InSlot, and neither arm carries one --
-- so no board today tells the two readings apart.
rewriteTargetSlot :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TargetSlot.TargetSlot -> TargetSlot.TargetSlot
rewriteTargetSlot pairs slot =
  slot
    { TargetSlot.filter = fmap (Filter.rewrite pairs) (TargetSlot.filter slot),
      TargetSlot.amount = fmap (rewriteQuantity pairs) (TargetSlot.amount slot)
    }

-- CR 612.1 through a trigger's own condition. Exhaustive rather than a wildcard,
-- so a later condition carrying a Filter fails to compile here instead of
-- silently keeping the printed word.
rewriteTriggerCondition :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> TriggerCondition.TriggerCondition -> TriggerCondition.TriggerCondition
rewriteTriggerCondition pairs condition = case condition of
  TriggerCondition.StateIs c -> TriggerCondition.StateIs (rewriteCondition pairs c)
  TriggerCondition.PermanentEnters f -> TriggerCondition.PermanentEnters (Filter.rewrite pairs f)
  TriggerCondition.CardPutIntoGraveyard f -> TriggerCondition.CardPutIntoGraveyard (Filter.rewrite pairs f)
  TriggerCondition.PermanentDies f -> TriggerCondition.PermanentDies (Filter.rewrite pairs f)
  TriggerCondition.PermanentsDie f -> TriggerCondition.PermanentsDie (Filter.rewrite pairs f)
  TriggerCondition.PermanentLeavesTheBattlefield f -> TriggerCondition.PermanentLeavesTheBattlefield (Filter.rewrite pairs f)
  TriggerCondition.PermanentReturnedToHand f -> TriggerCondition.PermanentReturnedToHand (Filter.rewrite pairs f)
  TriggerCondition.PermanentsReturnedToHand f -> TriggerCondition.PermanentsReturnedToHand (Filter.rewrite pairs f)
  -- The Filter is rewritten and the TurnScope carried through, the SpellCast arm's
  -- reason: a rebuild that dropped the field would reset the trigger to firing on
  -- every turn.
  TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard f scope) -> TriggerCondition.CardLeavesGraveyard (CardLeavesGraveyard.MkCardLeavesGraveyard (Filter.rewrite pairs f) scope)
  -- The Filter is rewritten and the counter kind is not: CR 612.1's pairs swap
  -- SUBTYPE words, and a counter kind names none.
  TriggerCondition.PermanentsGetCounters (CounterPlacement.MkCounterPlacement kind f) -> TriggerCondition.PermanentsGetCounters (CounterPlacement.MkCounterPlacement kind (Filter.rewrite pairs f))
  -- The arm above's per-permanent scope, rewritten the same way and for the same
  -- reason: same payload, and CR 612.1 knows nothing about the scope.
  TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement kind f) -> TriggerCondition.PermanentGetsCounters (CounterPlacement.MkCounterPlacement kind (Filter.rewrite pairs f))
  -- The TurnScope is carried through untouched, not dropped: a rebuild that
  -- forgot the field would reset a text-changed trigger to firing every turn.
  TriggerCondition.SpellCast (SpellCast.MkSpellCast f scope fromZone ordinal) -> TriggerCondition.SpellCast (SpellCast.MkSpellCast (Filter.rewrite pairs f) scope fromZone ordinal)
  TriggerCondition.SelfEnters -> condition
  TriggerCondition.StepBegins {} -> condition
  TriggerCondition.SelfDealsCombatDamageToPlayer -> condition
  TriggerCondition.SelfIsDealtDamage -> condition
  TriggerCondition.PermanentDealsCombatDamageToPlayer f -> TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.rewrite pairs f)
  TriggerCondition.PermanentsDealCombatDamageToPlayer f -> TriggerCondition.PermanentsDealCombatDamageToPlayer (Filter.rewrite pairs f)
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> condition
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> condition
  TriggerCondition.PlayerTookInitiative -> condition
  TriggerCondition.OpponentLostLifeDuringYourTurn -> condition
  TriggerCondition.SelfCycled -> condition
  TriggerCondition.SelfRevealedForMiracle -> condition
  TriggerCondition.SelfDiscarded -> condition
  TriggerCondition.SelfCast -> condition
  TriggerCondition.SelfBecomesTargeted _ -> condition
  TriggerCondition.ControllerBecomesTarget {} -> condition
  TriggerCondition.PlayerDiscards _ -> condition
  TriggerCondition.PlayerCycles _ -> condition
  TriggerCondition.PlayerDrawsNthCard {} -> condition
  TriggerCondition.PlayerBecomesMonarch _ -> condition
  TriggerCondition.SelfAttacks _ -> condition
  TriggerCondition.SelfAttacksWithAnother f -> TriggerCondition.SelfAttacksWithAnother (Filter.rewrite pairs f)
  TriggerCondition.CreatureAttacksAlone f -> TriggerCondition.CreatureAttacksAlone (Filter.rewrite pairs f)
  TriggerCondition.CreatureAttacksYou -> condition
  TriggerCondition.AttachedPlayerIsAttacked -> condition
  TriggerCondition.PlayerAttacks _ -> condition
  -- DESCENDS, where the arm above does not: rule 508.3c's Filter names a
  -- creature type, which is exactly what CR 612.1's text-changing effect
  -- rewrites.
  TriggerCondition.PlayerAttacksWith payload -> TriggerCondition.PlayerAttacksWith payload {PlayerAttacksWith.filter = Filter.rewrite pairs (PlayerAttacksWith.filter payload)}
  -- Two PlayerRelations and no Filter, so nothing here names a creature type
  -- for CR 612.1 to rewrite -- the arm two above's answer.
  TriggerCondition.PlayerAttacksPlayer {} -> condition
  TriggerCondition.SelfAttacksPlayerWithMostLife -> condition
  TriggerCondition.SelfBlocks -> condition
  TriggerCondition.SelfBlocksCreature f -> TriggerCondition.SelfBlocksCreature (Filter.rewrite pairs f)
  TriggerCondition.SelfBlocksAtLeast _ -> condition
  TriggerCondition.SelfBlocksOneOrMore f -> TriggerCondition.SelfBlocksOneOrMore (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesBlocked -> condition
  TriggerCondition.SelfBecomesBlockedBy f -> TriggerCondition.SelfBecomesBlockedBy (Filter.rewrite pairs f)
  TriggerCondition.PermanentBecomesBlockedBy f -> TriggerCondition.PermanentBecomesBlockedBy (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesBlockedByOneOrMore f -> TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.rewrite pairs f)
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> condition
  TriggerCondition.SelfAttacksUnblocked -> condition
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> condition
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> condition
  TriggerCondition.SelfDies -> condition
  TriggerCondition.SelfLeavesTheBattlefield -> condition
  TriggerCondition.HauntedCreatureDies -> condition
  TriggerCondition.SpellOrAbilityCounters _ -> condition
  TriggerCondition.DamageToPlayerPrevented _ -> condition
  TriggerCondition.SelfPreventsDamage f -> TriggerCondition.SelfPreventsDamage (Filter.rewrite pairs f)
  TriggerCondition.PlayerGainsLife _ -> condition
  TriggerCondition.PlayersGainLife _ -> condition
  TriggerCondition.PlayerLosesLife _ -> condition
  TriggerCondition.SelfCountersReached {} -> condition
  TriggerCondition.SelfBecomesClassLevel _ -> condition
  TriggerCondition.SelfLastCounterRemoved _ -> condition
  TriggerCondition.SelfCountersRemoved _ -> condition
  TriggerCondition.SelfHalfUnlocked _ -> condition
  TriggerCondition.RoomFullyUnlocked _ -> condition
  TriggerCondition.AnyOf conditions -> TriggerCondition.AnyOf (fmap (rewriteTriggerCondition pairs) conditions)
  TriggerCondition.SelfTurnedFaceUp -> condition
  -- CR 612.1's text change swaps SUBTYPES here (Pawl.Types.ChangeText's pairs);
  -- CR 701.27e's payload is a face's NAME, which is not one, so nothing in this
  -- condition is rewritten. SelfHalfUnlocked's answer for its own door.
  TriggerCondition.SelfTransformedInto _ -> condition
  TriggerCondition.PermanentTransforms f -> TriggerCondition.PermanentTransforms (Filter.rewrite pairs f)
  TriggerCondition.PermanentTurnedFaceUp f -> TriggerCondition.PermanentTurnedFaceUp (Filter.rewrite pairs f)
  TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d f) -> TriggerCondition.PermanentBecomesDesignated (PermanentBecomesDesignated.MkPermanentBecomesDesignated d (Filter.rewrite pairs f))
  TriggerCondition.SelfEvolves -> condition
  TriggerCondition.AttachedCreatureMentors -> condition
  TriggerCondition.AttachedCreatureDies -> condition
  TriggerCondition.AttachedCreatureBecomesTapped -> condition
  TriggerCondition.SelfBecomesUntapped -> condition
  TriggerCondition.AttachedPermanentTappedForMana -> condition
  TriggerCondition.SelfTrains -> condition
  TriggerCondition.PermanentSacrificed payload -> TriggerCondition.PermanentSacrificed payload {PermanentSacrificed.filter = Filter.rewrite pairs (PermanentSacrificed.filter payload)}
  TriggerCondition.SagaFinalChapterTriggers _ -> condition
  -- CR 603.7's slot name is card data but not card TEXT, so no CR 612.1 swap
  -- reaches it; what the slot holds is read off the projection instead.
  TriggerCondition.LoseControlOfBound _ -> condition
  TriggerCondition.RoomEntered _ -> condition
  TriggerCondition.PlayerScries _ -> condition
  TriggerCondition.RingTemptsPlayer _ -> condition
  TriggerCondition.PlayerBlights _ -> condition
  TriggerCondition.PlayerCompletesDungeon _ -> condition
  TriggerCondition.PlayerSurveils _ -> condition
  TriggerCondition.PlayerRollsDice _ -> condition
  TriggerCondition.PlayerWinsCoinFlip _ -> condition
  TriggerCondition.SelfBecomesPlotted -> condition
  TriggerCondition.PermanentExplores f -> TriggerCondition.PermanentExplores (Filter.rewrite pairs f)
  TriggerCondition.SelfExerted -> condition
  TriggerCondition.SelfBecomesAttachedBy f -> TriggerCondition.SelfBecomesAttachedBy (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesAttachedTo f -> TriggerCondition.SelfBecomesAttachedTo (Filter.rewrite pairs f)
  TriggerCondition.SelfBecomesUnattachedFrom f -> TriggerCondition.SelfBecomesUnattachedFrom (Filter.rewrite pairs f)
  -- CR 603.12's reflexive carries nothing at all, so there is no subtype to swap.
  TriggerCondition.Reflexive -> condition

-- CR 612.1 through Condition's predicate vocabulary. A Condition reached through
-- a CR 611.2b duration comes here by way of rewriteDuration below.
rewriteCondition :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Condition.Type.Condition -> Condition.Type.Condition
rewriteCondition pairs condition = case condition of
  Condition.Type.Compares c ->
    Condition.Type.Compares
      c
        { Compares.measured = rewriteQuantity pairs (Compares.measured c),
          Compares.threshold = rewriteQuantity pairs (Compares.threshold c)
        }
  Condition.Type.Any conditions -> Condition.Type.Any (fmap (rewriteCondition pairs) conditions)
  Condition.Type.All conditions -> Condition.Type.All (fmap (rewriteCondition pairs) conditions)

-- CR 612.1 through a Duration, which Pawl.Types.Duration holds as the card
-- prints it: a CR 611.2b "for as long as ..." clause is rules text like any
-- other, so the words inside its Condition are reachable. Every other arm is a
-- turn-structure window and names none.
--
-- Exhaustive rather than a wildcard, for rewriteTriggerCondition's reason: a new
-- arm carrying a Condition or a Filter must break this build.
rewriteDuration :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Duration.Duration -> Duration.Duration
rewriteDuration pairs duration = case duration of
  Duration.ForAsLongAs condition -> Duration.ForAsLongAs (rewriteCondition pairs condition)
  Duration.UntilEndOfTurn -> duration
  Duration.Indefinite -> duration
  Duration.Perpetual -> duration
  Duration.UntilYourNextTurn -> duration
  Duration.UntilEndOfYourNextTurn -> duration
  Duration.UntilEndOfCombat -> duration
  -- CR 116.2c's price, which is a Cost and not a Condition. An activated
  -- ability's own cost is left alone by this descent for the same reason: no
  -- Cost arm carries a subtype word outside a Filter, and the Filters a cost's
  -- non-mana components hold are matched where the cost is paid.
  Duration.UntilPaid _ -> duration
  Duration.UntilUsed -> duration

-- CR 612.1 through the counters a CR 122.5 move carries: the count is the only
-- place a subtype word can hide, since the kind is a CounterKind and Every,
-- AnyNumber and EachAbsentKind name nothing at all.
rewriteMovedKinds :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> MovedKinds.MovedKinds -> MovedKinds.MovedKinds
rewriteMovedKinds pairs kinds = case kinds of
  MovedKinds.Every -> kinds
  MovedKinds.Named kind quantity -> MovedKinds.Named kind (rewriteQuantity pairs quantity)
  MovedKinds.EveryOfKind _ -> kinds
  MovedKinds.Chosen quantity -> MovedKinds.Chosen (rewriteQuantity pairs quantity)
  MovedKinds.AnyNumber -> kinds
  MovedKinds.AtLeastOne -> kinds
  MovedKinds.AnyNumberOfKind _ -> kinds
  MovedKinds.EachAbsentKind -> kinds
  MovedKinds.UpToOneChosen -> kinds

-- CR 612.1 through a Quantity: a Count's Filter is where the subtype word hides,
-- and its Aggregation may name a further Quantity. Every remaining arm is a leaf.
rewriteQuantity :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Quantity.Type.Quantity -> Quantity.Type.Quantity
rewriteQuantity pairs quantity = case quantity of
  Quantity.Type.Count c ->
    Quantity.Type.Count
      c
        { Count.Type.filter = Filter.rewrite pairs (Count.Type.filter c),
          Count.Type.aggregation = rewriteAggregation pairs (Count.Type.aggregation c)
        }
  Quantity.Type.Plus (Plus.MkPlus x y) -> Quantity.Type.Plus (Plus.MkPlus (rewriteQuantity pairs x) (rewriteQuantity pairs y))
  Quantity.Type.Halved (Halved.MkHalved rounding inner) -> Quantity.Type.Halved (Halved.MkHalved rounding (rewriteQuantity pairs inner))
  Quantity.Type.Negate x -> Quantity.Type.Negate (rewriteQuantity pairs x)
  Quantity.Type.Literal _ -> quantity
  Quantity.Type.ManaValue -> quantity
  Quantity.Type.Power -> quantity
  Quantity.Type.Toughness -> quantity
  Quantity.Type.InSlot _ -> quantity
  Quantity.Type.Star -> quantity
  Quantity.Type.ManaCount _ -> quantity
  Quantity.Type.LifeTotal _ -> quantity
  Quantity.Type.Speed _ -> quantity
  Quantity.Type.IsMonarch _ -> quantity
  Quantity.Type.IsStartingPlayer _ -> quantity
  Quantity.Type.IsActivePlayer _ -> quantity
  Quantity.Type.HasDesignation _ -> quantity
  Quantity.Type.ClassLevel -> quantity
  Quantity.Type.WasKicked -> quantity
  -- A LEAF like WasKicked above, and the Cost it names is deliberately NOT
  -- rewritten: Pawl.Engine.Cast keys the record it stamps off the PRINTED face
  -- (Game.faceOf), so rewriting the identifier here would ask about a cost no
  -- announcement was ever recorded under.
  Quantity.Type.TimesKickedWith _ -> quantity
  Quantity.Type.TagWasSpent {} -> quantity
  Quantity.Type.WasToken -> quantity
  Quantity.Type.WasBlocking -> quantity
  Quantity.Type.DamageDealtToThisTurn -> quantity
  Quantity.Type.PlayerCounters {} -> quantity
  Quantity.Type.ObjectCounters _ -> quantity
  Quantity.Type.ObjectCountersOfAnyKind -> quantity
  Quantity.Type.OpponentsAttacked _ -> quantity
  Quantity.Type.CardsDiscardedThisTurn _ -> quantity
  Quantity.Type.LifeGainedThisTurn _ -> quantity
  Quantity.Type.PlayersDealtDamageThisTurn _ -> quantity
  Quantity.Type.DamageDealtToPlayersThisTurn _ -> quantity
  Quantity.Type.SpellsCastLastTurn _ -> quantity
  Quantity.Type.DungeonsCompleted _ -> quantity
  Quantity.Type.CompletedDungeon {} -> quantity
  Quantity.Type.EnteredThisTurn -> quantity
  Quantity.Type.EnteredFrom _ -> quantity
  Quantity.Type.WasCastFrom _ -> quantity
  Quantity.Type.BlockersBeyondFirst -> quantity
  -- A leaf like Power above: no subtype word to hide, whichever way it reads.
  Quantity.Type.StationMeasure -> quantity
  -- Not a leaf: the payload is a whole Quantity and may hide a Count.
  Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot inner) -> Quantity.Type.AgainstSlot (AgainstSlot.MkAgainstSlot slot (rewriteQuantity pairs inner))
  -- AgainstSlot's answer: not a leaf, and the payload may hide a Count.
  Quantity.Type.AgainstCardsExiledWith inner -> Quantity.Type.AgainstCardsExiledWith (rewriteQuantity pairs inner)

-- CR 612.1 over a damage clause: the recipient's ref, and the amount CR 120.1
-- has that recipient dealt.
rewriteDamagePart :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> DamagePart.DamagePart -> DamagePart.DamagePart
rewriteDamagePart pairs part =
  part
    { DamagePart.ref = rewriteObjectRef pairs (DamagePart.ref part),
      DamagePart.quantity = rewriteQuantity pairs (DamagePart.quantity part)
    }

-- The count alone: a PlayerRef names a rules category (CR 102.1) and not a word
-- rule 612 can swap.
rewritePlayerQuantity :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> PlayerQuantity.PlayerQuantity -> PlayerQuantity.PlayerQuantity
rewritePlayerQuantity pairs x = x {PlayerQuantity.quantity = rewriteQuantity pairs (PlayerQuantity.quantity x)}

-- CR 612.1 over an entry row's counter AMOUNTS -- "enters with a +1/+1 counter
-- for each Goblin you control" is a Count like any other. The keys are left as
-- printed, which is why this needs none of rewriteWithCounters' collision
-- combiner.
--
-- Not implemented: a CR 122.1b keyword counter named in a key keeps its printed
-- keyword (#1190).
rewriteEntryRiders :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> EntryRiders.EntryRiders Quantity.Type.Quantity -> EntryRiders.EntryRiders Quantity.Type.Quantity
rewriteEntryRiders pairs riders = riders {EntryRiders.counters = fmap (rewriteQuantity pairs) (EntryRiders.counters riders)}

-- Greatest is the only Aggregation carrying a Quantity.
rewriteAggregation :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> Aggregation.Aggregation Quantity.Type.Quantity -> Aggregation.Aggregation Quantity.Type.Quantity
rewriteAggregation pairs aggregation = case aggregation of
  Aggregation.Greatest q -> Aggregation.Greatest (rewriteQuantity pairs q)
  Aggregation.Members -> aggregation
  Aggregation.DistinctCardTypes -> aggregation

-- CR 612.1 through CR 208.2a's characteristic-defining power and toughness. Both
-- boxes are rewritten rather than only the one a card fills, since seedCharacteristicPT
-- already keeps them as a pair.
rewriteCharacteristicPT :: [(Subtype.Type.Subtype, Subtype.Type.Subtype)] -> CharacteristicPT.CharacteristicPT -> CharacteristicPT.CharacteristicPT
rewriteCharacteristicPT pairs cda =
  CharacteristicPT.MkCharacteristicPT
    { CharacteristicPT.power = rewriteQuantity pairs (CharacteristicPT.power cda),
      CharacteristicPT.toughness = rewriteQuantity pairs (CharacteristicPT.toughness cda)
    }

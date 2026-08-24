module Pawl.Types.Modification where

import qualified Data.Set as Set
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSlot as TargetSlot

-- | The open-half continuous-effect vocabulary: continuous-effect
-- specifications, classified by layer, distinct from Effect. Within the RULES
-- CORE, Pawl.Engine.Projection is the sole module that may case on a
-- constructor -- the same standing Pawl.Engine.Resolve has over Effect.
-- Pawl.CardSpec's lints also case on it, legitimately: a test-suite lint that
-- walks the card pool is not rules core. GainKeyword and LoseKeyword carry a
-- Keyword, a closed-half CITATION, so casing on it is not an invariant
-- violation; and GainAbility carries a whole open-half ability that nothing cases on beyond CR
-- 113.3's ability KIND -- the projection appends it to one of two lists, and
-- every reader downstream treats it as any other ability. The P/T arms carry
-- records of signed Quantity. The layer-4 arms below reach card types, subtypes
-- and supertypes, which CR 205.4b keeps independent of one another.
--
-- Parametric in `ability` for GainAbility's sake alone, and parametric rather
-- than concrete because this module cannot NAME an ability: an ActivatedAbility
-- carries Effects and Pawl.Types.Effect carries a Pawl.Types.ModifyTarget, which
-- carries one of these. Pawl.Types.StaticAbility and Pawl.Types.ContinuousEffect
-- instantiate the variable at `GrantedAbility card`, which is every position a
-- card's grant reaches; ModifyTarget instantiates it at Void, and says there
-- why.
data Modification ability
  = GainKeyword Keyword.Keyword -- layer 6 (Serpent's Gift)
  | -- | layer 6, CR 613.1f / CR 702.5a: this object gains an enchant ability --
    -- "becomes an Aura enchantment WITH ENCHANT CREATURE", the clause every
    -- printing that turns a permanent into an Aura carries.
    --
    -- Its own arm rather than GainAbility, and legitimately so: enchant is a rule
    -- 702 keyword, hence a closed-half CITATION exactly as the Keyword GainKeyword
    -- carries is. It cannot ride GainAbility in any case -- Pawl.Types.ModifyTarget
    -- instantiates the ability variable at Void (#1642), and this is the arm a
    -- RESOLUTION needs.
    --
    -- Carries a whole TargetSlot rather than a Filter, for Pawl.Types.Face.enchant's
    -- reason: CR 702.5d's enchant-player Auras need the Pool axis, so the printed
    -- field and the granted one are the same shape and
    -- Pawl.Engine.Card.foldEnchant folds them together under CR 702.5c.
    --
    -- APPENDED to the projection's list rather than replacing it (CR 702.5c: "if
    -- an Aura has multiple instances of enchant, all of them apply"), which is
    -- also what keeps a granted instance off the copiable values: the seed carries
    -- the printed instances alone (CR 707.2), the same posture activatedAbilities
    -- takes.
    --
    -- Its Filter is matched by Pawl.Engine.Target.admittedGiven, which fills
    -- `perspective` and `source` and leaves `slotObjects` empty -- so a granted
    -- enchant naming IsBound or SameNameAsBound would be vacuously False, exactly
    -- as a PRINTED one is (#2057). A "ControlledBy You" conjunct, the shape
    -- Old-Growth Troll prints, is answered honestly.
    GainEnchant TargetSlot.TargetSlot
  | -- | layer 6, CR 613.1f: this object gains a whole quoted ability, authored on
    -- the granting card (Presence of Gond's "Enchanted creature has '{T}: Create
    -- a 1/1 green Elf Warrior creature token.'", Sixth Sense's "Enchanted
    -- creature has 'Whenever this creature deals combat damage to a player, you
    -- may draw a card.'").
    --
    -- Folded into the ProjectedCharacteristics list CR 113.3 puts its kind in,
    -- which is what decides everything about the granted ability's identity: it
    -- becomes an ability OF THE RECEIVING OBJECT, so CR 113.7 makes that object
    -- the ability's source, CR 602.2 lets only that object's controller activate
    -- an activated one, CR 603.3a makes that same player the controller of a
    -- triggered one, CR 113.8 makes them the controller of the ability on the
    -- stack, and every binding it names -- the tap cost, IsSource, the counter it
    -- puts on "this creature", the "you" that draws -- resolves against the
    -- receiver. The granting permanent supplies only the text and, via CR 613.7a,
    -- the timestamp.
    --
    -- CR 303.4e says as much outright for the Aura case: "if the Aura grants an
    -- ability to the enchanted object (with 'gains' or 'has'), the enchanted
    -- object's controller is the only one who can activate that ability".
    --
    -- Rejected: keeping the ability anchored to the GRANTER and activating it
    -- there. Presence of Gond on an opponent's creature is the board that refutes
    -- that -- the opponent taps their own creature and gets the token, both of
    -- which a granter-anchored ability gets backwards. Sixth Sense refutes it in
    -- the other half: the card that draws is the enchanted creature's
    -- controller's.
    --
    -- Rejected: an arm per ability kind. The ability variable is the one this
    -- module cannot name, so two arms would need two variables, and every
    -- position instantiating them -- StaticAbility, ContinuousEffect,
    -- ModifyTarget's Void -- would carry both. The sum lives one module out
    -- instead, in Pawl.Types.GrantedAbility.
    GainAbility ability
  | LoseAllAbilities -- layer 6 (Humility)
  | -- | layer 6, CR 613.1f: this object loses the abilities carrying this name
    -- -- "this creature loses this ability", the clause every Licid prints ahead
    -- of animating itself into an Aura.
    --
    -- A NAME and not an index, Pawl.Types.ActivatedAbility's own posture: that
    -- type's header rules an index out for Action.Activate, and the same argument
    -- rules it out here. The name is written on the ability by its own card and
    -- joined back here, so the removal survives a card whose list of abilities is
    -- reordered.
    --
    -- An ACTIVATED ability (ActivatedAbility.name) and a PRINTED REPLACEMENT
    -- (PrintedReplacement.name) are the two carriers of a name, so those are what
    -- this reaches: Gliding Licid removes the first, Glittering Lion the second.
    -- Not implemented: naming a triggered ability, or a static ability whose
    -- continuous effect is not a replacement, for removal (gap #2212).
    --
    -- Distinct from LoseAllAbilities above, and observably so: a Licid keeps its
    -- other printed ability ("Enchanted creature has flying") while losing the
    -- one it activated, which a wipe gets wrong in both directions.
    LoseNamedAbility AbilityName.AbilityName
  | -- | layer 6, CR 613.1f: this object loses this rule-702 keyword -- Sky
    -- Tether's "enchanted creature has defender and loses flying", the clause an
    -- Aura or Equipment prints beside a drawback.
    --
    -- The MIRROR of GainKeyword above, and a closed-half CITATION for the same
    -- reason: the payload is a Keyword, which rule 702 defines, so casing on it
    -- is the act CLAUDE.md licenses rather than the effect-identity case it
    -- forbids.
    --
    -- Distinct from both removals above, and observably so. LoseAllAbilities
    -- would take the enchanted creature's other abilities with the flying, and
    -- LoseNamedAbility reaches only an ability the card that prints it named,
    -- which no printed keyword carries.
    --
    -- Carries a WRITTEN Keyword rather than a KeywordFamily, which is what the
    -- printings ask for: every one that removes a single keyword names the
    -- instance, payload and all -- "loses flying", "loses defender", "loses
    -- protection from black" (Cephalid Snitch), "loses forestwalk" (Scarwood
    -- Hag) -- so a family designator would be a second spelling of the same
    -- removal, the objection KeywordFamily's own header raises against widening
    -- HasKeyword. Not implemented: removing a whole family, which Hammerheim's
    -- "loses all landwalk abilities" is the printing for (#2203).
    LoseKeyword Keyword.Keyword
  | SetBasePowerToughness SetBasePowerToughness.SetBasePowerToughness -- layer 7b (Humility 1/1; Opalescence mana value)
  | ModifyPowerToughness ModifyPowerToughness.ModifyPowerToughness -- layer 7c (Giant Growth +3/+3)
  | SetLandSubtype Subtype.Subtype -- layer 4, CR 305.7 set (Blood Moon -> Mountain)
  | -- | layer 4, CR 613.1d / 305.7: set this object's land subtype to the basic
    -- land type chosen for THIS effect's SOURCE as that source entered
    -- (Object.chosenSubtype). Convincing Mirage's "enchanted land is the chosen
    -- type".
    --
    -- Payload-free because the subtype is DERIVED at projection time from the
    -- source rather than baked into card data, the posture AddChosenColor takes
    -- toward Object.chosenColor: a static ability's modification is card data
    -- and cannot name a type a player will choose, which is why this is a second
    -- constructor rather than a field on SetLandSubtype above.
    --
    -- Carries CR 305.7's ability strip in full, exactly as SetLandSubtype does:
    -- Pawl.Engine.Projection routes both through setLandSubtypeTo, and its
    -- setLandSubtypeEffects answers True for both, so the fold half and the
    -- candidate-list-gate half of that rule cannot drift apart.
    SetLandSubtypeToChosen
  | AddLandSubtype Subtype.Subtype -- layer 4, CR 305.7 add (Urborg -> Swamp)
  | -- | layer 4, CR 205.1a/205.1b set (Turn to Frog -> Frog). A SET over the
    -- CREATURE types only, which is narrower than either land arm above: CR
    -- 205.1b keeps the object's other card types and subtypes and replaces only
    -- its creature types. CR 205.3m is the list of what that reaches.
    --
    -- AddCreatureSubtype sits beside it, the way AddColor sits beside SetColor
    -- below. CR 205.1b allows several creature types and this carries exactly
    -- one, the same narrowing SetLandSubtype takes.
    SetCreatureSubtype Subtype.Subtype
  | -- | layer 4, CR 205.1b add (Life and Limb -> Saproling). The ADD beside the
    -- SET above, standing to it as AddLandSubtype stands to SetLandSubtype: the
    -- object keeps every creature type it had and gains this one.
    --
    -- No ability clause on this arm or on the set above. CR 305.7's strip is the
    -- land arms' alone, which is why neither creature-type arm routes through
    -- setLandSubtypeTo.
    AddCreatureSubtype Subtype.Subtype
  | -- | layer 4, CR 205.1b add over the WHOLE of CR 205.3m: this object gains
    -- every creature type. "Gains all creature types" (Wings of Velis Vel) and
    -- "is every creature type" (Maskwood Nexus) are the two printed wordings.
    --
    -- NULLARY, where AddCreatureSubtype above carries one type: rule 205.3m's
    -- list is the payload, it grows with every set, and no card enumerates it.
    -- A card that could would be authoring the rulebook.
    --
    -- An ADD, so the object keeps its other families (a Vehicle stays a Vehicle)
    -- and its own creature types. That is CR 702.73a's changeling stated as an
    -- ordinary timestamped effect -- which is exactly what the keyword becomes
    -- when another object GRANTS it (CR 604.3a denies the granted instance CDA
    -- status), so Pawl.Engine.Projection.grantedDefiningParts mints this arm.
    AddEveryCreatureSubtype
  | -- | layer 4, CR 613.1d / CR 205.1b add, over the subtype families the two
    -- family-tagged adds above cannot reach: CR 205.3g's artifact types (Ygra,
    -- Eater of All's "other creatures are Food artifacts in addition to their
    -- other types") and CR 205.3h's enchantment types (a permanent that becomes
    -- an Aura). CR 205.3d still refuses a type that corresponds to no card type
    -- the object has, so a card granting one authors the card type first.
    --
    -- UNTAGGED where AddLandSubtype and AddCreatureSubtype carry their family
    -- implicitly, and that is the whole reason those two are not generalised into
    -- this: their constructor IS CR 612.2's gate in
    -- Pawl.Engine.Projection.rewriteModificationWith, which asks whether the word
    -- being swapped is a land type or a creature type. This arm has no such gate
    -- to state, so it is deliberately left unrewritten there. Every printed text
    -- changer swaps a colour word, a basic land type or a creature type, and none
    -- reaches an artifact or enchantment type -- Scryfall
    -- o:"replacing all instances of one", 2026-08-22, twelve cards, all read.
    -- Pawl.CardSpec holds the fence: no AddSubtype in the pool may carry a land
    -- type or a creature type, since those have their own arms and their own gate.
    AddSubtype Subtype.Subtype
  | AddCardType CardType.CardType -- layer 4 (Opalescence -> Creature)
  | -- | layer 4, CR 613.1d / 205.1a set (Song of the Dryads -> land). The SET
    -- beside the add above, standing to it as SetLandSubtype stands to
    -- AddLandSubtype: the new card type REPLACES the ones the object had.
    --
    -- CR 205.1a keeps instant and sorcery through the replacement by name, and
    -- carries the consequence for subtypes: a subtype whose family correlates
    -- only with a card type just removed goes with it (Pawl.Engine.Subtype's
    -- correlatedCardTypes). That is the one thing this arm does which the add
    -- does not, and it is why Pawl.Engine.Projection.modificationWrites gives it
    -- Subtypes as well as Types.
    --
    -- Carries one CardType, the narrowing every layer-4 set here already takes.
    -- A card that set two ("becomes an artifact creature") is CR 205.1b's
    -- retaining phrase instead, which is the ADD twice over -- rule 702.122a's
    -- crew is written that way in Pawl.Engine.Keyword.
    --
    -- No REMOVAL arm beside it: no printed card takes a card type away without
    -- naming the one it becomes, so a remove would be a capability no card
    -- exercises. Gliding Licid stops being a Creature by becoming an
    -- Enchantment, which is this arm and not a removal.
    SetCardType CardType.CardType
  | -- | layer 4, CR 613.1d / 205.4b: this object gains a supertype (Leyline of
    -- Singularity's "All nonland permanents are legendary"). An ADD and never a
    -- set, because CR 205.4b says so outright -- "when an object gains or loses a
    -- supertype, it retains any other supertypes it had" -- so no supertype arm
    -- has the SetLandSubtype/AddLandSubtype pairing the subtypes need.
    --
    -- Carries one Supertype rather than a set, the narrowing SetLandSubtype and
    -- SetCreatureSubtype already take: no printing grants two at once, and a card
    -- that did would author two modifications.
    AddSupertype Supertype.Supertype
  | -- | layer 4, CR 613.1d / 205.4b: this object loses a supertype (Arcum's
    -- Weathervane's "Target snow land is no longer snow"). The removal beside the
    -- grant above, and the same rule governs it: the object's OTHER supertypes
    -- survive, and neither its card types nor its subtypes move.
    RemoveSupertype Supertype.Supertype
  | ChangeSubtypeWord ChangeSubtypeWord.ChangeSubtypeWord -- layer 3, CR 612 (Magical Hack, Artificial Evolution: from -> to)
  | -- | layer 2, CR 613.1b: set this object's controller. The PlayerId is BAKED at
    -- effect creation (CR 611.2c) by Resolve.applyEffect (GainControl) -- it is
    -- the effect's source's controller, never chosen. Applied only by
    -- Projection.controllerOf.
    --
    -- Meant to be runtime-only, and nothing ENFORCES that in the type: the
    -- codec round-trips the PlayerId, so card JSON could author one into an
    -- Effect.ModifyTarget. Baking a PlayerId into static card text is
    -- meaningless, so Pawl.CardSpec lints the pool against it (#199).
    SetController PlayerId.PlayerId
  | -- | layer 2, CR 613.1b: this object's controller becomes the controller of
    -- THIS effect's SOURCE (Control Magic). Payload-free because the player is
    -- DERIVED at projection time, the contrast with SetController above, whose
    -- PlayerId CR 611.2c fixes at resolution. A static ability's modification is
    -- CARD DATA and cannot name a PlayerId, so this is the only shape in which a
    -- printed card can grant control.
    --
    -- CR 303.4e: an Aura's controller and the enchanted object's controller are
    -- separate. Deriving from the SOURCE's controller is what keeps them so --
    -- gaining control of the creature does not gain control of the Aura, and
    -- gaining control of the Aura DOES move the creature.
    SetControllerToSource
  | -- | layer 5, CR 613.1e / 105.3: this object becomes exactly these colours. A
    -- SET, not an add: CR 105.3 says a new colour REPLACES all previous colours
    -- unless the effect says "in addition". SetColor with an empty set is
    -- "becomes colourless" (CR 105.2c).
    SetColor (Set.Set Color.Color)
  | -- | layer 5, CR 613.1e / 105.3: this object becomes these colours IN
    -- ADDITION to the ones it already has -- CR 105.3's parenthetical, so this
    -- unions where SetColor replaces. Indigo Faerie.
    AddColor (Set.Set Color.Color)
  | -- | layer 5, CR 613.1e / 105.3: this object gains, IN ADDITION to its other
    -- colours, the colour chosen for THIS effect's SOURCE as that source entered
    -- (Object.chosenColor). Painter's Servant's "the chosen color".
    --
    -- Payload-free because the colour is DERIVED at projection time from the
    -- source rather than baked into card data: a static ability's modification
    -- cannot name a colour a player will choose, which is why this is a
    -- constructor beside AddColor rather than a value it could carry.
    AddChosenColor
  | -- | layer 7d, CR 613.4d: switch this object's power and toughness. It acts
    -- on whatever 7a, 7b and 7c already produced, not on the printed box.
    -- Carries no payload: two applications return the object to normal for free.
    SwitchPowerToughness
  deriving (Eq, Ord, Show)

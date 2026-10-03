# Reduces MTGJSON's AtomicCards.json to what `pawl ingest` reads (#9): every
# single-faced card as a record it may build, and every face's Oracle text by
# card and face name, which it stamps on the pool's cards. Run as
#   jq -f script/ingest/candidates.jq _scratch/AtomicCards.json > _scratch/candidates.json
{
  version: .meta.version,
  cards: [
    .data[]
    | select(length == 1)
    | .[0]
    | select(.layout == "normal" and (.isFunny // false) == false)
    | {name, manaCost, colorIndicator, supertypes, types, subtypes, power, toughness, text}
  ],
  texts: [.data | to_entries[] | .key as $card | .value[] | {card: $card, name: (.faceName // .name), text: (.text // "")}] | unique
}

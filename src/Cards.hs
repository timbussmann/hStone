module Cards where

import Game;

minions = [
  Card "Faceless Behemoth" 10 (CreateMinion (Minion "Faceless Behemoth" 10 10 False)),
  -- MinionCard (Minion "Captured Jormungar" 5 9 7 False),
  -- MinionCard (Minion "Core Hound" 9 5 7 False),
  -- MinionCard (Minion "Boulderfist Ogre" 6 7 7 False),
  -- MinionCard (Minion "Chillwind Yeti" 4 5 4 False),
  -- MinionCard (Minion "Oasis Snapjaw" 2 7 4 False),
  -- MinionCard (Minion "Magma Rager" 5 1 3 False),
  -- MinionCard (Minion "Blodfen Raptor" 3 2 2 False),
  -- MinionCard (Minion "River Crocolisk" 2 3 2 False),
  -- MinionCard (Minion "Murloc Raider" 2 1 1 False),
   Card "Wisp" 1 (CreateMinion (Minion "Wisp" 1 1 False))
  ]

spells = [
  Card "Divine Spirit" 2 (TargetSpell (public . activePlayer) (\m -> m { mhealth = mhealth m * 2})), --double a minion's health
  Card "Shadow Word: Death" 3 (TargetSpell (\b -> filter (\m -> mpower m >= 5) (public $ inactivePlayer b)) (\m -> m { mhealth = 0 })), -- destroy a minion with 5 power or more
  Card "Arcane Intellect" 3  (Spell (\b -> 
    let p = activePlayer b 
    in b { activePlayer = p {
      hand = hand p ++ take 2 (deck p),
      deck = drop 2 (deck p)
    }})) -- NonTargetSpell "Arcane Intellect" 3, -- draw 2 cards
  -- EnemyTargetSpell "Mindcontrol" 10, -- mindcontrol enemy Minion
  -- NonTargetSpell "Vanish" 6, -- return all minions to the hand
  -- EnemyTargetSpell "Assassinate" 5, -- destroy an enemy minion
  -- NonTargetSpell "Holy Nova" 5, -- deal 2 damage to all ennemies (including hero) and restore 2 health on all own minions (+hero)
  -- EnemyTargetSpell "Fireball" 4, -- deal 6 damage to target
  -- EnemyTargetSpell "Hammer of Wrath" 4, -- deal 3 damage to target and draw a card
  -- NonTargetSpell "Hellfire" 4, -- deal 3 damage to all characters
  -- EnemyTargetSpell "Polymorph" 4, -- transforms a minion into a sheep

  -- NonTargetSpell "Frost Nova" 3, -- freezes all enemy minions,

  --AlliedTargetSpell "Divine Spirit" 2 (\m -> m { mhealth = mhealth m * 2}) (public . activePlayer),  -- destroy a minion with 5 power or more
  --AlliedTargetSpell "Shadow Word: Death" 3 (\m -> m { mhealth = 0 }) (\b -> filter (\m -> mpower m >= 5) (public $ inactivePlayer b)) --double a minion's health
  ]
module Cards where

import Game;

cards = [
  NewCard "Faceless Behemoth" 10 (CreateMinion (Minion "Faceless Behemoth" 10 10 False))
  -- MinionCard (Minion "Captured Jormungar" 5 9 7 False),
  -- MinionCard (Minion "Core Hound" 9 5 7 False),
  -- MinionCard (Minion "Boulderfist Ogre" 6 7 7 False),
  -- MinionCard (Minion "Chillwind Yeti" 4 5 4 False),
  -- MinionCard (Minion "Oasis Snapjaw" 2 7 4 False),
  -- MinionCard (Minion "Magma Rager" 5 1 3 False),
  -- MinionCard (Minion "Blodfen Raptor" 3 2 2 False),
  -- MinionCard (Minion "River Crocolisk" 2 3 2 False),
  -- MinionCard (Minion "Murloc Raider" 2 1 1 False),
  -- MinionCard (Minion "Wisp" 1 1 0 False)
  ]

spells = [
  -- EnemyTargetSpell "Mindcontrol" 10, -- mindcontrol enemy Minion
  -- NonTargetSpell "Vanish" 6, -- return all minions to the hand
  -- EnemyTargetSpell "Assassinate" 5, -- destroy an enemy minion
  -- NonTargetSpell "Holy Nova" 5, -- deal 2 damage to all ennemies (including hero) and restore 2 health on all own minions (+hero)
  -- EnemyTargetSpell "Fireball" 4, -- deal 6 damage to target
  -- EnemyTargetSpell "Hammer of Wrath" 4, -- deal 3 damage to target and draw a card
  -- NonTargetSpell "Hellfire" 4, -- deal 3 damage to all characters
  -- EnemyTargetSpell "Polymorph" 4, -- transforms a minion into a sheep
  -- NonTargetSpell "Arcane Intellect" 3, -- draw 2 cards
  -- NonTargetSpell "Frost Nova" 3, -- freezes all enemy minions,
  NewCard "Divine Spirit" 2 (TargetSpell (public . activePlayer) (\m -> m { mhealth = mhealth m * 2})), --double a minion's health
  NewCard "Shadow Word: Death" 3 (TargetSpell (\b -> filter (\m -> mpower m >= 5) (public $ inactivePlayer b)) (\m -> m { mhealth = 0 })) -- destroy a minion with 5 power or more
  --AlliedTargetSpell "Divine Spirit" 2 (\m -> m { mhealth = mhealth m * 2}) (public . activePlayer),  -- destroy a minion with 5 power or more
  --AlliedTargetSpell "Shadow Word: Death" 3 (\m -> m { mhealth = 0 }) (\b -> filter (\m -> mpower m >= 5) (public $ inactivePlayer b)) --double a minion's health
  ]


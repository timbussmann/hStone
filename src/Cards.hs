module Cards where

import Game;

cards = [
  MinionCard (Minion "Faceless Behemoth" 10 10 10 False),
  MinionCard (Minion "Captured Jormungar" 5 9 7 False),
  MinionCard (Minion "Core Hound" 9 5 7 False),
  MinionCard (Minion "Boulderfist Ogre" 6 7 7 False),
  MinionCard (Minion "Chillwind Yeti" 4 5 4 False),
  MinionCard (Minion "Oasis Snapjaw" 2 7 4 False),
  MinionCard (Minion "Magma Rager" 5 1 3 False),
  MinionCard (Minion "Blodfen Raptor" 3 2 2 False),
  MinionCard (Minion "River Crocolisk" 2 3 2 False),
  MinionCard (Minion "Murloc Raider" 2 1 1 False),
  MinionCard (Minion "Wisp" 1 1 0 False)
  ]

spells = [
  TargetSpell "Mindcontrol" 10, -- mindcontrol enemy Minion
  NonTargetSpell "Vanish" 6, -- return all minions to the hand
  TargetSpell "Assassinate" 5, -- destroy an enemy minion
  NonTargetSpell "Holy Nova" 5, -- deal 2 damage to all ennemies (including hero) and restore 2 health on all own minions (+hero)
  TargetSpell "Fireball" 4, -- deal 6 damage to target
  TargetSpell "Hammer of Wrath" 4, -- deal 3 damage to target and draw a card
  NonTargetSpell "Hellfire" 4, -- deal 3 damage to all characters
  TargetSpell "Polymorph" 4, -- transforms a minion into a sheep
  NonTargetSpell "Arcane Intellect" 3, -- draw 2 cards
  NonTargetSpell "Frost Nova" 3, -- freezes all enemy minions,
  TargetSpell "Shadow Word: Death" 3, -- destroy a minion with 5 power or more
  TargetSpell "Divine Spirit" 2 --double a minion's health
  ]

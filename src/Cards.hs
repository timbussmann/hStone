module Cards where

import Game;

cards = [
  Card "Faceless Behemoth" 10 10 10,
  Card "Captured Jormungar" 5 9 7,
  Card "Core Hound" 9 5 7,
  Card "Boulderfist Ogre" 6 7 7,
  Card "Chillwind Yeti" 4 5 4,
  Card "Oasis Snapjaw" 2 7 4,
  Card "Magma Rager" 5 1 3,
  Card "Blodfen Raptor" 3 2 2,
  Card "River Crocolisk" 2 3 2,
  Card "Murloc Raider" 2 1 1,
  Card "Wisp" 1 1 0
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

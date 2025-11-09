module Life.Game.Patterns
  ( collision
  , eater
  , flower
  , galaxy
  , glider
  , gliderGun
  , gliderSeNw
  , headphones
  , heart
  , octocat
  , p128Oscillator
  , p36Oscillator
  , pulsar
  , rabbits
  , shipsPassing
  , sky
  , spaceship
  )
  where

import Prelude

import Data.Set (Set)
import Data.Tuple.Nested ((/\))
import Life.Types.Grid.Cell (Cell)
import Life.Types.Grid.Cell as C

rabbits :: Array Cell
rabbits = C.fromAscii
  """
    . . # . . . . #
    # # . . . . . .
    . # # . # # # .
  """

glider :: Array Cell
glider = C.fromAscii
  """
    . . #
    # . #
    . # #
  """

gliderSeNw :: Array Cell
gliderSeNw = C.fromAscii
  """
    # # .
    # . #
    # . .
  """

collision :: Array Cell
collision =
  glider <> C.adjust (11 /\ 11) gliderSeNw

galaxy :: Array Cell
galaxy = C.fromAscii
  """
    . . . . . # # # # . .
    . # # . # . . . . # .
    # . . . # . . . . # .
    # . . . . # # . . . .
    # . . # . . . . # # .
    # . . # . . . # . . #
    . # # . . . . # . . #
    . . . . # # . . . . #
    . # . . . . # . . . #
    . # . . . . # . # # .
    . . # # # # . . . . .
  """

sky :: Array Cell
sky = C.fromAscii
  """
    . . . . . . . . . # # . . . . .
    . . . # # # . . # . . # . . . .
    . . # # # . . . . # # . . . . .
    . . . . . . . . . . . . . # # .
    . . . . . . . . . . . . # . . #
    . . . . . . . . . . . . . # # .
    . . . . . # # . . . . . . . . .
    . . . . # . . # . . . . . . . .
    . . . . . # # . . . . . . . . .
    . # # . . . . . . . . # # # . .
    # . . # . . . . . . # # # . . .
    . # # . . . . . . . . . . . . .
  """

flower :: Array Cell
flower = C.fromAscii
  """
    . . . . . . # . . . . . .
    . . . . . # . # . . . . .
    . . . . . # . # . . . . .
    . . . . . . # . . . . . .
    . . . . . . . . . . . . .
    . # # . . . # . . . # # .
    # . . # . # # # . # . . #
    . # # . . . # . . . # # .
    . . . . . . . . . . . . .
    . . . . . . # . . . . . .
    . . . . . # . # . . . . .
    . . . . . # . # . . . . .
    . . . . . . # . . . . . .
  """

spaceship :: Array Cell
spaceship = C.fromAscii
  """
    # . . # .
    . . . . #
    # . . . #
    . # # # #
  """

shipsPassing :: Array Cell
shipsPassing = C.fromAscii
  """
    # . . # . . . . . . . . . . . .
    . . . . # . . . . . . . . . . .
    # . . . # . . . . . . . . . . .
    . # # # # . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . . . . .
    . . . . . . . . . . . # # # # .
    . . . . . . . . . . . # . . . #
    . . . . . . . . . . . # . . . .
    . . . . . . . . . . . . # . . #
  """

pulsar :: Array Cell
pulsar = C.fromAscii
  """
    . . # # . . . . . # # . .
    . . . # # . . . # # . . .
    # . . # . # . # . # . . #
    # # # . # # . # # . # # #
    . # . # . # . # . # . # .
    . . # # # . . . # # # . .
    . . . . . . . . . . . . .
    . . # # # . . . # # # . .
    . # . # . # . # . # . # .
    # # # . # # . # # . # # #
    # . . # . # . # . # . . #
    . . . # # . . . # # . . .
    . . # # . . . . . # # . .
  """

heart :: Array Cell
heart = C.fromAscii
  """
    . . # # # . . . # # # . .
    . # . . . # . # . . . # .
    # . . . . . # . . . . . #
    # . . . . . . . . . . . #
    # # . . . . . . . . . . #
    . # # . . . . . . . . # .
    . . # # . . . . . . # . .
    . . . # # . . . . # . . .
    . . . . # # . . # . . . .
    . . . . . # # # . . . . .
    . . . . . . # . . . . . .
  """

headphones :: Array Cell
headphones = C.fromAscii
  """
    . . . . . # # # # # # . . . . .
    . . . . # # # # # # # # . . . .
    . . . # # . . . . . . # # . . .
    . . . # . . . . . . . . # . . .
    . . . # . . . . . # # . # . . .
    . . . # . . . . # . # . # . . .
    . # # # . . . . . # . . # # # .
    # . . # . # . # . . . . # . . #
    # . . # . # # . . # . . # . . #
    # . . # . . # . # . # . # . . #
    . # # # . . . . # . # . # # # .
    . . . . . . . . . # . . . . . .
  """

octocat :: Array Cell
octocat = C.fromAscii
  """
    . . # # . . . # # .
    . . # . # # # . # .
    . # . . . . . . . #
    . # . . . . . . . #
    . # . . . . . . . #
    . . # . . . . . # .
    # . . # . . . # . .
    . # # . # . # . . .
    . . . # . . . # . .
    . . . # . . . # . .
  """

eater :: Array Cell
eater = C.fromAscii
  """
    # # . .
    # . . .
    . # # #
    . . . #
  """

gliderGun :: Array Cell
gliderGun = C.fromAscii
  """
    . . . . . . . . . . . . . . . . . . . . . . . . # . . . . . . . . . . .
    . . . . . . . . . . . . . . . . . . . . . . # . # . . . . . . . . . . .
    . . . . . . . . . . . . # # . . . . . . # # . . . . . . . . . . . . # #
    . . . . . . . . . . . # . . . # . . . . # # . . . . . . . . . . . . # #
    # # . . . . . . . . # . . . . . # . . . # # . . . . . . . . . . . . . .
    # # . . . . . . . . # . . . # . # # . . . . # . # . . . . . . . . . . .
    . . . . . . . . . . # . . . . . # . . . . . . . # . . . . . . . . . . .
    . . . . . . . . . . . # . . . # . . . . . . . . . . . . . . . . . . . .
    . . . . . . . . . . . . # # . . . . . . . . . . . . . . . . . . . . . .
  """

p36Oscillator :: Set Cell
p36Oscillator = C.fromRLE
  """
    #N p36honeyfarmhassler8.rle
    #C https://conwaylife.com/wiki/Honey_farm_hasslers
    #C https://www.conwaylife.com/patterns/p36honeyfarmhassler8.rle
    x = 42, y = 42, rule = B3/S23
    18bo$16bobo$17bobob3o$17bo$9b2o14bob2o$10bob2o4bo9bo$8bo7bo2bo6b2o$8b
    2o2bobobobob6o2b3o$13bo13bo2bo3b2o$28b2o4bo2bo$36b2o$7b2o$7bobo14b2o8b
    obo$4b2obobo14b2o7bo2bo$4bobobo15b2o8bo$6bo$4bo2bo4b3obo6b3o8b2o4bo$7b
    o4b3obo21b2o$2bo4bo8bo17bobo3b2o$2bo4bo27bo3bo$2bo4bo26bo$7bo26bo4bo$
    2bo3bo27bo4bo$2o3bobo17bo8bo4bo$2b2o21bob3o4bo$bo4b2o8b3o6bob3o4bo2bo$
    35bo$7bo8b2o15bobobo$5bo2bo7b2o14bobob2o$5bobo8b2o14bobo$33b2o$4b2o$4b
    o2bo4b2o$6b2o3bo2bo13bo$11b3o2b6obobobobo2b2o$14b2o6bo2bo7bo$13bo9bo4b
    2obo$13b2obo14b2o$24bo$18b3obobo$23bobo$23bo!
  """

p128Oscillator :: Set Cell
p128Oscillator = C.fromRLE
  """
    #N p128honeyfarmhassler.rle
    #C https://conwaylife.com/wiki/Honey_farm_hasslers
    #C https://www.conwaylife.com/patterns/p128honeyfarmhassler.rle
    x = 39, y = 46, rule = B3/S23
    2o3bo27bo3b2o$4bob2o23b2obo$5b3o23b3o$bo6bo21bo6bo$obobobo2bo19bo2bobo
    bobo$b2obob3o21b3obob2o2$b2obob3o21b3obob2o$obobobo2bo19bo2bobobobo$bo
    6bo21bo6bo$5b3o23b3o$4bob2o23b2obo$2o3bo27bo3b2o3$12b3o9b3o$11bo3bo7bo
    3bo$10bo5bo5bo5bo$10bo5bo5bo5bo$10bo5bo5bo5bo$11bo3bo7bo3bo$12b3o9b3o
    3$12b3o9b3o$11bo3bo7bo3bo$10bo5bo5bo5bo$10bo5bo5bo5bo$10bo5bo5bo5bo$
    11bo3bo7bo3bo$12b3o9b3o3$2o3bo27bo3b2o$4bob2o23b2obo$5b3o23b3o$bo6bo
    21bo6bo$obobobo2bo19bo2bobobobo$b2obob3o21b3obob2o2$b2obob3o21b3obob2o
    $obobobo2bo19bo2bobobobo$bo6bo21bo6bo$5b3o23b3o$4bob2o23b2obo$2o3bo27b
    o3b2o!
  """

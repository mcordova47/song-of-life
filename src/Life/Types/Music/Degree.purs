module Life.Types.Music.Degree where

import Prelude hiding (degree)

import Control.Monad.Rec.Class (Step(..), loop3, tailRec3)
import Life.Types.Music.Note (Note, (\\))
import Life.Types.Music.Note as Note
import Life.Types.Music.PitchClass (PitchClass)
import Life.Types.Music.PitchClass as PitchClass
import Life.Utils ((>>>>))

type Degree = PitchClass -> Note -> Note

degree :: Int -> Degree
degree = tailRec3 go
  where
    go degrees key note@(pitchClass \\ octave')
      | degrees > 0 = loop3 (degrees - 1) key (Note.inc note)
      | degrees < 0 = loop3 (degrees + 1) key (Note.dec note)
      | otherwise = Done $ PitchClass.fix key pitchClass \\ octave'

tonal :: Degree
tonal = degree 0

second :: Degree
second = degree 1

third :: Degree
third = degree 2

fourth :: Degree
fourth = degree 3

fifth :: Degree
fifth = degree 4

sixth :: Degree
sixth = degree 5

seventh :: Degree
seventh = degree 6

octave :: Degree
octave = degree 7

pow :: Int -> Degree -> Degree
pow = go tonal
  where
    go acc n deg
      | n <= 0 = acc
      | otherwise = go (deg >>>> acc) (n - 1) deg

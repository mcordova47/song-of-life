module Life.Icons
  ( Props
  , arrowBarRight
  , dice
  , externalLink
  , github
  , pause
  , play
  , sawtoothWave
  , share
  , sineWave
  , squareWave
  , trash
  , triangleWave
  )
  where

import Data.Function.Uncurried (Fn1, runFn1)
import Elmish (ReactElement)

type Props = { size :: Int }

arrowBarRight ∷ Props -> ReactElement
arrowBarRight = runFn1 arrowBarRight_

dice ∷ Props -> ReactElement
dice = runFn1 dice_

externalLink ∷ Props -> ReactElement
externalLink = runFn1 externalLink_

github ∷ Props -> ReactElement
github = runFn1 github_

pause ∷ Props -> ReactElement
pause = runFn1 pause_

play ∷ Props -> ReactElement
play = runFn1 play_

trash ∷ Props -> ReactElement
trash = runFn1 trash_

-- Custom

share :: Props -> ReactElement
share = runFn1 share_

sineWave :: Props -> ReactElement
sineWave = runFn1 sineWave_

squareWave :: Props -> ReactElement
squareWave = runFn1 squareWave_

triangleWave :: Props -> ReactElement
triangleWave = runFn1 triangleWave_

sawtoothWave :: Props -> ReactElement
sawtoothWave = runFn1 sawtoothWave_

-- Foreign

foreign import arrowBarRight_ :: Fn1 Props ReactElement
foreign import dice_ :: Fn1 Props ReactElement
foreign import externalLink_ :: Fn1 Props ReactElement
foreign import github_ :: Fn1 Props ReactElement
foreign import pause_ :: Fn1 Props ReactElement
foreign import play_ :: Fn1 Props ReactElement
foreign import trash_ :: Fn1 Props ReactElement

foreign import share_ :: Fn1 Props ReactElement
foreign import sineWave_ :: Fn1 Props ReactElement
foreign import squareWave_ :: Fn1 Props ReactElement
foreign import triangleWave_ :: Fn1 Props ReactElement
foreign import sawtoothWave_ :: Fn1 Props ReactElement

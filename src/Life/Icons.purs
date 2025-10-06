module Life.Icons
  ( Props
  , arrowBarRight
  , externalLink
  , github
  , pause
  , play
  , trash
  )
  where

import Data.Function.Uncurried (Fn1, runFn1)
import Elmish (ReactElement)

type Props = { size :: Int }

arrowBarRight ∷ Props -> ReactElement
arrowBarRight = runFn1 arrowBarRight_

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

foreign import arrowBarRight_ :: Fn1 Props ReactElement
foreign import externalLink_ :: Fn1 Props ReactElement
foreign import github_ :: Fn1 Props ReactElement
foreign import pause_ :: Fn1 Props ReactElement
foreign import play_ :: Fn1 Props ReactElement
foreign import trash_ :: Fn1 Props ReactElement

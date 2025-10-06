module Feather
  ( Props
  , arrowRightCircle
  , externalLink
  , github
  , pauseCircle
  , playCircle
  , refreshCcw
  )
  where

import Elmish (ReactElement, createElement')
import Elmish.React.Import (ImportedReactComponent)

type Props = { size :: Int }

arrowRightCircle ∷ Props -> ReactElement
arrowRightCircle = createElement' arrowRightCircle_

externalLink ∷ Props -> ReactElement
externalLink = createElement' externalLink_

github ∷ Props -> ReactElement
github = createElement' github_

pauseCircle ∷ Props -> ReactElement
pauseCircle = createElement' pauseCircle_

playCircle ∷ Props -> ReactElement
playCircle = createElement' playCircle_

refreshCcw ∷ Props -> ReactElement
refreshCcw = createElement' refreshCcw_

foreign import arrowRightCircle_ :: ImportedReactComponent
foreign import externalLink_ :: ImportedReactComponent
foreign import github_ :: ImportedReactComponent
foreign import pauseCircle_ :: ImportedReactComponent
foreign import playCircle_ :: ImportedReactComponent
foreign import refreshCcw_ :: ImportedReactComponent
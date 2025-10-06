module Feather
  ( Props
  , externalLink
  , github
  )
  where

import Elmish (ReactElement, createElement')
import Elmish.React.Import (ImportedReactComponent)

type Props = { size :: Int }

externalLink ∷ Props -> ReactElement
externalLink = createElement' externalLink_

github ∷ Props -> ReactElement
github = createElement' github_

foreign import externalLink_ :: ImportedReactComponent
foreign import github_ :: ImportedReactComponent

module Life.Components.Header
  ( id
  , view
  )
  where

import Prelude

import Elmish (ReactElement)
import Elmish.HTML.Styled as H
import Life.Components.Icons as I

view ∷ ReactElement
view =
  H.div "w-100 bg-lightblue" $
    H.div "container d-flex justify-content-between align-items-center py-2"
    [ H.h1_ "d-inline-flex align-items-center mb-0"
      { id: id }$
      [ H.a_ "text-salmon hover:text-salmon-highlight text-decoration-none"
          { href: "/" } $
          H.img_ "hover:bright" { src: "/assets/images/logo.svg", style: H.css { height: "2.5rem" } }
      , H.a_ "text-salmon hover:text-salmon-highlight text-decoration-none ms-3"
          { href: "/" }
          "Songs of Life"
      ]
    , H.a_ "hover:bright"
        { href: "https://github.com/mcordova47/song-of-life"
        , target: "_blank"
        , title: "GitHub"
        } $
        I.github { size: 32 }
    ]

id :: String
id = "site-header"

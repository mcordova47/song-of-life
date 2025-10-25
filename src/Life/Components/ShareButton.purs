module Life.Components.ShareButton where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Traversable (fold, traverse)
import Effect (Effect)
import Elmish (ReactElement)
import Elmish.Dispatch as E
import Elmish.HTML.Styled as H
import Life.Types.Life (class VisibleLife)
import Life.Types.Life as Life
import Life.Types.Preset (PresetV0')
import Life.Types.Preset as Preset
import Life.Types.Route as Route
import Promise as Promise
import Web.Clipboard as Clipboard
import Web.HTML (window)
import Web.HTML.Location as Loc
import Web.HTML.Window (location, navigator)

type Args f r = PresetV0'
  ( game :: f Boolean
  , shareHash :: Maybe String
  | r
  )

type Events =
  { onCopied :: Effect Unit
  }

view :: forall f r. VisibleLife f => String -> Args f r -> Events -> ReactElement -> ReactElement
view className args { onCopied } =
  H.button_ className
    { onClick: E.handleEffect do
        origin <- window >>= location >>= Loc.origin
        path <- window >>= location >>= Loc.pathname
        _ <-
          window >>= navigator >>= Clipboard.clipboard
            >>= traverse \clipboard ->
              clipboard
                # Clipboard.writeText (shareText origin path)
                >>= Promise.then_ \_ -> do
                  onCopied
                  pure $ Promise.resolve unit
        pure unit
    , title: "Share"
    }
  where
    shareText origin path = fold
      [ "Made with Songs of Life\n\n"
      , Life.render
          { life: shareGame
          , rows: 6
          , cols: 6
          , renderRow: (_ <> "\n")
          , renderCol: \living -> if living then "🟪" else "⬜️"
          }
      , "\n"
      , shareUrl origin path
      ]

    shareGame = args.shareHash >>= Route.decode # case _ of
      Just (Route.Share p) -> Preset.toLife p
      _ -> args.game

    shareHash =
      Route.encode $ Route.Share $ Preset.fromState args

    shareUrl origin path =
      origin <> path <> "#/" <> case args.shareHash of
        Just hash -> hash
        Nothing -> shareHash

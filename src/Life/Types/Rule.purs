module Life.Types.Rule
  ( Rule
  , life
  )
  where

type Rule = Boolean -> Int -> Boolean

life :: Rule
life _ 3 = true
life b 2 = b
life _ _ = false

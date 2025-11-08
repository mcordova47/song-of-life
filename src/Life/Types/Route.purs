module Life.Types.Route
  ( Route(..)
  , codec
  , decode
  , encode
  )
  where

import Data.Codec as C
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Life.Types.Codec (Codec, (/>))
import Life.Types.Codec as Codec
import Life.Types.Preset (Preset)
import Life.Types.Preset as Preset

-- | Most routes are defined as separate entrypoints which are turned into
-- | separate html pages. These routes are for client-side routing, like sharing
-- | a given preset (living/dead cells, sound wave type, key, etc.).
newtype Route
  = Share Preset
derive instance Newtype Route _

codec :: Codec String Route
codec = wrapIso Share (Codec.literal "s" /> Preset.codec)

encode :: Route -> String
encode = C.encode codec

decode :: String -> Maybe Route
decode = C.decode codec


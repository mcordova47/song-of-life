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

newtype Route
  = Share Preset
derive instance Newtype Route _

codec :: Codec String Route
codec = wrapIso Share (Codec.literal "s" /> Preset.codec)

encode :: Route -> String
encode = C.encode codec

decode :: String -> Maybe Route
decode = C.decode codec


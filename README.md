# Songs of Life

`song-of-life` uses Conway’s Game of Life to generate music. It’s hosted at [songsof.life](https://songsof.life).

## The Idea

Click some cells to change the starting conditions, then press play and Conway’s *Game of Life* will play out. Each row corresponds to a note and each column is a beat in a measure. Each beat will play and then the living cells will change and the next measure will play.

## The Tech

Built with [PureScript](https://www.purescript.org/) using the [Elmish](https://pursuit.purescript.org/packages/purescript-elmish/0.13.0) framework. There is no backend.

## Things of Note

### Game Engine

Of course, there is the GoL engine itself. Most of that code can be found under [Life/Game](src/Life/Game/) or [Life/Types/Game](src/Life/Types/Game/). There are several different implementations, which instantiate the typeclasses defined in [Life.Types.Game.Life](src/Life/Types/Game/Life.purs).

There are three comonadic implementations, which implement the `CellularComonad` typeclass and get `Automaton` instances for free. Then there is a more optimized implementation ([Life.Types.Game.Engines.Optimized.Unbounded](src/Life/Types/Game/Engines/Optimized/Unbounded.purs)) that uses the FFI and mutable sets for better performance.

There are also some types for defining different rules besides Life.

### Music

This was my first time using the [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API). Learned a few things like avoiding pops and distortion with exponential ramp-ups/-downs.

A PureScript music API can be found in [Life/Types/Music](src/Life/Types/Music/). There are types for pitch classes, notes, chords, intervals, note durations, etc.

As far as generating music, there are two appraoches, so far. The first is the more straightforward approach: the y-axis corresponds to notes and the x-axis is time (one column per beat). The next generation of the automaton is a new measure.

For the second version ([EntryPoints.Cascade](src/EntryPoints/Cascade.purs)), I really wanted to:

1. Use a larger grid
2. Make time only correspond to steps instead of an axis

It felt like this would better capture the chaotic spirit of cellular automata. What I landed on was:

- A background chord is determined by the row with the most living cells
- For melody and harmony
  - The grid is broken up into clusters and I look for the two most active clusters (i.e. where the most cells were newly born)
  - I take the discrete geometric median cell in both clusters and those determine the note, based on distance from the edge of the grid

I still want to experiment with other methods, but those two have been interesting so far.

### Visualization

For the smaller grid, aside from some tricky UX design problems, it was fairly simple — just a grid of divs. Though I did make an abstraction for folding over any `VisibleAutomaton`, which cleaned up the code a bit.

For the larger grid, the performance of rendering all those divs was pretty bad, so I decided to use canvas. I made a sort of Elm-like wrapper around canvas, defined in [Life.Components.Scene](src/Life/Components/Scene.purs). It handles the imperative render loop / update loops and uses mutable refs for the state and props, but the consumer just passes and update function and a declarative view function.

[Life.Components.GridScene](src/Life/Components/GridScene.purs) was written on top of it and renders a cellular automaton grid that you can drag around, zoom in, and toggle cells in.

### Serialization

#### Links

I wanted the ability to share small links for starting seeds / configuration, but there’s no backend, so that meant encoding. Of course, the simplest way would be url parameters, but if the grid is too big and there are a lot of living cells, that can get really unwieldy. So the next idea was to use some off-the-shelf compression tools, and run the JSON representation of the state through them. Unfortunately these tools aren’t great for data this small, so I ended up implementing something from scratch.

I used [`purescript-codec`](https://pursuit.purescript.org/packages/purescript-codec/6.1.0) to write codecs for encoding/decoding the state. I think there are some codecs I could compose a bit better, but overall it feels pretty nice. The main codec is in [`Life.Types.Route`](src/Life/Types/Route.purs), which is composed of other smaller codecs. I made some combinator codecs in [`Life.Types.Codec`](src/Life/Types/Codec.purs). The routes are versioned so that old links don’t rot.

The algorithm for encoding itself was interesting to work on. Recognizing that the seeds often have a long runs of living or dead cells, the idea I went with was similar to SVG paths — a set of instructions encoded as a string. I basically come up with a bounding box for the living cells, convert all the living cells to an index mapped to that bounding box, then encode the instructions. E.g. `15c0.1/o1m178o2` translates to `15 columns; starting at (0, 1); turn on 1, move 178, turn on 2`.

So that’s v0 and then for v1 I decided to compress it a bit more. `Move n` is encoded as a base 26 number converted to a lowercase string, and likewise with `TurnOn n` being an uppercase string. So the above example becomes `15c0.1/AfvB`.

#### Parsing Pre-Defined Patterns

After rolling my own custom encoding, I learned that there is a standard format — RLE (Run Length Encoding) — specifically for this purpose. It turned out to be very similar to my original implementation (o1m3o3 -> o2b3o!). I implemented a parser for RLE and a simple ASCII-art parser, both for convenience of more easily defining patterns, which can often be downloaded as .rle files.

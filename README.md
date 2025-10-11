# Songs of Life

`song-of-life` uses Conway’s Game of Life to generate music. It’s hosted at [songsof.life](https://songsof.life).

## The Idea

Click some cells to change the starting conditions, then press play and Conway’s *Game of Life* will play out. Each row corresponds to a note and each column is a beat in a measure. Each beat will play and then the living cells will change and the next measure will play.

## The Tech

Built with [PureScript](https://www.purescript.org/) using the [Elmish](https://pursuit.purescript.org/packages/purescript-elmish/0.13.0) framework. There is no backend.

## Things of Note

### Game Engine

Of course, there is the GoL engine itself, which was pretty straightforward and is in the [`Life.Game`](src/Life/Game.purs) module.

### Music

This was my first time using the [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API). Learned a few things like avoiding pops and distortion with exponential ramp-ups/-downs.

### Link Sharing

I spent a while working on link sharing. I wanted the ability to share small links for starting seeds / configuration, but there’s no backend, so that meant encoding. Of course, the simplest way would be url parameters, but if the grid is too big and there are a lot of living cells, that can get really unwieldy. So the next idea was to use some off-the-shelf compression tools, and run the JSON representation of the state through them. Unfortunately these tools aren’t great for data this small, so I ended up implementing something from scratch.

I used [`purescript-codec`](https://pursuit.purescript.org/packages/purescript-codec/6.1.0) to write codecs for encoding/decoding the state. It was pretty nice once I learned to use it. The main codec is in [`Life.Types.Route`](src/Life/Types/Route.purs), which is composed of other smaller codecs. I made some combinator codecs in [`Life.Types.Codec](src/Life/Types/Codec.purs). The routes are versioned so that old links don’t rot.

The algorithm for encoding itself was interesting to work on. Recognizing that the seeds often have a long runs of living or dead cells, the idea I went with was similar to SVG paths — a set of instructions encoded as a string. I basically come up with a bounding box for the living cells, convert all the living cells to an index mapped to that bounding box, then encode the instructions. E.g. `15c0.1/o1m178o2` translates to `15 columns; starting at (0, 1); turn on 1, move 178, turn on 2`.

So that’s v0 and then for v1 I decided to compress it a bit more. `Move n` is encoded as a base 26 number converted to a lowercase string, and likewise with `TurnOn n` being an uppercase string. So the above example becomes `15c0.1/AfvB`.

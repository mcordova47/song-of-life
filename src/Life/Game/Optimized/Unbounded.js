const NEIGHBOR_OFFSETS = [
  -0x10001, -0x10000, -0x0ffff,
  -1,                 +1,
  +0xffff,  +0x10000, +0x10001
]

export const steps_ = (rule, n, cells) => {
  let set = cellSet(cells)
  for (let i = 0; i < n; i++) {
    set = step(rule, set)
  }
  return fromCellSet(set)
}

const step = (rule, cells) => {
  const next = new Set(cells)
  for (const c of relevantCells(cells)) {
    stepCell(rule, c, cells, next)
  }
  return next
}

const stepCell = (rule, cell, cells, next) => {
  const living = rule(cells.has(cell), livingNeighbors(cell, cells))
  if (living) next.add(cell)
  else next.delete(cell)
}

const pack = ({ row, col }) => (row << 16) | (col & 0xffff)
const unpack = i => ({ row: i >> 16, col: i & 0xffff })

const cellSet = arr => new Set(arr.map(pack))
const fromCellSet = set => [...set].map(unpack)

const neighbors = cell => NEIGHBOR_OFFSETS.map(o => cell + o)

const relevantCells = cells => {
  const rel = new Set(cells)
  for (const c of cells) union(rel, neighbors(c))
  return rel
}

const union = (a, b) => {
  for (const x of b) a.add(x)
  return a
}

const livingNeighbors = (cell, cells) => {
  let acc = 0
  for (const n of neighbors(cell)) {
    if (cells.has(n)) acc++
  }
  return acc
}

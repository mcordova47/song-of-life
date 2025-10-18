export const steps_ = (n, cells) => {
  let set = cellSet(cells)
  for (let i = 0; i < n; i++) {
    set = step(set)
  }
  return fromCellSet(set)
}

const step = cells => {
  const next = new Set(cells)
  relevantCells(cells).forEach(c => stepCell(c, cells, next))
  return next
}

const stepCell = (cell, cells, next) => {
  switch (livingNeighbors(cell, cells)) {
    case 3:
      next.add(cell)
      break
    case 2:
      break
    default:
      next.delete(cell)
  }
  return next
}

const pack = ({ row, col }) => (row << 16) | col

const unpack = i => ({ row: i >> 16, col: i << 16 >> 16 })

const cellSet = arr => new Set(arr.map(pack))

const fromCellSet = set => [...set].map(unpack)

const neighbors = cell => {
  const neighborOffsets = [
    -0x10001, -0x10000, -0x0ffff,
    -1,                   +1,
    +0xffff,  +0x10000,  +0x10001
  ]

  return neighborOffsets.map(offset => cell + offset)
}

const relevantCells = cells => {
  const rel = new Set(cells)
  cells.forEach(c => {
    union(rel, neighbors(c))
  })
  return rel
}

fromCellSet(neighbors(pack({ row: 1, col: 1 })))

const union = (a, b) => {
  b.forEach(x => a.add(x))
  return b
}

const livingNeighbors = (cell, cells) => {
  let acc = 0
  neighbors(cell).forEach(c => {
    if (cells.has(c)) {
      acc += 1
    }
  })
  return acc
}

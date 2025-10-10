export const decodeGridParts_ = s => {
  const matches = s.match(/^([0-9]+)c([0-9]+)\.([0-9]+)(.*)$/)
  if (matches.length < 5) return

  return {
    cols: matches[1],
    row: matches[2],
    col: matches[3],
    instructions: matches[4]
  }
}

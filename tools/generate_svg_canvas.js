const inc = n => n == null ? 0 : n + 1
const range = (size, acc = []) => size <= 0 ? acc : range(size - 1, [...acc, inc(acc[acc.length - 1])])
const svg = size =>
  range(size)
  .map(y => range(size).map(x => `  <path d="M${x * 5} ${y * 5}v4h4v-4z" fill="rgb(248, 249, 250)"/>`).join("\n"))
  .join("\n")
